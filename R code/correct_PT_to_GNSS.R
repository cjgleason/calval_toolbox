correct_pt_to_gnss= function(raw_pt_file,pt_key_file,dist_thresh,time_thresh,pt_data_directory,
                             gnss_drift_data_directory,QA_QC_pt_output_directory, flagged_pt_output_directory,
                             gnss_sd_thresh,offset_sd_thresh,change_thresh_15_min){
  
  library(dplyr)
  library(tidyr)
  library(fuzzyjoin)
  library(geodist)
  library(bayesbio)
  library(ncdf4)
  
  filename=raw_pt_file
  
  handle_raw_pt=function(raw_pt_file,pt_key_file,pt_data_directory,gnss_drift_data_directory){
    #read in raw pt--------------
    #ID could come from the filename, right now it is reading the serial number from the file
    
    #kluge that quickly gets what we want by reading in the csv flat and pulling the first entry
    pt_serial = strtoi(as.character(read.csv(paste0(pt_data_directory,raw_pt_file,'.csv'))$Serial_number.[1]))
    
    #11 lines of headers to skip to deal with read in function
    
    #!!!!!!!!!!!!!!
    #The willamette data have a missing header, so we skip the 12th line and assign our own header. Deadly !
    #!!!!!!!!!!!!!
    
    pt_data= read.csv(paste0(pt_data_directory,raw_pt_file,'.csv'),skip=12,header=TRUE,fill=TRUE,col.names = c('Date','Time','ms','Level','Temperature')) %>%
      mutate(pt_serial=pt_serial)
    #----------------------------
    
    #read in offset--------------
    keyfile=read.csv(pt_key_file,stringsAsFactors = FALSE)%>%
      mutate('driftID'= final_log_file)%>%
      mutate(pt_serial=PT_Serial)
    #left joining the key in 'offset' gets us a tidy data frame where the info from the key is promulgated to just that pt. a right join would give
    #an n fold expansion across n pts
    pt_data=pt_data %>%
      left_join(keyfile,by='pt_serial')%>%
      mutate(datetime= as.POSIXct(paste(Date,Time),format= "%Y/%m/%d %H:%M:%S") )%>%
      mutate(pt_Lat= Lat_WGS84)%>%
      mutate(pt_Lon=Long_WGS84)%>%
      mutate(pt_time_UTC=datetime)
    
    
    #----------------------------
  } # end pt function
  correct_pt=function(prepped_pt,dist_thresh,time_thresh,gnss_drift_data_directory){
    
    
    log_files= unique(prepped_pt$driftID) #the second from the join.
    if(length(log_files)==0){
      return(NA)
    }
    
    if(is.na(log_files)){
      return(NA)
    }
    
    unit_pt_process = function(log_file,prepped_pt,dist_thresh,time_thresh,gnss_drift_data_directory){
      
      gnss_log=read.csv(paste0(gnss_drift_data_directory,log_file,'.csv'),header=TRUE,stringsAsFactors = FALSE)%>%
        mutate(datetime=gnss_time_UTC)%>%
        mutate(datetime=as.POSIXct(datetime))#needed as when it gets written to csv it becomes not a posix object
      
      #half a second faster to join first on time and then on space
      clean_pt_time=difference_inner_join(prepped_pt,gnss_log,by='datetime',max_dist=time_thresh)
      
      #need lon then lat
      distance_m=geodist(cbind(clean_pt_time$pt_lon, clean_pt_time$pt_lat), cbind(clean_pt_time$gnss_Lon, clean_pt_time$gnss_Lat), paired=TRUE,measure='haversine')
      
      
      clean_pt=cbind(clean_pt_time,distance_m)%>%
        filter(distance_m<dist_thresh)%>%
        #ok, let's select for stuff we want to keep now
        transmute(pt_level=pt_level, temperature=temperature,gnss_time_UTC=gnss_time_UTC,gnss_wse=gnss_wse,pt_time_UTC=pt_time_UTC,
                  pt_serial=pt_serial,
                  gnss_lat=gnss_Lat, gnss_lon=gnss_Lon, pt_lat=pt_lat, pt_lon=pt_lon,gnss_pt_dist_m=distance_m )
      
      
      #next, ensure there are enough gnss points to make a valid offset correction
      if(nrow(clean_pt)==0){
        return(NA)
        
      }
      
      #now, multiple gnss points go to one pt. Need to group by pt timestwp and apply the correction at that level
      #the differneces in these offsets is the uncertainty of this mapping
      
      
      offset_pt_mean=clean_pt%>%
        group_by(pt_time_UTC)%>%
        mutate( offset=(gnss_wse-pt_level))%>%
        summarize(pt_correction= mean(offset))
      
      offset_pt_sd =clean_pt%>%
        group_by(pt_time_UTC)%>%
        mutate( offset=(gnss_wse-pt_level))%>%
        summarize(pt_correction_sd=sd(offset))
      
      
      wse_pt=clean_pt%>%
        left_join(offset_pt_mean,by='pt_time_UTC')%>%
        left_join(offset_pt_sd,by='pt_time_UTC')%>%
        mutate(pt_wse=pt_level+pt_correction)%>%
        mutate(pt_wse_sd= pt_correction_sd + 0.001) #sets the uncertainty to equal to the variance in all of the offsets used to create the 
      #PT wse, plus 1mm for PT meas error
      
      
      return(wse_pt)
      
      
      
    }
    
    
    
    #loop through the log files, find the right gnss data to associate with the install,
    # average the gnss heigh within a distance theshold, and then create a corrected pt df
    #by binding each log file together. This is uncessary, but will handle that one special case
    finalpt=do.call(rbind,lapply(log_files,unit_pt_process,prepped_pt=prepped_pt,dist_thresh=dist_thresh,
                                 time_thresh=time_thresh,gnss_drift_data_directory=gnss_drift_data_directory))
    
    
    
  }
  
  #read in pt
  prepped_pt=handle_raw_pt(raw_pt_file,pt_key_file,pt_data_directory,gnss_drift_data_directory)%>%
    transmute(pt_time_UTC=pt_time_UTC,pt_lat=pt_Lat,pt_lon=pt_Lon,
              pt_install_UTC=as.POSIXct(paste(Date_Install,Time_Install_UTC),format= "%m/%d/%Y %H:%M"),
              pt_uninstall_UTC=as.POSIXct(paste(Date_Uninstall,Time_Uninstall),format= "%m/%d/%Y %H:%M"),
              install_method=Install_method, pt_serial=pt_serial,
              pt_level=Level,temperature=Temperature,driftID=driftID,datetime=pt_time_UTC)
  
  if(sum(is.na(prepped_pt$pt_lat))==nrow(prepped_pt)){
    print('this pt isnt in the key')
    saveRDS(prepped_pt,file=paste0(flagged_pt_output_directory,filename,'_',prepped_pt$pt_serial[1],'.rds'))
    return(NA)
  }
  
  
  
  #make gnss offset
  offset_pt=correct_pt(prepped_pt,dist_thresh,time_thresh,gnss_drift_data_directory)
  
  
  if (all(is.na(offset_pt))){
    print(filename)
    print('There are no gnss data within the space-time thresholds')
    output='There are no gnss data within the space-time thresholds'
    saveRDS(output,file=paste0(flagged_pt_output_directory,filename,'_',prepped_pt$pt_serial[1],'.rds'))
    return(NA)}
  # 
  
  
  if (nrow(offset_pt)<5 ){
    print(filename)
    print('not enough data to create offset, change your thresholds or double check the data')
    output='not enough data to create offset, change your thresholds or double check the data'
    offset_pt=mutate(offset_pt,error=output)
    saveRDS(offset_pt,file=paste0(flagged_pt_output_directory,filename,'_',offset_pt$pt_serial[1],'.rds'))
    return(NA)
  }
  
  
  #now check the offset for a few things that we care about. We're checking only the pt corrections we've made
  
  #1- are the gnss data too noisy?
  if(sd(offset_pt$gnss_wse)>gnss_sd_thresh  ){
    print(filename)
    print('gnss range too large, check thresholds or data')
    output='gnss range too large, check thresholds or data'
    offset_pt=mutate(offset_pt,error=output)
    saveRDS(offset_pt,file=paste0(flagged_pt_output_directory,filename,'_',offset_pt$pt_serial[1],'.rds'))
    return(NA)
  }
  
  #2- are the pt data too noisy?
  if(sd(offset_pt$pt_correction_sd) >offset_sd_thresh  ){
    print(filename)
    print('correction too noisy, check thresholds or data')
    output='correction too noisy, check thresholds or data'
    offset_pt=mutate(offset_pt,error=output)
    saveRDS(offset_pt,file=paste0(flagged_pt_output_directory,filename,'_',offset_pt$pt_serial[1],'.rds'))
    return(NA)
  }
  
  #3 -are the offsets changing too much in time?
  if(sd(offset_pt$pt_correction) >offset_sd_thresh  ){
    print(filename)
    print('offsets are too different over time, check thresholds or data')
    output='offsets are too different over time, check thresholds or data'
    offset_pt=mutate(offset_pt,error=output)
    saveRDS(offset_pt,file=paste0(flagged_pt_output_directory,filename,'_',offset_pt$pt_serial[1],'.rds'))
    return(NA)
  }
  
  
  #if those pass, we're good! now, we need to apply the offsets to the rest of the pt data
  #apply the offset to the original (unjoined) pt data
  #use the closest offset in time
  #bayesbio has a nice nearest time join!
  
  #first strip the offest df into just the gnss time and the pt correction and SD
  svelte_offset_pt=select(offset_pt,pt_correction,pt_wse_sd,gnss_time_UTC)
  timenow=Sys.time()
  
  final_pt =  nearestTime(prepped_pt,svelte_offset_pt,'pt_time_UTC','gnss_time_UTC')%>%
    #drop pt data before instal' time
    mutate(timediff_install=pt_install_UTC-pt_time_UTC)%>%
    filter(timediff_install<0)%>%
    #drop pt data after uninstall time
    mutate(timediff_uninstall=pt_uninstall_UTC-pt_time_UTC)%>%
    filter(timediff_uninstall>0)%>%
    select(-timediff_install,-timediff_uninstall)%>%
    mutate(pt_wse=pt_level+pt_correction)
  
  
  print(Sys.time()-timenow)
  print(filename)
  print('this file passed all checks')
  write.csv(final_pt,file=paste0(QA_QC_pt_output_directory,filename,'.csv'))
  
  
  
  
  
}#end function 









