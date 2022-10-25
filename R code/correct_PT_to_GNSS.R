correct_PT_to_GNSS= function(raw_PT_file,PT_key_file,dist_thresh,time_thresh,PT_data_directory,
                             GNSS_drift_data_directory,QA_QC_PT_output_directory, flagged_PT_directory,
                             GNSS_sd_thresh,offset_diff_thresh,change_thresh_15_min){
  
  library(dplyr)
  library(tidyr)
  library(fuzzyjoin)
  library(geodist)
  library(bayesbio)
  library(ncdf4)
  
  filename=raw_PT_file
  
  handle_raw_PT=function(raw_PT_file,PT_key_file,PT_data_directory,GNSS_drift_data_directory){
    #read in raw PT--------------
    #ID could come from the filename, right now it is reading the serial number from the file
    
    #kluge that quickly gets what we want by reading in the csv flat and pulling the first entry
    PT_serial = strtoi(as.character(read.csv(paste0(PT_data_directory,raw_PT_file,'.csv'))$Serial_number.[1]))
    
    #11 lines of headers to skip to deal with read in function
    
    #!!!!!!!!!!!!!!
    #The willamette data have a missing header, so we skip the 12th line and assign our own header. Deadly !
    #!!!!!!!!!!!!!
    
    PT_data= read.csv(paste0(PT_data_directory,raw_PT_file,'.csv'),skip=12,header=TRUE,fill=TRUE,col.names = c('Date','Time','ms','Level','Temperature')) %>%
      mutate(PT_Serial=PT_serial)
    #----------------------------
    
    #read in offset--------------
    keyfile=read.csv(PT_key_file,stringsAsFactors = FALSE)%>%
      mutate('driftID'= final_log_file)
    #left joining the key in 'offset' gets us a tidy data frame where the info from the key is promulgated to just that PT. a right join would give
    #an n fold expansion across n PTs
    PT_data=PT_data %>%
      left_join(keyfile,by='PT_Serial')%>%
      mutate(datetime= as.POSIXct(paste(Date,Time),format= "%Y/%m/%d %H:%M:%S") )%>%
      mutate(PT_Lat= Lat_WGS84)%>%
      mutate(PT_Lon=Long_WGS84)%>%
      mutate(PT_time_UTC=datetime)
    
    
    #----------------------------
  } # end PT function
  correct_PT=function(prepped_PT,dist_thresh,time_thresh,GNSS_drift_data_directory){
    log_files= unique(prepped_PT$driftID) #the second from the join.
    if(length(log_files)==0){
      return(NA)
    }
    
     if(is.na(log_files)){
      return(NA)
    }
    
    unit_PT_process = function(log_file,prepped_PT,dist_thresh,time_thresh,GNSS_drift_data_directory){
      GNSS_log=readRDS(paste0(GNSS_drift_data_directory,log_file,'.rds'))

      #half a second faster to join first on time and then on space
      clean_PT_time=difference_inner_join(prepped_PT,GNSS_log,by='datetime',max_dist=time_thresh)
      #need lon then lat
     distance_m=geodist(cbind(clean_PT_time$PT_lon, clean_PT_time$PT_lat), cbind(clean_PT_time$GNSS_Lon, clean_PT_time$GNSS_Lat), paired=TRUE,measure='haversine')
      

     clean_PT=cbind(clean_PT_time,distance_m)%>%
        filter(distance_m<dist_thresh)%>%
        #ok, let's select for stuff we want to keep now
        transmute(PT_level=PT_level, Temperature=Temperature,GNSS_time_UTC=GNSS_time_UTC,GNSS_wse=GNSS_wse,PT_time_UTC=PT_time_UTC,
                  PT_Serial=PT_serial,
                  GNSS_lat=GNSS_Lat, GNSS_lon=GNSS_Lon, PT_lat=PT_lat, PT_lon=PT_lon,GNSS_PT_dist_m=distance_m )


      #next, ensure there are enough GNSS points to make a valid offset correction
   if(nrow(clean_PT)==0){
     return(NA)
     
   }
      
      #now, multiple GNSS points go to one PT. Need to group by PT timestwp and apply the correction at that level
      #the differneces in these offsets is the uncertainty of this mapping
 
      
      offset_PT_mean=clean_PT%>%
        group_by(PT_time_UTC)%>%
        mutate( offset=(GNSS_wse-PT_level))%>%
        summarize(PT_correction= mean(offset))
      
      offset_PT_sd =clean_PT%>%
        group_by(PT_time_UTC)%>%
        mutate( offset=(GNSS_wse-PT_level))%>%
        summarize(PT_correction_sd=sd(offset))
      
      
      wse_PT=clean_PT%>%
        left_join(offset_PT_mean,by='PT_time_UTC')%>%
        left_join(offset_PT_sd,by='PT_time_UTC')%>%
        mutate(PT_wse=PT_level+PT_correction)%>%
        mutate(PT_wse_sd= sqrt(sum(sd(PT_level)^2 + PT_correction_sd^2) ))
      
      return(wse_PT)
      

 
    }
    
    
    #loop through the log files, find the right GNSS data to associate with the install,
    # average the GNSS heigh within a distance theshold, and then create a corrected PT df
    #by binding each log file together. This is uncessary, but will handle that one special case
    finalPT=do.call(rbind,lapply(log_files,unit_PT_process,prepped_PT=prepped_PT,dist_thresh=dist_thresh,
                                 time_thresh=time_thresh,GNSS_drift_data_directory=GNSS_drift_data_directory))
    

    
  }
  
  #read in PT
  prepped_PT=handle_raw_PT(raw_PT_file,PT_key_file,PT_data_directory,GNSS_drift_data_directory)%>%
    transmute(PT_time_UTC=PT_time_UTC,PT_lat=PT_Lat,PT_lon=PT_Lon,
              PT_install_UTC=as.POSIXct(paste(Date_Install,Time_Install_UTC),format= "%m/%d/%Y %H:%M"),
              PT_uninstall_UTC=as.POSIXct(paste(Date_Uninstall,Time_Uninstall),format= "%m/%d/%Y %H:%M"),
              Install_method=Install_method, PT_serial=PT_Serial,PT_level=Level,Temperature=Temperature,driftID=driftID,datetime=PT_time_UTC)
  
  if(sum(is.na(prepped_PT$PT_lat))==nrow(prepped_PT)){
    print('this PT isnt in the key')
    saveRDS(prepped_PT,file=paste0(flagged_PT_output_directory,filename,'_',prepped_PT$PT_Serial[1],'.rds'))
    return(NA)
  }
  
  
              
  #make GNSS offset
  offset_PT=correct_PT(prepped_PT,dist_thresh,time_thresh,GNSS_drift_data_directory)


if (all(is.na(offset_PT))){
  print(filename)
  print('There are no GNSS data within the space-time thresholds')
  output='There are no GNSS data within the space-time thresholds'
  saveRDS(output,file=paste0(flagged_PT_output_directory,filename,'_',prepped_PT$PT_Serial[1],'.rds'))
  return(NA)}
# 


  # if(nrow(offset_PT)==0){
  #   print('I was asked to find a drift file that doesnt exist')
  #   output='I was asked to find a drift file that doesnt exist'
  #   saveRDS(output,file=paste0(flagged_PT_output_directory,filename,'_',prepped_PT$PT_Serial[1],'.rds'))
  #   return(NA)}

   


  if (nrow(offset_PT)<5 ){
    print(filename)
    print('not enough data to create offset, change your thresholds or double check the data')
    output='not enough data to create offset, change your thresholds or double check the data'
    offset_PT=mutate(offset_PT,error=output)
    saveRDS(offset_PT,file=paste0(flagged_PT_output_directory,filename,'_',offset_PT$PT_Serial[1],'.rds'))
    return(NA)
  }

  
  #now check the offset for a few things that we care about. We're checking only the PT corrections we've made
  
  #1- are the GNSS data too noisy?
  if(sd(offset_PT$GNSS_wse)>GNSS_sd_thresh  ){
    print(filename)
    print('GNSS range too large, check thresholds or data')
    output='GNSS range too large, check thresholds or data'
    offset_PT=mutate(offset_PT,error=output)
    saveRDS(offset_PT,file=paste0(flagged_PT_output_directory,filename,'_',offset_PT$PT_Serial[1],'.rds'))
    return(NA)
  }
  
  #2- are the PT data too noisy?
  if(sd(offset_PT$PT_correction_sd) >offset_sd_thresh  ){
    print(filename)
    print('correction too noisy, check thresholds or data')
    output='correction too noisy, check thresholds or data'
    offset_PT=mutate(offset_PT,error=output)
    saveRDS(offset_PT,file=paste0(flagged_PT_output_directory,filename,'_',offset_PT$PT_Serial[1],'.rds'))
    return(NA)
  }
  
  #3 -are the offsets changing too much in time?
  if(sd(offset_PT$PT_correction) >offset_sd_thresh  ){
    print(filename)
    print('offsets are too different over time, check thresholds or data')
    output='offsets are too different over time, check thresholds or data'
    offset_PT=mutate(offset_PT,error=output)
    saveRDS(offset_PT,file=paste0(flagged_PT_output_directory,filename,'_',offset_PT$PT_Serial[1],'.rds'))
    return(NA)
  }
  
  
  #if those pass, we're good! now, we need to apply the offsets to the rest of the PT data
  #apply the offset to the original (unjoined) PT data
  #use the closest offset in time
  #bayesbio has a nice nearest time join!
  
  #first strip the offest df into just the GNSS time and the PT correction and SD
   svelte_offset_PT=select(offset_PT,PT_correction,PT_wse_sd,GNSS_time_UTC)
  
  final_PT =  nearestTime(prepped_PT,svelte_offset_PT,'PT_time_UTC','GNSS_time_UTC')%>%
    #drop PT data obefore instal time
    mutate(timediff_install=PT_install_UTC-PT_time_UTC)%>%
    filter(timediff_install<0)%>%
  #drop PT data after uninstall time
  mutate(timediff_uninstall=PT_uninstall_UTC-PT_time_UTC)%>%
    filter(timediff_uninstall>0)%>%
    select(-timediff_install,-timediff_uninstall)%>%
    mutate(PT_wse=PT_level+PT_correction)
  
  #now, check for discontinuities in the PT data. A common discontinuity is at the beginning or end. 
  diffvec=(c(0,final_PT$PT_wse)-c(final_PT$PT_wse,0))[2:(nrow(final_PT)-1)]
  
  change_detected=which(abs(diffvec)>change_thresh_15_min)
  #if it is just first and last, then it is put in pull out
  #in that case, just make sure the data are static before and after the changes
 # if (length(change_detected)==0){
    print(filename)
    print('this file passed all checks')
    saveRDS(final_PT,file=paste0(QA_QC_PT_output_directory,filename,'_',final_PT$PT_Serial[1],'.rds'))
  # } else {
  #   print(filename)
  #   print('there is an offset discontinuity')
  #   final_PT=mutate(final_PT,error='there is an offset discontinuity')
  #   saveRDS(final_PT,file=paste0(flagged_PT_output_directory,filename,'_',final_PT$PT_Serial[1],'.rds'))
  # }
  

  

  
}#end function 
  
  







