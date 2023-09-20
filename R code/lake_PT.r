lake_PT=function(raw_pt_file,pt_key_file,pt_data_directory,
                 dist_thresh,time_thresh,gnss_drift_data_directory,
                QA_QC_pt_output_directory,
                flagged_pt_output_directory,
                     gnss_sd_thresh,offset_sd_thresh,change_thresh_15_min){

    #create WSE in lakes from PTs and GNSS

library(dplyr)
library(stringr)
library(tidyr)
library(fuzzyjoin)
library(geodist)
library(bayesbio)
library(ncdf4)
    
    filename=str_split(raw_pt_file,'/')[[1]][2]


  handle_raw_pt=function(raw_pt_file,pt_key_file,pt_data_directory){
      
           pt_serial=as.integer(read.table(paste0(pt_data_directory,raw_pt_file), header = FALSE, nrow = 1)$V1)
      
    pt_data=read.csv(paste0(pt_data_directory,raw_pt_file),skip=10,header=TRUE) %>%
      mutate(pt_serial=as.integer(pt_serial))
      
      #this !@#$! field sometimes has AM/PM and sometimes 24 hour time. we need to parse it to figure out which is which and then process.
            
   sum1=sum(!is.na(str_match(pt_data$Time, 'pm')))
   sum2=sum(!is.na(str_match(pt_data$Time, 'am')))
   sum3=sum(!is.na(str_match(pt_data$Time, 'PM')))
   sum4=sum(!is.na(str_match(pt_data$Time, 'AM')))
      
      if(any(c(sum1,sum2,sum3,sum4)>0)){ #this means we're in ampmtime.
          pt_data=pt_data%>%
        mutate(datetime=as.POSIXct(paste(Date,Time),format="%m/%d/%Y %I:%M:%S %p"))
        }else{ #24 hour time
          pt_data=pt_data%>%
        mutate(pt_data,datetime=as.POSIXct(paste(Date,Time),format= "%Y/%m/%d %H:%M:%S"))
      }
         
         #print(head(pt_data))
   
    #read in offset--------------
    keyfile=read.csv(pt_key_file,stringsAsFactors = FALSE)%>%
      mutate('driftID_install'= Final_Install_Log_File)%>%
      mutate('driftID_uninstall'= Final_Uninstall_Log_File)%>%
      mutate(pt_serial=as.integer(PT_Serial))

      
    #left joining the key in 'offset' gets us a tidy data frame where the info from the key is promulgated to just that pt. a right join would give
    #an n fold expansion across n pts
    pt_data=pt_data %>%
      left_join(keyfile,by='pt_serial')%>%
      mutate(pt_Lat= Lat_WGS84)%>%
      mutate(pt_Lon=Long_WGS84)%>%
      mutate(pt_time_UTC=datetime)%>%
      mutate(pt_install_UTC=as.POSIXct(paste(Date_GNSS_Install,Time_GNSS_Install_Start_UTC),format= "%m/%d/%Y %H:%M"))%>%
      mutate(pt_uninstall_UTC=as.POSIXct(paste(Date_GNSS_Uninstall,Time_GNSS_Uninstall_End_UTC),format= "%m/%d/%Y %H:%M"))%>%
    mutate(timediff_install=pt_install_UTC-pt_time_UTC)%>%
    filter(timediff_install<0)%>%
    #drop pt data after uninstall time
    mutate(timediff_uninstall=pt_uninstall_UTC-pt_time_UTC)%>%
    filter(timediff_uninstall>0)%>%
    select(-timediff_install,-timediff_uninstall)
      
   
  #print(head(pt_data))
      
      }


correct_pt=function(prepped_pt,dist_thresh,time_thresh,gnss_drift_data_directory){
    
   
    
     log_files=rbind( unique(prepped_pt$driftID_install),unique(prepped_pt$driftID_uninstall)) #need both files to work on
    #log_files=unique(prepped_pt$driftID_install)
    log_files=log_files[!is.na(log_files)]
   

    
    if(length(log_files)==0){
      return(NA)
    }
    
    if(all(is.na(log_files))){
      return(NA)
    }
    
    
unit_pt_process = function(log_file,prepped_pt,dist_thresh,time_thresh,gnss_drift_data_directory){
      
      #the install file is missing a date, need to go a hunting for it
      
      logstring=str_replace(paste0(gnss_drift_data_directory,log_file,'.csv'),"L0",'L2')
    
   # print(logstring)

      #standardized files means we can pull this in a fixed position
      splitter=strsplit(logstring,'_')
        #match the first and second dates
        if (length(splitter[[1]]) ==9){
              string_to_match=paste(splitter[[1]][8], splitter[[1]][9],sep='_')
            splitter2=strsplit(string_to_match,'\\.')
            string_to_match=splitter2[[1]][1]
            }else{
        string_to_match=paste(splitter[[1]][9], splitter[[1]][10],sep='_')
        splitter2=strsplit(string_to_match,'\\.')
            string_to_match=splitter2[[1]][1]
        }



        correct_drift_index=which(!is.na(str_match(list.files(gnss_drift_data_directory),string_to_match)))

      driftstring=list.files(gnss_drift_data_directory,full.names=TRUE)[correct_drift_index]
    
    #edge case- CU has a drift file taht doesn't exist.
      if (identical( correct_drift_index, integer(0))){
           wse_pt=data.frame(pt_level=NA,temperature=NA,gnss_time_UTC=NA,  gnss_wse=NA, 
                             
                             pt_time_UTC =NA,    pt_serial  =NA,      gnss_lat     =NA,   gnss_lon    =NA, 
                             pt_lat    =NA,      pt_lon  =NA,        gnss_pt_dist_m =NA,  pt_correction =NA,
                             pt_correction_sd=NA, pt_wse=NA,          pt_wse_sd  =NA)
          return(wse_pt)
          
          }
    
      gnss_log=read.csv(driftstring,header=TRUE,stringsAsFactors = FALSE)%>%
        mutate(datetime=gnss_time_UTC)%>%
        mutate(datetime=as.POSIXct(datetime))#needed as when it gets written to csv it becomes not a posix object

    
    # print(head(gnss_log))
    # print(head(prepped_pt))
       clean_pt_time=difference_inner_join(prepped_pt,gnss_log,by='datetime',max_dist=time_thresh,distance_col='test')


      #need lon then lat
      distance_m=geodist(cbind(clean_pt_time$pt_lon, clean_pt_time$pt_lat),    cbind(clean_pt_time$gnss_Lon, clean_pt_time$gnss_Lat), paired=TRUE,measure='haversine')

    clean_pt=cbind(clean_pt_time,distance_m)%>%
    filter(distance_m<dist_thresh)%>%
        #ok, let's select for stuff we want to keep now
    transmute(pt_level=pt_level, temperature=temperature,gnss_time_UTC=as.POSIXct(gnss_time_UTC,format ="%Y-%m-%d %H:%M:%S"),gnss_wse=gnss_wse,pt_time_UTC=pt_time_UTC,
                  pt_serial=pt_serial,
                  gnss_lat=gnss_Lat, gnss_lon=gnss_Lon, pt_lat=pt_lat, pt_lon=pt_lon,gnss_pt_dist_m=distance_m )
      

      #next, ensure there are enough gnss points to make a valid offset correction
      if(nrow(clean_pt)==0){
        return(NA)
        
      }
       
      #now, multiple gnss points go to one pt. Need to group by pt timestep and apply the correction at that level
      #the differences in these offsets is the uncertainty of this mapping
      
    #in this case the pt install time above corresponds to the gnss install and uninstall date. any pt data collected before or after the gnss was in the water will not be used for the offset.
      
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
      
       #print(names(wse_pt))
      
      return(wse_pt)
      
      
      
    }
    
    
    
    #loop through the log files, find the right gnss data to associate with the install,
    # average the gnss height within a distance threshold, and then create a corrected pt df
    #by binding each log file together. This is unnecessary, but will handle that one special case
 
      #in case the PT went dry, need to eliminate missing installa nd uninstall files
      log_files=log_files[log_files != ""]
     # print(log_files)
    finalpt=do.call(rbind,lapply(log_files,unit_pt_process,prepped_pt=prepped_pt,dist_thresh=dist_thresh,
                                 time_thresh=time_thresh,gnss_drift_data_directory=gnss_drift_data_directory))%>%
    filter(!is.na(pt_level)) #handle that edge case

    
  } #end the correct PT function
  

  #read in pt
  prepped_pt=handle_raw_pt(raw_pt_file,pt_key_file,pt_data_directory)%>%
    transmute(pt_time_UTC=pt_time_UTC,pt_lat=pt_Lat,pt_lon=pt_Lon,
              pt_install_UTC=pt_install_UTC,
              pt_uninstall_UTC=pt_uninstall_UTC,
              install_method=Install_method, pt_serial=pt_serial,
              pt_level=Level,temperature=Temperature,driftID_install=driftID_install,driftID_uninstall=driftID_uninstall,
              datetime=datetime)
    
    

    # print(prepped_pt)
    # print('boo')
  
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
    #need to group by date- overall noise is not a problem, noise within date is a problem
    
    noise_calc = offset_pt %>%
    mutate(date= as.Date(gnss_time_UTC))%>%
    group_by(date)%>%
    summarize(variances=sd(gnss_wse))
    
  if(any(noise_calc$variances >gnss_sd_thresh  )){
    print(filename)
    print('put in, pull out, or both too noisy within spatial window')
    output='put in, pull out, or both too noisy within spatial window'
    offset_pt=mutate(offset_pt,error=output)
  write.csv(offset_pt,file=paste0(flagged_pt_output_directory,filename,'.csv'), row.names=FALSE)
    return(NA)
  }
  
  #2- are the pt data too noisy?
    #look at the offset over time- it should have a low variance
  if(sd(offset_pt$pt_correction) >offset_sd_thresh  ){
    print(filename)
    print('correction too noisy, check thresholds or data')
    output='correction too noisy, check thresholds or data'
    offset_pt=mutate(offset_pt,error=output)
    write.csv(offset_pt,file=paste0(flagged_pt_output_directory,filename,'.csv'), row.names=FALSE)
    return(NA)
  }

  
  
  #if those pass, we're good! now, we need to apply the offsets to the rest of the pt data
  #apply the offset to the original (unjoined) pt data
  #use the closest offset in time
  #bayesbio has a nice nearest time join!
  
  #first strip the offset df into just the gnss time and the pt correction and SD
  svelte_offset_pt=select(offset_pt,pt_correction,pt_wse_sd,gnss_time_UTC)
  

  
  #     write.csv(svelte_offset_pt, '/nas/cee-water/cjgleason/calval/Processed data/CU/Munged PT/svelte.csv')
  #     write.csv(prepped_pt,'/nas/cee-water/cjgleason/calval/Processed data/CU/Munged PT/prepped.csv')
  

#   prepped_pt2=prepped_pt %>%
#     mutate(timediff_install=pt_install_UTC-pt_time_UTC)%>%
#     filter(timediff_install<0)%>%
#     #drop pt data after uninstall time
#     mutate(timediff_uninstall=pt_uninstall_UTC-pt_time_UTC)%>%
#     filter(timediff_uninstall>0)%>%
#     select(-timediff_install,-timediff_uninstall)
    
  
  findmintime=function(prepped_pt_row,gnss_times){
   # print(as.POSIXct(prepped_pt_row['pt_time_UTC']))

  first(which(as.POSIXct(prepped_pt_row['pt_time_UTC']) - gnss_times == min(as.POSIXct(prepped_pt_row['pt_time_UTC']) - gnss_times)))
   }
  
  
  pt_index=apply(prepped_pt,1, findmintime, gnss_times=svelte_offset_pt$gnss_time_UTC)
  
  final_pt=cbind(svelte_offset_pt[pt_index,],prepped_pt)%>%
    mutate(pt_wse=pt_level+pt_correction)%>%
    select(-datetime)
  
  
  # print('final')
  # print(final_pt)
  
  print(filename)
  print('this file passed all checks')
  write.csv(final_pt,file=paste0(QA_QC_pt_output_directory,filename),row.names=FALSE)
  
  
}#end function 
