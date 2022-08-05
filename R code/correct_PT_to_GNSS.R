correct_PT_to_GNSS= function(raw_PT_file,PT_key_file,dist_thresh,time_thresh,PT_data_directory,
                             GNSS_drift_data_directory,QA_QC_PT_output_directory, flagged_PT_directory,
                             change_thresh_15_min,GNSS_sd_thresh,offset_diff_thresh){
  
  library(dplyr)
  library(tidyr)
  library(fuzzyjoin)
  library(geodist)
  library(bayesbio)
  filename=sub(,"",raw_PT_file)
  
  handle_raw_PT=function(raw_PT_file,PT_key_fil,PT_data_directory,GNSS_drift_data_directory){
    #read in raw PT--------------
    #ID could come from the filename, right now it is reading the serial number from the file
    
    #kluge that quickly gets what we want by reading in the csv flat and pulling the first entry
    PT_serial = strtoi(as.character(read.csv(paste0(PT_data_directory,raw_PT_file))$Serial_number.[1]))
    
    #11 lines of headers to skip to deal with read in function
    PT_data= read.csv(paste0(PT_data_directory,raw_PT_file),skip=11,header=T) %>%
      mutate(PT_Serial=PT_serial)
    #----------------------------
    
    #read in offset--------------
    offset=read.csv(PT_key_file,stringsAsFactors = FALSE)
    #left joining the key in 'offset' gets us a tidy data frame where the info from the key is promulgated to just that PT. a right join would give
    #an n fold expansion across n PTs
    PT_data=PT_data %>%
      left_join(offset,by='PT_Serial')%>%
      mutate(Lon=Long)%>%
      select(-Long)%>%
      mutate(datetime= as.POSIXct(paste(Date,Time),format= "%m/%d/%Y %I:%M:%S %p") )
    
    
    
    #----------------------------
  } # end PT function
  correct_PT=function(prepped_PT,dist_thresh,time_thresh,GNSS_drift_data_directory){
    log_files= unique(prepped_PT$GNSS_file)
    
    
    unit_PT_process = function(log_file,prepped_PT,dist_thresh,time_thresh,GNSS_drift_data_directory){
      #convert native date format to posix and drop redudnat columns
      
      
      
      GNSS_log=read.csv(paste0(GNSS_drift_data_directory,log_file,'.csv'))%>%
        mutate(Lat=latitude_decimal_degree)%>%
        mutate(Lon=longitude_decimal_degree)%>%
        select(-latitude_decimal_degree,-longitude_decimal_degree)%>%
        mutate(hour=decimal_hour*3600)%>% #hours to seconds, as posix is seconds base
        mutate(datetime=as.POSIXct(paste(year,day_of_year),format= '%Y %j'))%>%
        mutate(datetime=datetime + hour)%>%
        select(-decimal_hour,-day_of_year,-year,-rcvr_clk_ns,-hour)
      
   
      
      #half a second faster to join first on time and then on space
      clean_PT_time=difference_inner_join(prepped_PT,GNSS_log,by='datetime',max_dist=time_thresh)
      distance_m=geodist(cbind(clean_PT_time$Lon.x, clean_PT_time$Lat.x), cbind(clean_PT_time$Lon.y, clean_PT_time$Lat.y), paired=TRUE,measure='haversine')
      clean_PT=cbind(clean_PT_time,distance_m)%>%
        filter(distance_m<dist_thresh)%>%
        #artifical date change to test
        #clean_PT$datetime.x[1000:2000]=clean_PT$datetime.x[1000:2000] +(2*24*3600)
        mutate(days=format(datetime.x,format='%m%d%Y'))
      
      #next, ensure there are enough GNSS points to make a valid offset correction
      if (nrow(clean_PT)<50){
        print('not enough data to create offset, change your thresholds or double check the data')
        return(clean_PT)
      }
      
      #now, imagine there were put in and pull out times. We want to check the offset at each.
      #find the different days
      
      offset_PT=clean_PT%>%
        group_by(days)%>%
        summarize(offset=mean(ortho_height_m_cgvd2013)-mean(LEVEL),sd_PT=sd(LEVEL),sd_GNSS=sd(ortho_height_m_cgvd2013))%>%
        ungroup()
      
      
      
      
      #average across points within threshold to characterize PT to GNSS
      print(paste('for days', offset_PT$days))
      print(paste('PT sd (m) is',as.character(offset_PT$sd_PT)))
      print(paste('GNSS sd (m) is',as.character(offset_PT$sd_GNSS)))
      print(paste('offset (m) is',as.character(offset_PT$offset)))
      
      clean_PT=clean_PT%>%
        left_join(offset_PT,by='days')
    }
    
    
    #loop through the log files, find the right GNSS data to associate with the install,
    # average the GNSS heigh within a distance theshold, and then create a corrected PT df
    #by binding each log file together. This is uncessary, but will handle that one special case
    finalPT=do.call(rbind,lapply(log_files,unit_PT_process,prepped_PT=prepped_PT,dist_thresh=dist_thresh,
                                 time_thresh=time_thresh,GNSS_drift_data_directory=GNSS_drift_data_directory))
    
    
    
  }
  
  #read in PT
  prepped_PT=handle_raw_PT(raw_PT_file,PT_key_fil,PT_data_directory,GNSS_drift_data_directory)
  #make GNSS offset
  offset_PT=correct_PT(prepped_PT,dist_thresh,time_thresh,GNSS_drift_data_directory)
  

  
  if (nrow(offset_PT)<50 ){
    print(filename)
    print('not enough data to create offset, change your thresholds or double check the data')
    output='not enough data to create offset, change your thresholds or double check the data'
    offset_PT=mutate(offset_PT,error=output)
    saveRDS(offset_PT,file=paste0(flagged_PT_output_directory,filename,'_',prepped_PT$PT_Serial[1],'.rds'))
    return(NA)
  }
  
  #pull the offset
  offset=offset_PT%>%
    group_by(days)%>%
    select(days,offset,sd_GNSS,sd_PT)%>%
    summarize(offset=unique(offset),sd_GNSS=unique(sd_GNSS),sd_PT=unique(sd_PT))%>%
    ungroup()%>%
    mutate(datetime=as.POSIXct(paste0(days,'12:00:00'),format='%m%d%Y%H:%M:%S'))
  
  #now check the offset for a few tricks
  
  #1- are the GNSS data too noisy?
  if(sd(offset_PT$ortho_height_m_cgvd2013)>GNSS_sd_thresh  ){
    print(filename)
    print('GNSS range too large, check thresholds or data')
    output='GNSS range too large, check thresholds or data'
    offset_PT=mutate(offset_PT,error=output)
    saveRDS(offset_PT,file=paste0(flagged_PT_output_directory,filename,'_',prepped_PT$PT_Serial[1],'.rds'))
    return(NA)
    }
  
  #2- are the offsets too different?
  if(length(offset$offset)>1){
  if((offset$offset[1]-offset$offset[2])>offset_diff_thresh  ){
    print(filename)
    print('offsets too different range too large, check thresholds or data')
    output='GNSS range too large, check thresholds or data'
    offset_PT=mutate(offset_PT,error=output)
    saveRDS(offset_PT,file=paste0(flagged_PT_output_directory,filename,'_',prepped_PT$PT_Serial[1],'.rds'))
    return(NA)
  }}
  

  #apply the offset to the original (unjoined) PT data
  #use the closest offset in time
  #bayesbio has a nice nearest time join!
  final_PT = nearestTime(prepped_PT,offset,'datetime','datetime') %>%
    mutate(corrected_level= LEVEL+offset-GNSS_offset) #add the PT level and the correction, and then subtract the GNSS distance from the H2O


#now, check for discontinuities in the PT data. A common discontinuity is at the beginning or end. 
#final_PT=readRDS('D:/OneDrive -\ University of Massachusetts/calval/Toolbox/calval_toolbox/Taylor data 7 12/QAQC PTs/CPT01_20210909_2135437.rds')%>%
final_PT=final_PT%>% 
  mutate(diffvec=(c(0,corrected_level)-c(corrected_level,0))[1:length(corrected_level)] )

final_PT$diffvec[1]=0 #first one is way off by definition

# windows()
# plot(final_PT$datetime,final_PT$diffvec, ylim = c(-0.5,0.5))

change_detected=which(abs(final_PT$diffvec)>change_thresh_15_min)
#if it is just first and last, then it is put in pull out
#in that case, just make sure the data are static before and after the changes
if (is.na(change_detected)){
  final_PT=final_PT
  saveRDS(final_PT,file=paste0(QA_QC_PT_output_directory,filename,'_',prepped_PT$PT_Serial[1],'.rds'))
} else if (length(change_detected)==2){
  final_PT=final_PT[(change_detected[1]+1):(change_detected[2]-1),]
  saveRDS(final_PT,file=paste0(QA_QC_PT_output_directory,filename,'_',prepped_PT$PT_Serial[1],'.rds'))
} else if (length(change_detected)==1){
  if(change_detected[1]<(0.5*nrow(final_PT))){
    final_PT=final_PT[(change_detected+1):nrow(final_PT),] #throw out beginning
  } else {
    final_PT=final_PT[1:(change_detected-1),]#throw out end
  }
  saveRDS(final_PT,file=paste0(QA_QC_PT_output_directory,filename,'_',prepped_PT$PT_Serial[1],'.rds'))
} else {
  print('more than first/last shift detected')
  final_PT=mutate(final_PT,error='more than first/last shift detected')
  saveRDS(final_PT,file=paste0(flagged_PT_output_directory,filename,'_',prepped_PT$PT_Serial[1],'.rds'))
}





}







