correct_pt_to_gnss_multikey= function(raw_pt_file,master_key,dist_thresh,time_thresh,pt_data_directory,
                             gnss_drift_data_directory,QA_QC_pt_output_directory, flagged_pt_output_directory,
                             gnss_sd_thresh,offset_sd_thresh,change_thresh_15_min){
library(dplyr)
library(stringr)
library(tidyr)
library(fuzzyjoin)
library(geodist)
library(bayesbio)
library(ncdf4)

  pt_serial_file=as.integer(read.table(paste0(pt_data_directory,raw_pt_file), header = FALSE, nrow = 1)$V1)
      
    #read in master_key--------------
    keyfile=master_key%>%
      mutate('driftID_install'= sub("\\..*","",Final_Install_Log_File))%>%
      mutate('driftID_uninstall'= sub("\\..*","",Final_Uninstall_Log_File))%>%
      mutate(pt_serial=as.integer(PT_Serial))

    #check for a serial match-----------
    if( pt_serial_file %in% keyfile$pt_serial == FALSE){return(NA)}
   
    print(paste0(pt_data_directory,raw_pt_file))
    #now get the PT data, standardized(ish) by Taylor--------
    tryCatch({
    pt_data=read.csv(paste0(pt_data_directory,raw_pt_file),skip=10,header=TRUE) %>%
      mutate(pt_serial=as.integer(pt_serial_file)) 
    },
        error=function(cond){return(NA)})
    
    #this !@#$! field sometimes has AM/PM and sometimes 24 hour time. we need to parse it to figure out
    #which is which and then process.
   
    # print('here')
    if(!exists('pt_data')){return(NA)}
        
    # print('here?')
    if(all(is.na(pt_data))){return(NA)}
    
 
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
         
    

    
  #left joining the key in 'offset' gets us a tidy data frame where the info from the key is
    #promulgated to just that pt. a right join would give
    #an n fold expansion across n pts
    pt_data=pt_data %>%
      left_join(keyfile,by='pt_serial')%>%
      mutate(pt_Lat= Lat_WGS84)%>%
      mutate(pt_Lon=Long_WGS84)%>%
      mutate(pt_time_UTC=datetime)%>%
      filter(pt_serial==pt_serial_file)%>% #limit to just the PT we want. The filter above should take care 
    #of the case where the serial is not in the key file
    transmute(pt_time_UTC=pt_time_UTC,pt_lat=pt_Lat,pt_lon=pt_Lon,
     pt_install_UTC=as.POSIXct(paste(Date_GNSS_Install,Time_GNSS_Install_Start_UTC),format= "%m/%d/%Y %H:%M"),
     pt_uninstall_UTC=as.POSIXct(paste(Date_GNSS_Uninstall,Time_GNSS_Uninstall_End_UTC),format= "%m/%d/%Y %H:%M"),
         install_method=Install_method, pt_serial=pt_serial,         
         pt_level=Level,temperature=Temperature,
              driftID_install=driftID_install,driftID_uninstall=driftID_uninstall,
              datetime=datetime,
             keyid=keyid) #cleanup
    

  
    #now, we've got to go get all of the gnss data associated with our pt. let's go find the data
    log_files=rbind( unique(pt_data$driftID_install),unique(pt_data$driftID_uninstall)) #need both files to work on
    #log_files=unique(prepped_pt$driftID_install)
    log_files=log_files[!is.na(log_files)]
        
    #in case the PT went dry, need to eliminate missing installa nd uninstall files
    log_files=log_files[log_files != ""]
    
    #exception handling for when the indicated GNSS file is not available-----
    if(length(log_files)==0){
      return(NA)
    }
    if(all(is.na(log_files))){
      return(NA)
    }

    #our field log file is an 'L0' product, but the data come back as 'L2'. We need to find the L2 version
      logstring=str_replace(log_files,"L0",'L2')
      
    
getit_positive=function(longstring, shortstrings){
    #search for a pattern between one and many strings with partial matches allows
        output=any(str_detect(longstring,shortstrings))  
}
#    driftstring=list.files(gnss_drift_data_directory)[do.call(rbind,lapply(list.files(gnss_drift_data_directory),getit_positive,logstring))]
    
    
    #standardized files means we can pull this in a fixed position
      splitter=strsplit(logstring,'_')
        #match the first and second dates
        if (length(splitter[[1]]) ==8){
              string_to_match=paste(splitter[[1]][6], splitter[[1]][7],sep='_')
            }else{
        string_to_match=paste(splitter[[1]][6], splitter[[1]][7],sep='_')}
    
    

       correct_drift_index=which(!is.na(str_match(list.files(gnss_drift_data_directory),string_to_match)))
       driftstring=list.files(gnss_drift_data_directory,full.names=TRUE)[correct_drift_index]

    # print(splitter)
    # print(string_to_match)
#      print('drift ID from key file')
#     print(logstring)
#     print('files in munged drifts')
#     print(list.files(gnss_drift_data_directory))

# #     print(do.call(rbind,lapply(list.files(gnss_drift_data_directory),getit_positive,logstring)))
#      print(driftstring)
#     bonk
    
    #function to read mutliple gnss files
   read_multiple_gnss=function(driftstring){
      gnss_log=read.csv(driftstring,header=TRUE,stringsAsFactors = FALSE)%>%
        mutate(datetime=gnss_time_UTC)%>%
        mutate(datetime=as.POSIXct(datetime)) #needed as when it gets written to csv it becomes not a posix object
   }
      
    gnss_log=do.call(rbind,lapply(driftstring,read_multiple_gnss))
    

    #now, join the GNSS 1Hz data to the PT 15 minute data
    clean_pt_time=difference_inner_join(pt_data,gnss_log,by='datetime',max_dist=time_thresh,distance_col='test')
      
    #need lon then lat
    distance_m=geodist(cbind(clean_pt_time$pt_lon, clean_pt_time$pt_lat),    cbind(clean_pt_time$gnss_Lon, clean_pt_time$gnss_Lat), paired=TRUE,measure='haversine')

    clean_pt=cbind(clean_pt_time,distance_m)%>%
    filter(distance_m<dist_thresh)%>%
        #ok, let's select for stuff we want to keep now
    transmute(pt_level=pt_level, temperature=temperature,gnss_time_UTC=as.POSIXct(gnss_time_UTC,format ="%Y-%m-%d %H:%M:%S"),gnss_wse=gnss_wse,pt_time_UTC=pt_time_UTC,
                  pt_serial=pt_serial,
                  gnss_lat=gnss_Lat, gnss_lon=gnss_Lon, pt_lat=pt_lat, pt_lon=pt_lon,
              gnss_pt_dist_m=distance_m ,keyid=keyid, pt_install_UTC=pt_install_UTC,
             pt_uninstall_UTC=pt_uninstall_UTC)
      
      #next, ensure there are enough gnss points to make a valid offset correction
      if(nrow(clean_pt)==0){
        return(NA)
        
      }
       
      #now, multiple gnss points go to one pt. Need to group by pt timestep and apply the correction at that level
      #the differences in these offsets is the uncertainty of this mapping
    
    #also, we need to group by the keyfile we're using. different keyfiles have different offsets!!!
   
    
      offset_pt_mean=clean_pt%>%
        group_by(pt_time_UTC,keyid)%>%
        mutate( offset=(gnss_wse-pt_level))%>%
        summarize(pt_correction= mean(offset))
    
 
      offset_pt_sd =clean_pt%>%
      group_by(pt_time_UTC,keyid)%>%
        mutate( offset=(gnss_wse-pt_level))%>%
        summarize(pt_correction_sd=sd(offset))
      
      wse_pt=clean_pt%>%
        left_join(offset_pt_mean,by=c('pt_time_UTC','keyid'))%>%
        left_join(offset_pt_sd,by=c('pt_time_UTC','keyid'))%>%
        mutate(pt_wse=pt_level+pt_correction)%>%
        mutate(pt_wse_sd= pt_correction_sd + 0.001) #sets the uncertainty to equal to the variance in all of the offsets used to create the 
    

    #split into multiple files based on the keyids
    final_pt_data_frames=split(wse_pt,wse_pt$keyid)
    
    filename=raw_pt_file    
    filename_base=sub("\\..*","",strsplit(filename,'/')[[1]][length(strsplit(filename,'/')[[1]])])
  
    
  
    
    #now we've got a dataframe for each key file. Check each for what we need
check_data_frames=function(wse_pt,pt_data,filename_base){
    
      filename=paste0(filename_base,'_',unique(wse_pt$keyid))

          if (all(is.na(wse_pt))){
            print(filename)
            print('There are no gnss data within the space-time thresholds')
            output='There are no gnss data within the space-time thresholds'
            return(NA)}
          # 


          if (nrow(wse_pt)<5 ){
            print(filename)
            print('not enough data to create offset, change your thresholds or double check the data')
            output='not enough data to create offset, change your thresholds or double check the data'
            offset_pt=mutate(offset_pt,error=output)
            write.csv(offset_pt,file=paste0(flagged_pt_output_directory,filename))
            return(NA)
          }


        #   #now check the offset for a few things that we care about. We're checking only the pt corrections we've made

          #1- are the gnss data too noisy?
            #need to group by date- overall noise is not a problem, noise within date is a problem
            offset_pt=wse_pt

            noise_calc = offset_pt %>%
            mutate(date= as.Date(gnss_time_UTC))%>%
            group_by(date)%>%
            summarize(variances=sd(gnss_wse,na.rm=TRUE))

          if(any(noise_calc$variances >gnss_sd_thresh  )){
            print(filename)
            print('put in, pull out, or both too noisy within spatial window')
            output='put in, pull out, or both too noisy within spatial window'
            offset_pt=mutate(offset_pt,error=output)
              offset_pt=offset_pt[1,]
          write.csv(offset_pt,file=paste0(flagged_pt_output_directory,
                                          filename), row.names=FALSE)
            return(NA)
          }

          #2- are the pt data too noisy?
            #look at the offset over time- it should have a low variance
          if(sd(offset_pt$pt_correction) >offset_sd_thresh  ){
            print(filename)
            print('correction too noisy, check thresholds or data')
            output='correction too noisy, check thresholds or data'
            offset_pt=mutate(offset_pt,error=output)
               offset_pt=offset_pt[1,]
            write.csv(offset_pt,file=paste0(flagged_pt_output_directory,
                                            filename), row.names=FALSE)
            return(NA)
          }



          #if those pass, we're good! now, we need to apply the offsets to the rest of the pt data
          #apply the offset to the original (unjoined) pt data
          #use the closest offset in time
          #bayesbio has a nice nearest time join!

          #first strip the offset df into just the gnss time and the pt correction and SD
          svelte_offset_pt=select(offset_pt,pt_correction,pt_wse_sd,gnss_time_UTC)
    
          #need the OG pt data

 
    prepped_pt2=pt_data
  if(!all(is.na(unique(pt_data$pt_install_UTC)))){
          prepped_pt2= prepped_pt2 %>%
            mutate(timediff_install=pt_install_UTC-pt_time_UTC)%>%
            filter(timediff_install<0)%>%
            select(-timediff_install)}
     
  if(!all(is.na(unique(pt_data$pt_uninstall_UTC)))){
           # drop pt data after uninstall time
            prepped_pt2= prepped_pt2 %>%
            mutate(timediff_uninstall=pt_uninstall_UTC-pt_time_UTC)%>%
            filter(timediff_uninstall>0)%>%
            select(-timediff_uninstall)}
    

    
          findmintime=function(prepped_pt_row,gnss_times){
          first(which(as.POSIXct(prepped_pt_row['pt_time_UTC']) - gnss_times == 
                      min(as.POSIXct(prepped_pt_row['pt_time_UTC']) - gnss_times,na.rm=TRUE)))
           }

          pt_index=apply(prepped_pt2,1, findmintime, gnss_times=svelte_offset_pt$gnss_time_UTC)

          final_pt=cbind(svelte_offset_pt[pt_index,],prepped_pt2)%>%
            mutate(pt_wse=pt_level+pt_correction)%>%
            select(-datetime)


          print(filename)
          print('this file passed all checks')
          write.csv(final_pt,file=paste0(QA_QC_pt_output_directory,filename),row.names=FALSE)


    } #end loop function for dataframes
    
    dummy=lapply(final_pt_data_frames,check_data_frames,pt_data=pt_data,filename_base=filename_base)
    
    gc()
    rm(dummy)


}#end toplevel function 









