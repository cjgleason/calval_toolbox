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
    
    flag=0

    #Creates the PT record as WSE instead of level. Uses multiple key files in cases where PTs went in and out of the water.
    
  pt_serial_file=as.integer(read.table(paste0(pt_data_directory,raw_pt_file), header = FALSE, nrow = 1)$V1)
    
    print(pt_serial_file)
    bonk
        
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
          #cleanup
    filename=raw_pt_file    
    filename_base=sub("\\..*","",strsplit(filename,'/')[[1]][length(strsplit(filename,'/')[[1]])])
    filename=paste0(filename_base,'_',unique(pt_data$keyid))
    
    
   if(!is.na(str_match(pt_data$Date[1],'UNIT') )){
            clean_pt='this PT has header issues related to the HTC version'
                write.csv(clean_pt,file=paste0(flagged_pt_output_directory,filename))
       return(NA)}

    
    #this !@#$!##*& field sometimes has AM/PM and sometimes 24 hour time. we need to parse it to figure out
    #which is which and then process.
   
    # print('here')
    if(!exists('pt_data')){
             clean_pt='this PT doesnt exist. I looked in the keyfile and couldnt find a PT that matches it'
                write.csv(clean_pt,file=paste0(flagged_pt_output_directory,filename))
        return(NA)}
        
    # print('here?')
    if(all(is.na(pt_data))){
             clean_pt='this PT has no data- I opened the file and it was empty'
                write.csv(clean_pt,file=paste0(flagged_pt_output_directory,filename))
        return(NA)}
    
 
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
      left_join(keyfile,by='pt_serial')  %>%
    mutate(pt_Lat= Lat_WGS84)%>%
      mutate(pt_Lon=Long_WGS84)%>%
      mutate(pt_time_UTC=datetime)%>%
      filter(pt_serial==pt_serial_file)%>% #limit to just the PT we want. The filter above should take care 
    #of the case where the serial is not in the key file
    transmute(pt_time_UTC=pt_time_UTC,pt_lat=pt_Lat,pt_lon=pt_Lon,
     pt_install_UTC=as.POSIXct(paste(Date_PT_Install,Time_PT_Install_UTC),format= "%m/%d/%Y %H:%M"),
     pt_uninstall_UTC=as.POSIXct(paste(Date_PT_Uninstall,Time_PT_Uninstall_UTC),format= "%m/%d/%Y %H:%M"),
                   gnss_install_UTC=as.POSIXct(paste(Date_GNSS_Install,Time_GNSS_Install_Start_UTC),format= "%m/%d/%Y %H:%M"),
     gnss_uninstall_UTC=as.POSIXct(paste(Date_GNSS_Uninstall,Time_GNSS_Uninstall_End_UTC),format= "%m/%d/%Y %H:%M"),
         install_method=Install_method, pt_serial=pt_serial,         
         pt_level=Level,temperature=Temperature,
              driftID_install=driftID_install,driftID_uninstall=driftID_uninstall,
              datetime=datetime,
              keyid=keyid, Date_GNSS_Install=Date_GNSS_Install,Date_GNSS_Uninstall=Date_GNSS_Uninstall) %>%
    filter(pt_time_UTC >= pt_install_UTC)%>%
    filter(pt_time_UTC <= pt_uninstall_UTC)
    
    
    if(nrow(pt_data)==0){
          clean_pt='there are no GNSS pings in common with the pt data based on the keyfiles. That is, if you limit the PT record to install and uninstall times in the key, we lose all the PT data. KEYFILE ERROR'
                write.csv(clean_pt,file=paste0(flagged_pt_output_directory,filename))
        return(NA)
    }
    
    #check to see if the PT thinks it shifted 
    pt_shift_vector= c(0,pt_data$pt_level)-c(pt_data$pt_level,0)
    if(length(pt_shift_vector)>5){
    pt_shift_vector=pt_shift_vector[3:(length(pt_shift_vector)-3)]}
    
  
    if(any(abs(pt_shift_vector)>change_thresh_15_min)){
       # print(plot(pt_shift_vector))
        flag=1
      # clean_pt='the pt self-reports a shift greater than the 15 miniute change threshold'
      #           write.csv(clean_pt,file=paste0(flagged_pt_output_directory,filename))
      #   return(NA)
        }

    
    
    if (is.na(pt_data[1,]$Date_GNSS_Uninstall)){ #in the case where there is no uninstall
    pt_data_for_offset =pt_data%>%
#         #taylor says we can throw out all the data before and after the GNSS install, but i don't want to lose the 'approach' time
        #### Are we filtering by GNSS install/unintsall or PT install/uninstall??? ####
#     #comjpromise is to filter by DATE, not DATETIME
    filter(pt_time_UTC >= as.POSIXct(paste0(Date_GNSS_Install,' 00:00:00'),format= "%m/%d/%Y %H:%M:%S"))%>%
    # filter(case_when(!is.na(Date_GNSS_Uninstall)~ pt_time_UTC <= as.POSIXct(Date_GNSS_Uninstall,format= "%m/%d/%Y"))) %>% 
     dplyr::select(-Date_GNSS_Uninstall,-Date_GNSS_Install)}else{
        
            pt_data_for_offset =pt_data%>%
    filter(pt_time_UTC >= as.POSIXct(paste0(Date_GNSS_Install,' 00:00:00'),format= "%m/%d/%Y %H:%M:%S"))%>%
    filter(pt_time_UTC <= as.POSIXct(paste0(Date_GNSS_Uninstall,' 23:59:59'),format= "%m/%d/%Y %H:%M:%S")) %>% 
     dplyr::select(-Date_GNSS_Uninstall,-Date_GNSS_Install)
    
    }
    
    #now, we've got to go get all of the gnss data associated with our pt. let's go find the data
    log_files=rbind( unique(pt_data$driftID_install),unique(pt_data$driftID_uninstall)) #need both files to work on
    #log_files=unique(prepped_pt$driftID_install)
    log_files=log_files[!is.na(log_files)]
        
  
    #in case the PT went dry, need to eliminate missing installa nd uninstall files
    log_files=log_files[log_files != ""]
    
    #exception handling for when the indicated GNSS file is not available-----
    if(length(log_files)==0){
       
         clean_pt='there are no gnss files for this pt. I looked in the keyfile for GNSS files and cant find them. KEYFILE ERROR'
                write.csv(clean_pt,file=paste0(flagged_pt_output_directory,filename))
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

    #standardized files means we can pull this in a fixed position
  
      splitter=strsplit(logstring,'_')
 
        #match the first and second dates
    get_all_gnss=function(splitter){
    
        string_to_match=paste(splitter[6], splitter[7],sep='_')
        
        print(string_to_match)
       correct_drift_index=which(!is.na(str_match(list.files(gnss_drift_data_directory),string_to_match)))
       driftstring=list.files(gnss_drift_data_directory,full.names=TRUE)[correct_drift_index]

        
        
        if(length(driftstring)>1){#we want the most recent
            split2=as.POSIXct(paste0(substring(do.call(rbind,strsplit(driftstring,'_'))[,11],1,8),substring(do.call(rbind,strsplit(driftstring,'_'))[,11],10,15) ),
                              format='%Y%m%d%H%M%S')
            
           latest_file_index=which(split2==max(split2))
            #position 11 is the munge date. Take the most recent
    
            driftstring=driftstring[latest_file_index]

            }
       
            read_multi_gnss=function(driftstring){
                output=read.csv(driftstring,header=TRUE,stringsAsFactors = FALSE)
                }
        
        if(identical(driftstring, character(0))){
               clean_pt='this file has a gnss file that likely hasnt been processed yet. PUSH/PULL GNSS from JPL'
                write.csv(clean_pt,file=paste0(flagged_pt_output_directory,filename))
        return(NA)}
     
    
    #function to read mutliple gnss files
      gnss_log=do.call(rbind,lapply(driftstring,read_multi_gnss))%>%
        mutate(datetime=gnss_time_UTC)%>%
        mutate(datetime=as.POSIXct(datetime)) #needed as when it gets written to csv it becomes not a posix objec
       
                       # print(gnss_log)
                       # bonk
    }
    
  gnss_log=do.call(rbind,lapply(splitter,get_all_gnss))
    


    #now, join the GNSS 1Hz data to the PT 15 minute data
    clean_pt_time=difference_inner_join(pt_data_for_offset,gnss_log,by='datetime',max_dist=time_thresh,distance_col='test')
    
    #need lon then lat
    distance_m=geodist(cbind(clean_pt_time$pt_lon, clean_pt_time$pt_lat),    cbind(clean_pt_time$gnss_Lon, clean_pt_time$gnss_Lat), paired=TRUE,measure='haversine')

    clean_pt=cbind(clean_pt_time,distance_m)%>%
    filter(distance_m<dist_thresh)%>%
        #ok, let's dplyr::select for stuff we want to keep now
    transmute(pt_level=pt_level, temperature=temperature,gnss_time_UTC=as.POSIXct(gnss_time_UTC,format ="%Y-%m-%d %H:%M:%S"),gnss_wse=gnss_wse,pt_time_UTC=pt_time_UTC,
                  pt_serial=pt_serial,
                  gnss_lat=gnss_Lat, gnss_lon=gnss_Lon, pt_lat=pt_lat, pt_lon=pt_lon,
                  gnss_pt_dist_m=distance_m ,keyid=keyid, pt_install_UTC=pt_install_UTC,
                  pt_uninstall_UTC=pt_uninstall_UTC)
   
    #if we want to filter to just times covered by the install and uninstall, we'd do so with 
    #a pipe here. Currently not doing that as we want to check in the middle for changes.
      
      #next, ensure there are enough gnss points to make a valid offset correction
      if(nrow(clean_pt)==0){
     
         # bonk
          clean_pt='there are no gnss points to make a correction with. The thresholds that define -close enough- between GNSS and pt arent met'
                write.csv(clean_pt,file=paste0(flagged_pt_output_directory,filename))
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

    #now we've got a dataframe for each key file. Check each for what we need
    check_data_frames=function(wse_pt,pt_data,filename_base){
    
      filename=paste0(filename_base,'_',unique(wse_pt$keyid))

          if (all(is.na(wse_pt))){
            print(filename)
                    clean_pt='there are no gnss points to make a correction with. The thresholds that define -close enough- between GNSS and pt arent met'
                write.csv(clean_pt,file=paste0(flagged_pt_output_directory,filename))
            return(NA)}
          # 


          if (nrow(wse_pt)<5 ){
              flag=flag+10
            # print(filename)
            # print('not enough data to create offset, change your thresholds or double check the data')
            # output='not enough data to create offset, change your thresholds or double check the data. There are data, but not a lot'
            # offset_pt=mutate(offset_pt,error=output)
            # write.csv(offset_pt,file=paste0(flagged_pt_output_directory,filename))
            # return(NA)
          }


          #first strip the offset df into just the gnss time and the pt correction and SD

    svelte_offset_pt=dplyr::select(wse_pt,pt_correction,pt_wse_sd,pt_time_UTC)%>%
    mutate(pt_time_UTC=as.POSIXct(pt_time_UTC,format ="%Y-%m-%d %H:%M:%S"))%>%
    group_by(pt_time_UTC)%>%
    summarise(pt_time_UTC=first(pt_time_UTC),pt_correction=first(pt_correction),pt_wse_sd=first(pt_wse_sd))
    
 
    find_closest_ping=function(pt_df,offset_df){
        time_diff= as.POSIXct(pt_df['pt_time_UTC'])- offset_df$pt_time_UTC
       time_index= first(which (abs(time_diff) == min(abs(time_diff),na.rm=TRUE)))
        correct_ping=offset_df[time_index,]%>%
        transmute(ping_time_UTC=pt_time_UTC,pt_correction=pt_correction,pt_wse_sd=pt_wse_sd)
        }
    
    correct_pings=do.call(rbind,apply(pt_data,1,find_closest_ping,offset_df=svelte_offset_pt))
    
    final_pt=cbind(pt_data,correct_pings)%>%
            mutate(pt_wse=pt_level+pt_correction)%>%
            mutate(sigma_pt_correction_m=sd(pt_correction))%>%
            mutate(flag=flag)
    #this is a weighted variance of allt he corrections that were applied. There may be only 1-5 corrections ('ping times'), but if e.g. a value is only applied to one PT time then it isn't really affecting the PT record that much. Instead of flagging this, we pass this to the PT file.       

          print(filename)
          print('this file passed all checks')
          write.csv(final_pt,file=paste0(QA_QC_pt_output_directory,filename),row.names=FALSE)


    } #end loop function for dataframes
    
        
    
    dummy=lapply(final_pt_data_frames,check_data_frames,pt_data=pt_data,filename_base=filename_base)
    
    gc()
    rm(dummy)


}#end toplevel function 









