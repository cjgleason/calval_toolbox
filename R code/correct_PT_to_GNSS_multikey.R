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
    
    
    #check to see if the PT thinks it shifted 
    pt_shift_vector= c(0,pt_data$pt_level)-c(pt_data$pt_level,0)
    pt_shift_vector=pt_shift_vector[2:(length(pt_shift_vector)-2)]
    
 
    if(any(pt_shift_vector>change_thresh_15_min)){
      clean_pt='the pt self-reports a shift greater than the 15 miniute change threshold'
                write.csv(clean_pt,file=paste0(flagged_pt_output_directory,filename))
        }
  

    
    if (is.na(pt_data[1,]$Date_GNSS_Uninstall)){ #in the case where there is no uninstall
    pt_data_for_offset =pt_data%>%
#         #taylor says we can throw out all the data before and after the GNSS install, but i don't want to lose the 'approach' time
#     #comjpromise is to filter by DATE, not DATETIME
    filter(pt_time_UTC >= as.POSIXct(paste0(Date_GNSS_Install,' 00:00:00'),format= "%m/%d/%Y %H:%M:%S"))%>%
    # filter(case_when(!is.na(Date_GNSS_Uninstall)~ pt_time_UTC <= as.POSIXct(Date_GNSS_Uninstall,format= "%m/%d/%Y"))) %>% 
     dplyr::select(-Date_GNSS_Uninstall,-Date_GNSS_Install)}else{
        
            pt_data_for_offset =pt_data%>%
    filter(pt_time_UTC >= as.POSIXct(paste0(Date_GNSS_Install,' 00:00:00'),format= "%m/%d/%Y %H:%M:%S"))%>%
    filter(pt_time_UTC <= as.POSIXct(paste0(Date_GNSS_Uninstall,' 23:59:59'),format= "%m/%d/%Y %H:%M:%S")) %>% 
     dplyr::select(-Date_GNSS_Uninstall,-Date_GNSS_Install)
    
    }
    
    # print(min(pt_data_for_offset$pt_time_UTC))
    # print(max(pt_data_for_offset$pt_time_UTC))
    # bonk
    

 
    #cleanup
    filename=raw_pt_file    
    filename_base=sub("\\..*","",strsplit(filename,'/')[[1]][length(strsplit(filename,'/')[[1]])])
    filename=paste0(filename_base,'_',unique(pt_data$keyid))

    #now, we've got to go get all of the gnss data associated with our pt. let's go find the data
    log_files=rbind( unique(pt_data$driftID_install),unique(pt_data$driftID_uninstall)) #need both files to work on
    #log_files=unique(prepped_pt$driftID_install)
    log_files=log_files[!is.na(log_files)]
        
  
    #in case the PT went dry, need to eliminate missing installa nd uninstall files
    log_files=log_files[log_files != ""]
    
    #exception handling for when the indicated GNSS file is not available-----
    if(length(log_files)==0){
         clean_pt='there are no gnss files for this pt'
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
#    driftstring=list.files(gnss_drift_data_directory)[do.call(rbind,lapply(list.files(gnss_drift_data_directory),getit_positive,logstring))]
    
    
    #standardized files means we can pull this in a fixed position
      splitter=strsplit(logstring,'_')
        #match the first and second dates
    get_all_gnss=function(splitter){
    
        if (length(splitter) ==8){
              string_to_match=paste(splitter[6], splitter[7],sep='_')
            }else{
        string_to_match=paste(splitter[6], splitter[7],sep='_')}
    
       correct_drift_index=which(!is.na(str_match(list.files(gnss_drift_data_directory),string_to_match)))
       driftstring=list.files(gnss_drift_data_directory,full.names=TRUE)[correct_drift_index]
        
        # print(driftstring)
    #function to read mutliple gnss files
      gnss_log=read.csv(driftstring,header=TRUE,stringsAsFactors = FALSE)%>%
        mutate(datetime=gnss_time_UTC)%>%
        mutate(datetime=as.POSIXct(datetime)) #needed as when it gets written to csv it becomes not a posix objec
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
          clean_pt='there are no gnss points to make a correciton with'
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
    
    # print('in the offset pt mean')
    # print(unique(offset_pt_mean$pt_correction))
    
      offset_pt_sd =clean_pt%>%
      group_by(pt_time_UTC,keyid)%>%
        mutate( offset=(gnss_wse-pt_level))%>%
        summarize(pt_correction_sd=sd(offset))
      
      wse_pt=clean_pt%>%
        left_join(offset_pt_mean,by=c('pt_time_UTC','keyid'))%>%
        left_join(offset_pt_sd,by=c('pt_time_UTC','keyid'))%>%
        mutate(pt_wse=pt_level+pt_correction)%>%
        mutate(pt_wse_sd= pt_correction_sd + 0.001) #sets the uncertainty to equal to the variance in all of the offsets used to create the 
    
    # print('in the wse pt')
    # print(unique(wse_pt$pt_correction))

    #split into multiple files based on the keyids
    final_pt_data_frames=split(wse_pt,wse_pt$keyid)


    
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
    # print('here')
    # print(head(offset_pt))
    # print('sd offset')
    # print(sd(offset_pt$pt_correction))
    # bonk
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

          svelte_offset_pt=dplyr::select(offset_pt,pt_correction,pt_wse_sd,pt_time_UTC)%>%
    mutate(pt_time_UTC=as.POSIXct(pt_time_UTC,format ="%Y-%m-%d %H:%M:%S"))%>%
    group_by(pt_time_UTC)%>%
    summarise(pt_time_UTC=first(pt_time_UTC),pt_correction=first(pt_correction),pt_wse_sd=first(pt_wse_sd))
    
# print('in the svelte offset pt')
#     print(unique(svelte_offset_pt$pt_time_UTC))
#     print(unique(svelte_offset_pt$pt_correction))
    
  
    
          #need the OG pt data

 
    # in this case we want all the data from when the PT was in the water. 
 
    find_closest_ping=function(pt_df,offset_df){
 
        time_diff= as.POSIXct(pt_df['pt_time_UTC'])- offset_df$pt_time_UTC
        #time_diff= pt_df$pt_time_UTC- offset_df$pt_time_UTC
        # print(time_diff)
       time_index= first(which (abs(time_diff) == min(abs(time_diff),na.rm=TRUE)))
        correct_ping=offset_df[time_index,]%>%
        transmute(ping_time_UTC=pt_time_UTC,pt_correction=pt_correction,pt_wse_sd=pt_wse_sd)
 
        }
    
    correct_pings=do.call(rbind,apply(pt_data,1,find_closest_ping,offset_df=svelte_offset_pt))
    
            final_pt=cbind(pt_data,correct_pings)%>%
            mutate(pt_wse=pt_level+pt_correction)
  # print('corrections in the final output')
  #   print(unique(final_pt$pt_correction))
  
    check_diff_in_out=svelte_offset_pt%>%
    mutate(date=as.Date(pt_time_UTC))%>%
    group_by(date)%>%
    summarise(mean_correction=mean(pt_correction))
    
    difference_index=expand.grid(a=1:nrow(check_diff_in_out),b=1:nrow(check_diff_in_out))%>%
    filter(a!=b)
    #google search to find unique combos. tested and verified
    difference_index=difference_index[!duplicated(t(apply(difference_index[c("a", "b")], 1, sort))), ]
    
   #print( difference_index)
  
    
    difference_vector=list()
    for(i in 1:nrow(difference_index)){ 
    difference_vector[[i]]=  abs(check_diff_in_out$mean_correction[difference_index$a[i]]- check_diff_in_out$mean_correction[difference_index$b[i]])
        
    }


    final_pt=final_pt%>%
    mutate(mean_install_minus_uninstall_correction_differences_m=difference_vector[[1]])
    
    # print(head(final_pt))
    # bonk

 
          print(filename)
          print('this file passed all checks')
          write.csv(final_pt,file=paste0(QA_QC_pt_output_directory,filename),row.names=FALSE)


    } #end loop function for dataframes
    
        
    
    dummy=lapply(final_pt_data_frames,check_data_frames,pt_data=pt_data,filename_base=filename_base)
    
    gc()
    rm(dummy)


}#end toplevel function 









