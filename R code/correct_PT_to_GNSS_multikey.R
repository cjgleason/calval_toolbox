correct_pt_to_gnss_multikey= function(raw_pt_file,master_key,dist_thresh,time_thresh,pt_data_directory,
                             gnss_drift_data_directory,QA_QC_pt_output_directory, flagged_pt_output_directory,
                             gnss_sd_thresh,offset_sd_thresh,change_thresh_15_min,dry_threshold){
library(dplyr)
library(stringr)
library(tidyr)
library(fuzzyjoin)
library(geodist)
library(bayesbio)
library(ncdf4)
library(lubridate)
  
  #munge PTs if needed------
  # source('/nas/cee-water/cjgleason/calval_toolbox/R code/correct_PT_to_GNSS_multikey.R')
  # if (process_PTs==1){
  # dist_thresh=150 # 150m
  # time_thresh= 15*60 #minutes as seconds, centered, so 15 =30 mins total time
  # GNSS_sd_thresh=0.15 # 15cm how much variance do you want in the GNSS data when it is within the distance threshold?
  # offset_sd_thresh=0.10 #m, so 10cm. the the PT apparantly shift by more than a cm?
  # change_thresh_15_min=0.15#m- does it change more than 5cm in 15 minutes? that is a discontinuity in offset
  # dry_threshold = 0.10 #This is a raw pt level where anything below is considered a PT out of water - this can change if we want
  # #first, move .csv files with an 'L1' in them over to the PT_data_directory
  # munged_files= list.files(PT_data_directory,
  #                          recursive= TRUE)
  
  # PT_index=which(!is.na(do.call(rbind,lapply(munged_files,str_match,'PT_L1'))))
  # PT_files=munged_files[PT_index]
  # csv_index=which(!is.na(do.call(rbind,lapply(PT_files,str_match,'.csv'))))
  # raw_PT_files=PT_files[csv_index]
  
  
  # #open the key files   - Taylor added column labels and way to handle blank cells to make as NA
  
  # read_keys=function(keyfile){
  
  #     # Read in key file and check column names and NODE ID for precision lost with scientific notation #
  #   this_key= read.csv(keyfile,stringsAsFactors=FALSE, na.strings = c("","NA","na","NaN", " "))%>%
  #     mutate(keyid=keyfile)%>%
  #     mutate(pt_serial=as.integer(PT_Serial))
  
  #     }
  
  # master_key= do.call(rbind,lapply(PT_key_file,read_keys))
  #     # key_file column names to check against (keyfile and pt_serial are added in this process, do not add to original key files)#
  # key_col_names = c("PT_Serial", "Label", "Baro_Comp", "Node_ID", "Reach_ID", "US_Reach_ID", 
  #                       "DS_Reach_ID", "Lat_WGS84", "Long_WGS84", "Install_method",
  #                       "Date_PT_Install", "Time_PT_Install_UTC", "Date_PT_Uninstall",
  #                       "Time_PT_Uninstall_UTC", "Date_GNSS_Install", "Time_GNSS_Install_Start_UTC", 
  #                       "Time_GNSS_Install_End_UTC", "GNSS_Offset_m", "Receiver_Install", "Original_Install_Log_File", 
  #                       "Final_Install_Log_File", "Date_GNSS_Uninstall", "Time_GNSS_Uninstall_Start_UTC", 
  #                       "Time_GNSS_Uninstall_End_UTC", "Receiver_Uninstall", "Original_Uninstall_Log_File", "Final_Uninstall_Log_File",
  #                       "keyid", "pt_serial")
  # hub_key_colnames = colnames(master_key)
  
  # key_check <- function(key_col_names, hub_key_colnames){
  #   if(length(key_col_names) == length(hub_key_colnames)) {
  #     print('Key file column names are of equal length')
  #   }
  #   if(length(which(is.na(match(key_col_names,hub_key_colnames))))!=0) {
  #     stop(paste('Key file has a missing/misnamed column',which(is.na(match(key_col_names,hub_key_colnames))),",",
  #                key_col_names[!key_col_names %in% hub_key_colnames],", please fix and reupload, and/or the Key file has an extra/misnamed column",
  #                which(is.na(match(hub_key_colnames,key_col_names))),",", hub_key_colnames[!hub_key_colnames %in% key_col_names],", please fix and reupload"))
  #   }
  #   if(length(unique(master_key$Node_ID))<=2){stop("Check that node ID in key file did not lose precision with scientific notation, if so, fix key and reupload.")}
  #     else{print("Key file passes QA/QC checks")}
  # }
  
  # key_check(key_col_names,hub_key_colnames)
  
  # #three key info here-
  #     #1 PTs in the key
  #     #2 unmunged PTs
  #     #3 QA QC PTs
  #     #4 flagged PTs
  
  #     #3 + 4 are processed files
  
  # getit_processed=function(inputstring){   
  #     output=paste(strsplit(inputstring,'_')[[1]][1:8],collapse='_')
  #     output=sub("\\..*","",output)
  # }
  
  # getit_key =function(inputstring){   
  #     output=paste(c('SWOTCalVal',rivername,'PT','L1',inputstring),collapse='_')
  #     output=sub("\\..*","",output)
  # }
  
  # getit_negative=function(longstring, shortstrings){
  #     #search for a pattern between one and many strings with partial matches allows
  #         output=!any(str_detect(longstring,shortstrings)  )
  # }
  
  # getit_positive=function(longstring, shortstrings){
  #     #search for a pattern between one and many strings with partial matches allows
  #         output=any(str_detect(longstring,shortstrings))  
  # }
  
  # processed_files= do.call(rbind,lapply(c(list.files(QA_QC_PT_output_directory),list.files(flagged_PT_output_directory)),getit_processed))
  # key_files=do.call(rbind,lapply(master_key$Label,getit_key))
  
  #     if (is.null(processed_files)){unprocessed_files=raw_PT_files}else{
  # unprocessed_files=raw_PT_files[do.call(rbind,lapply(raw_PT_files,getit_negative,processed_files))]}
  # in_key_unprocessed=unprocessed_files[do.call(rbind,lapply(unprocessed_files,getit_positive,key_files))]
  
  # # print('raw')
  # # print(raw_PT_files)
  # # print('processed')
  # # print(processed_files)
  # # print('unprocessed')
  # # print(unprocessed_files)
  # # print('unprocessed in key')
  
  # for(thisone in in_key_unprocessed){
  
  #         correct_pt_to_gnss_multikey(thisone,
  #                   master_key=master_key,
  #                   dist_thresh=dist_thresh,
  #                   time_thresh=time_thresh,
  #                   pt_data_directory=PT_data_directory,
  #                   gnss_drift_data_directory=QA_QC_drift_output_directory,
  #                   QA_QC_pt_output_directory=QA_QC_PT_output_directory,
  #                   flagged_pt_output_directory=flagged_PT_output_directory,
  #                   gnss_sd_thresh=GNSS_sd_thresh,
  #                   offset_sd_thresh=offset_sd_thresh,
  #                   change_thresh_15_min=change_thresh_15_min,
  #                   dry_threshold = dry_threshold) 
  
  
  # }
  
  
  # #       cl=makeCluster(20,type='FORK')
  # #   dummy=parLapply(cl, in_key_unprocessed,correct_pt_to_gnss_multikey,
  # #                    master_key=master_key,
  #                   # dist_thresh=dist_thresh,
  #                   # time_thresh=time_thresh,
  #                   # pt_data_directory=PT_data_directory,
  #                   # gnss_drift_data_directory=QA_QC_drift_output_directory,
  #                   # QA_QC_pt_output_directory=QA_QC_PT_output_directory,
  #                   # flagged_pt_output_directory=flagged_PT_output_directory,
  #                   # gnss_sd_thresh=GNSS_sd_thresh,
  #                   # offset_sd_thresh=offset_sd_thresh,
  #                   # change_thresh_15_min=change_thresh_15_min) 
  # #   stopCluster(cl)
  
  
  #     }#end if process PT
  
  
    
    flag=0

    #Creates the PT record as WSE instead of level. Uses multiple key files in cases where PTs went in and out of the water.
    
  pt_serial_file=as.integer(read.table(paste0(pt_data_directory,raw_pt_file), header = FALSE, nrow = 1)$V1) 
 
    #read in master_key--------------
    keyfile=master_key%>%
      mutate('driftID_install'= sub("\\..*","",Final_Install_Log_File))%>%
      mutate('driftID_uninstall'= sub("\\..*","",Final_Uninstall_Log_File))%>%
      mutate(pt_serial=as.integer(PT_Serial))

    #check for a serial match-----------
    if( pt_serial_file %in% keyfile$pt_serial == FALSE){return(NA)}
   
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
    #Added reach ID to plot
    
    pt_data=pt_data %>%
      left_join(keyfile,by='pt_serial')  %>%
    mutate(pt_Lat= Lat_WGS84)%>%
      mutate(pt_Lon=Long_WGS84)%>%
      mutate(pt_time_UTC=datetime)%>%
      filter(pt_serial==pt_serial_file)%>% #limit to just the PT we want. The filter above should take care 
    #of the case where the serial is not in the key file
    # This is where the Munged_PT columns come from key file - 12/20 added reach and nodeID. 1/05 added gnss install end and gnss uninstall start to limit
    transmute(pt_time_UTC=pt_time_UTC,pt_lat=pt_Lat,pt_lon=pt_Lon,
     pt_install_UTC=as.POSIXct(paste(Date_PT_Install,Time_PT_Install_UTC),format= "%m/%d/%Y %H:%M"),
     pt_uninstall_UTC=as.POSIXct(paste(Date_PT_Uninstall,Time_PT_Uninstall_UTC),format= "%m/%d/%Y %H:%M"),
     gnss_install_UTC=as.POSIXct(paste(Date_GNSS_Install,Time_GNSS_Install_Start_UTC),format= "%m/%d/%Y %H:%M"),
     gnss_install_UTC_end = as.POSIXct(paste(Date_GNSS_Install,Time_GNSS_Install_End_UTC),format= "%m/%d/%Y %H:%M"),
     gnss_uninstall_UTC_start = as.POSIXct(paste(Date_GNSS_Uninstall,Time_GNSS_Uninstall_Start_UTC),format= "%m/%d/%Y %H:%M"), 
     gnss_uninstall_UTC=as.POSIXct(paste(Date_GNSS_Uninstall,Time_GNSS_Uninstall_End_UTC),format= "%m/%d/%Y %H:%M"),
         install_method=Install_method, pt_serial=pt_serial,         
         pt_level=Level,temperature=Temperature,
              driftID_install=driftID_install,driftID_uninstall=driftID_uninstall,
              datetime=datetime,
              keyid=keyid, Date_GNSS_Install=Date_GNSS_Install,Date_GNSS_Uninstall=Date_GNSS_Uninstall, Reach_ID=Reach_ID, Node_ID=Node_ID) %>%
    filter(pt_time_UTC >= pt_install_UTC)%>%
    filter(pt_time_UTC <= pt_uninstall_UTC)%>%
    filter(pt_level >= dry_threshold)# limit for dry here with a filter in this pipe - must check that dates are ok
   
    ### Test here for pt checking of second condition ###

    if(nrow(pt_data)==0){
          clean_pt='the keyfile says this pt was never in the water. KEYFILE ERROR'
                write.csv(clean_pt,file=paste0(flagged_pt_output_directory,filename))
        return(NA)
    }
    
    #check to see if the PT thinks it shifted 
    pt_shift_vector= c(0,pt_data$pt_level)-c(pt_data$pt_level,0)
    if(length(pt_shift_vector)>5){
    pt_shift_vector=pt_shift_vector[3:(length(pt_shift_vector)-3)]}
    
  
    ### PT has shifted, but we flag and move to munged PT bin ###
    if(any(abs(pt_shift_vector)>change_thresh_15_min)){
       # print(plot(pt_shift_vector))
        flag=1
      # clean_pt='the pt self-reports a shift greater than the 15 miniute change threshold'
      #           write.csv(clean_pt,file=paste0(flagged_pt_output_directory,filename))
      #   return(NA)
        }
    
    ### An exception to handle when install time is not in PT file, 15 minutes is added to start GNSS occupy time to encompass PT click ###
    
    if((as.POSIXct(pt_data[1,]$gnss_install_UTC + minutes(15)) < as.POSIXct(pt_data[1,]$pt_time_UTC))){
        clean_pt='the keyfile indicates the install time did not occur within the PT data range, fix key file'
            write.csv(clean_pt,file=paste0(flagged_pt_output_directory,filename))
        return(NA)
        }
  
    if (is.na(pt_data[1,]$Date_GNSS_Uninstall)){ #in the case where there is no uninstall
    pt_data_for_offset =pt_data%>%
#         #taylor says we can throw out all the data before and after the GNSS install, but i don't want to lose the 'approach' time - I think we need to toss approach/
        #### Are we filtering by GNSS install/unintsall or PT install/uninstall??? ####
#     #comjpromise is to filter by DATE, not DATETIME
    filter(pt_time_UTC >= as.POSIXct(paste0(Date_GNSS_Install,' 00:00:00'),format= "%m/%d/%Y %H:%M:%S"))%>%
    # filter(case_when(!is.na(Date_GNSS_Uninstall)~ pt_time_UTC <= as.POSIXct(Date_GNSS_Uninstall,format= "%m/%d/%Y"))) %>% 
     dplyr::select(-Date_GNSS_Uninstall,-Date_GNSS_Install)}
    else{
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
            #position 11 is the version munge date. Take the most recent
    
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
    }
    
  gnss_log=do.call(rbind,lapply(splitter,get_all_gnss))
    
  ### 1/05 I am not convinced this is working for all key files at once...
    #double handle. above we determined that there aren't gnss data ready for this yet, but it returned an 
    #NA to here, which was writing teh file correctly and then bonking here. 
     if(all(is.na(gnss_log))){
         return(NA)} 

    #now, join the GNSS 1Hz data to the PT 15 minute data - 10/26 may need to make this ignore seconds in pt file - dies during PD335 file at 1 minute data
    clean_pt_time=difference_inner_join(pt_data_for_offset,gnss_log,by='datetime',max_dist=time_thresh,distance_col='test')
    
    #need lon then lat
    distance_m=geodist(cbind(clean_pt_time$pt_lon, clean_pt_time$pt_lat),    cbind(clean_pt_time$gnss_Lon, clean_pt_time$gnss_Lat), paired=TRUE,measure='haversine')

    clean_pt=cbind(clean_pt_time,distance_m)%>%
    filter(distance_m<dist_thresh)%>% ### 1/05 - We should remove the distance and time threshold and use install/unintstall GNSS times
        #ok, let's dplyr::select for stuff we want to keep now
    transmute(pt_level=pt_level, temperature=temperature,gnss_time_UTC=as.POSIXct(gnss_time_UTC,format ="%Y-%m-%d %H:%M:%S"),
                  gnss_wse=gnss_wse,pt_time_UTC=pt_time_UTC,
                  pt_serial=pt_serial,
                  gnss_lat=gnss_Lat, gnss_lon=gnss_Lon, pt_lat=pt_lat, pt_lon=pt_lon,
                  gnss_pt_dist_m=distance_m ,keyid=keyid, pt_install_UTC=pt_install_UTC,
                  pt_uninstall_UTC=pt_uninstall_UTC,gnss_uncertainty_m=gnss_uncertainty_m)
  
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
    summarize(pt_correction= mean(offset,na.rm=TRUE),
              pt_correction_sd=  sqrt( sd(offset,na.rm=TRUE)^2 +sd(gnss_uncertainty_m)^2 ),
              pt_correction_wse_sd_m=sd(offset,na.rm=TRUE),
              pt_correction_gnss_sd_m=sd(gnss_uncertainty_m)) 

  # offset_pt_sd =clean_pt%>%
  # group_by(pt_time_UTC,keyid)%>%
  #   mutate( offset=(gnss_wse-pt_level))%>%
  #   summarize(,na.rm=TRUE)
  
  #clean_pt is the tidyied up PT file that keeps only the observations within some time and distance threshold to a GNSS ping
  #commenting new code on 12/21/23 to remove confusion. The wse was calculated, but this wasn't the whole record so it is moot.
  #we just need the correction here
correction_file_pt=clean_pt%>%
  left_join(offset_pt_mean,by=c('pt_time_UTC','keyid'))#%>%
  #left_join(offset_pt_sd,by=c('pt_time_UTC','keyid'))%>%
  # mutate(pt_wse=pt_level+pt_correction)%>%
  # mutate(pt_wse_sd= pt_correction_sd + 0.001) #sets the uncertainty to equal to the variance in all of the offsets used to create the

    #split into multiple files based on the keyids
    final_pt_data_frames=split(correction_file_pt,clean_pt$keyid)

    #now we've got a dataframe for each key file. Check each for what we need and make the correction
    #the idea is that we make a different csv file per each key file so we can track where they
    #came from, and since the keyfile defines when the PT was in the water
    check_data_frames=function(correction_file_pt,pt_data,filename_base){
   
      filename=paste0(filename_base,'_',unique(correction_file_pt$keyid))

          if (all(is.na(clean_pt))){
            print(filename)
                    clean_pt='there are no gnss points to make a correction with. The thresholds that define -close enough- between GNSS and pt arent met'
                write.csv(clean_pt,file=paste0(flagged_pt_output_directory,filename))
            return(NA)}
          if (nrow(clean_pt)<5 ){
              flag=flag+10
          }
      
      # final_pt=mutate(wse_pt,flag=flag)
    

          #first strip the offset df into just the gnss time and the pt correction and SD
    svelte_offset_pt=dplyr::select(correction_file_pt,pt_correction,pt_correction_sd,pt_time_UTC,pt_correction_wse_sd_m,pt_correction_gnss_sd_m)%>%
    mutate(pt_time_UTC=as.POSIXct(pt_time_UTC,format ="%Y-%m-%d %H:%M:%S"))%>%
    group_by(pt_time_UTC)%>%
    summarise(pt_time_UTC=first(pt_time_UTC),pt_correction=first(pt_correction),pt_correction_sd=first(pt_correction_sd),
              pt_correction_wse_sd_m=first(pt_correction_wse_sd_m),pt_correction_gnss_sd_m=first(pt_correction_gnss_sd_m))
    
    find_closest_ping=function(pt_df,offset_df){
      #make a time difference vector between the 15 min pt data and the correction vector defined above
       time_diff= as.POSIXct(pt_df['pt_time_UTC'])- offset_df$pt_time_UTC 
       
      #find the first difference, i.e., the closest time is the one we want
       time_index= first(which (abs(time_diff) == min(abs(time_diff),na.rm=TRUE)))
    

       
       
      #save that single row as a correction # 1/05 was -transmute(ping_time_UTC=pt_time_UTC
        correct_ping=offset_df[time_index,]%>%
        transmute(pt_time_UTC=pt_time_UTC,pt_correction=pt_correction,pt_correction_sd=pt_correction_sd,
                  pt_correction_wse_sd_m=pt_correction_wse_sd_m,pt_correction_gnss_sd_m=pt_correction_gnss_sd_m)
        }
    
    #loop through the PT dataframe that we read in at the very beginning- the full PT record filtered
    #for when it is in the water 'pt_data', and apply this closest ping operation
    correct_pings=do.call(rbind,apply(pt_data,1,find_closest_ping,offset_df=svelte_offset_pt))
#     print(unique(correct_pings$pt_time_UTC))
# print(unique(correct_pings$pt_correction))
    
    #join those closest ping values to the original PT data. IMPLICIT MATCH! but, no reordering was done
    #create the final wse and look across the corrections to quantify the variance in that correction
    # print(nrow(correct_pings))
    #     print(head(correct_pings))
    #     print(max(correct_pings$pt_time_UTC))
    # print(nrow(pt_data))
        
        # print(max(pt_data$pt_time_UTC)) # 1/05 These match length but are not applying offset before or after gnss install/uninstall...why?
    # final_pt=cbind(pt_data,correct_pings)%>% 
        ### This is NOT RIGHT - MUST FIXXXXXXX ####
    final_pt=pt_data %>%
        left_join(correct_pings,by='pt_time_UTC')%>%
            mutate(pt_wse=pt_level+pt_correction)%>%
            mutate(sigma_total_pt_correction_m=sd(pt_correction)) #this is the overall variance in corrections
      #add the quality flags
    
   print(final_pt[4141,])
        bonk
    #check to see if the PT thinks it shifted 
    pt_shift_vector_final= c(0,final_pt$pt_wse)-c(final_pt$pt_wse,0)
    if(length(pt_shift_vector_final)>5){
      pt_shift_vector_final=pt_shift_vector_final[3:(length(pt_shift_vector_final)-3)]}
    
    ### PT has shifted due to processing and offset changes ###
    if(any(abs(pt_shift_vector_final)>change_thresh_15_min)){
      # print(plot(pt_shift_vector))
      flag=flag+100
    }
    
    
    
    if(final_pt$sigma_total_pt_correction_m[1]>gnss_sd_thresh ){
      flag=flag+1000
    }
      
    #1, 10, 11,100,1000
    #   1 - if there is a shift in the raw PT data-
    #  10- if there are less than 5 GNSS points to make a correciton
    # 100- if after processing there is a shift in the PT data
    #1000- if if the overall corrections are noisy
    
    #xample
    # 1010 - both 1000 and 10 are on
    # 101 - shift in raw PT and shift in processed PT
    final_pt=final_pt%>%
    mutate(flag=flag)
         
### New analysis for offset correction ###

        #Step 1 - If(first-last) < tolerance (what is this tolerance?), then average first + last and apply to all timeseries (no fly-by's)
        
        
        
          print(filename)
          print('this file passed all checks')
          write.csv(final_pt,file=paste0(QA_QC_pt_output_directory,filename),row.names=FALSE)


    } #end loop function for dataframes
    
    dummy=lapply(final_pt_data_frames,check_data_frames,pt_data=pt_data,filename_base=filename_base)
    
    gc()
    rm(dummy)

}#end toplevel function 









