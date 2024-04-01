Sys.umask('002')
lake_PT=function(raw_pt_file,master_key,dist_thresh,time_thresh,pt_data_directory,
                 gnss_drift_data_directory,QA_QC_PT_output_directory, flagged_pt_output_directory,
                 gnss_sd_thresh,offset_sd_thresh,change_thresh_15_min,dry_threshold){

    #create WSE in lakes from PTs and GNSS

  library(dplyr)
  library(stringr)
  library(tidyr)
  library(fuzzyjoin)
  library(geodist)
  library(bayesbio)
  library(ncdf4)
  library(lubridate)
  # 
  # hubname='UNC'
  # continent='na'
  # lakename='YF'
  # PT_key_file=c('SWOTCalVal_YF_KEY_20230521_20230923.csv',
  #               'SWOTCalVal_YF_KEY_20230603_20230722.csv') #WM
  # utm_zone=6 #WM= 10
  # 
  # setwd(paste0('/nas/cee-water/cjgleason/calval/Processed data/',hubname,'/'))
  # 
  # pt_data_directory = paste0('/nas/cee-water/cjgleason/calval/xml_scripts/',hubname,'/Munged/')
  # munged_files= list.files(pt_data_directory,recursive= TRUE)
  # 
  # PT_index=which(!is.na(do.call(rbind,lapply(munged_files,str_match,'PT_L1'))))
  # PT_files=munged_files[PT_index]
  # 
  # csv_index=which(!is.na(do.call(rbind,lapply(PT_files,str_match,'.csv'))))
  # csv_PT_files=PT_files[csv_index]
  # 
  # correct_lake_index=which(!is.na(do.call(rbind,lapply(csv_PT_files,str_match,lakename))))
  # csv_PT_files=csv_PT_files[correct_lake_index]
  # 
  # gnss_drift_data_directory=paste0('/nas/cee-water/cjgleason/calval/Processed data/',hubname,'/Munged_lake_gnss_TR/')
  # QA_QC_PT_output_directory= paste0('/nas/cee-water/cjgleason/calval/Processed data/',hubname,'/Munged_lake_PT/')
  # flagged_pt_output_directory=paste0('/nas/cee-water/cjgleason/calval/Processed data/',hubname,'/flagged_lake_PT/')
  # 
  # 
  # gnss_sd_thresh=0.15 # 15cm how much variance do you want in the GNSS data when it is within the distance threshold?
  # offset_sd_thresh=0.10 #m, so 10cm. the the PT apparantly shift by more than a cm?
  # change_thresh_15_min=0.05#m- does it change more than 5cm in 15 minutes? that is a discontinuity in offset
  # dist_thresh=200
  # time_thresh=15*60
  # dry_threshold=0.10
  # 
  # read_keys=function(keyfile){
  # 
  #   # Read in key file and check column names and NODE ID for precision lost with scientific notation #
  #   this_key= read.csv(keyfile,stringsAsFactors=FALSE, na.strings = c("","NA","na","NaN", " "))%>%
  #     mutate(keyid=keyfile)%>%
  #     mutate(pt_serial=as.integer(PT_Serial))
  # 
  # }
  # 
  # ### Key file get and check ###
  # master_key= do.call(rbind,lapply(PT_key_file,read_keys))
  # 
  # raw_pt_file=csv_PT_files
  # raw_pt_file = raw_pt_file[13]
  
  #grab the serial number from the PT file itself. Standarized file
  pt_serial_file=as.integer(substring(read.table(paste0(pt_data_directory,raw_pt_file), 
                                                 header = FALSE, nrow = 1)$V1,1,7))
  print(paste0(pt_data_directory,raw_pt_file))
  
  #read in master_key--------------
  keyfile=master_key%>%
    mutate('driftID_install'= sub("\\..*","",Final_Install_Log_File))%>%
    mutate('driftID_uninstall'= sub("\\..*","",Final_Uninstall_Log_File))%>%
    mutate(pt_serial=as.integer(PT_Serial))
  #--------------------------------
print(pt_serial_file)
  #check for a serial match-----------
  if( pt_serial_file %in% keyfile$pt_serial == FALSE){
    print("pt is not in the keyfile")
    return(NA)}
  #-----------------------------------

  #now get the PT data, standardized(ish) by Taylor--------
  tryCatch({
    pt_data=read.csv(paste0(pt_data_directory,raw_pt_file),skip=10,header=TRUE) %>%
      mutate(pt_serial=as.integer(pt_serial_file)) 
  },
  error=function(cond){return(NA)})
  
  
  filename=raw_pt_file  
  filename_base=sub("\\..*","",strsplit(filename,'/')[[1]][length(strsplit(filename,'/')[[1]])])
  filename=paste0(filename_base,'_',unique(pt_data$keyid))
  
  if(!is.na(str_match(pt_data$Date[1],'UNIT') )){
    clean_pt='this PT has header issues related to the HTC version'
    write.csv(clean_pt,file=paste0(flagged_pt_output_directory,filename))
    return(NA)}
  
  
  #this !@#$!##*& field sometimes has AM/PM and sometimes 24 hour time. we need to parse it to figure out
  #which is which and then process.

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
  } else { #24 hour time
    pt_data=pt_data%>%
      mutate(datetime=as.POSIXct(paste(Date,Time),format= "%Y/%m/%d %H:%M:%S"))
  }
  #-----------------------------------
  
  # Check for non 15 minute data (1 minute now), but maybe could do anything less than 15? #
  # if (difftime(pt_data$datetime[2],pt_data$datetime[1],units="secs")<=60){
  #   pt_data = pt_data%>%
  #     mutate(datetime = lubridate::floor_date(datetime, unit = "15 mins")) %>% 
  #     group_by(datetime) %>% 
  #     summarise(Date = first(Date),
  #               Time = first(Time),
  #               ms= first(ms),
  #               Level = mean(Level),
  #               Temperature=mean(Temperature),
  #               pt_serial=first(pt_serial))%>%
  #     mutate(Time=format(datetime ,format="%H:%M:%S"))%>%
  #     select(Date,Time,ms,Level,Temperature,pt_serial,datetime)
  # }else{
  #   print("Data are 15 min")
  # }
  #now, we have a multikey that will sow mass confusion later. We need to select.
  #the right keyfile from all the keyfiles, using the 'pt in the water' logic, which 
  #says that the keyfile indicates and install and uninstall (in the water) time, which is always
  # %in% the total PT record. Grab pt_data min(time) and pt_data max(time), check for
  # %in%, and then we can proceed without a split on keyfiles.
  
  pt_mintime=min(pt_data$datetime,na.rm=TRUE)
  pt_maxtime=max(pt_data$datetime,na.rm=TRUE)
  
  keyfile_check=filter(keyfile,pt_serial==pt_serial_file)%>%
    mutate(  pt_install_UTC=as.POSIXct(paste(Date_PT_Install,Time_PT_Install_UTC),format= "%m/%d/%Y %H:%M"),
             pt_uninstall_UTC=as.POSIXct(paste(Date_PT_Uninstall,Time_PT_Uninstall_UTC),format= "%m/%d/%Y %H:%M"),
             # gnss_install_UTC=as.POSIXct(paste(Date_GNSS_Install,Time_GNSS_Install_End_UTC),format= "%m/%d/%Y %H:%M"),
             gnss_install_UTC=ifelse(Time_GNSS_Install_End_UTC < Time_GNSS_Install_Start_UTC,
                                     as.POSIXct(paste(Date_GNSS_Install,Time_GNSS_Install_End_UTC),format= "%m/%d/%Y %H:%M:%S")+86400,
                                     as.POSIXct(paste(Date_GNSS_Install,Time_GNSS_Install_End_UTC),format= "%m/%d/%Y %H:%M:%S")),
             gnss_install_UTC=as.POSIXct(gnss_install_UTC, format="%m/%d/%Y %H:%M:%S", origin ="01/01/1970 00:00:00"),
             gnss_uninstall_UTC=as.POSIXct(paste(Date_GNSS_Uninstall,Time_GNSS_Uninstall_Start_UTC),format= "%m/%d/%Y %H:%M"))#%>%
  #check for the %in% here
  keyfile_check2 = filter(keyfile_check, gnss_install_UTC >= (pt_mintime) )%>%
    filter(case_when(is.na(gnss_uninstall_UTC) ~   floor_date(pt_uninstall_UTC, unit="15 mins") <= pt_maxtime,  
                     !(is.na( gnss_uninstall_UTC)) ~ gnss_uninstall_UTC <= pt_maxtime))
  
  if(nrow(keyfile_check2)==0){
    #this happens due to a comjplicated chain of events involving barologoggers making the effective pt record mouch shorter
    #than the keyfile says it is.
    
    
    
    clean_pt=paste('error arising from mismatch between baro-corrected PT times and keyfile times', 
                   print(paste("Raw PT record range is", pt_mintime, "to", pt_maxtime)), 
                   print(paste("Keyfile PT record range is",keyfile_check$pt_install_UTC, "to", keyfile_check$pt_uninstall_UTC)),
                   print(paste("Keyfile GNSS install time is",keyfile_check$gnss_install_UTC, "Keyfile GNSS uninstall time is", keyfile_check$gnss_uninstall_UTC)))
    
    
    
    write.csv(clean_pt,file=paste0(flagged_pt_output_directory,filename))
    return(NA)
    
    
  }
  
  keyfile=keyfile_check2
  
  
  #later, we will filter again to only 'occupy' times-----------------
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
              gnss_install_UTC_start=as.POSIXct(paste(Date_GNSS_Install,Time_GNSS_Install_Start_UTC),format= "%m/%d/%Y %H:%M"),
              gnss_install_UTC_end = ifelse(Time_GNSS_Install_End_UTC < Time_GNSS_Install_Start_UTC,
                                            as.POSIXct(paste(Date_GNSS_Install,Time_GNSS_Install_End_UTC),format= "%m/%d/%Y %H:%M:%S")+86400,
                                            as.POSIXct(paste(Date_GNSS_Install,Time_GNSS_Install_End_UTC),format= "%m/%d/%Y %H:%M:%S")),
              gnss_install_UTC_end = as.POSIXct(gnss_install_UTC_end,format="%m/%d/%Y %H:%M:%S", origin ="01/01/1970 00:00:00"),
              gnss_uninstall_UTC_start = as.POSIXct(paste(Date_GNSS_Uninstall,Time_GNSS_Uninstall_Start_UTC),format= "%m/%d/%Y %H:%M"), 
              gnss_uninstall_UTC_end = ifelse(Time_GNSS_Uninstall_End_UTC < Time_GNSS_Uninstall_Start_UTC,
                                              as.POSIXct(paste(Date_GNSS_Uninstall,Time_GNSS_Uninstall_End_UTC),format= "%m/%d/%Y %H:%M:%S")+86400,
                                              as.POSIXct(paste(Date_GNSS_Uninstall,Time_GNSS_Uninstall_End_UTC),format= "%m/%d/%Y %H:%M:%S")),
              gnss_uninstall_UTC_end=as.POSIXct(gnss_uninstall_UTC_end,format="%m/%d/%Y %H:%M:%S", origin ="01/01/1970 00:00:00"),
              install_method=Install_method, pt_serial=pt_serial,         
              pt_level=Level,temperature=Temperature,
              driftID_install=driftID_install,driftID_uninstall=driftID_uninstall,
              datetime=datetime,
              keyid=keyid, Date_GNSS_Install=Date_GNSS_Install,Date_GNSS_Uninstall=Date_GNSS_Uninstall, Reach_ID=Reach_ID, Node_ID=Node_ID) %>%
    filter(pt_time_UTC >=pt_install_UTC  )%>%
    filter(pt_time_UTC <=pt_uninstall_UTC  )%>%
    filter(pt_level >= dry_threshold)# limit for dry here with a filter in this pipe - must check that dates are ok
  #-----------------------------------
  
  #check for a keyfile error----------
  if(nrow(pt_data)==0){
    clean_pt='the keyfile says this pt was never in the water. KEYFILE ERROR'
    write.csv(clean_pt,file=paste0(flagged_pt_output_directory,filename))
    return(NA)
  }
  #-----------------------------------
  
  
  # # An exception to handle when install time is not in PT file, 
  # #15 minutes is added to start GNSS occupy time to encompass PT click--------
  # if((as.POSIXct(pt_data[1,]$gnss_install_UTC_start + minutes(15)) < as.POSIXct(pt_data[1,]$pt_time_UTC))){
  #   clean_pt='the keyfile indicates the install time did not occur within the PT data range, fix key file'
  #   write.csv(clean_pt,file=paste0(flagged_pt_output_directory,filename))
  #   return(NA)
  # }
  # #-----------------------------------
  # 
  
  #Now, we need to select our PT and save as a different copy so we can create
  #the offsets by keeping only those data described in the keyfile as install or
  #uninstall events. 
  
  
  #check to see if the PT thinks it shifted and flag with '1' ----------
  pt_shift_vector= c(0,pt_data$pt_level)-c(pt_data$pt_level,0)
  if(length(pt_shift_vector)>5){
    pt_shift_vector=pt_shift_vector[3:(length(pt_shift_vector)-3)]}
  
  if(any(abs(pt_shift_vector)>change_thresh_15_min)){
    flag=1
  }else{flag=0}
  #-----------------------------------
  
  
  if (is.na(pt_data[1,]$Date_GNSS_Uninstall)){ #in the case where there is no uninstall
    pt_data_for_offset =  pt_data%>% #get the raw data and filter it
      filter(pt_time_UTC >= gnss_install_UTC_start[1] & pt_time_UTC <= gnss_install_UTC_end[1])
    
  }else{ #if we have both an install and an uninstall
    pt_data_for_offset =pt_data%>%
      filter( pt_time_UTC >= gnss_install_UTC_start[1] & pt_time_UTC <= gnss_install_UTC_end[1] | 
                pt_time_UTC >= gnss_uninstall_UTC_start[1] & pt_time_UTC <= gnss_uninstall_UTC_end[1] )
    
  }
  
  
  #we need to tag installs and uninstalls so we get the TPs right where we want to
  log_df=distinct(select(pt_data_for_offset,driftID_install,driftID_uninstall))%>%
    gather(file_id,value)%>%
    filter(!is.na(value))
  
  #exception handling for when the indicated GNSS file is not available-----
  if(nrow(log_df)==0){
    clean_pt='there are no gnss files for this pt. I looked in the keyfile for GNSS files and cant find them. KEYFILE ERROR'
    write.csv(clean_pt,file=paste0(flagged_pt_output_directory,filename))
    return(NA)
  }
  
  #----------------------------------
  
  
  #match the first and second dates
  get_all_gnss=function(log_df_row){
    
    #our field log file is an 'L0' product, but the data come back as 'L2'. 
    #We need to find the L2 version
    logstring=str_replace(log_df_row['value'],"L0",'L2')
    splitter=strsplit(logstring,'_')[[1]]
    
    string_to_match=paste(splitter[6], splitter[7],sep='_')
    correct_drift_index=which(!is.na(str_match(list.files(gnss_drift_data_directory),string_to_match)))
    driftstring=list.files(gnss_drift_data_directory,full.names=TRUE)[correct_drift_index]
    
    # print(logstring)
    # print(splitter)
    # print(string_to_match)
    # print(correct_drift_index)
    # print(driftstring)
    # print(strsplit(driftstring,'_'))
    
    if(length(driftstring)>1){#we want the most recent
      split2=as.POSIXct(paste0(substring(do.call(rbind,strsplit(driftstring,'_'))[,11],1,8), # changed from 11 to 10 - is this correct though?
                               substring(do.call(rbind,strsplit(driftstring,'_'))[,11],10,15) ),
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
      mutate(datetime=as.POSIXct(gnss_time_UTC))%>%
      mutate(occupy_id=strsplit(log_df_row['file_id'],'_')[[1]][2])
    
    
    
    
  }
  
  #this should get all the GNSS files that match the first two dates of the GNSS
  #file ID in the key. It there are multiple dates, it will pull the most recent
  #if there are turning points, it will return all files. we want to tag those
  #with an install or uninstall id
  
  
  gnss_log= do.call(rbind,apply(log_df,1,get_all_gnss))
  
  
  
  if(all(is.na(gnss_log))){
    return(NA)} 
  
  #at this point, we have a gnss_logfile of all the GNSS data from the occupy files
  #indicated by the key.
  
  #unlike the processing prior to January 2024, we here will join this to the pt
  #data that are limited to install and uninstall. In this version, we don't
  #care about distance or time, we treat the keyfile as sacrosanct.However, the 
  #difference join function will only pull the closest without a max distance, so we 
  #include it. this will also help with the turning points
  
  #note this is a many to many join, so all gnnss are linked to all pt pings whtin the occupy intervals
  #this is what we want
  
  #we join by the nearest and track the time difference
  offset_dataframe= difference_inner_join(pt_data_for_offset,gnss_log,max_dist=time_thresh,
                                          by='datetime',distance_col='dt_pt_gnss_offset_calc')%>%
    transmute(pt_level=pt_level, temperature=temperature,
              gnss_time_UTC=as.POSIXct(gnss_time_UTC,format ="%Y-%m-%d %H:%M:%S"),
              gnss_wse=gnss_wse,pt_time_UTC=pt_time_UTC,
              pt_serial=pt_serial,
              gnss_lat=gnss_Lat, 
              gnss_lon=gnss_Lon, 
              pt_lat=pt_lat, 
              pt_lon=pt_lon,
              keyid=keyid, 
              pt_install_UTC=pt_install_UTC,
              pt_uninstall_UTC=pt_uninstall_UTC,
              gnss_uncertainty_m=gnss_uncertainty_m,
              dt_pt_gnss_offset_calc=dt_pt_gnss_offset_calc,
              drift_id=drift_id,
              occupy_id=occupy_id,
              gnss_install_UTC_start=gnss_install_UTC_start,
              gnss_install_UTC_end=gnss_install_UTC_end,
              gnss_uninstall_UTC_start=gnss_uninstall_UTC_start,
              gnss_uninstall_UTC_end=gnss_uninstall_UTC_end) 
  
  
  
  #now, filter for times within the install and uninstall. this will solve the TP issue
  #nested if else statement needed to limit the GNSS time to occupy periods
  #note that the PT has already been limited to this timeframe, but we've just 
  #joined in a new GNSS file that is broader
  #the if statement is in case there is only an install
  
  if (is.na(offset_dataframe$gnss_uninstall_UTC_start[1])){
    offset_dataframe=filter(offset_dataframe, (gnss_time_UTC >= gnss_install_UTC_start[1] &   gnss_time_UTC <= gnss_install_UTC_end[1]))
  }else{
    offset_dataframe=filter(offset_dataframe,(gnss_time_UTC >= gnss_install_UTC_start[1] &   gnss_time_UTC <= gnss_install_UTC_end[1] )| 
                              (gnss_time_UTC >= gnss_uninstall_UTC_start[1] & gnss_time_UTC <= gnss_uninstall_UTC_end[1])  )
  }
  
  
  offset_dataframe=offset_dataframe %>%
    
    #since we are doing this by keyfile, we don't need to group by keyid
    
    #the grouping variable is important. We want to assign the gnss to the closest ping,
    #but, if all we care about is an install or uninstall then we are going to obliterate 
    #that grouping later when we average by file id. Therefore, we group now by 
    # occupy_id, which might encompass some turning points
    
    group_by(occupy_id,drift_id)%>%
    mutate(offset=(gnss_wse-pt_level))
  
  
  
  
  # plot(filter(offset_dataframe,occupy_id=='install')$offset,ylim = c(-0.36,-0.18))
  # points(filter(offset_dataframe,occupy_id=='uninstall')$offset,col='red')
  # 
  # points(320,mean(filter(offset_dataframe,occupy_id=='install')$offset),col='blue',lwd=15)
  # points(280,mean(filter(offset_dataframe,occupy_id=='uninstall')$offset),col='orange',lwd=15)
  # 
  # point_a= mean(filter(offset_dataframe,occupy_id=='install')$offset) -
  #   mean(filter(offset_dataframe,occupy_id=='install')$gnss_uncertainty_m) -
  #   sd(filter(offset_dataframe,occupy_id=='install')$offset)
  # 
  # point_b= mean(filter(offset_dataframe,occupy_id=='install')$offset) +
  #   mean(filter(offset_dataframe,occupy_id=='install')$gnss_uncertainty_m) +
  #   sd(filter(offset_dataframe,occupy_id=='install')$offset)
  # 
  # points(rep(320,times=120),seq(point_a,point_b,length.out=120),col='blue')
  # 
  # point_a= mean(filter(offset_dataframe,occupy_id=='uninstall')$offset) -
  #   mean(filter(offset_dataframe,occupy_id=='uninstall')$gnss_uncertainty_m) -
  #   sd(filter(offset_dataframe,occupy_id=='uninstall')$offset)
  # 
  # point_b= mean(filter(offset_dataframe,occupy_id=='uninstall')$offset) +
  #   mean(filter(offset_dataframe,occupy_id=='uninstall')$gnss_uncertainty_m) +
  #   sd(filter(offset_dataframe,occupy_id=='uninstall')$offset)
  # 
  # points(rep(280,times=120),seq(point_a,point_b,length.out=120),col='orange')
  
  
  
  #to do a t-test, we need a column for install and one for uninstall  
 
    p_value=NA
  
  
  
  
  #summarizing this offset now gives us what we want, grouped by pt time
  #the correction is the mean off the offset by the group
  final_offset_df=offset_dataframe%>%
    group_by(occupy_id,drift_id)%>%
    
    summarize(
      #correction is the mean of the offsets by group (group is install or uninstall)
      pt_correction_m= mean(offset,na.rm=TRUE),
      
      #error is equal to the sd of the wse (boat bobbing) and the gnss average error, propogated
      pt_correction_total_error_m=  sqrt( sd(offset,na.rm=TRUE)^2 + (sum(gnss_uncertainty_m)/n()) ^2 ),
      
      #this is the error due to changing wse without GNSS error
      pt_correction_offset_sd_m=sd(offset,na.rm=TRUE),
      
      #this is the error due to GNSS error
      pt_correction_gnss_average_error_m=sum(gnss_uncertainty_m)/n(),
      
      #retain the keyid, which should be static across all groups
      keyid=paste(unique(keyid)),
      
      #look at the time differences between GNSS and PT
      mean_dt_pt_gnss_offset_calc=mean(dt_pt_gnss_offset_calc),
      
      #what drift ids were used here
      drift_id=paste(unique(drift_id)),
      
      pt_serial=paste(unique(pt_serial)),
      
      t_test_means_p_value=p_value)
  
  
  #thus, we have now created an offset dataframe for every PT datapoint included
  #in the 'occupy' times indicated by the keyfile
  
  
  
  #now we need to check these offsets
  
  
  #check to see if  is more than 2 rows. if so, this means either
  # a PT appears in one keyfile with the same driftID for install and uninstall.
  
  if (nrow(final_offset_df)>2){
    clean_pt='it looks like there is a driftID used for both install and uninstall. KEYFILE ERROR'
    write.csv(clean_pt,file=paste0(flagged_pt_output_directory,filename))
    return(NA)
    
  }
  
  if(nrow(final_offset_df)==0){
    clean_pt='there were no GNSS points associated with any PT pings'
    write.csv(clean_pt,file=paste0(flagged_pt_output_directory,filename))
    return(NA)
  }
  
  ###Flag Description###
  #0- all good
  #1- shift in raw PT (15cm within 15 mins)
  #10- no uninstall data
  #100- total error of install/uninstall correction > threshold
  #1000- (install - uninstall) > threshold 
  #10000- Flybys don’t agree with the offset. All install, uninstall, flybys averaged.
  #100000- there are not enough flybys: not appropriate to do anything
  #1000000- pt is likely settling. Linear fit was applied.
  #10000000 - flybys agree with OG file. we did nothing
  
  if (nrow(final_offset_df)==1){ #no uninstall
    flag = flag +10 
  }
  
  if(any(final_offset_df$pt_correction_total_error_m >gnss_sd_thresh )) {
    flag= flag + 100
  }
  
  # if( any(final_offset_df$pt_correction_offset_sd_m >gnss_sd_thresh ) ){
  #   flag= flag + 1000
  # }    
  # 
  # if( any(final_offset_df$pt_correction_gnss_average_error_m >gnss_sd_thresh ) ){
  #   flag= flag + 10000
  # }  
  
  # if(!is.na(final_offset_df$t_test_means_p_value[1]) <0.05){ #means are different
  #   flag= flag + 100000
  # }else {flag = flag}
  
  if(nrow(final_offset_df)==2){ # if there is an install and uninstall
    in_out_diff=max(final_offset_df$pt_correction_m)- min(final_offset_df$pt_correction_m)
    if (in_out_diff>gnss_sd_thresh) {
      flag= flag + 1000
    }
    
  }
  
  
  
  #whether it is 1 or 2 rows this will return the right value, as a mean of 1 is itself
  final_offset_m=mean(final_offset_df$pt_correction_m)
  
  if(nrow(final_offset_df)==1){
    if(final_offset_df$occupy_id=='uninstall'){
      clean_pt='this PT has an uninstall only- metadata errors on GNSS XML'
      write.csv(clean_pt,file=paste0(flagged_pt_output_directory,filename))
      return(NA)
      
    }
  }
  
  
  if (nrow(final_offset_df)==2){
    
    
    
    final_offset_to_join=pivot_wider(final_offset_df, names_from = occupy_id,
                                     values_from =c(drift_id,pt_correction_total_error_m,
                                                    pt_correction_offset_sd_m,
                                                    pt_correction_gnss_average_error_m, keyid,
                                                    mean_dt_pt_gnss_offset_calc,pt_serial,
                                                    t_test_means_p_value,pt_correction_m) )%>%
      #cleanup. some things are meant to be identical here
      mutate(keyid=keyid_install)%>%
      select(-keyid_install,-keyid_uninstall)%>%
      mutate(pt_serial=as.integer(pt_serial_install))%>%
      select(-pt_serial_install,-pt_serial_uninstall)%>%
      mutate(t_test_means_p_value=t_test_means_p_value_install)%>%
      select(-t_test_means_p_value_install,-t_test_means_p_value_uninstall)%>%
      mutate(in_out_diff=in_out_diff,
             pt_correction_mean_total_error_m = sqrt((pt_correction_total_error_m_install^2)+(pt_correction_total_error_m_uninstall^2)),
             pt_correction_mean_offset_sd_m = sqrt((pt_correction_offset_sd_m_install^2)+(pt_correction_offset_sd_m_uninstall^2)))
    
  }else{
    final_offset_to_join=pivot_wider(final_offset_df, names_from = occupy_id,
                                     values_from =c(drift_id,pt_correction_total_error_m,
                                                    pt_correction_offset_sd_m,
                                                    pt_correction_gnss_average_error_m, keyid,
                                                    mean_dt_pt_gnss_offset_calc,pt_serial,
                                                    t_test_means_p_value,pt_correction_m) )%>%
      #cleanup. some things are meant to be identical here
      mutate(keyid=keyid_install)%>%
      select(-keyid_install)%>%
      mutate(pt_serial=as.integer(pt_serial_install))%>%
      select(-pt_serial_install)%>%
      mutate(drift_id_uninstall=NA,
             pt_correction_total_error_m_uninstall=NA,
             pt_correction_offset_sd_m_uninstall=NA,
             pt_correction_gnss_average_error_m_uninstall=NA,
             mean_dt_pt_gnss_offset_calc_uninstall=NA,
             pt_correction_m_uninstall=NA)%>%
      mutate(t_test_means_p_value=t_test_means_p_value_install)%>%
      select(-t_test_means_p_value_install)%>%
      mutate(in_out_diff=NA,
             pt_correction_mean_total_error_m = pt_correction_total_error_m_install,
             pt_correction_mean_offset_sd_m = pt_correction_offset_sd_m_install)%>%
      select(drift_id_install, drift_id_uninstall, 
             pt_correction_total_error_m_install,pt_correction_total_error_m_uninstall,
             pt_correction_offset_sd_m_install,pt_correction_offset_sd_m_uninstall,
             pt_correction_gnss_average_error_m_install,pt_correction_gnss_average_error_m_uninstall,
             mean_dt_pt_gnss_offset_calc_install,mean_dt_pt_gnss_offset_calc_uninstall,
             pt_correction_m_install, pt_correction_m_uninstall,
             keyid,pt_serial,t_test_means_p_value,in_out_diff,pt_correction_mean_total_error_m,pt_correction_mean_offset_sd_m)
    
    
  }
  
  
  #get OG PT data
  final_pt=pt_data %>%
    left_join( final_offset_to_join    ,by=c('pt_serial','keyid'))%>%
    mutate(final_offset_m=final_offset_m)%>%
    mutate(pt_wse_m= pt_level + final_offset_m)%>%
    mutate(flag=flag)
  #filter for when it is in the water
  
  filename=paste0(filename_base,'_',unique(final_pt$keyid)) 
  print(filename)
  print('this file passed all checks')
  
  # 
  write.csv(final_pt,file=paste0(QA_QC_PT_output_directory,filename),row.names=FALSE)
  
  
}#end function 
