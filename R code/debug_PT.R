library(dplyr)
library(parallel)
library(stringr)
library(tidyr)
library(fuzzyjoin)
library(geodist)
library(bayesbio)
library(ncdf4)


reprocess_switch=0 #0 or 1. 1 means to recreate all reach and node products following some change in processing. 
#0 means to just append to existing dataframes

# hubname='UNC'
# rivername='WK'
# continent='oc'
# PT_key_file= 'WK_Key_20230331.csv' #WK
# utm_zone='58 +south'#WK='58 +south'

# hubname='UMass'
# rivername='CR'
# continent='na'
# PT_key_file='Key_CR_FS_20230322.csv' #CT
# utm_zone=18 #Ct= 18

hubname='CU'
rivername='WM'
continent='na'
PT_key_file= 'SWOTCalVal_WM_KEY_20230326_20230509.csv' #WM
utm_zone=10 #WM= 10


buffer=500 #m, 'extends' the reach

setwd(paste0('/nas/cee-water/cjgleason/calval/Processed data/',hubname,'/'))
domain_file=keyfile=paste0(rivername,'_domain.csv')

#PT paths---------
PT_data_directory='Raw PT/'
QA_QC_PT_output_directory='Munged PT/'
flagged_PT_output_directory='Flagged PT/'
#--------------------------------------------------

#drift paths------------------------------------------
GNSS_drift_data_directory=paste0('From Andy/',hubname,'_netCDFs/')
if(reprocess_switch==1){
  
  drift_string= paste0('Munged drifts/','reprocessed_',  str_replace_all(as.character(Sys.Date()),'\\-','_'))
  reachnode_string= paste0('Data frames/','reprocessed_',  str_replace_all(as.character(Sys.Date()),'\\-','_'))
  node_string =paste0('Data frames/','reprocessed_',  str_replace_all(as.character(Sys.Date()),'\\-','_'),'/node')
  reach_string=paste0('Data frames/','reprocessed_',  str_replace_all(as.character(Sys.Date()),'\\-','_'),'/reach')
  
  #check if we've already reprocessed today
  if (dir.exists(drift_string)){
    #if we have, then use that as the output and clear the files in the drift directory
    unlink(drift_string, recursive = TRUE)
    dir.create(drift_string)
    QA_QC_drift_output_directory=paste0(drift_string,'/')
    reachnode_output_directory=paste0(reachnode_string,'/')
  } else {
    #if we haven't reprocessed today
    dir.create(drift_string)
    dir.create(reachnode_string)
    dir.create(node_string)
    dir.create(reach_string)
    
    QA_QC_drift_output_directory=paste0(drift_string,'/')
    reachnode_output_directory=paste0(reachnode_string,'/')
  }
  
} else { #we aren't reprocessing. find the most recent folders to use
  
  folderlist= list.files('Munged drifts',full.names = TRUE)
  
  foldertimes=file.info(folderlist)%>%
    mutate(mintime= Sys.time()-mtime) %>%
    filter(mintime== min(mintime)) 
  
  QA_QC_drift_output_directory=paste0(row.names(foldertimes),'/')  
  folderlist2= list.files('Data frames',full.names = TRUE)
  
  foldertimes2=file.info(folderlist2)%>%
    mutate(mintime= Sys.time()-mtime) %>%
    filter(mintime== min(mintime)) 
  
  reachnode_output_directory=row.names(foldertimes2)
  
}

flagged_drift_output_directory='Flagged drifts/'
#--------------------------------------------------

#sword paths----------------------------------------
SWORD_path=paste0('/nas/cee-water/cjgleason/calval/SWORD_15/netcdf/',continent,
                  '_sword_v15.nc')
munged_drift_directory='Munged drifts/'
PT_directory='Munged PTs/'
#------------------------------



#munge PTs if needed------
dist_thresh=150 # 150m
time_thresh= 15*60 #minutes as seconds, centered, so 15 =30 mins total time
GNSS_sd_thresh=0.15 # 15cm how much variance do you want in the GNSS data when it is within the distance threshold?
offset_sd_thresh=0.10 #m, so 10cm. the the PT apparantly shift by more than a cm?
change_thresh_15_min=0.05#m- does it change more than 5cm in 15 minutes? that is a discontinuity in offset

#check for un-munged PT data
#pull filename before the .csv
raw_PT=sub( "\\..*","", list.files(PT_data_directory))
#pull filename before the second _
QA_QC_PTs=sub( "\\..*","",list.files(QA_QC_PT_output_directory))
flagged_PTs=sub( "\\..*","",list.files(flagged_PT_output_directory))
#what raw PT data have not been munged
unmunged_PTs=setdiff(raw_PT,c(flagged_PTs,QA_QC_PTs))
#run the PTs that are not yet munged




  
raw_pt_file='SWOTCalVal_WM_PT_L1_PT100_20230328T140000_20230509T174500_20230510T112409'
                           pt_key_file=PT_key_file
                           dist_thresh=dist_thresh
                           time_thresh=time_thresh
                           pt_data_directory=PT_data_directory
                           gnss_drift_data_directory=QA_QC_drift_output_directory
                           QA_QC_pt_output_directory=QA_QC_PT_output_directory
                           flagged_pt_output_directory=flagged_PT_output_directory
                           gnss_sd_thresh=GNSS_sd_thresh
                           offset_sd_thresh=offset_sd_thresh
                           change_thresh_15_min=change_thresh_15_min
    
  
  handle_raw_pt=function(raw_pt_file,pt_key_file,pt_data_directory,gnss_drift_data_directory){
    #read in raw pt--------------
    #ID could come from the filename, right now it is reading the serial number from the file
    
    #kluge that quickly gets what we want by reading in the csv flat and pulling the first entry
    pt_serial = strtoi(as.character(read.csv(paste0(pt_data_directory,raw_pt_file,'.csv'))$Serial_number.[1]))
    
    #11 lines of headers to skip to deal with read in function
    
    #!!!!!!!!!!!!!!
    #The willamette data have a missing header, so we skip the 12th line and assign our own header. Deadly !
    #!!!!!!!!!!!!!
    pt_data=read.csv(paste0(pt_data_directory,raw_pt_file,'.csv'),skip=11,header=TRUE) %>%
      mutate(pt_serial=pt_serial)
    
    
    #     pt_data= read.csv(paste0(pt_data_directory,raw_pt_file,'.csv'),skip=12,header=TRUE,fill=TRUE,col.names = c('Date','Time','ms','Level','Temperature')) %>%
    #       mutate(pt_serial=pt_serial)
    #----------------------------
    
    #read in offset--------------
    keyfile=read.csv(pt_key_file,stringsAsFactors = FALSE)%>%
      mutate('driftID_install'= Final_Install_Log_File)%>%
      mutate('driftID_uninstall'= Final_Uninstall_Log_File)%>%
      mutate(pt_serial=PT_Serial)
    #left joining the key in 'offset' gets us a tidy data frame where the info from the key is promulgated to just that pt. a right join would give
    #an n fold expansion across n pts
    pt_data=pt_data %>%
      left_join(keyfile,by='pt_serial')%>%
      mutate(datetime=as.POSIXct(paste(Date,Time),format= "%m/%d/%Y %I:%M:%S %p"))%>%
      mutate(pt_Lat= Lat_WGS84)%>%
      mutate(pt_Lon=Long_WGS84)%>%
      mutate(pt_time_UTC=datetime)
    
    
    # print(head(pt_data))
    
    
    #----------------------------
  } # end pt function
  correct_pt=function(prepped_pt,dist_thresh,time_thresh,gnss_drift_data_directory){
    
    
    # log_files=rbind( unique(prepped_pt$driftID_install),unique(prepped_pt$driftID_uninstall)) #the second from the join.
    log_files=unique(prepped_pt$driftID_install)
    log_files=log_files[!is.na(log_files)]
    # print(log_files)
    
    
    if(length(log_files)==0){
      return(NA)
    }
    
    if(all(is.na(log_files))){
      return(NA)
    }
    
    unit_pt_process = function(log_file,prepped_pt,dist_thresh,time_thresh,gnss_drift_data_directory){
      
      #the install file is missing a date, need to go a hunting for it
      
      logstring=str_replace(paste0(gnss_drift_data_directory,log_file,'.csv'),"L0",'L2')
      #standardized files means we can pull this in a fixed position
      string_to_match=substr(logstring,nchar(logstring)-35,nchar(logstring)-4)
      
      correct_drift_index=which(!is.na(str_match(list.files(gnss_drift_data_directory),string_to_match)))
      
      driftstring=list.files(gnss_drift_data_directory,full.names=TRUE)[correct_drift_index]
      
      #print(driftstring)
      
      gnss_log=read.csv(driftstring,header=TRUE,stringsAsFactors = FALSE)%>%
        mutate(datetime=gnss_time_UTC)%>%
        mutate(datetime=as.POSIXct(datetime))#needed as when it gets written to csv it becomes not a posix object
      
      #print('gnss_log')  
      
      time_thresh=1000
      #half a second faster to join first on time and then on space
      clean_pt_time=difference_inner_join(prepped_pt,gnss_log,by='datetime',max_dist=time_thresh,distance_col='test')

      
      #need lon then lat
      distance_m=geodist(cbind(clean_pt_time$pt_lon, clean_pt_time$pt_lat), cbind(clean_pt_time$gnss_Lon, clean_pt_time$gnss_Lat), paired=TRUE,measure='haversine')
    
      #print('distance_m')
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
      
      # print('wse_pt')
      
      return(wse_pt)
      
      
      
    }
    
    
    
    #loop through the log files, find the right gnss data to associate with the install,
    # average the gnss height within a distance threshold, and then create a corrected pt df
    #by binding each log file together. This is unnecessary, but will handle that one special case
    finalpt=do.call(rbind,lapply(log_files,unit_pt_process,prepped_pt=prepped_pt,dist_thresh=dist_thresh,
                                 time_thresh=time_thresh,gnss_drift_data_directory=gnss_drift_data_directory))
    
    
    
  }
  
  #read in pt
  prepped_pt=handle_raw_pt(raw_pt_file,pt_key_file,pt_data_directory,gnss_drift_data_directory)%>%
    transmute(pt_time_UTC=pt_time_UTC,pt_lat=pt_Lat,pt_lon=pt_Lon,
              pt_install_UTC=as.POSIXct(paste(Date_GNSS_Install,Time_GNSS_Install_Start),format= "%m/%d/%Y %H:%M"),
              pt_uninstall_UTC=as.POSIXct(paste(Date_GNSS_Uninstall,Time_GNSS_Uninstall_End),format= "%m/%d/%Y %H:%M"),
              install_method=Install_method, pt_serial=pt_serial,
              pt_level=LEVEL,temperature=TEMPERATURE,driftID_install=driftID_install,driftID_uninstall=driftID_uninstall,
              datetime=datetime)
  
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
  if(sd(offset_pt$pt_correction) >change_thresh_15_min  ){
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
  
  #first strip the offset df into just the gnss time and the pt correction and SD
  svelte_offset_pt=select(offset_pt,pt_correction,pt_wse_sd,gnss_time_UTC)
  

  
  #     write.csv(svelte_offset_pt, '/nas/cee-water/cjgleason/calval/Processed data/CU/Munged PT/svelte.csv')
  #     write.csv(prepped_pt,'/nas/cee-water/cjgleason/calval/Processed data/CU/Munged PT/prepped.csv')
  

  prepped_pt2=prepped_pt %>%
    mutate(timediff_install=pt_install_UTC-pt_time_UTC)%>%
    filter(timediff_install<0)%>%
    #drop pt data after uninstall time
    mutate(timediff_uninstall=pt_uninstall_UTC-pt_time_UTC)%>%
    filter(timediff_uninstall>0)%>%
    select(-timediff_install,-timediff_uninstall)
    
  
  findmintime=function(prepped_pt_row,gnss_times){
   # print(as.POSIXct(prepped_pt_row['pt_time_UTC']))

  first(which(as.POSIXct(prepped_pt_row['pt_time_UTC']) - gnss_times == min(as.POSIXct(prepped_pt_row['pt_time_UTC']) - gnss_times)))
   }
  
  
  pt_index=apply(prepped_pt2,1, findmintime, gnss_times=svelte_offset_pt$gnss_time_UTC)
  
  final_pt=cbind(svelte_offset_pt[pt_index,],prepped_pt2)%>%
    mutate(pt_wse=pt_level+pt_correction)%>%
    select(-datetime)
  
  
  # print('final')
  # print(final_pt)
  
  print(filename)
  print('this file passed all checks')
  write.csv(final_pt,file=paste0(QA_QC_pt_output_directory,filename,'.csv'))
  
  
  
  











