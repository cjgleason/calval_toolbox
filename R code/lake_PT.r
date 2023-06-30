hub_data_directory
QA_QC_PT_output_directory
key_directory
lakename


#get all the PTs in the data directory
munged_files= list.files(hub_data_directory,
                         recursive= TRUE)

PT_index=which(!is.na(do.call(rbind,lapply(munged_files,str_match,'PT_L1'))))
PT_files=munged_files[PT_index]
csv_index=which(!is.na(do.call(rbind,lapply(PT_files,str_match,'.csv'))))
csv_PT_files=PT_files[csv_index]

#pull filename before the .csv
raw_PT=sub("\\..*","",csv_PT_files)
raw_PT=sub(".*/","",raw_PT)
#pull filenam\\..*e before the second _
QA_QC_PTs=sub("\\..*","",list.files(QA_QC_PT_output_directory))

#what raw PT data have not been munged
    #need to preserve the full path in csv_PT_files!
unmunged_PTs=setdiff(csv_PT_files,QA_QC_PTs)

#read in the raw PT file
    pt_serial = strtoi(read.csv(paste0(pt_data_directory,raw_pt_file))$Serial_number.[1])   
    #11 lines of headers to skip to deal with read in function
    pt_data=read.csv(paste0(pt_data_directory,raw_pt_file),skip=11,header=TRUE) %>%
      mutate(pt_serial=pt_serial)


#Get the key based on the lake name-
pt_key_file= rivername lookup in PT directory

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

#now we've got just the PT we care about and its location and metadata from the key

#grab the install/uninstall files
  log_files=rbind( unique(prepped_pt$driftID_install),unique(prepped_pt$driftID_uninstall)) #need both files to work on
    #log_files=unique(prepped_pt$driftID_install)
    log_files=log_files[!is.na(log_files)]
    log_files=paste0(hub_data_directory,log_files,'.csv')
    # print(log_files)
        
    if(length(log_files)==0){
      return(NA)
    }
    
    if(all(is.na(log_files))){
      return(NA)
    }
    

                   
     do.call(rbind,lapply(read.csv,log_files))
      
      #print(driftstring)
      
      gnss_log=read.csv(driftstring,header=TRUE,stringsAsFactors = FALSE)%>%
        mutate(datetime=gnss_time_UTC)%>%
        mutate(datetime=as.POSIXct(datetime))#needed as when it gets written to csv it becomes not a posix object

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
      
      #time_thresh=1000
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
      
      





