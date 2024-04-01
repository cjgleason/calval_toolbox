Sys.umask('002')

library(dplyr)
library(stringr)
library(tidyr)
library(fuzzyjoin)
library(geodist)
library(bayesbio)
library(ncdf4)
library(lubridate)
# 
#
hubname='T2'
continent='na'
#
setwd(paste0('/nas/cee-water/cjgleason/calval/Processed data/',hubname,'/'))
working_dir=(paste0('/nas/cee-water/cjgleason/calval/Processed data/',hubname,'/'))
flagged_pt_output_directory = (paste0('/nas/cee-water/cjgleason/calval/Processed data/',hubname,'/Flagged WL/'))


dist_thresh_offset=300 # 150m *Trying 300 for WM to rid of 118-120 from naughty bin\n",
time_thresh= 15*60 #minutes as seconds, centered, so 15 =30 mins total time\n",
gnss_sd_thresh=0.05 # 15cm how much variance do you want in the GNSS data when it is within the distance threshold?\n",
# offset_sd_thresh=0.10 #m, so 10cm. the the PT apparantly shift by more than a cm?\n",
change_thresh_15_min=0.10#m- does it change more than 5cm in 15 minutes? that is a discontinuity in offset\n",
dry_threshold = 0.10 #m - This
alongstream_error= 0.0001*200 #m error we get from the downstream slope of a reach in a node. This placeholder is a 1e-4 slope over a 200m node\n",
crossstream_error= 0.005 #m error we get from PT not representing cross stream superelevation/noise in a node\n",
measurement_error= 0.001 #m error we get from PT measurement itself \n",
time_threshold_sec_match= 120*60 #two hour\n",
wse_threshold_m_match=0.05 #within 5cm\n",
distance_threshold_m_match =200 #within 200m\n",
#

PT_key_file= 'SWOTCalVal_T2_KEY_20230110_20230806.csv'

read_keys=function(keyfile){

  this_key= read.csv(keyfile,stringsAsFactors=FALSE, na.strings = c("","NA","na","NaN", " "))%>%
    mutate(keyid=keyfile)
}
master_key= do.call(rbind,lapply(PT_key_file,read_keys))
# pt_data_directory='/nas/cee-water/cjgleason/calval/xml_scripts/CU/Munged/Munged__20230516/Munged__20230617/'

pt_data_directory='/nas/cee-water/cjgleason/calval/Processed data/T2/Raw WL/'
QA_QC_PT_output_directory ='/nas/cee-water/cjgleason/calval/Processed data/T2/Munged WL/'

# list.files(pt_data_directory, pattern='.csv', recursive=TRUE)

raw_pt_file=list.files(pt_data_directory, pattern='.csv', recursive = TRUE)
gnss_drift_data_directory='/nas/cee-water/cjgleason/calval/Processed data/T2/Munged drifts'

keyfile=master_key%>%
  mutate('driftID'= sub("\\..*","",Final_Occupy_Log_File))%>%
  mutate(SiteNumber=as.integer(SiteNumber))

#   ######## above here comment out ########
 raw_pt_file=raw_pt_file[4]
 i = raw_pt_file
for (i in raw_pt_file){
 
#grab the serial number from the PT file itself. Standarized file
pt_serial_file=read.csv(paste0(pt_data_directory,i), header = TRUE, nrow=1)
pt_serial_file=pt_serial_file$SiteNumber
print(paste0(pt_data_directory,i))

#read in master_key--------------

#--------------------------------

#check for a serial match-----------
if( pt_serial_file %in% keyfile$SiteNumber == FALSE){return(NA)}
#-----------------------------------

#now get the PT data, standardized(ish) by Taylor--------
tryCatch({
  pt_data=read.csv(paste0(pt_data_directory,i),header=TRUE)%>%
    mutate(datetime=ymd_hms(DateTime_UTC))
},
error=function(cond){return(NA)})

# # Function to fill NAs in the new column with corresponding values
# fill_nas_datetime <- function(column) {
#   ifelse(is.na(column), ymd_hms("1970-01-01 00:00:00"), column)
# }

# Identify date-only strings and convert them to POSIXct
date_only_indices <- !grepl(":", pt_data$DateTime_UTC)
pt_data$datetime[date_only_indices] <- ymd_hms(paste0(pt_data$DateTime_UTC[date_only_indices], " 00:00:00"))

# # Use dplyr's mutate to create a new column with NAs filled
# pt_data <- pt_data %>%
#   mutate(
#     filled_datetime_objects = fill_nas_datetime(datetime)
#     filled_datetime_objects = as.POSIXct(filled_datetime_objects, format="%Y-%m-%d %H:%M:%S", origin = "1970-01-01 00:00:00")
#   )


filename=i 
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

# sum1=sum(!is.na(str_match(pt_data$Time, 'pm')))
# sum2=sum(!is.na(str_match(pt_data$Time, 'am')))
# sum3=sum(!is.na(str_match(pt_data$Time, 'PM')))
# sum4=sum(!is.na(str_match(pt_data$Time, 'AM')))
# 
# if(any(c(sum1,sum2,sum3,sum4)>0)){ #this means we're in ampmtime.
#   pt_data=pt_data%>%
#     mutate(datetime=as.POSIXct(paste(Date,Time),format="%m/%d/%Y %I:%M:%S %p"))
# } else { #24 hour time
#   pt_data=pt_data%>%
#     mutate(datetime=as.POSIXct(paste(Date,Time),format= "%Y/%m/%d %H:%M:%S"))
# }
#-----------------------------------

#
#now, we have a multikey that will sow mass confusion later. We need to select.
#the right keyfile from all the keyfiles, using the 'pt in the water' logic, which 
#says that the keyfile indicates and install and uninstall (in the water) time, which is always
# %in% the total PT record. Grab pt_data min(time) and pt_data max(time), check for
# %in%, and then we can proceed without a split on keyfiles.

pt_mintime=min(pt_data$datetime,na.rm=TRUE)
pt_maxtime=max(pt_data$datetime,na.rm=TRUE)

keyfile_check=filter(keyfile,SiteNumber==pt_serial_file)%>%
  mutate( gnss_install_start_UTC=as.POSIXct(paste(Date_GNSS_Occupy_Start,Time_GNSS_Occupy_Start),format= "%m/%d/%Y %H:%M"),
          gnss_install_end_UTC=as.POSIXct(paste(Date_GNSS_Occupy_End,Time_GNSS_Occupy_End),format= "%m/%d/%Y %H:%M"))#%>%



#later, we will filter again to only 'occupy' times-----------------
pt_data=pt_data %>%
  left_join(keyfile,by='SiteNumber')%>%
  mutate(pt_Lat= Lat_WGS84)%>%
  mutate(pt_Lon=Long_WGS84)%>%
  # mutate(pt_time_UTC=datetime)%>%
  filter(SiteNumber==pt_serial_file)%>% #limit to just the PT we want. The filter above should take care 
  #of the case where the serial is not in the key file
  # This is where the Munged_PT columns come from key file - 12/20 added reach and nodeID. 1/05 added gnss install end and gnss uninstall start to limit
  transmute(pt_time_UTC=as.POSIXct(datetime, format ="%m/%d/%Y %H:%M"),
            pt_lat=pt_Lat,
            pt_lon=pt_Lon,
            # pt_install_UTC=as.POSIXct(paste(Date_PT_Install,Time_PT_Install_UTC),format= "%m/%d/%Y %H:%M"),
            # pt_uninstall_UTC=as.POSIXct(paste(Date_PT_Uninstall,Time_PT_Uninstall_UTC),format= "%m/%d/%Y %H:%M"),
            gnss_install_start_UTC=as.POSIXct(paste(Date_GNSS_Occupy_Start,Time_GNSS_Occupy_Start),format= "%m/%d/%Y %H:%M"),
            gnss_install_end_UTC=as.POSIXct(paste(Date_GNSS_Occupy_End,Time_GNSS_Occupy_End),format= "%m/%d/%Y %H:%M"),
            # gnss_uninstall_UTC_start = as.POSIXct(paste(Date_GNSS_Uninstall,Time_GNSS_Uninstall_Start_UTC),format= "%m/%d/%Y %H:%M"), 
            # gnss_uninstall_UTC_end=as.POSIXct(paste(Date_GNSS_Uninstall,Time_GNSS_Uninstall_End_UTC),format= "%m/%d/%Y %H:%M"),
            # install_method=Install_method, pt_serial=pt_serial,         
            pt_level=Stage_Meters,
            driftID=driftID,
            # pt_time_UTC=datetime,
            keyid=keyid, Reach_ID=Reach_ID, Node_ID=Node_ID, SiteNumber=SiteNumber)#%>%
  # filter(pt_time_UTC >=gnss_install_start_UTC)%>%
  # filter(pt_time_UTC <=gnss_install_end_UTC)
  
#-----------------------------------

#check for a keyfile error----------
# if(nrow(pt_data)==0){
#   clean_pt='the keyfile says this pt was never in the water. KEYFILE ERROR'
#   write.csv(clean_pt,file=paste0(flagged_pt_output_directory,filename))
#   return(NA)
# }
#-----------------------------------



# }else{ #if we have both an install and an uninstall
  pt_data_for_offset =pt_data%>%
    filter( pt_time_UTC >= gnss_install_start_UTC[1] & pt_time_UTC <= gnss_install_end_UTC[1] | 
            pt_time_UTC >= gnss_install_end_UTC[1] & pt_time_UTC <= gnss_install_start_UTC[1] )%>%
   mutate(datetime = pt_time_UTC)
  
# }


#we need to tag installs and uninstalls so we get the TPs right where we want to
log_df=distinct(select(pt_data_for_offset,driftID))%>%
  gather(file_id,value)%>%
  filter(!is.na(value))
# 
# #exception handling for when the indicated GNSS file is not available-----
# if(nrow(log_df)==0){
#   clean_pt='there are no gnss files for this pt. I looked in the keyfile for GNSS files and cant find them. KEYFILE ERROR'
#   write.csv(clean_pt,file=paste0(flagged_pt_output_directory,filename))
#   return(NA)
# }

#----------------------------------


#match the first and second dates
# get_all_gnss=function(log_df_row){
  
  #our field log file is an 'L0' product, but the data come back as 'L2'. 
  #We need to find the L2 version
  logstring=str_replace(log_df['value'],"L0",'L2')
  splitter=strsplit(logstring,'_')[[1]]
  
  string_to_match=paste(splitter[6], splitter[7],sep='_')
  correct_drift_index=which(!is.na(str_match(list.files(gnss_drift_data_directory),string_to_match)))
  driftstring=list.files(gnss_drift_data_directory,full.names=TRUE)[correct_drift_index]
  
  # print(logstring)
  # print(splitter)
  # print(string_to_match)
  # print(correct_drift_index)
  # print(driftstring)
  
  if(length(driftstring)>1){#we want the most recent
    split2=as.POSIXct(paste0(substring(do.call(rbind,strsplit(driftstring,'_'))[,11],1,8),
                             substring(do.call(rbind,strsplit(driftstring,'_'))[,11],10,15) ),
                      format='%Y%m%d%H%M%S')
    
    latest_file_index=which(split2==max(split2))
    #position 11 is the version munge date. Take the most recent
    driftstring=driftstring[latest_file_index]
    
  }
  
  # read_multi_gnss=function(driftstring){
    output=read.csv(driftstring,header=TRUE,stringsAsFactors = FALSE)
    
  # }
  
  if(identical(driftstring, character(0))){
    clean_pt='this file has a gnss file that likely hasnt been processed yet. PUSH/PULL GNSS from JPL'
    write.csv(clean_pt,file=paste0(flagged_pt_output_directory,filename))
    return(NA)}
  
  
  #function to read mutliple gnss files
  # gnss_log=do.call(rbind,lapply(driftstring,read_multi_gnss))%>%
  #   mutate(datetime=as.POSIXct(gnss_time_UTC))%>%
  #   mutate(occupy_id=strsplit(log_df_row['file_id'],'_')[[1]][2])
  
  
  
  
# }

#this should get all the GNSS files that match the first two dates of the GNSS
#file ID in the key. It there are multiple dates, it will pull the most recent
#if there are turning points, it will return all files. we want to tag those
#with an install or uninstall id


# gnss_log= do.call(rbind,apply(log_df,1,get_all_gnss))
    gnss_log=output%>%
      mutate(datetime=as.POSIXct(gnss_time_UTC))



# if(all(is.na(gnss_log))){
#   return(NA)} 

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
                                        by='datetime',distance_col='dt_pt_gnss_offset_calc')%>% # Pick up here 02/23S
  transmute(pt_level=pt_level,
            gnss_time_UTC=as.POSIXct(gnss_time_UTC,format ="%Y-%m-%d %H:%M:%S"),
            gnss_wse=gnss_wse,pt_time_UTC=pt_time_UTC,
            SiteNumber=SiteNumber,
            gnss_lat=gnss_Lat, 
            gnss_lon=gnss_Lon, 
            pt_lat=pt_lat, 
            pt_lon=pt_lon,
            keyid=keyid, 
            # pt_install_UTC=pt_install_UTC,
            # pt_uninstall_UTC=pt_uninstall_UTC,
            gnss_uncertainty_m=gnss_uncertainty_m,
            dt_pt_gnss_offset_calc=dt_pt_gnss_offset_calc,
            drift_id=drift_id,
            # occupy_id=occupy_id,
            gnss_install_start_UTC=gnss_install_start_UTC,
            gnss_install_end_UTC=gnss_install_end_UTC) 



#now, filter for times within the install and uninstall. this will solve the TP issue
#nested if else statement needed to limit the GNSS time to occupy periods
#note that the PT has already been limited to this timeframe, but we've just 
#joined in a new GNSS file that is broader
#the if statement is in case there is only an install

if (is.na(offset_dataframe$gnss_install_start_UTC[1])){
  offset_dataframe=filter(offset_dataframe, (gnss_time_UTC >= gnss_install_start_UTC[1] &   gnss_time_UTC <= gnss_install_end_UTC[1]))
}else{
  offset_dataframe=filter(offset_dataframe,(gnss_time_UTC >= gnss_install_start_UTC[1] &   gnss_time_UTC <= gnss_install_end_UTC[1] )| 
                            (gnss_time_UTC >= gnss_install_start_UTC[1] & gnss_time_UTC <= gnss_install_end_UTC[1])  )
}


offset_dataframe=offset_dataframe %>%
  
  #since we are doing this by keyfile, we don't need to group by keyid
  
  #the grouping variable is important. We want to assign the gnss to the closest ping,
  #but, if all we care about is an install or uninstall then we are going to obliterate 
  #that grouping later when we average by file id. Therefore, we group now by 
  # occupy_id, which might encompass some turning points
  
  group_by(drift_id)%>%
  mutate(offset=(gnss_wse-pt_level))


#summarizing this offset now gives us what we want, grouped by pt time
#the correction is the mean off the offset by the group
final_offset_df=offset_dataframe%>%
  group_by(drift_id)%>%
  
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
    
    SiteNumber=paste(unique(SiteNumber)))%>%
  mutate(SiteNumber = as.integer(SiteNumber))


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


final_offset_m=mean(final_offset_df$pt_correction_m)

  final_offset_to_join=pivot_wider(final_offset_df, #names_from = drift_id,
                                   values_from =c(pt_correction_total_error_m,
                                                  pt_correction_offset_sd_m,
                                                  pt_correction_gnss_average_error_m, keyid,
                                                  mean_dt_pt_gnss_offset_calc,SiteNumber,
                                                  pt_correction_m) )%>%
    #cleanup. some things are meant to be identical here
    mutate(keyid=keyid_)%>%
    select(-keyid_)%>%
    mutate(SiteNumber=SiteNumber_)%>%
    select(-SiteNumber_)%>%
    mutate(pt_correction_mean_total_error_m = pt_correction_total_error_m_,
           pt_correction_mean_offset_sd_m = pt_correction_offset_sd_m_)%>%
    select(drift_id,
           pt_correction_total_error_m_,
           pt_correction_offset_sd_m_,
           pt_correction_gnss_average_error_m_,
           mean_dt_pt_gnss_offset_calc_,
           pt_correction_m_,
           keyid,SiteNumber,pt_correction_mean_total_error_m,pt_correction_mean_offset_sd_m)

#
# }


#get OG PT data
final_pt=pt_data %>%
  left_join( final_offset_df,by=c('SiteNumber'))%>%
  mutate(final_offset_m=final_offset_m)%>%
  mutate(pt_wse_m= pt_level + final_offset_m, 
         keyid=keyid.x)%>%
  select(-keyid.y)
#filter for when it is in the water

filename=paste0(filename_base,'_',unique(final_pt$keyid)) 
print(filename)
print('this file passed all checks')

# 
write.csv(final_pt,file=paste0(QA_QC_PT_output_directory,filename),row.names=FALSE)

} # end of function
 
