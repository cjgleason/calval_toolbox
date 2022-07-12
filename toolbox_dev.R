setwd('D:/OneDrive -\ University of Massachusetts/calval/Toolbox/')

library(dplyr)
library(tidyr)
library(fuzzyjoin)

# raw PT.csv --> example PT data
# GNSS_drift_key.csv --> example GNSS drift key by file name and date and reach of river
# LOG6_LP_2420.csv --> example post-processed GNSS file from CSRS 
# PT offset.csv --> example key for offsets at each PT and which GNSS file was used during install
# 
# lid_clip_PT.R
# lid_GNSSatPTlocs.R ---> Had many R files to help manipulate data, I think this was the most complete? 
#   Both files are more for reference at my thought process


handle_raw_PT=function(raw_file,offset_file){
#read in raw PT--------------
#ID could come from the filename, right now it is reading the serial number from the file

#kluge that quickly gets what we want by reading in the csv flat and pulling the first entry
PT_serial = strtoi(as.character(read.csv(raw_file)$Serial_number.[1]))
#11 lines of headers to skip to deal with read in function
PT_data= read.csv(raw_file,skip=11,header=T) %>%
  mutate(PT_Serial=PT_serial)
#----------------------------

#read in offset--------------
offset=read.csv(offset_file)
#left joining the key in 'offset' gets us a tidy data frame where the info from the key is promulgated to just that PT. a right join would give
#an n fold expansion across n PTs
PT_data=PT_data %>%
  left_join(offset,by='PT_Serial')%>%
  mutate(Lon=Long)%>%
  select(-Long)%>%
  mutate(datetime= as.POSIXct(paste(Date,Time),format= "%m/%d/%Y %I:%M:%S %p") )

#----------------------------
} # end PT function


#test="08-30-2021 08:00:00 am"

prepped_PT=handle_raw_PT('Taylor data 7 12/raw PT.csv','Taylor data 7 12/PT offset.csv' )
 
 correct_PT=function(log_file,prepped_PT,dist_thresh,time_thresh){
   log_files= unique(levels(prepped_PT$GNSS_file))
   #there is no time variable in the GNSS, as we want to know when it was overtop the PT
   #!!!!!!!!
   # we therefore MUST keep these logs to cover one float over the install location
   #!!!!!!!
   decimal_to_sec=function(dec_time){
     hour=dec_time*3600
   }
   
 
   #convert native date format to posix and drop redudnat columns
   GNSS_log=read.csv(paste0('Taylor data 7 12/',log_file,'.csv'))%>%
     mutate(Lat=latitude_decimal_degree)%>%
     mutate(Lon=longitude_decimal_degree)%>%
     select(-latitude_decimal_degree,-longitude_decimal_degree)%>%
     mutate(hour=decimal_to_sec(decimal_hour))%>%
     mutate(datetime=as.POSIXct(paste(year,day_of_year),format= '%Y %j'))%>%
     mutate(datetime=datetime + hour)%>%
     select(-decimal_hour,-day_of_year,-year,-rcvr_clk_ns,-hour)
     
   
   
   #first, pull only the GNSS data within the distance threshold from the PT
   #use the fuzzyjoin pacakge to convert lat lon to meters
   clean_PT= geo_inner_join(prepped_PT,GNSS_log,max_dist=dist_thresh, unit='km',distance_col='GNSS_dist',by=c('Lat','Lon'))%>%

   #next, make sure that the data overlap in time. We don't want a correction based on hours old PT data
    mutate(time_dif=abs(datetime.x-datetime.y))%>%
    filter(time_dif<time_thresh)
   
   #next, ensure there are enough GNSS points to make a valid offset correction
   if (nrow(clean_PT)<20){return('not enough data to create offset, change your thresholds or double check the data')}

   #average across points within threshold to characterize PT to GNSS
   print(paste('PT range is',as.character(max(clean_PT$LEVEL)-min(clean_PT$LEVEL)),'m'))
   print(paste('GNSS range is',as.character(max(clean_PT$ortho_height_m_cgvd2013)-min(clean_PT$ortho_height_m_cgvd2013)),'m'))
   
   #produce a final DF with raw and corrected heights
   clean_PT=clean_PT%>%
     mutate(offset=mean(ortho_height_m_cgvd2013)-mean(LEVEL))%>%
     mutate(corrected_level=LEVEL + offset)
 
 
 #loop through the log files, find the right GNSS data to associate with the install,
 # average the GNSS heigh within a distance theshold, and then create a corrected PT df
 #by binding each log file together. The 
  finalPT=do.call(rbind,lapply(log_files,read_extract_GNSS,prepped_PT=prepped_PT,dist_thresh=dist_thresh))
  
}



 


log_file=log_files[4]
dist_thresh=0.010 #km
time_thresh= 15*60 #minutes as seconds
corrected_PT=correct_PT(log_file,prepped_PT,dist_thresh)


  