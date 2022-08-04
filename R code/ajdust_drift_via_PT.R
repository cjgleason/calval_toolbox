drift_directory='D:/OneDrive -\ University of Massachusetts/calval/Toolbox/calval_toolbox/Taylor data 7 12/drifts/'
PT_key_file='D:/OneDrive -\ University of Massachusetts/calval/Toolbox/calval_toolbox/Taylor data 7 12/PT drift key.csv'
PT_directory='D:/OneDrive -\ University of Massachusetts/calval/Toolbox/calval_toolbox/Taylor data 7 12/QAQC PTs/'
max_PT_to_drift= 2.00 #km,  
SWOT_time= as.POSIXct('2021-09-02 23:04:37') # a dummy value far away from the drift

#correct drifts to PTs-----

library(dplyr)
library(fuzzyjoin)
library(stringr)


#get the 1HZ GNSS data and calculate distances to PTs within a threshold----------
setwd(drift_directory)
drift_in=read.csv(list.files(drift_directory)[1],header = TRUE) %>%
  mutate(lat=latitude_decimal_degree)%>%
  mutate(lon=longitude_decimal_degree)%>%
  select(-latitude_decimal_degree,-longitude_decimal_degree)%>%
  mutate(hour=decimal_hour*3600)%>% #hours to seconds, as posix is seconds base
  mutate(datetime=as.POSIXct(paste(year,day_of_year),format= '%Y %j'))%>%
  mutate(datetime=datetime + hour)%>%
  select(-decimal_hour,-day_of_year,-year,-rcvr_clk_ns,-hour)

PT_key_in=read.csv(PT_key_file,header=TRUE)%>%
  transmute(lat=Lat,lon=Long,PT=PT,PT_Serial=as.character(PT_Serial),PT_install_GNSS_offset=GNSS_offset)

#this key file was created for matching drifts, BUT, it also gives the lat/lon of all teh PTs on a river
#therefore, we can use it to ID the closest PTs in a particular drift section
#ssuming this key is for one river, and the drift is for the same river

#important to do a geo_join and not a geo_left_join, so that it finds all matches, not just the closest.

GNSS_with_weights= geo_join(drift_in,PT_key_in,  by = c('lon','lat'),  method = "haversine", unit="km",max_dist=max_PT_to_drift,distance_col='GNSS_to_PT_km')%>%
  #filter(GNSS_to_PT_km<max_PT_to_drift)%>%
  group_by(datetime) %>%
  mutate(PT_weight= (1/GNSS_to_PT_km)^4)%>%
  ungroup()  %>%
  select(-lat.x,-lat.y,-lon.x,-lon.y,-PT_install_GNSS_offset)
#this gives us the distance from every GNSS timestep to the nearest PT. 
#we've grouped by datetime, as at each instant we want to figure out the nearst PTs and apply that
#as a first cut, we can do an IDW to figuure out a weighting function for that part of the drift.
#we'll then need to smooth these, probably
#------------------

#now, make a concatenated DF of all of the PT serials that appear in our filtered 1hz df----
#get serials
serials=as.character(unique(GNSS_with_weights$PT_Serial))
#this pulls the characters between the _ and the ., and then pulls after the _ to get the serial number
pattern <- "_\\s*(.*?)\\s*\\."
PT_file_index=which( sub(".*_" ,"",str_match(list.files(PT_directory),pattern)[,2]    )  %in%  serials )
PTs_to_read=paste0(PT_directory,list.files(PT_directory)[PT_file_index])

matching_PTs= do.call(rbind,lapply(PTs_to_read,readRDS))%>%
  mutate(PT_Serial=as.character(PT_Serial))
#-------------

#now, get a df of the PTs at closest to SWOT time, join to PTs-----
SWOT_PT=group_by(matching_PTs,PT_Serial)%>%
  mutate(time_diff_to_SWOT_sec=abs(datetime-SWOT_time))%>%
  filter(time_diff_to_SWOT_sec==min(time_diff_to_SWOT_sec))%>%
  ungroup()%>%
  transmute(PT_Serial=PT_Serial,SWOT_datetime=datetime,time_diff_to_SWOT_sec=time_diff_to_SWOT_sec,
            level_at_SWOT_time=corrected_level,SWOT_sd_GNSS=sd_GNSS,PT_sd_GNSS=sd_GNSS)

#create an offset based on the swot tim--
final_PT_with_SWOT=matching_PTs%>%
  left_join(SWOT_PT,by='PT_Serial')  %>%
  mutate(SWOT_time_level_correction = corrected_level -level_at_SWOT_time)%>%
  select(PT_Serial,datetime,time_diff_to_SWOT_sec,
         SWOT_time_level_correction,corrected_level,sd_GNSS,sd_PT)%>%
  mutate(level_at_obs_time=corrected_level)%>%
  select(-corrected_level)
#----------

#at this point, we have:
# GNSS_with_weights : a 1HZ GNSS df () that has an IDW weight column
# final_PT_with_SWOT : a DF of all of the PT Ids within the 1 HZ df with a correction for SWOT's overpass time

# we need to join the PTs to the 1hz df (left join to GNSS) to get all that info within the GNSS dataframe

#join the GNSS and PT dfs, calculate levels at 1hz and swot time-----

match_fun= function(PT_Serial_in,GNSS_in,PT_in){
  
stupid=function(index){
    which_row= which(abs(temp_PT_df$datetime-temp_GNSS_df$datetime[index]) ==min(abs( temp_PT_df$datetime-temp_GNSS_df$datetime[index]))   )
    
    return(cbind(temp_GNSS_df[index,],temp_PT_df[which_row,]))
  }

temp_GNSS_df= filter(GNSS_in,PT_Serial==PT_Serial_in)
temp_PT_df=  filter(PT_in,PT_Serial==PT_Serial_in)

joined_DF=do.call(rbind,lapply(1:nrow(temp_GNSS_df),stupid))
joined_DF=joined_DF[!duplicated(as.list(joined_DF))] %>%
  distinct()

}

#change PT serials to integers for our kluge
final_PT_with_SWOT$PT_Serial=as.integer(final_PT_with_SWOT$PT_Serial)
GNSS_with_weights$PT_Serial=as.integer(GNSS_with_weights$PT_Serial)

#final_DF=fuzzy_left_join(GNSS_with_weights,final_PT_with_SWOT, by=c('datetime','PT_Serial'),match_fun = match_fun)
 #^^^ way too slow

#kicking it old school
#joined_DF= do.call(rbind,lapply(unique(GNSS_with_weights$PT_Serial), match_fun, GNSS_in=GNSS_with_weights, PT_in=final_PT_with_SWOT )   )

joined_DF=difference_left_join(GNSS_with_weights,final_PT_with_SWOT,by=c("datetime"),max_dist=7.51*60)%>%
  #this just joins all PTs to all GNSS locations based on the closest time. Many are not relevant, so we need to filter
  #on the serial number
   mutate(Serial_hack=PT_Serial.x-PT_Serial.y)%>%
   filter(Serial_hack==0)%>%
  # at this point we have the GNSS data, its closest PTs at 1hz, and the corresponding 
  # level and SWOT time correction for each of those closest PTs
  
  #we'll group these at 1 Hz and then apply IDW
   group_by(datetime.x) %>%
   mutate(drift_shift=  sum(SWOT_time_level_correction / PT_weight))%>%
   mutate(final_drift_height = ortho_height_m_cgvd2013 + drift_shift)
  
#---------

plot(joined_DF$datetime.x,joined_DF$ortho_height_m_cgvd2013)
points(joined_DF$datetime.x,joined_DF$final_drift_height,col='red')


#--------------------------