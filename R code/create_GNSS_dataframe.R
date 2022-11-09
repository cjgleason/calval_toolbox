create_GNSS_dataframe= function(log_file,GNSS_drift_data_directory,output_directory){
library(ncdf4)
  library(stringr)
#log_file=unmunged_drifts[1]


GNSS_nc=nc_open(paste0(GNSS_drift_data_directory,log_file,'.nc'))
# variables we need
# wse- water surface height wrt geoid. All JPL corrections applied
# longitude- longitude
# latitude- latitute
# time_tai - time in some wierd TAI format. It is seconds since January 1 2000 at midnight WITHOUT leap seconds.
#motion flag- 0,1, or 2. COdes 0 and 1 incidate no good data, keep only 2
#surface type flag - 10, 11, or 12. Simialrly, only code 12 incicates quality data


Lat=ncvar_get(GNSS_nc,'latitude')
Lon=ncvar_get(GNSS_nc,'longitude')
GNSS_wse= ncvar_get(GNSS_nc,'wse')
GNSS_time_tai=ncvar_get(GNSS_nc,'time_tai')
GNSS_motion_flag  =ncvar_get(GNSS_nc,'motioncode_flag')
GNSS_surf_flag  =ncvar_get(GNSS_nc,'surfacetype_flag')
GNSS_ellipsoid= paste(ncatt_get(GNSS_nc,0,'ellipsoid_semi_major_axis')$value,ncatt_get(GNSS_nc,0,'ellipsoid_flattening')$value,sep=",")
GNSS_uncertainty=ncvar_get(GNSS_nc,'position_3drss_formal_error')

Info_event=ncvar_get(GNSS_nc,'infoEventDescription')
Info_event_start=ncvar_get(GNSS_nc,'infoEventStartTime')
Info_event_end=ncvar_get(GNSS_nc,'infoEventEndTime')

Info_df=data.frame(Event_code=Info_event, Event_start= Info_event_start, Event_end=Info_event_end)%>%
  mutate(Event_start_UTC = as.POSIXct(Event_start,origin='2000-01-01 00:00:00',tz='UTC'))%>%
  mutate(Event_end_UTC = as.POSIXct(Event_end,origin='2000-01-01 00:00:00',tz='UTC' ))%>%
  select(-Event_end,-Event_start)%>%
  #add 2 minutes to the event codes
  mutate(Event_start_UTC=Event_start_UTC-2*60)%>%
  mutate(Event_end_UTC=Event_end_UTC+2*60)%>%
  filter(Event_code=='Bridge' | Event_code == 'Powerlines' )



GNSS_log=data.frame(GNSS_Lat=Lat,GNSS_Lon=Lon,GNSS_wse=GNSS_wse,GNSS_time_tai=GNSS_time_tai,GNSS_uncertainty=GNSS_uncertainty,
                    GNSS_surf_flag=GNSS_surf_flag,GNSS_motion_flag=GNSS_motion_flag)%>%
  #R's native POSIXCT also doesn't have leap seconds, so we're good
  mutate(GNSS_time_UTC = as.POSIXct(GNSS_time_tai,origin='2000-01-01 00:00:00',tz='UTC' ))%>%
  #need this to join, but let's presrve original
  filter(GNSS_surf_flag==12)%>%
  filter(GNSS_motion_flag==2)%>%
  mutate(GNSS_ellipsoid=GNSS_ellipsoid)%>%
  filter(GNSS_uncertainty<0.05)%>%
  mutate(drift_ID= sub('',"",log_file))

#need to recurse this, so a for loop is actually needed!
for(i in 1:nrow(Info_df)){
  GNSS_log=filter(GNSS_log, GNSS_time_UTC >= Info_df$Event_end_UTC[i] | GNSS_time_UTC <= Info_df$Event_start_UTC[i] )
  
}

if (nrow(GNSS_log)==0){
  print(paste('filename',log_file,'bonked'))
  return(NA)
}

plot(GNSS_log$GNSS_time_UTC,GNSS_log$GNSS_wse)

nc_close(GNSS_nc)

print(paste0(output_directory,log_file,'.csv'))

write.csv(GNSS_log,paste0(output_directory,log_file,'.csv'))}
