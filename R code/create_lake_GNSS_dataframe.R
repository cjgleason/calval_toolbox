Sys.umask('002')
create_lake_gnss_dataframe= function(log_file,gnss_drift_data_directory,output_directory){
  library(ncdf4)
  library(stringr)
  library(dplyr)
  
  # hubname='UNC'
  # continent='na'
  # lakename='YF'
  # PT_key_file=c('SWOTCalVal_YF_KEY_20230521_20230923.csv',
  #               'SWOTCalVal_YF_KEY_20230603_20230722.csv') #WM
  # utm_zone=6 #WM= 10
  # 
  # 
  #   gnss_drift_data_directory=paste0('/nas/cee-water/cjgleason/calval/Processed data/',hubname,'/From Andy/UNC_netCDFs/')
  #   output_directory=paste0('/nas/cee-water/cjgleason/calval/Processed data/',hubname,'/Munged drifts/')
  #   naughty_bin_directory=paste0('/nas/cee-water/cjgleason/calval/Processed data/',hubname,'/Flagged drifts/')
  #   log_file = list.files(gnss_drift_data_directory)
  #   log_file = log_file[grep("_YF_",log_file)]
  #   gnss_nc=nc_open(paste0(gnss_drift_data_directory,log_file))
    #create GNSS files for lakes

  gnss_nc=nc_open(paste0(gnss_drift_data_directory,log_file,'.nc'))
  # variables we need
  # wse- water surface height wrt geoid. All JPL corrections applied
  # longitude- longitude
  # latitude- latitute
  # time_tai - time in some wierd TAI format. It is seconds since January 1 2000 at midnight WITHOUT leap seconds.
  #motion flag- 0,1, or 2. Codes 0 and 1 incidate no good data, keep only 2
  #surface type flag - 10, 11, or 12. Simialrly, only code 12 incicates quality data
  
  
  
  Lat=ncvar_get(gnss_nc,'latitude')
  Lon=ncvar_get(gnss_nc,'longitude')
  gnss_wse= ncvar_get(gnss_nc,'wse')
  gnss_time_tai=ncvar_get(gnss_nc,'time_tai')
  gnss_motion_flag  =ncvar_get(gnss_nc,'motioncode_flag')
  gnss_surf_flag  =ncvar_get(gnss_nc,'surfacetype_flag')
  gnss_ellipsoid= paste(ncatt_get(gnss_nc,0,'ellipsoid_semi_major_axis')$value,ncatt_get(gnss_nc,0,'ellipsoid_flattening')$value,sep=",")
  gnss_uncertainty=ncvar_get(gnss_nc,'position_3drss_formal_error')
  height_above_ellipsoid=ncvar_get(gnss_nc,'height_water')
  
  Info_event=ncvar_get(gnss_nc,'infoEventDescription')
  Info_event_start=ncvar_get(gnss_nc,'infoEventStartTime')
  Info_event_end=ncvar_get(gnss_nc,'infoEventEndTime')
  


  Info_df=data.frame(Event_code=Info_event, Event_start= Info_event_start, Event_end=Info_event_end)%>%
    mutate(Event_start_UTC = as.POSIXct(Event_start ,origin='2000-01-01 00:00:00',tz='UTC'))%>% 
    mutate(Event_end_UTC = as.POSIXct(Event_end ,origin='2000-01-01 00:00:00',tz='UTC' ))%>%
    select(-Event_end,-Event_start)%>%
    #add 1 minutes to the event codes
    mutate(Event_start_UTC=Event_start_UTC-1*60)%>%
    mutate(Event_end_UTC=Event_end_UTC+1*60)%>%
    filter(Event_code=='Bridge' | Event_code == 'Powerlines' | Event_code=='bridge' |
           Event_code=='Bridges' |Event_code=='bridges'| Event_code == 'Powerline'|
           Event_code == 'powerlines' | Event_code == 'powerline' | Event_code == 'Birdge')
  

  gnss_log=data.frame(gnss_Lat=Lat,gnss_Lon=Lon,gnss_wse=gnss_wse,gnss_time_tai=gnss_time_tai,gnss_uncertainty_m=gnss_uncertainty,
                      gnss_surf_flag=gnss_surf_flag,gnss_motion_flag=gnss_motion_flag, height_above_ellipsoid= height_above_ellipsoid)%>%
    #R's native POSIXCT also doesn't have leap seconds, so we're good
    mutate(gnss_time_UTC = as.POSIXct(gnss_time_tai-37,origin='2000-01-01 00:00:00',tz='UTC' ))%>% # +37 because netcdf states a 37 second difference between TAI and UTC. only apply this here
    #need this to join, but let's preserve original
    mutate(gnss_ellipsoid=gnss_ellipsoid)%>%
    #filter for self ID uncertainty at 5cm
    filter(gnss_uncertainty_m<0.05)%>%
    mutate(drift_id= sub('',"",log_file))
    


  #need to recurse this, so a for loop is actually needed!
if (nrow(Info_df)>0){
  for(i in 1:nrow(Info_df)){
    gnss_log=filter(gnss_log, gnss_time_UTC >= Info_df$Event_end_UTC[i] | gnss_time_UTC <= Info_df$Event_start_UTC[i] )
    
  }
}
  
  if (nrow(gnss_log)==0){
    print(paste('filename',log_file,'bonked'))
    return(NA)
  }
  
  # plot(gnss_log$gnss_time_UTC,gnss_log$gnss_wse)
  
  nc_close(gnss_nc)
  
  print(paste0(output_directory,log_file,'.csv'))
  
  write.csv(gnss_log,paste0(output_directory,log_file,'.csv'))




}
