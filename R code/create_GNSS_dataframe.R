create_GNSS_dataframe= function(log_file,GNSS_drift_data_directory,output_directory){
library(ncdf4)
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

GNSS_log=data.frame(GNSS_Lat=Lat,GNSS_Lon=Lon,GNSS_wse=GNSS_wse,GNSS_time_tai=GNSS_time_tai,GNSS_surf_flag=GNSS_surf_flag,GNSS_motion_flag=GNSS_motion_flag)%>%
  #R's native POSIXCT also doesn't have leap seconds, so we're good
  mutate(GNSS_time_UTC = as.POSIXct(GNSS_time_tai,origin='2000-01-01 00:00:00' ))%>%
  #need this to join, but let's presrve original
  filter(GNSS_surf_flag==12)%>%
  filter(GNSS_motion_flag==2)%>%
  mutate(GNSS_ellipsoid=GNSS_ellipsoid)

nc_close(GNSS_nc)

print(paste0(output_directory,log_file,'.rds'))

saveRDS(GNSS_log,paste0(output_directory,log_file,'.rds'))}
