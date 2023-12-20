create_gnss_dataframe= function(log_file,gnss_drift_data_directory,output_directory,rivername,keyfile,naughty_bin_directory){
  library(ncdf4)
  library(stringr)
  library(dplyr)
    
    #this takes correcte GNSS and writes it to file, applying our cleanup QA QC
  
# print(naughty_bin_directory)
    
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
    dplyr::select(-Event_end,-Event_start)%>%
    #add 1 minutes to the event codes
    mutate(Event_start_UTC=Event_start_UTC-1*60)%>%
     mutate(Event_end_UTC=Event_end_UTC+1*60)%>%
    mutate(Event_code=case_when(Event_code=='Bridge' |Event_code=='bridge' |
           Event_code=='Bridges' |Event_code=='bridges'|Event_code == 'Birdge' ~ 'bridge', Event_code == 'Powerline'|
           Event_code == 'powerlines' | Event_code == 'powerline' |  Event_code == 'Powerlines'  |  Event_code == 'power lines' ~ 'powerlines' ,
           Event_code == 'Top of Reach' | Event_code == 'top of reach' | Event_code == 'Top of reach' |  Event_code == 'top of Reach' |
           Event_code == 'Bottom of Reach' | Event_code == 'bottom of reach' | Event_code == 'Bottom of reach' |  Event_code == 'bottom of Reach' |
           Event_code == 'tp' | Event_code == 'TP' ~'TP'))%>%
    filter(!is.na(Event_code))
    

  gnss_log=data.frame(gnss_Lat=Lat,gnss_Lon=Lon,gnss_wse=gnss_wse,gnss_time_tai=gnss_time_tai,gnss_uncertainty_m=gnss_uncertainty,
                      gnss_surf_flag=gnss_surf_flag,gnss_motion_flag=gnss_motion_flag, height_above_ellipsoid= height_above_ellipsoid)%>%
    #R's native POSIXCT also doesn't have leap seconds, so we're good
    mutate(gnss_time_UTC = as.POSIXct(gnss_time_tai-37,origin='2000-01-01 00:00:00',tz='UTC' ))%>% # +37 because netcdf states a 37 second difference between TAI and UTC. only apply this here
    #need this to join, but let's preserve original
    filter(gnss_surf_flag==12)%>%
    filter(gnss_motion_flag==2)%>%
    mutate(gnss_ellipsoid=gnss_ellipsoid)%>%
    #filter for self ID uncertainty at 5cm
    filter(gnss_uncertainty_m<0.05)%>%
    mutate(drift_id= sub('',"",log_file))
 
    
#     print(gnss_motion_flag)
#     print(gnss_log)
#     print(log_file)
#     bonk

#waimak is special
    if (rivername =='WK'){
        
        #check to see if this is a PT install file
        #if so, it will be in the key
        #in that case, we need to include the motion code of 0
    installfile=keyfile$Final_Install_Log_File
    takeoutfile= keyfile$Final_Uninstall_Log_File
    filenames=c(installfile,takeoutfile)
        
#         print('in keyfile')
       # print(filenames)
#         print('from raw GNSS')
#         print(log_file)
  
#          print('detector')
#         print(str_replace(log_file,"L2",'L0'))
#         print(any(str_detect(filenames,str_replace(log_file,"L2",'L0')),na.rm=TRUE))
   
       
    if(any(str_detect(str_replace(log_file,"L2",'L0'),filenames),na.rm=TRUE)){
        
       #this means the fileis in the key, and is a PT occupy. We therefore need a '0' motion code in this case.
        
  gnss_log=data.frame(gnss_Lat=Lat,gnss_Lon=Lon,gnss_wse=gnss_wse,gnss_time_tai=gnss_time_tai,gnss_uncertainty_m=gnss_uncertainty,
                      gnss_surf_flag=gnss_surf_flag,gnss_motion_flag=gnss_motion_flag, height_above_ellipsoid= height_above_ellipsoid)%>%
    #R's native POSIXCT also doesn't have leap seconds, so we're good
    mutate(gnss_time_UTC = as.POSIXct(gnss_time_tai-37,origin='2000-01-01 00:00:00',tz='UTC' ))%>% # +37 because netcdf states a 37 second difference between TAI and UTC. only apply this here
    mutate(gnss_ellipsoid=gnss_ellipsoid)%>%
    #filter for self ID uncertainty at 5cm
    filter(gnss_uncertainty_m<0.05)%>%
    mutate(drift_id= sub('',"",log_file))
            }
        }

 #at this point, we have two pieces of info- turning points that need to be borken ito separate drifts, and events we need to filter out
    #let's make two info_dfs
    
    bad_info_df= filter(Info_df,Event_code == 'bridge' | Event_code == 'powerlines')
    good_info_df=filter(Info_df,Event_code=='TP')

  #get rid of the bad data
  #need to recurse this, so a for loop is actually needed!
if (nrow(bad_info_df)>0){
  for(i in 1:nrow(bad_info_df)){
    gnss_log=filter(gnss_log, gnss_time_UTC >= Info_df$Event_end_UTC[i] | gnss_time_UTC <= Info_df$Event_start_UTC[i] )
    
  }
}
    
    

  if (nrow(gnss_log)==0){
      
    write.csv(gnss_log,paste0(naughty_bin_directory,log_file,"_1.csv"))
    print(paste('filename',log_file,'bonked'))  
 
    nc_close(gnss_nc)
    return(NA)
  }
  
if (nrow(good_info_df)==0){
    gnss_log=gnss_log%>%
        mutate(drift_id=paste0(output_directory,drift_id,"_1.csv"))
    
    write.csv(gnss_log,paste0(output_directory,log_file,"_",as.character(1),'.csv'))
    nc_close(gnss_nc)
    return(NA)
  }
    #now, split the file based on turning points.
    #tricky.
    #there could be an infinite number of turning points, so we need to pre-assess how many splits we'll make
    #add a TP at the very beginning of the new good data
    start_tp=data.frame(Event_code='TP',Event_start_UTC=min(gnss_log$gnss_time_UTC),Event_end_UTC=min(gnss_log$gnss_time_UTC))
    end_tp=data.frame(Event_code='TP',Event_start_UTC=max(gnss_log$gnss_time_UTC),Event_end_UTC=max(gnss_log$gnss_time_UTC))
    good_info_df=rbind(start_tp,good_info_df,end_tp)%>%
    arrange(Event_start_UTC)#gotta be sorted
    
 # print('driftname')
  
  
    split_on_turning_point=function(row,good_info_df,output_directory,log_file){
            this_tp=good_info_df[row,]
            next_tp=good_info_df[(row+1),]
            new_df=filter(gnss_log, gnss_time_UTC >= this_tp$Event_end_UTC & gnss_time_UTC <= next_tp$Event_start_UTC )%>%
                   mutate(drift_id=paste0(output_directory,drift_id,"_",as.character(row),'.csv'))
        
        if(nrow(new_df)==0){return(NA)} #shouldn't happen, but does?
            write.csv(new_df,paste0(output_directory,log_file,"_",as.character(row),'.csv'))
            print(paste0(output_directory,log_file,"_",as.character(row),'.csv'))
       }  
    
    

    new_gnss_dfs=lapply(seq(1,(nrow(good_info_df)-1),by=1),split_on_turning_point,
                        good_info_df=good_info_df,
                       output_directory=output_directory,
                       log_file=log_file)
  

  


  nc_close(gnss_nc)


}
