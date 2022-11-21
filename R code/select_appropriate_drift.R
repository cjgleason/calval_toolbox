
#select_appropriate_drift=function(passname,SWOT_time_UTC,time_threshold_sec,wse_threshold_m,distance_threshold_m){

library(dplyr)
library(fuzzyjoin)
passname='fake swot pass ID'
SWOT_time_UTC=as.POSIXct('2022-07-26 21:44:47')
time_threshold_sec= 120*60 #two hour
wse_threshold_m=0.05 #within 5cm
distance_threshold_m =200 #within 200m

#FIRST CHECK- if time matches, use the time matched dirft
#read in node levels from drift
drift_nodes= read.csv('Willamette/SWORD products/node/Willamette_node_wses.csv')%>%
  mutate(time=as.POSIXct(time))%>% #beacuse it is a .csv and not .rds, it loses its datetime
  mutate(time_diff_to_SWOT_drift_sec=abs(time-SWOT_time_UTC))

keep_index= which(drift_nodes$time_diff_to_SWOT_drift_sec<=time_threshold_sec)
remove_index =which(drift_nodes$time_diff_to_SWOT_drift_sec>time_threshold_sec)

direct_match_drift=drift_nodes[keep_index,]%>%
  mutate(SWOT_time_UTC=SWOT_time_UTC)%>%
  mutate(SWOT_passid=passname)%>%
  select(node_ID,reach_ID,drift_ID,time_diff_to_SWOT_drift_sec,SWOT_time_UTC,SWOT_passid)

#write it to file
if (nrow(direct_match_drift)>0){
  write.csv(direct_match_drift,paste0('Willamette/SWOT drift pairs/',passname,'direct.csv'))
}

if (nrow(direct_match_drift)==nrow(drift_nodes)){
  break
  #this means they are all within some time window of SWOT
}

indirect_drift= drift_nodes[remove_index,]%>%
  mutate(wse=node_wse)%>%
  select(-X)

#get the 1hz data for those left behind
drift_1hz=do.call(rbind,lapply(paste0('Willamette/Willamette munged drifts/',
                                      unique(indirect_drift$drift_ID),'.csv' ),read.csv))%>%
  mutate(GNSS_time_UTC=as.POSIXct(GNSS_time_UTC))%>%#csv read scrubs date
  mutate(Lon=GNSS_Lon,Lat=GNSS_Lat)
#pull PT levels at SWOT time-----------
PT_at_SWOT_time= do.call(rbind,lapply(paste0('Willamette/Willamette munged PTs/',list.files('Willamette/Willamette munged PTs/')), read.csv))%>%
  mutate(PT_time_UTC=as.POSIXct(PT_time_UTC))%>%#csv read strips the datetime
  group_by(PT_serial)%>%
  mutate(time_diff_to_SWOT_PT_sec=abs(PT_time_UTC-SWOT_time_UTC))%>%
  filter(time_diff_to_SWOT_PT_sec==min(time_diff_to_SWOT_PT_sec))%>%
  ungroup()%>%
  select(-driftID,-X)%>%#this was the drift used to correct it, but taht is ocnfusing here
 mutate(Lat=PT_lat,Lon=PT_lon) #for joining

#compare drift node levels with PT levels
#do a difference join based lat/lon. Slow.

drift_pt_join_df= geo_left_join(drift_1hz,PT_at_SWOT_time, by=c('Lon','Lat'),unit='km',method='haversine',
                                       distance_col='distance_km') %>% #this is a nearest neighbor join
mutate(wse_difference= GNSS_wse - PT_wse)%>%
  group_by(PT_serial) %>%
  filter(distance_km<distance_threshold_m/1000)%>%
  #here, we have now found all drifts within a threshold of the PTs. Since all of these were NOT collected close enough to 
  #SWOT's overpass, we do not need to time match
  
  #fitlering now by level within a specified distance will give us the abilltiy to find all 'good' matches.
  filter(wse_difference<wse_threshold_m)%>%
  group_by(drift_ID,add = TRUE)%>%
  #sweet. Now we've got e.g. all drifts within 200m of a PT within 5cm of a PT level.
  transmute(Drift_PT_dist_km=distance_km,wse_difference_m=wse_difference,SWOT_passid=passname,SWOT_time_UTC=SWOT_time_UTC)%>%
  summarize(Drift_PT_dist_km=mean(Drift_PT_dist_km),wse_dffierence_m_bar=mean(wse_difference_m),
            wse_dffierence_m_sd=sd(wse_difference_m),SWOT_passid=first(SWOT_passid),SWOT_time_UTC=first(SWOT_time_UTC))
  

#write to file
write.csv(drift_pt_join_df,paste0('Willamette/SWOT drift pairs/',passname,'matched.csv'))


