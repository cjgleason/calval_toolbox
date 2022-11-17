library(dplyr)
library(fuzzyjoin)
passname='fake swot pass ID'
SWOT_time_UTC=as.POSIXct('2022-07-26 21:44:47')
time_threshold_sec= 120*60 #two hour
wse_threshold_m=0.05 #within 5cm

#FIRST CHECK- if time matches, use the time matched dirft
#read in node levels from drift
drift_nodes= read.csv('Willamette/SWORD products/node/Willamette_node_wses.csv')%>%
  mutate(time=as.POSIXct(time))%>% #beacuse it is a .csv and not .rds, it loses its datetime
  mutate(time_diff_to_SWOT_drift_sec=abs(time-SWOT_time_UTC))

keep_index= which(drift_nodes$time_diff_to_SWOT_drift_sec<=time_threshold_sec)
remove_index =which(drift_nodes$time_diff_to_SWOT_drift_sec>time_threshold_sec)

direct_match_drift=drift_nodes[keep_index,]%>%
  mutate(SWOT_time_UTC=SWOT_time_UTC)%>%
  mutate(SWOT_passid=passname)

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
  mutate()
#pull PT levels at SWOT time-----------
PT_at_SWOT_time= do.call(rbind,lapply(paste0('Willamette/Willamette munged PTs/',list.files('Willamette/Willamette munged PTs/')), read.csv))%>%
  mutate(PT_time_UTC=as.POSIXct(PT_time_UTC))%>%#csv read strips the datetime
  group_by(PT_serial)%>%
  mutate(time_diff_to_SWOT_PT_sec=abs(PT_time_UTC-SWOT_time_UTC))%>%
  filter(time_diff_to_SWOT_PT_sec==min(time_diff_to_SWOT_PT_sec))%>%
  ungroup()%>%
  select(-driftID,-X)%>%#this was the drift used to correct it, but taht is ocnfusing here
 mutate(lat=PT_lat,lon=PT_lon) #for joining

#compare drift node levels with PT levels
#do a difference join based on wse. PT wse vs drift WSW
drift_pt_join_df= geo_left_join(indirect_drift,PT_at_SWOT_time, unit='km',method='haversine',
                                       distance_col='distance_m')%>%
  
#now select the closest drift per node
  group_by(node_ID) %>%
  filter(wse_difference==min(wse_difference))%>%
  ungroup()%>%
  filter(wse_difference<wse_threshold_m)%>%
  select(node_ID,node_wse,node_wse_sd,time,reach_ID,drift_ID,
         time_diff_to_SWOT_drift_sec,wse_difference)%>%#there are ambigous, as they aren't node products
  mutate(SWOT_passid=passname)%>%
mutate(SWOT_time_UTC=SWOT_time_UTC)
#write to file
write.csv(drift_pt_join_df,paste0('Willamette/SWOT drift pairs/',passname,'matched.csv'))
