
select_appropriate_drift=function(passname,swot_time_UTC,time_threshold_sec,wse_threshold_m,distance_threshold_m,keyfile){

library(dplyr)
library(fuzzyjoin)
# passname='fake swot pass id'
# swot_time_UTC=as.POSIXct('2022-07-26 21:44:47')
# time_threshold_sec= 120*60 #two hour
# wse_threshold_m=0.05 #within 5cm
# distance_threshold_m =200 #within 200m
# keyfile='Willamette/WM_Key.csv'

#FIRST CHECK- if time matches, use the time matched dirft
#read in node levels from drift
drift_nodes= read.csv('Willamette/SWORD products/node/Willamette_node_wses.csv')%>%
  mutate(time=as.POSIXct(time))%>% #beacuse it is a .csv and not .rds, it loses its datetime
  mutate(time_diff_to_swot_drift_sec=abs(time-swot_time_UTC))

keep_index= which(drift_nodes$time_diff_to_swot_drift_sec<=time_threshold_sec)
remove_index =which(drift_nodes$time_diff_to_swot_drift_sec>time_threshold_sec)

direct_match_drift=drift_nodes[keep_index,]%>%
  mutate(swot_time_UTC=swot_time_UTC)%>%
  mutate(swot_passid=passname)%>%
  transmute(node_id=as.character(format(node_id,scientific=FALSE)),reach_id=as.character(format(reach_id,scientific=FALSE)),
            drift_id=drift_id,time_diff_to_swot_drift_sec,swot_time_UTC,swot_passid)

#write it to file
if (nrow(direct_match_drift)>0){
  write.csv(direct_match_drift,paste0('Willamette/swot drift pairs/',passname,'direct.csv'))
}

if (nrow(direct_match_drift)==nrow(drift_nodes)){
  break
  #this means they are all within some time window of swot
}

indirect_drift= drift_nodes[remove_index,]%>%
  mutate(wse=node_wse)%>%
  select(-X)

#get the 1hz data for those left behind
drift_1hz=do.call(rbind,lapply(paste0('Willamette/Willamette munged drifts/',
                                      unique(indirect_drift$drift_id),'.csv' ),read.csv))%>%
  mutate(gnss_time_UTC=as.POSIXct(gnss_time_UTC))%>%#csv read scrubs date
  mutate(Lon=gnss_Lon,Lat=gnss_Lat)
#pull pt levels at swot time-----------
pt_at_swot_time= do.call(rbind,lapply(paste0('Willamette/Willamette munged pts/',list.files('Willamette/Willamette munged pts/')), read.csv))%>%
  mutate(pt_time_UTC=as.POSIXct(pt_time_UTC))%>%#csv read strips the datetime
  group_by(pt_serial)%>%
  mutate(time_diff_to_swot_pt_sec=abs(pt_time_UTC-swot_time_UTC))%>%
  filter(time_diff_to_swot_pt_sec==min(time_diff_to_swot_pt_sec))%>%
  ungroup()%>%
  select(-driftID,-X)%>%#this was the drift used to correct it, but taht is ocnfusing here
 mutate(Lat=pt_lat,Lon=pt_lon) #for joining

#compare drift node levels with pt levels
#do a difference join based lat/lon. Slow.

#read in key df first
key_df=read.csv(keyfile)%>%
  select(PT_Serial,Node_ID,Reach_ID)%>%
  transmute(pt_serial=PT_Serial,node_id=Node_ID,reach_id=Reach_ID)


drift_pt_join_df= geo_left_join(drift_1hz,pt_at_swot_time, by=c('Lon','Lat'),unit='km',method='haversine',
                                       distance_col='distance_km') %>% #this is a nearest neighbor join
mutate(wse_difference= gnss_wse - pt_wse)%>%
  left_join(key_df,by='pt_serial')%>%
  group_by(pt_serial,node_id,drift_id) %>%
  filter(distance_km<distance_threshold_m/1000)%>%
  #here, we have now found all drifts within a threshold of the pts. Since all of these were NOT collected close enough to 
  #swot's overpass, we do not need to time match
  
  #filtering now by level within a specified distance will give us the ability to find all 'good' matches.
  filter(abs(wse_difference)<wse_threshold_m)%>%
  #sweet. Now we've got e.g. all drifts within 200m of a pt within 5cm of a pt level. grouping by pts did the wse error by pt
  #now we will summarize those per pt errors within each node and drift
  group_by(node_id,drift_id) %>%
  transmute(drift_pt_dist_km_bar=distance_km,wse_difference_m=wse_difference,swot_passid=passname,
            swot_time_UTC=swot_time_UTC)%>%
  summarize(drift_pt_dist_km_bar=mean(drift_pt_dist_km_bar),
            wse_difference_m_bar=mean(wse_difference_m),
            wse_difference_m_sd=sd(wse_difference_m),
            swot_passid=first(swot_passid),
            swot_time_UTC=first(swot_time_UTC))%>%
  mutate(node_id=as.character(node_id))


  


#write to file
write.csv(drift_pt_join_df,paste0('Willamette/swot drift pairs/',passname,'matched.csv'))


}