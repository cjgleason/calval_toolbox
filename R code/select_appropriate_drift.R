
passname='fake swot pass ID'
SWOT_time_UTC=as.POSIXct('2022-07-26 21:44:47')
time_threshold_sec= 120*60 #two hour

#FIRST CHECK- if time matches, use the time matched dirft
#read in node levels from drift
drift_nodes= read.csv('Willamette/SWORD products/node/Willamette_node_wses.csv')%>%
  mutate(time=as.POSIXct(time))%>% #beacuse it is a .csv and not .rds, it loses its datetime
  mutate(time_diff_to_SWOT_sec=abs(time-SWOT_time_UTC))

keep_index= which(drift_nodes$time_diff_to_SWOT_sec<=time_threshold_sec)
remove_index =which(drift_nodes$time_diff_to_SWOT_sec>time_threshold_sec)

direct_match_drift=drift_nodes[keep_index,]%>%
  mutate(SWOT_time_UTC=SWOT_time_UTC)

#write it to file
if (nrow(direct_match_drift)>0){
  write.csv(direct_match_drift,paste0('Willamette/SWOT drift pairs/',passname,'.csv'))
}

#pull PT levels at SWOT time-----------
PT_df= do.call(rbind,lapply(paste0('Willamette/Willamette munged PTs/',list.files('Willamette/Willamette munged PTs/')), read.csv))
#at the mooment, using all full PTs                          


#compare drift node levels with PT levels
#using some spatial matching

#identify which drift to use at the node level, or ID that they're too different
