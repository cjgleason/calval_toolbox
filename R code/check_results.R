library(dplyr)
library(ggplot2)
library(fuzzyjoin)

#temporary code that looked at a specific time period. Do not use

#compare reach WSE pt vs drift-------------
reach_pt_wse=read.csv('Willamette/SWORD products/reach/Willamette_PT_reach_wse.csv')%>%
  select(-X)

reach_drift_wse=read.csv('Willamette/SWORD products/reach/Willamette_drift_reach_wse_slope.csv')%>%
  select(-X)

reach_pt_drift_df=inner_join(reach_pt_wse,reach_drift_wse,by='reach_id')%>%
  #make a time check column
  filter(as.POSIXct(pt_time_UTC)> as.POSIXct(wse_start) & as.POSIXct(pt_time_UTC)< as.POSIXct(wse_end) )

plot_wse=ggplot(reach_pt_drift_df) +
  
  geom_point(aes(x=mean_reach_pt_wse_m,y=wse_bar,col=as.factor(wse_start)))  +
  geom_line(aes(x=mean_reach_pt_wse_m,y=mean_reach_pt_wse_m),col='black')+
  labs(x='PT reach wse',y='drift reach wse', title= 'pts within duration of drift through reach')

plot(plot_wse)
#--------------------------------

#compare node WSE pt vs drift----------
node_pt_wse=read.csv('Willamette/SWORD products/node/Willamette_PT_node_wse.csv')%>%
  select(-X)%>%
  mutate(datetime=as.POSIXct(pt_time_UTC))

node_drift_wse=read.csv('Willamette/SWORD products/node/Willamette_drift_node_wses.csv')%>%
  select(-X)%>%
  mutate(datetime=as.POSIXct(time))

node_pt_drift_df=difference_inner_join(node_pt_wse,node_drift_wse,by='datetime',distance_col='time diff',max_dist=7.5*60)%>%
  mutate(node_dif= as.numeric(node_id.x) -as.numeric(node_id.y))%>%
  filter(node_dif==0)%>%
  mutate(node_id=node_id.x)

rmse= sqrt(sum((node_pt_drift_df$mean_reach_pt_wse_m-node_pt_drift_df$node_wse)^2)/nrow(node_pt_drift_df))

plot_wse2=ggplot(node_pt_drift_df) +

  geom_point(aes(x=mean_reach_pt_wse_m,y=node_wse,col=as.factor(node_id)))  +
  geom_line(aes(x=mean_reach_pt_wse_m,y=mean_reach_pt_wse_m),col='black')+
  labs(x='PT node wse',y='drift node wse', title= 'drift and pt within same node and within 30min')


plot(plot_wse2)
#-------------------------------------

#does averaging node PTs look like a reach averaged PTs?---------
node_joiner=node_drift_wse%>%
  select(node_id,reach_id)

node_pt_wse=read.csv('Willamette/SWORD products/node/Willamette_PT_node_wse.csv')%>%
  select(-X)%>%
  mutate(datetime=as.POSIXct(pt_time_UTC))%>%
  left_join(node_joiner,by='node_id')%>%
  group_by(pt_time_UTC,reach_id)%>%
  summarize(node_averaged_to_reach=mean(mean_reach_pt_wse_m))%>%
  ungroup()%>%
  mutate(datetime=pt_time_UTC)

reach_pt_wse=read.csv('Willamette/SWORD products/reach/Willamette_PT_reach_wse.csv')%>%
  select(-X)%>%
  mutate(datetime=pt_time_UTC)

joined_reach_pt_df=left_join(node_pt_wse,reach_pt_wse,by=c('datetime','reach_id'))%>%
  transmute(pt_time_UTC=datetime,node_averaged_to_reach=node_averaged_to_reach, pt_averaged_in_reach=mean_reach_pt_wse_m)

rmse= sqrt(sum((joined_reach_pt_df$node_averaged_to_reach-joined_reach_pt_df$pt_averaged_in_reach)^2)/nrow(joined_reach_pt_df))

plot3=ggplot(joined_reach_pt_df)+
  geom_point(aes(x=node_averaged_to_reach,y=pt_averaged_in_reach))+
  geom_line(aes(x=node_averaged_to_reach,y=node_averaged_to_reach),col='black')+
  labs(x='PT nodes averaged to reach',y='PTs averaged within a reach directly', title= 'Time matched reach average vs node-to-reach average (PT only)')


plot(plot3)
#-------------------

