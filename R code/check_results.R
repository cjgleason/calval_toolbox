
for (i in 1:length(list.files(QA_QC_PT_output_directory))){
PT_test=readRDS(paste0(QA_QC_PT_output_directory,list.files(QA_QC_PT_output_directory)[i]))
windows()
plot(PT_test$PT_time_UTC,PT_test$PT_wse)}

for (i in 1:length(list.files(QA_QC_drift_output_directory))){
drift_test=read.csv(paste0(QA_QC_drift_output_directory,list.files(QA_QC_drift_output_directory)[i]),header=TRUE)
windows()
plot(drift_test$GNSS_time_UTC,drift_test$GNSS_wse)}

for (i in 1:length(list.files('Willamette/SWORD products/reach/'))){
  reach_test=readRDS(paste0('Willamette/SWORD products/reach/',list.files('Willamette/SWORD products/reach/')[i]))
}

for (i in 1:length(list.files('Willamette/SWORD products/node/'))){
  node_test=read.csv(paste0('Willamette/SWORD products/node/',list.files('Willamette/SWORD products/node/')[i]))
}


reach_PT_slope=read.csv(paste0('Willamette/SWORD products/reach/Willamette_reach_PT_slope.csv'))%>%
  mutate(reach_id=as.character(reach_id))%>%
  mutate(PT_time_UTC=as.POSIXct(PT_time_UTC))%>%
  filter(reach_id=='78220000171')

reach_drift_slope=read.csv('Willamette/SWORD products/reach//Willamette_drift_wse_slope.csv')%>%
  mutate(reach_id=as.character(reach_ID))%>%
  mutate(wse_start=as.POSIXct(wse_start))%>%
  mutate(wse_end=as.POSIXct(wse_end))%>%
  filter(reach_id=='78220000171')

plot(reach_PT_slope$PT_time_UTC,reach_PT_slope$slope)
points(reach_drift_slope$wse_start,-reach_drift_slope$slope,col='red',lwd=5)
points(reach_drift_slope$wse_start,-(reach_drift_slope$slope+reach_drift_slope$slope_sd),col='red',lwd=5)
points(reach_drift_slope$wse_start,-(reach_drift_slope$slope-reach_drift_slope$slope_sd),col='red',lwd=5)
