
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
  node_test=readRDS(paste0('Willamette/SWORD products/node/',list.files('Willamette/SWORD products/node/')[i]))
}
