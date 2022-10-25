

PT_test=readRDS(paste0(QA_QC_PT_output_directory,list.files(QA_QC_PT_output_directory)[1]))
plot(PT_test$PT_time_UTC,PT_test$PT_wse)

for (i in 1:length(list.files(QA_QC_drift_output_directory))){
drift_test=readRDS(paste0(QA_QC_drift_output_directory,list.files(QA_QC_drift_output_directory)[i]))
windows()
plot(drift_test$GNSS_time_UTC,drift_test$GNSS_wse)}