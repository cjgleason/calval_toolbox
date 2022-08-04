#TODO
#calculate separate offset for put in and pull out, and check that to ensure a lack of shifting.
#make hdyro corrections bit.

PT_data_directory='D:/OneDrive -\ University of Massachusetts/calval/Toolbox/calval_toolbox/Taylor data 7 12/raw PTs/'
QA_QC_PT_output_directory='D:/OneDrive -\ University of Massachusetts/calval/Toolbox/calval_toolbox/Taylor data 7 12/QAQC PTs/'
flagged_PT_output_directory='D:/OneDrive -\ University of Massachusetts/calval/Toolbox/calval_toolbox/Taylor data 7 12/flagged PTs/'

library(dplyr)

dist_thresh=15 # 15m
time_thresh= 15*60 #minutes as seconds, centered, so 15 =30 mins total time
GNSS_sd_thresh=0.15 # 15cm how much variance do you want in the GNSS data when it is within the distance threshold?
change_thresh_15_min= 0.20 #m, so 20cm. This checks for a time-on-time change in flow of more than xxxcm to check for suddent shifts in the PT.
#typically, these occur during put in and takeout as suddent shifts while it is deployed are rare. 
offset_diff_thresh=0.01 #m, so 1cm. the the PT apparantly shift by more than a cm?

# munge PTs if needed------
#check for un-munged PT data
#pull filename before the .csv
raw_PT=sub( "\\..*","", list.files(PT_data_directory))
#pull filename before the second _
QA_QC_PTs=sub('_([^_]*)$',"",list.files(QA_QC_PT_output_directory))
flagged_PTs=sub('_([^_]*)$',"",list.files(flagged_PT_output_directory))
#what raw PT data have not been munged
unmunged_PTs=setdiff(raw_PT,c(flagged_PTs,QA_QC_PTs))
#run the PTs that are not yet munged


if(!identical(unmunged_PTs,character(0))){
  unmunged_PTs=paste0(unmunged_PTs,'.csv')
  
source('D:/OneDrive -\ University of Massachusetts/calval/Toolbox/calval_toolbox/R code/correct_PT_to_GNSS.R')

  GNSS_drift_data_directory='D:/OneDrive -\ University of Massachusetts/calval/Toolbox/calval_toolbox/Taylor data 7 12/drifts/'
  clean_PT_output_directory='D:/OneDrive -\ University of Massachusetts/calval/Toolbox/calval_toolbox/Taylor data 7 12/offset PTs/'
  raw_PT_file='CPT01_20210909.csv'
  PT_key_file='D:/OneDrive -\ University of Massachusetts/calval/Toolbox/calval_toolbox/Taylor data 7 12/PT drift key.csv'


  PT_files=list.files(PT_data_directory)
  dummy=lapply(unmunged_PTs,correct_PT_to_GNSS,PT_key_file=PT_key_file,dist_thresh=dist_thresh,
         time_thresh=time_thresh,PT_data_directory=PT_data_directory,GNSS_drift_data_directory=GNSS_drift_data_directory,
         QA_QC_PT_output_directory=QA_QC_PT_output_directory,flagged_PT_directory=flagged_PT_directory,
         change_thresh_15_min=change_thresh_15_min,GNSS_sd_thresh,offset_diff_thresh)
  }
#-----------------------------


#correct drifts to PTs-----
drift_directory='D:/OneDrive -\ University of Massachusetts/calval/Toolbox/calval_toolbox/Taylor data 7 12/drifts/'
PT_key_file='D:/OneDrive -\ University of Massachusetts/calval/Toolbox/calval_toolbox/Taylor data 7 12/PT drift key.csv'
PT_directory='D:/OneDrive -\ University of Massachusetts/calval/Toolbox/calval_toolbox/Taylor data 7 12/QAQC PTs/'
max_PT_to_drift= 2.00 #km,  
SWOT_time= as.POSIXct('2021-09-02 23:04:37') # a dummy value far away from the drift

#warning! no output is saved yet. 
source('D:/OneDrive -\ University of Massachusetts/calval/Toolbox/calval_toolbox/R code/ajdust_drift_via_PT.R')
test=adjust_drift_via_PT(drift_directory,PT_key_file,PT_directory,max_PT_to_drift,SWOT_time)
#--------------------------




