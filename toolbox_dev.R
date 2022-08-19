#TODO
#calculate separate offset for put in and pull out, and check that to ensure a lack of shifting.
#make hdyro corrections bit.

PT_data_directory='D:/OneDrive -\ University of Massachusetts/calval/Toolbox/calval_toolbox/Taylor data 7 12/raw PTs/'
QA_QC_PT_output_directory='D:/OneDrive -\ University of Massachusetts/calval/Toolbox/calval_toolbox/Taylor data 7 12/QAQC PTs/'
flagged_PT_output_directory='D:/OneDrive -\ University of Massachusetts/calval/Toolbox/calval_toolbox/Taylor data 7 12/flagged PTs/'

library(dplyr)

# munge PTs if needed------

dist_thresh=15 # 15m
time_thresh= 15*60 #minutes as seconds, centered, so 15 =30 mins total time
GNSS_sd_thresh=0.15 # 15cm how much variance do you want in the GNSS data when it is within the distance threshold?
change_thresh_15_min= 0.20 #m, so 20cm. This checks for a time-on-time change in flow of more than xxxcm to check for suddent shifts in the PT.
#typically, these occur during put in and takeout as suddent shifts while it is deployed are rare. 
offset_diff_thresh=0.01 #m, so 1cm. the the PT apparantly shift by more than a cm?

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
output_directory='D:/OneDrive -\ University of Massachusetts/calval/Toolbox/calval_toolbox/Taylor data 7 12/corrected drifts/'
max_PT_to_drift= 2.00 #km,  
SWOT_time= as.POSIXct('2021-09-02 23:04:37') # a dummy value far away from the drift
zone=18 #UTM zone 18N
# 
# fileID=list.files(drift_directory)[1]
source('D:/OneDrive -\ University of Massachusetts/calval/Toolbox/calval_toolbox/R code/ajdust_drift_via_PT.R')

#check for un-munged drift data
#pull filename before the .rds
raw_drifts=sub( "\\..*","", list.files(drift_directory))

#what raw drift data have not been munged
unmunged_drifts=setdiff(raw_drifts,sub( "\\..*","", list.files(output_directory)))
#run the drifts that are not yet munged

if(!identical(unmunged_drifts,character(0))){
dummy=lapply(paste0(drift_directory,unmunged_drifts),adjust_drift_via_PT,drift_directory=drift_directory,
       PT_key_file=PT_key_file,PT_directory=PT_directory,max_PT_to_drift=max_PT_to_drift,
       SWOT_time=SWOT_time,output_directory=output_directory,zone=zone)
}

#--------------------------

#calculate slopes and heights within nodes and reaches------
library(readxl)
metadatain= read_excel('D:/OneDrive -\ University of Massachusetts/calval/Toolbox/calval_toolbox/Taylor data 7 12/CTR_Aug21_nodes_reaches.xlsx')
this_river_reach_IDs= as.numeric(as.character(unique(metadatain$reach_id)))
this_river_node_IDs= as.numeric(as.character(unique(metadatain$node_id)))
utm_zone=18
buffer=500 #m
rivername='Connecticut'

SWORD_path='D:/OneDrive -\ University of Massachusetts/calval/Toolbox/calval_toolbox/Taylor data 7 12/na_sword_v11.nc'
drift_directory='D:/OneDrive -\ University of Massachusetts/calval/Toolbox/calval_toolbox/Taylor data 7 12/corrected drifts/'
PT_directory='D:/OneDrive -\ University of Massachusetts/calval/Toolbox/calval_toolbox/Taylor data 7 12/QAQC PTs/'
output_directory='D:/OneDrive -\ University of Massachusetts/calval/Toolbox/calval_toolbox/Taylor data 7 12/slopes and wses/'

source('D:/OneDrive -\ University of Massachusetts/calval/Toolbox/calval_toolbox/R code/calculate_slope_wse_fromdrift.R')

dummy=calculate_sope_wse_fromdrift(SWORD_path=SWORD_path,drift_directory=drift_directory,PT_directory=PT_directory,
                                   output_directory=output_directory,this_river_reach_IDs=this_river_reach_IDs,
                                   this_river_node_IDs=this_river_node_IDs,utm_zone=utm_zone, buffer=buffer,rivername=rivername)
#--------


test=readRDS('D:/OneDrive -\ University of Massachusetts/calval/Toolbox/calval_toolbox/Taylor data 7 12/slopes and wses/Connecticut_dritf_wse_slope.rds')
