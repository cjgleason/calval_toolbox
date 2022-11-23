library(dplyr)

#setwd('D:/OneDrive -\ University of Massachusetts/calval/Toolbox/calval_toolbox/')
#setwd('C:/Users/confluence/Desktop/calval_toolbox/')
setwd('C:/Users/colin/Documents/GitHub/calval_toolbox/')
#PT paths------------------------------------------
PT_data_directory='Willamette/Willamette raw PTs/'
QA_QC_PT_output_directory='Willamette/Willamette munged PTs/'
flagged_PT_output_directory='Willamette/Willamette flagged PTs/'
PT_key_file='Willamette/WM_Key.csv'
#--------------------------------------------------

#drift paths------------------------------------------
GNSS_drift_data_directory='Willamette/Willamette Raw Drifts/'
QA_QC_drift_output_directory='Willamette/Willamette munged drifts/'
flagged_drift_output_directory='Willamette/Willamette flagged drifts/'
#--------------------------------------------------

#sword paths----------------------------------------
SWORD_path='na_sword_v11.nc'
munged_drift_directory='Willamette/Willamette munged drifts/'
PT_directory='Willamette/Willamette munged PTs/'
output_directory='Willamette/SWORD products/'
#---------------------------------------------------

###################################################################

#create dataframes from drifts---------------------------------------------------------
#check for un-munged PT data
#pull filename before the .csv
raw_GNSS=sub( "\\..*","", list.files(GNSS_drift_data_directory))
#pull filename before the second _
QA_QC_drifts=sub( "\\..*","",list.files(QA_QC_drift_output_directory))
flagged_drifts=sub("\\..*","",list.files(flagged_drift_output_directory))
#what raw drift data have not been munged
unmunged_drifts=setdiff(raw_GNSS,c(flagged_drifts,QA_QC_drifts))

if(!identical(unmunged_drifts,character(0))){
  source('R code/create_GNSS_dataframe.R')
  
  dummy=lapply(unmunged_drifts,create_GNSS_dataframe,
               GNSS_drift_data_directory=GNSS_drift_data_directory,output_directory=QA_QC_drift_output_directory)
}

#-------------------------------------------------

# munge PTs if needed------

dist_thresh=150 # 150m
time_thresh= 15*60 #minutes as seconds, centered, so 15 =30 mins total time
GNSS_sd_thresh=0.15 # 15cm how much variance do you want in the GNSS data when it is within the distance threshold?
offset_sd_thresh=0.10 #m, so 10cm. the the PT apparantly shift by more than a cm?
change_thresh_15_min=0.05#m- does it change more than 5cm in 15 minutes? that is a discontinuity in offset

#check for un-munged PT data
#pull filename before the .csv
raw_PT=sub( "\\..*","", list.files(PT_data_directory))
#pull filename before the second _
QA_QC_PTs=sub( "\\..*","",list.files(QA_QC_PT_output_directory))
flagged_PTs=sub( "\\..*","",list.files(flagged_PT_output_directory))
#what raw PT data have not been munged
unmunged_PTs=setdiff(raw_PT,c(flagged_PTs,QA_QC_PTs))
#run the PTs that are not yet munged


if(!identical(unmunged_PTs,character(0))){
  source('R code/correct_PT_to_GNSS.R')
  
  dummy=lapply(unmunged_PTs,correct_PT_to_GNSS,PT_key_file=PT_key_file,dist_thresh=dist_thresh,
               time_thresh=time_thresh,PT_data_directory=PT_data_directory,GNSS_drift_data_directory=QA_QC_drift_output_directory,
               QA_QC_PT_output_directory=QA_QC_PT_output_directory,flagged_PT_directory=flagged_PT_directory,
               GNSS_sd_thresh=GNSS_sd_thresh,offset_diff_thresh=offset_diff_thresh,change_thresh_15_min=change_thresh_15_min)
}

#-----------------------------

#calculate slopes and heights from drifts within nodes and reaches------

SWORD_reach= read.csv('Willamette/Willamette nodes.csv')
this_river_reach_IDs= as.numeric(as.character(unique(SWORD_reach$reach_id)))
this_river_node_IDs= as.numeric(as.character(unique(SWORD_reach$node_id)))
utm_zone=10
buffer=500 #m, 'extends' the reach
rivername='Willamette'

source('R code/calculate_slope_wse_fromdrift.R')

dummy=calculate_sope_wse_fromdrift(SWORD_path=SWORD_path,drift_directory=munged_drift_directory,PT_directory=PT_directory,
                                   output_directory=output_directory,this_river_reach_IDs=this_river_reach_IDs,
                                   this_river_node_IDs=this_river_node_IDs,utm_zone=utm_zone, buffer=buffer,rivername=rivername)
#-----------------------------

#calculate slopes and heights from PTs within nodes and reaches----
keyfile='Willamette/WM_Key.csv'
PT_files=paste0('Willamette/Willamette munged PTs/',list.files('Willamette/Willamette munged PTs/'))
SWORD_path='na_sword_v11.nc'
SWORD_reach= read.csv('Willamette/Willamette nodes.csv')
this_river_reach_IDs= as.numeric(as.character(unique(SWORD_reach$reach_id)))
source('R code/calculate_slope_wse_fromPT.R')

dummy=calculate_slope_wse_fromPT(keyfile=keyfile,PT_files=PT_files,SWORD_path=SWORD_path,
                                 SWORD_reach=SWORD_reach,this_river_reach_IDs=this_river_reach_IDs)
#-----------------------------

#define what drift goes with what SWOT overpass--------------------
passname='fake swot pass ID'
SWOT_time_UTC=as.POSIXct('2022-07-26 21:44:47')
time_threshold_sec= 120*60 #two hour
wse_threshold_m=0.05 #within 5cm
distance_threshold_m =200 #within 200m
keyfile='Willamette/WM_Key.csv'
source('R code/select_appropriate_drift.R')

dummy=select_appropriate_drift(passname=passname,SWOT_time_UTC=SWOT_time_UTC,time_threshold_sec=time_threshold_sec,
                               wse_threshold_m= wse_threshold_m,distance_threshold_m=distance_threshold_m,keyfile=keyfile)
#-----------------------------








# #pull lidar heights from lidar rasters (very slow)----
# SWORD_path='D:/OneDrive -\ University of Massachusetts/calval/Toolbox/calval_toolbox/Taylor data 7 12/na_sword_v11.nc'
# library(readxl)
# metadatain= read_excel('D:/OneDrive -\ University of Massachusetts/calval/Toolbox/calval_toolbox/Taylor data 7 12/CTR_Aug21_nodes_reaches.xlsx')
# this_river_reach_IDs= as.numeric(as.character(unique(metadatain$reach_id)))
# this_river_node_IDs= as.numeric(as.character(unique(metadatain$node_id)))
# utm_zone=18
# lidar_date= '08-30-2021 12:00:00'
# raster_path='D:/OneDrive -\ University of Massachusetts/calval/Toolbox/calval_toolbox/Taylor data 7 12/Rasters/'
# output_path= 'D:/OneDrive -\ University of Massachusetts/calval/Toolbox/calval_toolbox/Taylor data 7 12/lidar wse products/'
# river_name= 'Connecticut'
# 
# 
# 
# source('D:/OneDrive -\ University of Massachusetts/calval/Toolbox/calval_toolbox/R code/sample_lidar_at_SWOT.R')
# 
# dummy=sample_lidar_at_SWOT(SWORD_path=SWORD_path,this_river_node_IDs= this_river_node_IDs,this_river_reach_IDs= this_river_reach_IDs,
#                            utm_zone=utm_zone,lidar_date=lidar_date,raster_path=raster_path,output_path=output_path,river_name=river_name)
# #-----------------------------




