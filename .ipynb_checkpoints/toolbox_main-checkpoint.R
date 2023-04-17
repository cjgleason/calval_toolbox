library(dplyr)
library(parallel)

setwd('/nas/cee-water/cjgleason/calval/Processed data/UNC/')


#PT paths---------
PT_data_directory='Raw PT/'
QA_QC_PT_output_directory='Munged PT/'
flagged_PT_output_directory='Flagged PT/'
PT_key_file='Willamette/WM_Key.csv'
#--------------------------------------------------

#drift paths------------------------------------------
GNSS_drift_data_directory='Raw drifts/'
QA_QC_drift_output_directory='Munged drifts/'
flagged_drift_output_directory='Flagged drifts/'
#--------------------------------------------------

#sword paths----------------------------------------
SWORD_path='/nas/cee-water/cjgleason/SWORDv14/oc_sword_v14.nc'
munged_drift_directory='Munged drifts/'
PT_directory='Munged PTs/'
output_directory='Data frames/'
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
  cl=makeCluster(16)
  
  dummy=parLapply(cl=cl,unmunged_drifts,create_gnss_dataframe,
                  gnss_drift_data_directory=GNSS_drift_data_directory,output_directory=QA_QC_drift_output_directory)
  stopCluster(cl)
}

#-------------------------------------------------


#munge PTs if needed------
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
  
  cl=makeCluster(16)
  
  dummy=parLapply(cl,unmunged_PTs,correct_pt_to_gnss,
                  pt_key_file=PT_key_file,
                  dist_thresh=dist_thresh,
                  time_thresh=time_thresh,
                  pt_data_directory=PT_data_directory,
                  gnss_drift_data_directory=QA_QC_drift_output_directory,
                  QA_QC_pt_output_directory=QA_QC_PT_output_directory,
                  flagged_pt_output_directory=flagged_PT_output_directory,
                  gnss_sd_thresh=GNSS_sd_thresh,
                  offset_sd_thresh=offset_sd_thresh,
                  change_thresh_15_min=change_thresh_15_min)
  
  stopCluster(cl)
}

#-----------------------------

#calculate slopes and heights from drifts within nodes and reaches------
SWORD_reach= read.csv('Willamette/Willamette nodes.csv')
this_river_reach_IDs= as.numeric(as.character(unique(SWORD_reach$reach_id)))
this_river_node_IDs= as.numeric(as.character(unique(SWORD_reach$node_id)))
utm_zone=10
buffer=50 #m, 'extends' the reach
rivername='Willamette'
photo_path= 'Willamette/Watermask/Demo_output/Shapefile/'

source('R code/calculate_slope_wse_fromdrift.R')

dummy=calculate_slope_wse_fromdrift(SWORD_path=SWORD_path,
                                    drift_directory=munged_drift_directory,
                                    PT_directory=PT_directory,
                                    output_directory=output_directory,
                                    this_river_reach_ids=this_river_reach_IDs,
                                    this_river_node_ids=this_river_node_IDs,
                                    utm_zone=utm_zone, 
                                    buffer=buffer,
                                    rivername=rivername,
                                    photo_path=photo_path)
#-----------------------------

#calculate slopes and heights from PTs within nodes and reaches----
keyfile='Willamette/WM_Key.csv'
PT_files=paste0('Willamette/Willamette munged PTs/',list.files('Willamette/Willamette munged PTs/'))
SWORD_reach= read.csv('Willamette/Willamette nodes.csv')
this_river_reach_IDs= as.numeric(as.character(unique(SWORD_reach$reach_id)))
alongstream_error= 0.0001*200 #m error we get from the downstream slope of a reach in a node. This placeholder is a 1e-4 slope over a 200m node
crossstream_error= 0.005 #m error we get from PT not representing cross stream superelevation/noise in a node
measurement_error= 0.001 #m error we get from PT measurement itself 
source('R code/calculate_slope_wse_fromPT.R')

dummy=calculate_slope_wse_fromPT(keyfile=keyfile,
                                 pt_files=PT_files,
                                 SWORD_path=SWORD_path,
                                 SWORD_reach=SWORD_reach,
                                 this_river_reach_ids=this_river_reach_IDs,
                                 alongstream_error=alongstream_error,
                                 crossstream_error=crossstream_error,
                                 measurement_error=measurement_error)
#-----------------------------

#define what drift goes with what SWOT overpass--------------------
passname='fake swot pass ID'
SWOT_time_UTC=as.POSIXct('2022-07-26 21:44:47')
time_threshold_sec= 120*60 #two hour
wse_threshold_m=0.05 #within 5cm
distance_threshold_m =200 #within 200m
keyfile='Willamette/WM_Key.csv'
source('R code/select_appropriate_drift.R')

dummy=select_appropriate_drift(passname=passname,
                               swot_time_UTC=SWOT_time_UTC,
                               time_threshold_sec=time_threshold_sec,
                               wse_threshold_m= wse_threshold_m,
                               distance_threshold_m=distance_threshold_m,
                               keyfile=keyfile)
#-----------------------------

#calcluate areas from images------------------
utm_zone = 10
#image acquisition time (based on imagery)
image_time = '11:00'
#scale max width (in cases of under/overestimated or centerline offset)
scale_maxwidth = 3

##other parameter setting
#Threshold for water mapping
water_index_threshold = 0.2
#Offset the threshold to calculate water area uncertainty
ThresholdOffset_4_uncertainty = 0.1

#path of input image 
Inputimagefile = '/nas/cee-water/cjgleason/calval/willamette_geotiffs/raw/WillametteROI/PHR_2022_07_29_Willamette_32610__shifted_to__WilaBing32610.tif'
#path of input SWORD data (netcdf)
SWORD_path='/nas/cee-water/cjgleason/SWORDv14/Reaches_Nodes/netcdf/na_sword_v11.nc'
#output dir
dir_output = '/nas/cee-water/cjgleason/calval_toolbox/Willamette/Watermask/'

#selected reach and nodes to be processed
this_river_reach_IDs <- c(78220000191,78220000181,78220000171)
this_river_node_IDs <- c(78220000190011,78220000180811,78220000180661,78220000180511,78220000180351,78220000180341,78220000180201,
                         78220000180191,78220000170571,78220000170401,78220000170281,78220000170161,78220000170011)


source('/nas/cee-water/cjgleason/calval_toolbox/R code/calculate_area_from_imagery.R')
calculate_area_from_imagery( Inputimagefile=Inputimagefile,
                             utm_zone=utm_zone,image_time=image_time,scale_maxwidth=scale_maxwidth,
                             water_index_threshold=water_index_threshold,
                             ThresholdOffset_4_uncertainty=ThresholdOffset_4_uncertainty,
                             SWORD_path=SWORD_path,
                             dir_output=dir_output,
                             this_river_reach_IDs=this_river_reach_IDs,
                             this_river_node_IDs=this_river_node_IDs)
#------00000000000000



