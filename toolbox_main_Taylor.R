### main toolbox workflow ###

library(dplyr)
library(ncdf4)
library(stringr)
library(sf)
library(tidyr)
library(fuzzyjoin)
library(geodist)
library(bayesbio)
library(purrr)
library(ggplot2)


setwd(paste0('/nas/cee-water/cjgleason/calval/Processed data/',hubname,'/Data frames/reprocessed_2023_10_31/node'))
working_dir=(paste0('/nas/cee-water/cjgleason/calval/Processed data/',hubname,'/Data frames/reprocessed_2023_10_31/node'))


#PT paths---------
PT_data_directory=paste0('/nas/cee-water/cjgleason/calval/xml_scripts/',hubname,'/Munged/')



setwd("C:/Users/throwley/OneDrive - University of Massachusetts/Calval/Toolbox_Testing/NS_test")

# List of CSV file names (replace with your file names)
csv_files <- list.files("C:/Users/throwley/OneDrive - University of Massachusetts/Calval/Toolbox_Testing/NS_test")

# Function to read a CSV file and return a data frame
read_csv_file <- function(file) {
  read.csv(file, header = TRUE)
}

# Use purrr::map_df to read and combine all CSV files into a single data frame
combined_data <- map_df(csv_files, read_csv_file, na.rm = TRUE)
combined_data <- combined_data[,-9]
combined_data <- combined_data[!is.na(combined_data[[3]]),]
# date = as.POSIXct(combined_data$pt_time_UTC, format = "%Y-%m-%d %H:%M:%S")
combined_data$pt_time_UTC = as.POSIXct(combined_data$pt_time_UTC, format = "%Y-%m-%d %H:%M:%S")

plot <- ggplot(combined_data, aes(x = pt_time_UTC, y = mean_node_pt_wse_m, group = pt_serial, color = factor(pt_serial))) +
  geom_line() +
  labs(title = "Time Series Plot by Group", x = "time_UTC", y = "WSE_m", color = "PTs") +
  scale_x_datetime(date_labels = "%Y-%m-%d", date_breaks = "2 days") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~reach_id, scales = "free_y", ncol = 1)
 
plot

#--------------------------------------------------
#drift paths------------------------------------------
GNSS_drift_data_directory=paste0('From Andy/',hubname,'_netCDFs/')
if(reprocess_switch==1){
  
  drift_string= paste0('Munged drifts/','reprocessed_',  str_replace_all(as.character(Sys.Date()),'\\-','_'))
  PT_string =paste0('Munged PT/','reprocessed_',  str_replace_all(as.character(Sys.Date()),'\\-','_'))
  reachnode_string= paste0('Data frames/','reprocessed_',  str_replace_all(as.character(Sys.Date()),'\\-','_'))
  node_string =paste0('Data frames/','reprocessed_',  str_replace_all(as.character(Sys.Date()),'\\-','_'),'/node')
  reach_string=paste0('Data frames/','reprocessed_',  str_replace_all(as.character(Sys.Date()),'\\-','_'),'/reach')
  flagged_PT_output_directory=paste0('Flagged PT/','reprocessed_',  str_replace_all(as.character(Sys.Date()),'\\-','_'))
  
  #check if we've already reprocessed today
  if (dir.exists(drift_string)){
    #if we have, then use that as the output and clear the files in the drift directory
    unlink(drift_string, recursive = TRUE)
    unlink(PT_string,recursive = TRUE)
    unlink(flagged_PT_output_directory,recursive = TRUE)
    dir.create(drift_string)
    dir.create(flagged_PT_output_directory)
    dir.create(PT_string)
    QA_QC_drift_output_directory=paste0(drift_string,'/')
    QA_QC_PT_output_directory=paste0(PT_string,'/')
    reachnode_output_directory=paste0(reachnode_string,'/')
    flagged_PT_output_directory=paste0(flagged_PT_output_directory,'/')
  } else {
    #if we haven't reprocessed today
    
    dir.create(flagged_PT_output_directory)
    dir.create(drift_string)
    dir.create(PT_string)
    dir.create(reachnode_string)
    dir.create(node_string)
    dir.create(reach_string)
    
    flagged_PT_output_directory=paste0(flagged_PT_output_directory,'/')
    QA_QC_drift_output_directory=paste0(drift_string,'/')
    reachnode_output_directory=paste0(reachnode_string,'/')
    QA_QC_PT_output_directory=paste0(PT_string,'/')
  }
  
} else { #we aren't reprocessing. find the most recent folders to use
  
  folderlist= list.files('Munged drifts',full.names = TRUE)
  
  foldertimes=file.info(folderlist)%>%
    mutate(mintime= Sys.time()-mtime) %>%
    filter(mintime== min(mintime)) 
  
  QA_QC_drift_output_directory=paste0(row.names(foldertimes),'/')  
  
  folderlist2= list.files('Data frames',full.names = TRUE)
  
  foldertimes2=file.info(folderlist2)%>%
    mutate(mintime= Sys.time()-mtime) %>%
    filter(mintime== min(mintime)) 
  
  reachnode_output_directory=paste0(row.names(foldertimes2),'/')
  
  folderlist3= list.files('Munged PT',full.names = TRUE)
  
  foldertimes3=file.info(folderlist3)%>%
    mutate(mintime= Sys.time()-mtime) %>%
    filter(mintime== min(mintime))  
  
  QA_QC_PT_output_directory=paste0(row.names(foldertimes3),'/') 
  
  folderlist4= list.files('Flagged PT',full.names = TRUE)
  
  foldertimes4=file.info(folderlist4)%>%
    mutate(mintime= Sys.time()-mtime) %>%
    filter(mintime== min(mintime))  
  
  flagged_PT_output_directory=paste0(row.names(foldertimes4),'/') 
}

flagged_drift_output_directory='Flagged drifts/'
#--------------------------------------------------

#sword paths----------------------------------------
SWORD_path=paste0('/nas/cee-water/cjgleason/calval/SWORD_15/netcdf/',continent,
                  '_sword_v15.nc')
#------------------------------

image_directory=paste0('/nas/cee-water/cjgleason/calval/cnes_watermasks/fromCNES_20230724/',rivername,'/extracteo/') 
print(image_directory)

############# Print Directories ###############################

QA_QC_drift_output_directory
QA_QC_PT_output_directory
reachnode_output_directory
flagged_PT_output_directory

############# Dataframes from drifts #########################

#pull filename before the .csv
source('/nas/cee-water/cjgleason/calval_toolbox/R code/create_GNSS_dataframe.R')
raw_GNSS=sub( "\\..*","", list.files(GNSS_drift_data_directory,recursive=TRUE))
raw_GNSS_river=which(!is.na(do.call(rbind,lapply(raw_GNSS,str_match,rivername))))
raw_GNSS=raw_GNSS[raw_GNSS_river]

#pull filename before the second _
QA_QC_drifts=sub( "\\..*","",list.files(QA_QC_drift_output_directory))
flagged_drifts=sub("\\..*","",list.files(flagged_drift_output_directory))
#what raw drift data have not been munged
unmunged_drifts=setdiff(raw_GNSS,c(flagged_drifts,QA_QC_drifts))

#open the key files   
if(rivername=='WK'){
  read_keys=function(keyfile){
    this_key= read.csv(keyfile,stringsAsFactors=FALSE)%>%
      mutate(keyid=keyfile)%>%
      mutate(pt_serial=as.integer(PT_Serial))
  }
  
  master_key= do.call(rbind,lapply(PT_key_file,read_keys))}else {master_key=NULL}

for (i in 1:length(unmunged_drifts)){
  create_gnss_dataframe(unmunged_drifts[i],
                        gnss_drift_data_directory=GNSS_drift_data_directory,
                        output_directory=QA_QC_drift_output_directory,
                        keyfile= master_key,
                        rivername=  rivername,
                        naughty_bin_directory=flagged_drift_output_directory )}

################# MUNGE PTs ##############################

source('/nas/cee-water/cjgleason/calval_toolbox/R code/correct_PT_to_GNSS_multikey.R')
if (process_PTs==1){
  dist_thresh=150 # 150m
  time_thresh= 15*60 #minutes as seconds, centered, so 15 =30 mins total time
  GNSS_sd_thresh=0.15 # 15cm how much variance do you want in the GNSS data when it is within the distance threshold?
  offset_sd_thresh=0.10 #m, so 10cm. the the PT apparantly shift by more than a cm?
  change_thresh_15_min=0.15#m- does it change more than 5cm in 15 minutes? that is a discontinuity in offset
  
  #first, move .csv files with an 'L1' in them over to the PT_data_directory
  munged_files= list.files(PT_data_directory,
                           recursive= TRUE)
  
  PT_index=which(!is.na(do.call(rbind,lapply(munged_files,str_match,'PT_L1'))))
  PT_files=munged_files[PT_index]
  csv_index=which(!is.na(do.call(rbind,lapply(PT_files,str_match,'.csv'))))
  raw_PT_files=PT_files[csv_index]
  
  #open the key files   
  read_keys=function(keyfile){
    this_key= read.csv(keyfile,stringsAsFactors=FALSE)%>%
      mutate(keyid=keyfile)%>%
      mutate(pt_serial=as.integer(PT_Serial))
  }
  
  master_key= do.call(rbind,lapply(PT_key_file,read_keys))
  
  
  #three key info here-
  #1 PTs in the key
  #2 unmunged PTs
  #3 QA QC PTs
  #4 flagged PTs
  
  #3 + 4 are processed files
  
  getit_processed=function(inputstring){   
    output=paste(strsplit(inputstring,'_')[[1]][1:8],collapse='_')
    output=sub("\\..*","",output)
  }
  
  getit_key =function(inputstring){   
    output=paste(c('SWOTCalVal',rivername,'PT','L1',inputstring),collapse='_')
    output=sub("\\..*","",output)
  }
  
  getit_negative=function(longstring, shortstrings){
    #search for a pattern between one and many strings with partial matches allows
    output=!any(str_detect(longstring,shortstrings)  )
  }
  
  getit_positive=function(longstring, shortstrings){
    #search for a pattern between one and many strings with partial matches allows
    output=any(str_detect(longstring,shortstrings))  
  }
  
  processed_files= do.call(rbind,lapply(c(list.files(QA_QC_PT_output_directory),list.files(flagged_PT_output_directory)),getit_processed))
  key_files=do.call(rbind,lapply(master_key$Label,getit_key))
  
  
  if (is.null(processed_files)){unprocessed_files=raw_PT_files}else{
    unprocessed_files=raw_PT_files[do.call(rbind,lapply(raw_PT_files,getit_negative,processed_files))]}
  in_key_unprocessed=unprocessed_files[do.call(rbind,lapply(unprocessed_files,getit_positive,key_files))]
  
  # print('raw')
  # print(raw_PT_files)
  # print('processed')
  # print(processed_files)
  # print('unprocessed')
  # print(unprocessed_files)
  # print('unprocessed in key')
  
  for(thisone in in_key_unprocessed){
    
    correct_pt_to_gnss_multikey(thisone,
                                master_key=master_key,
                                dist_thresh=dist_thresh,
                                time_thresh=time_thresh,
                                pt_data_directory=PT_data_directory,
                                gnss_drift_data_directory=QA_QC_drift_output_directory,
                                QA_QC_pt_output_directory=QA_QC_PT_output_directory,
                                flagged_pt_output_directory=flagged_PT_output_directory,
                                gnss_sd_thresh=GNSS_sd_thresh,
                                offset_sd_thresh=offset_sd_thresh,
                                change_thresh_15_min=change_thresh_15_min) 
    
    
  }
  
  
  #       cl=makeCluster(20,type='FORK')
  #   dummy=parLapply(cl, in_key_unprocessed,correct_pt_to_gnss_multikey,
  #                    master_key=master_key,
  # dist_thresh=dist_thresh,
  # time_thresh=time_thresh,
  # pt_data_directory=PT_data_directory,
  # gnss_drift_data_directory=QA_QC_drift_output_directory,
  # QA_QC_pt_output_directory=QA_QC_PT_output_directory,
  # flagged_pt_output_directory=flagged_PT_output_directory,
  # gnss_sd_thresh=GNSS_sd_thresh,
  # offset_sd_thresh=offset_sd_thresh,
  # change_thresh_15_min=change_thresh_15_min) 
  #   stopCluster(cl)
  
  
}#end if process PT

###################### CALC SLOPE FROM DRIFT #######################

#calculate slopes and heights from drifts within nodes and reaches------
SWORD_reach= read.csv(domain_file)
this_river_reach_IDs= as.numeric(unique(SWORD_reach$Reach_ID[!is.na(SWORD_reach$Reach_ID)]))
this_river_node_IDs= as.numeric(unique(SWORD_reach$Node_ID[!is.na(SWORD_reach$Node_ID)]))


source('/nas/cee-water/cjgleason/calval_toolbox/R code/calculate_slope_wse_fromdrift.R')


dummy=calculate_slope_wse_fromdrift(SWORD_path=SWORD_path,
                                    drift_directory=QA_QC_drift_output_directory,
                                    PT_directory=PT_directory,
                                    output_directory=reachnode_output_directory,
                                    this_river_reach_ids=this_river_reach_IDs,
                                    this_river_node_ids=this_river_node_IDs,
                                    utm_zone=utm_zone, 
                                    buffer=reach_end_buffer,
                                    rivername=rivername,
                                    reprocess_switch=reprocess_switch,
                                    core_count=core_count)

######################## CALC SLOPE FROM PTs ###########################

#calculate slopes and heights from PTs within nodes and reaches----
if (process_PTs==1){
  PT_files=paste0(QA_QC_PT_output_directory,list.files(QA_QC_PT_output_directory))
  SWORD_reach= read.csv(domain_file)
  this_river_reach_IDs= as.numeric(as.character(unique(SWORD_reach$Reach_ID)))
  this_river_node_IDs= as.numeric(unique(SWORD_reach$Node_ID[!is.na(SWORD_reach$Node_ID)]))
  
  alongstream_error= 0.0001*200 #m error we get from the downstream slope of a reach in a node. This placeholder is a 1e-4 slope over a 200m node
  crossstream_error= 0.005 #m error we get from PT not representing cross stream superelevation/noise in a node
  measurement_error= 0.001 #m error we get from PT measurement itself 
  source('/nas/cee-water/cjgleason/calval_toolbox/R code/calculate_slope_wse_fromPT.R')
  
  read_keys=function(keyfile){
    this_key= read.csv(keyfile,stringsAsFactors=FALSE)%>%
      mutate(keyid=keyfile)%>%
      mutate(pt_serial=as.integer(PT_Serial))
  }
  
  master_key= do.call(rbind,lapply(PT_key_file,read_keys))
  
  dummy=calculate_slope_wse_fromPT(keyfile=master_key,
                                   pt_files=PT_files,
                                   SWORD_path=SWORD_path,
                                   SWORD_reach=SWORD_reach,
                                   this_river_reach_ids=this_river_reach_IDs,
                                   this_river_node_ids=this_river_node_IDs,
                                   rivername=rivername,
                                   output_directory= reachnode_output_directory,
                                   alongstream_error=alongstream_error,
                                   crossstream_error=crossstream_error,
                                   measurement_error=measurement_error)
}
