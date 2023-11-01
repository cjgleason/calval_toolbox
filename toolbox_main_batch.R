#function that replaces the notebook with a batchable version

toolbox_main_batch=function(hubname,
                            rivername,
                            continent,
                            PT_key_file,
                            utm_zone,
                            reach_end_buffer,
                           reprocess_switch,
                           process_PTs,
                           process_airborne,
                           dist_thresh_offset,
                           time_thresh_offset,
                           GNSS_sd_thresh,
                           offset_sd_thresh,
                           change_thresh_15_min,
                           alongstream_error,
                           crossstream_error,
                           measurement_error, 
                            time_threshold_sec_match,
                            wse_threshold_m_match,
                            distance_threshold_m_match){

library(dplyr)
library(parallel)
library(stringr)
    
    options(dplyr.summarise.inform = FALSE)


setwd(paste0('/nas/cee-water/cjgleason/calval/Processed data/',hubname,'/'))
working_dir=(paste0('/nas/cee-water/cjgleason/calval/Processed data/',hubname,'/'))
domain_file=paste0(rivername,'_domain.csv')
paste0('/nas/cee-water/cjgleason/calval/Processed data/',hubname,'/')

#PT paths---------
PT_data_directory=paste0('/nas/cee-water/cjgleason/calval/xml_scripts/',hubname,'/Munged/')

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
    
    
    
 #create dataframes from drifts---------------------------------------------------------
#pull filename before the .csv
    print('starting drift munging')
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

    print(flagged_drift_output_directory)
    
#print(unmunged_drifts)
for (i in 1:length(unmunged_drifts)){
create_gnss_dataframe(unmunged_drifts[i],
                  gnss_drift_data_directory=GNSS_drift_data_directory,
                  output_directory=QA_QC_drift_output_directory,
                  keyfile= master_key,
                  rivername=  rivername,
                  naughty_bin_directory=flagged_drift_output_directory)}


#     cl=makeCluster(44)

    
#   dummy=parLapply(cl=cl,unmunged_drifts,create_gnss_dataframe,
#                   gnss_drift_data_directory=GNSS_drift_data_directory,
#                   output_directory=QA_QC_drift_output_directory)
#   stopCluster(cl)

print('ending drift munging')
#-------------------------------------------------   
    
    
#munge PTs if needed------
source('/nas/cee-water/cjgleason/calval_toolbox/R code/correct_PT_to_GNSS_multikey.R')
if (process_PTs==1){
    print('starting PT munging')
# dist_thresh=150 # 150m
# time_thresh= 15*60 #minutes as seconds, centered, so 15 =30 mins total time
# GNSS_sd_thresh=0.15 # 15cm how much variance do you want in the GNSS data when it is within the distance threshold?
# offset_sd_thresh=0.10 #m, so 10cm. the the PT apparantly shift by more than a cm?
# change_thresh_15_min=0.05#m- does it change more than 5cm in 15 minutes? that is a discontinuity in offset

#first, move .csv files with an 'L1' in them over to the PT_data_directory
munged_files= list.files(PT_data_directory,
                         recursive= TRUE)

PT_index=which(!is.na(do.call(rbind,lapply(munged_files,str_match,'PT_L1'))))
PT_files=munged_files[PT_index]
csv_index=which(!is.na(do.call(rbind,lapply(PT_files,str_match,'.csv'))))
raw_PT_files=PT_files[csv_index]

#open the key files   
read_keys=function(keyfile){
   this_key= read.csv(keyfile,stringsAsFactors=FALSE, na.strings = c("","NA","na","NaN", " "))%>%
    mutate(keyid=keyfile)%>%
    mutate(pt_serial=as.integer(PT_Serial))
    }
    
master_key= do.call(rbind,lapply(PT_key_file,read_keys))

# Key file QA/QC check #
key_col_names = c("PT_Serial", "Label", "Node_ID", "Reach_ID", "US_Reach_ID", 
                      "DS_Reach_ID", "Lat_WGS84", "Long_WGS84", "Install_method",
                      "Date_PT_Install", "Time_PT_Install_UTC", "Date_PT_Uninstall",
                      "Time_PT_Uninstall_UTC", "Date_GNSS_Install", "Time_GNSS_Install_Start_UTC", 
                      "Time_GNSS_Install_End_UTC", "GNSS_Offset_m", "Receiver_Install", "Original_Install_Log_File", 
                      "Final_Install_Log_File", "Date_GNSS_Uninstall", "Time_GNSS_Uninstall_Start_UTC", 
                      "Time_GNSS_Uninstall_End_UTC", "Receiver_Uninstall", "Original_Uninstall_Log_File", "Final_Uninstall_Log_File",
                      "keyid", "pt_serial")
hub_key_colnames = colnames(master_key)

key_check <- function(key_col_names, hub_key_colnames){
  if(length(key_col_names) == length(hub_key_colnames)) {
    print('Key file column names are of equal length')
  }
  if(length(which(is.na(match(key_col_names,hub_key_colnames))))!=0) {
    stop(paste('Key file has a missing/misnamed column',which(is.na(match(key_col_names,hub_key_colnames))),",",
               key_col_names[!key_col_names %in% hub_key_colnames],", please fix and reupload, and/or the Key file has an extra/misnamed column",
               which(is.na(match(hub_key_colnames,key_col_names))),",", hub_key_colnames[!hub_key_colnames %in% key_col_names],", please fix and reupload"))
  }
  if(length(unique(master_key$Node_ID))<=2){stop("Check that node ID in key file did not lose precision with scientific notation, if so, fix key and reupload.")}
    else{print("Key file passes QA/QC checks")}
}

key_check(key_col_names,hub_key_colnames)
    
    
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
# print(in_key_unprocessed)

    
for(thisone in in_key_unprocessed){

        correct_pt_to_gnss_multikey(thisone,
                  master_key=master_key,
                  dist_thresh=dist_thresh_offset,
                  time_thresh=time_thresh_offset,
                  pt_data_directory=PT_data_directory,
                  gnss_drift_data_directory=QA_QC_drift_output_directory,
                  QA_QC_pt_output_directory=QA_QC_PT_output_directory,
                  flagged_pt_output_directory=flagged_PT_output_directory,
                  gnss_sd_thresh=GNSS_sd_thresh,
                  offset_sd_thresh=offset_sd_thresh,
                  change_thresh_15_min=change_thresh_15_min) 
}
    
    
    
#       cl=makeCluster(40,type='FORK')
#   dummy=parLapply(cl, in_key_unprocessed,correct_pt_to_gnss_multikey,
#                    master_key=master_key,
#                   dist_thresh=dist_thresh_offset,
#                   time_thresh=time_thresh_offset,
#                   pt_data_directory=PT_data_directory,
#                   gnss_drift_data_directory=QA_QC_drift_output_directory,
#                   QA_QC_pt_output_directory=QA_QC_PT_output_directory,
#                   flagged_pt_output_directory=flagged_PT_output_directory,
#                   gnss_sd_thresh=GNSS_sd_thresh,
#                   offset_sd_thresh=offset_sd_thresh,
#                   change_thresh_15_min=change_thresh_15_min) 
#   stopCluster(cl)
    
    
    print('ending PT processing')
    
    }#end if process PT
 
    
    
  #calculate slopes and heights from drifts within nodes and reaches------
print('starting drift dataframe creation')
SWORD_reach= read.csv(domain_file)
this_river_reach_IDs= as.numeric(unique(SWORD_reach$Reach_ID[!is.na(SWORD_reach$Reach_ID)]))
this_river_node_IDs= as.numeric(unique(SWORD_reach$Node_ID[!is.na(SWORD_reach$Node_ID)]))


source('/nas/cee-water/cjgleason/calval_toolbox/R code/calculate_slope_wse_fromdrift.R')


dummy=calculate_slope_wse_fromdrift(SWORD_path=SWORD_path,
                                    drift_directory=QA_QC_drift_output_directory,
                                    PT_directory=PT_directory,
                                    output_directory=paste0('/nas/cee-water/cjgleason/calval/Processed data/',hubname,'/',reachnode_output_directory),
                                    this_river_reach_ids=this_river_reach_IDs,
                                    this_river_node_ids=this_river_node_IDs,
                                    utm_zone=utm_zone, 
                                    buffer=reach_end_buffer,
                                    rivername=rivername,
                                    reprocess_switch=reprocess_switch,
                                    core_count=core_count)
    
    
print('ending drift dataframe creation')

#end calculate slopes and heights from drifts
    
    
    #calculate slopes and heights from PTs within nodes and reaches----
if (process_PTs==1){
    
    print('starting pt dataframe creation')
PT_files=paste0(QA_QC_PT_output_directory,list.files(QA_QC_PT_output_directory))
SWORD_reach= read.csv(domain_file)
this_river_reach_IDs= as.numeric(as.character(unique(SWORD_reach$Reach_ID)))

 

# alongstream_error= 0.0001*200 #m error we get from the downstream slope of a reach in a node. This placeholder is a 1e-4 slope over a 200m node
# crossstream_error= 0.005 #m error we get from PT not representing cross stream superelevation/noise in a node
# measurement_error= 0.001 #m error we get from PT measurement itself 
source('/nas/cee-water/cjgleason/calval_toolbox/R code/calculate_slope_wse_fromPT.R')

read_keys=function(keyfile){
   this_key= read.csv(keyfile,stringsAsFactors=FALSE, na.strings = c("","NA","na","NaN", " "))%>%
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
print('ending pt dataframe creation')     

}
#end calculates slopes and heights from PTs-----------------------------
    
    
    
    #define what drift goes with what SWOT overpass--------------------
#SWOT_L2_HR_RiverSP_<FileIdentifier>_<CycleID>_<PassID>_<ContinentID>_<RangeBeginningDateTime>_<RangeEndingDateTime>_<CRID>_<ProductCounter>.<extension> 

if (process_PTs ==1){
    print('starting matching')
passfile=paste0('/nas/cee-water/cjgleason/calval/Processed data/riversp_list_clean_',rivername,'_20230726.txt')

 
passnames=read.delim(passfile,header=F)$V1
 
  
# time_threshold_sec= 120*60 #two hour
# wse_threshold_m=0.05 #within 5cm
# distance_threshold_m =200 #within 200m

if (!dir.exists(paste0('Matched_drifts/','processed_',str_replace_all(as.character(Sys.Date()),'\\-','_')))){
    dir.create(paste0('Matched_drifts/','processed_',str_replace_all(as.character(Sys.Date()),'\\-','_')))}


matched_output_directory= paste0('Matched_drifts/','processed_',str_replace_all(as.character(Sys.Date()),'\\-','_'),'/')
munged_drift_directory= QA_QC_drift_output_directory
munged_pt_directory=paste0(working_dir,QA_QC_PT_output_directory)

source('/nas/cee-water/cjgleason/calval_toolbox/R code/select_appropriate_drift.R')

read_keys=function(keyfile){
   this_key= read.csv(keyfile,stringsAsFactors=FALSE, na.strings = c("","NA","na","NaN", " "))%>%
    mutate(keyid=keyfile)%>%
    mutate(pt_serial=as.integer(PT_Serial))
    }
    
master_key= do.call(rbind,lapply(PT_key_file,read_keys))
  
for (i in 1:length(passnames)){
  #  print(passnames[i])
   select_appropriate_drift(passnames[i],
                               drift_node_directory= paste0(reachnode_output_directory,'/node/'),
                               munged_drift_directory=munged_drift_directory,
                               matched_output_directory=matched_output_directory,
                               munged_pt_directory=munged_pt_directory,
                               time_threshold_sec=time_threshold_sec_match,
                               wse_threshold_m= wse_threshold_m_match,
                               distance_threshold_m=distance_threshold_m_match,
                               keyfile=master_key,
                               rivername=rivername)
    
    }

  #too memory intensive to parallize. Need to properly paralellize via rslurm to send across the cluster 
    #rahter than a single node like this
#    cl=makeCluster(2,  type = "FORK")
# dummy=parLapply(cl, passnames,select_appropriate_drift,
#                                drift_node_directory= paste0(reachnode_output_directory,'/node/'),
#                                munged_drift_directory=munged_drift_directory,
#                                matched_output_directory=matched_output_directory,
#                                munged_pt_directory=munged_pt_directory,
#                                time_threshold_sec=time_threshold_sec,
#                                wse_threshold_m= wse_threshold_m,
#                                distance_threshold_m=distance_threshold_m,
#                                keyfile=master_key,
#                                rivername=rivername)

#  stopCluster(cl)
    print('ending matching')
    }
    
# end matching-----------------------------
    
    
    #calcluate areas from images------------------
if (process_airborne ==1){

    print('starting area dataframes')
a=Sys.time()

library(parallel)
library(raster)
source('/nas/cee-water/cjgleason/calval_toolbox/R code/calculate_area_from_imagery.R')

scale_maxwidth = 3
#path of input image 
#image = '/nas/cee-water/cjgleason/calval/Processed data/Imagery/Input/BinaryMap.tif'

    #make this use a rivername code
imagelist=as.list(list.files(image_directory,full.names=TRUE))
    
 #  print(imagelist)
    
#make this point to a hub
dir_output =paste0('/nas/cee-water/cjgleason/calval/Processed data/',hubname,'/Area/')

    image_reaches= unique(read.csv(domain_file)$Reach_ID)
    image_nodes=unique(read.csv(domain_file)$Node_ID)
    

# calculate_area_from_imagery(image=imagelist[1],
#                  this_river_reach_ids=image_reaches,
#                  this_river_node_ids=image_nodes,
#                  rivercode=rivername,
#                  utm_zone=utm_zone,
#                  scale_maxwidth=scale_maxwidth, 
#                  SWORD_path=SWORD_path)

cl=makeCluster(47, type ='FORK')

dummy=parLapply(cl,  imagelist,calculate_area_from_imagery,
                      this_river_reach_ids=image_reaches,
                 this_river_node_ids=image_nodes,
                 rivercode=rivername,
                 utm_zone=utm_zone,
                 scale_maxwidth=scale_maxwidth, 
                 SWORD_path=SWORD_path)

stopCluster(cl)
    
 print(Sys.time()-a)#calcluate areas from images----

    print('ending area dataframes')
 }

    
    
    
    
    
    
    
    
    
    
    
    
    
    }