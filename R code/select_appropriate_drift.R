#matches drifts to SWOT when SWOT and the drift are not coincident.
#in fact, first checks for coincidence


select_appropriate_drift=function(passname,time_threshold_sec,wse_threshold_m,distance_threshold_m,keyfile,
                                  drift_node_directory,matched_output_directory,munged_drift_directory,
                                  flyby_pt_directory,rivername){

# hubname='Brown'
# rivername='NS'
# continent='na'
# PT_key_file= 'SWOTCalVal_NS_KEY_20230525_20230613.csv' #WM
# utm_zone=13 #NS= 13
# setwd(paste0('/nas/cee-water/cjgleason/calval/Processed data/',hubname,'/'))
# working_dir=(paste0('/nas/cee-water/cjgleason/calval/Processed data/',hubname,'/'))
# 
#  
# passfile=paste0('/nas/cee-water/cjgleason/calval/Processed data/riversp_list_clean_',rivername,'_20240201.txt')
#     passnames=read.delim(passfile,header=F)$V1
#     time_threshold_sec= 120*60 #two hour
#     wse_threshold_m=0.05 #within 5cm
#     distance_threshold_m =200 #within 200m
#    
#     drift_node_directory='/nas/cee-water/cjgleason/calval/Processed data/Brown/Data frames/reprocessed_2024_01_31/node/'
#     matched_output_directory= '/nas/cee-water/cjgleason/calval/Processed data/Brown/Matched_drifts/processed_2024_02_01/'
#     munged_drift_directory=paste0('/nas/cee-water/cjgleason/calval/Processed data/Brown/Munged drifts/reprocessed_2024_02_01/')
#     flyby_pt_directory=paste0('/nas/cee-water/cjgleason/calval/Processed data/Brown/Flyby PT/reprocessed_2024_02_01/')
#     '/nas/cee-water/cjgleason/calval/Processed data/Brown/Data frames/reprocessed_2024_01_31/'
#   
#       read_keys=function(keyfile){
#         this_key= read.csv(keyfile,stringsAsFactors=FALSE, na.strings = c("","NA","na","NaN", " "))%>%
#           mutate(keyid=keyfile)%>%
#           mutate(pt_serial=as.integer(PT_Serial))
#       }
#     
#    keyfile= do.call(rbind,lapply(PT_key_file,read_keys))
#   
#    passname=passnames
#    print(passname)
   
    library(dplyr)
    library(fuzzyjoin)
    library(stringr)
   
    

   
    time1=substr(passname,nchar(passname)-30,nchar(passname)-16)
    time2=substr(passname,nchar(passname)-14,nchar(passname))
    
    #average the start and end dates of the granule.
    #date first, then numeric
    swot_time_UTC= as.numeric(mean(c(as.POSIXct(time1,format="%Y%m%dT%H%M%S"),as.POSIXct(time2,format="%Y%m%dT%H%M%S"))))

    
#FIRST CHECK- if time matches, use the time matched dirft
  #read in node levels from drift
  drift_node_index=which(!is.na(str_match(list.files(drift_node_directory),'drift_node_wse')))
  drift_node_file=list.files(drift_node_directory,full.names=TRUE)[drift_node_index]

  
  drift_nodes= read.csv(drift_node_file)%>%
    mutate(time=as.numeric(as.POSIXct(time_UTC,format= "%Y-%m-%d %H:%M:%S")))%>% 
    # mutate(time=as.numeric(as.POSIXct(time_UTC)))%>%#beacuse it is a .csv and not .rds, it loses its datetime
    mutate(time_diff_to_swot_drift_sec=abs(time-swot_time_UTC))
  
  keep_index= which(drift_nodes$time_diff_to_swot_drift_sec<=time_threshold_sec)
  remove_index =which(drift_nodes$time_diff_to_swot_drift_sec>time_threshold_sec)
  
  direct_match_drift=drift_nodes[keep_index,]%>%
    mutate(swot_time_UTC=swot_time_UTC)%>%
    mutate(swot_passid=passname)%>%
    transmute(node_id=as.character(format(node_id,scientific=FALSE)),
              drift_id=drift_id,time_diff_to_swot_drift_sec,swot_time_UTC,swot_passid)
  #write it to file
  if (nrow(direct_match_drift)>0){
    write.csv(direct_match_drift,paste0(matched_output_directory,passname,'direct',
                                        str_replace_all(as.character(Sys.Date()),'\\-','_'),'_',rivername,'.csv'),row.names=FALSE)
  }


    
  if (nrow(direct_match_drift)==nrow(drift_nodes)){
    break
    #this means they are all within some time window of swot
  }
  
  indirect_drift= drift_nodes[remove_index,]%>%
    mutate(wse=mean_node_drift_wse_m)
    

   
  #get the 1hz data for those left behind
  drift_1hz=do.call(rbind,lapply(unique(indirect_drift$drift_id) ,read.csv))%>%
    mutate(gnss_time_UTC=as.numeric(as.POSIXct(gnss_time_UTC)))%>%
    # mutate(gnss_time_UTC=as.numeric(as.POSIXct(gnss_time_UTC,format= "%Y-%m-%d %H:%M:%S")))%>%#csv read scrubs date
    mutate(Lon=gnss_Lon,Lat=gnss_Lat)
    
  
  #pull pt levels at swot time-----------
  pt_at_swot_time= do.call(rbind,lapply(list.files(flyby_pt_directory,full.names=TRUE), read.csv))%>%
    mutate(pt_time_UTC=as.numeric(as.POSIXct(pt_time_UTC)))%>%#csv read strips the datetime
    mutate(node_id=Node_ID)%>%
    group_by(node_id)%>%
    mutate(time_diff_to_swot_pt_sec=abs(pt_time_UTC-swot_time_UTC))%>%
    filter(time_diff_to_swot_pt_sec==min(time_diff_to_swot_pt_sec))%>%
    ungroup()%>%
    dplyr::select(-driftID_install,-driftID_uninstall)%>%#this was the drift used to correct it, but that is confusing here
    mutate(Lat=pt_lat,Lon=pt_lon) %>%
    filter(time_diff_to_swot_pt_sec<=time_threshold_sec)
    
    
    if(nrow(pt_at_swot_time)==0){
    write.csv('there are no appropriate drifts for this pass',paste0(matched_output_directory,passname,'none',
                                        str_replace_all(as.character(Sys.Date()),'\\-','_'),'_',rivername,'.csv'),row.names=FALSE)
    
        return(NA)
    }
  
  #compare drift node levels with pt levels
  #do a difference join based lat/lon. Slow.
  
  #read in key df first
  key_df=keyfile%>%
    dplyr::select(PT_Serial,Node_ID,Reach_ID)%>%
    transmute(pt_serial=PT_Serial,node_id=Node_ID,reach_id=Reach_ID)%>%
    filter(!is.na(pt_serial))
  
  
  drift_pt_join_df= geo_left_join(drift_1hz,pt_at_swot_time, by=c('Lon','Lat'),unit='km',method='haversine',
                                  distance_col='distance_km') %>% #this is a nearest neighbor join
    mutate(wse_difference= gnss_wse - pt_with_flyby_wse_m)%>%
    left_join(key_df,by='node_id')%>%
    group_by(pt_serial.x,node_id,drift_id) %>%
    filter(distance_km<distance_threshold_m/1000)%>%
    #here, we have now found all drifts within a threshold of the pts. Since all of these were NOT collected close enough to 
    #swot's overpass, we do not need to time match
    
    #filtering now by level within a specified distance will give us the ability to find all 'good' matches.
    filter(abs(wse_difference)<wse_threshold_m)%>%
    #sweet. Now we've got e.g. all drifts within 200m of a pt within 5cm of a pt level. grouping by pts did the wse error by pt
    #now we will summarize those per pt errors within each node and drift
    group_by(node_id,drift_id) %>%
    transmute(drift_pt_dist_km_bar=distance_km,wse_difference_m=wse_difference,swot_passid=passname,
              swot_time_UTC=swot_time_UTC)%>%
    summarize(drift_pt_dist_km_bar=mean(drift_pt_dist_km_bar),
              wse_difference_m_bar=mean(wse_difference_m),
              wse_difference_m_sd=sd(wse_difference_m),
              swot_passid=first(swot_passid),
              swot_time_UTC=first(swot_time_UTC))%>%
    ungroup()%>%
    mutate(node_id=as.character(node_id))%>%
    mutate(swot_time_UTC=as.POSIXct(swot_time_UTC,origin='1970-01-01 00:00:00 UTC'))
  
  #write to file
  if(nrow(drift_pt_join_df)==0){
    write.csv('there are no appropriate drifts for this pass',paste0(matched_output_directory,passname,'none',
                                        str_replace_all(as.character(Sys.Date()),'\\-','_'),'_',rivername,'.csv'),row.names=FALSE)
  }else{
  write.csv(drift_pt_join_df,paste0(matched_output_directory,passname,'matched',
                                        str_replace_all(as.character(Sys.Date()),'\\-','_'),'_',rivername,'.csv'),row.names=FALSE)
    }
  
  
   rm(list = ls())
    gc()

}