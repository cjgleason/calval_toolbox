
pt_files=list.files('/nas/cee-water/cjgleason/calval/Processed data/CU/Munged PT/processed_2023_05_11/',full.names=TRUE)
SWORD_reach= read.csv('/nas/cee-water/cjgleason/calval/Processed data/CU/SWOTCalVal_WM_KEY_20230326_20230509.csv')
this_river_reach_ids= as.numeric(as.character(unique(SWORD_reach$Reach_ID)))



alongstream_error= 0.0001*200 #m error we get from the downstream slope of a reach in a node. This placeholder is a 1e-4 slope over a 200m node
crossstream_error= 0.005 #m error we get from PT not representing cross stream superelevation/noise in a node
measurement_error= 0.001 #m error we get from PT measurement itself 
keyfile='/nas/cee-water/cjgleason/calval/Processed data/CU/SWOTCalVal_WM_KEY_20230326_20230509.csv'

  SWORD_path=paste0('/nas/cee-water/cjgleason/calval/SWORD_15/netcdf/','na',
                    '_sword_v15.nc')
 
  rivername='WM'
  output_directory= '/nas/cee-water/cjgleason/calval/Processed data/CU/Data frames/reprocessed_2023_05_10/'


  
  library(ncdf4)
  library(dplyr)
  #read in key file
  key_df=read.csv(keyfile)%>%
    transmute(pt_serial=PT_Serial,node_id=Node_ID,reach_id=as.character(Reach_ID),us_reach_id=as.character(US_Reach_ID),ds_reach_id=as.character(DS_Reach_ID))
  #read in SWORD
  
  SWORD_in=nc_open(SWORD_path,verbose=FALSE)
  reachids=ncvar_get(SWORD_in, 'reaches/reach_id',verbose=FALSE)
  reach_index= which(reachids %in% this_river_reach_ids)
  reach_length=ncvar_get(SWORD_in, 'reaches/reach_length',verbose=FALSE)[reach_index]
  
  reach_length_df=data.frame(reach_id=as.character(reachids[reach_index]),reach_length=reach_length)
  
  #read in pt files
  pt_df=do.call(rbind,lapply(pt_files,read.csv))%>%
    transmute(pt_serial,pt_time_UTC,pt_wse,pt_wse_sd)%>%
    left_join(key_df,by='pt_serial')
  
  #calculate node wse
  node_df=pt_df%>%
    group_by(node_id,pt_time_UTC)%>%
    summarise(mean_node_pt_wse_m=mean(pt_wse),mean_pt_wse_precision_m=alongstream_error+crossstream_error+measurement_error)%>% #JPL wants precision, not variance mean(pt_wse_sd)
    ungroup()%>%#based on grouping, it will repeat
    mutate(node_id=as.character(node_id))%>%
    distinct()#based on grouping, it will repeat
  
  #calculate reach wse
  reach_df=pt_df%>%
    group_by(reach_id,pt_time_UTC)%>%
    summarise(mean_reach_pt_wse_m=mean(pt_wse),mean_reach_pt_wse_precision_m=alongstream_error+crossstream_error+measurement_error)%>%# JPL wants precision, not variance mean(pt_wse_sd),
    ungroup()%>%#based on grouping, it will repeat
    mutate(reach_id=as.character(reach_id))%>%
    distinct()#based on grouping, it will repeat
  
    #calculate slope
  unique_reaches=unique(pt_df$reach_id)
  
  slope_df= pt_df%>%
    select(-us_reach_id,-ds_reach_id)%>%
    group_by(reach_id)%>%
    mutate(upstream_node=max(node_id),downstream_node=min(node_id))%>%
    group_by(node_id,pt_time_UTC)%>%
    mutate(mean_pt_node = mean(pt_wse,na.rm=TRUE), sd_pt_node =sd(pt_wse,na.rm=TRUE))%>%
    ungroup()
  
   us_df=filter(slope_df,node_id == upstream_node)%>%
     transmute(reach_id=reach_id,pt_time_UTC=pt_time_UTC,upstream_node=upstream_node,us_mean=mean_pt_node,us_sd=sd_pt_node)
   #if there is only a single PT in the node, we need to set the sd manually to a small number (measurement uncertainty)
   us_df$us_sd[is.na(us_df$us_sd)]= measurement_error
   
   ds_df=filter(slope_df,node_id == downstream_node)%>%
     transmute(reach_id=reach_id,pt_time_UTC=pt_time_UTC,downstream_node=downstream_node,ds_mean=mean_pt_node,ds_sd=sd_pt_node)
   ds_df$ds_sd[is.na(ds_df$ds_sd)]= measurement_error
   
  slope_df2=slope_df%>%
    select(-mean_pt_node,-sd_pt_node)%>%
    left_join(us_df,by=c('reach_id','pt_time_UTC','upstream_node'))%>%
    left_join(ds_df,by=c('reach_id','pt_time_UTC','downstream_node'))%>%
    left_join(reach_length_df,by='reach_id')%>%
    group_by(pt_time_UTC)%>%
    mutate(slope_m_m= (us_mean- ds_mean)/reach_length, slope_uncertainty_m_m = (us_sd^2+ds_sd^2+alongstream_error^2 + measurement_error^2 + crossstream_error)/reach_length)%>%
    filter(!is.na(slope_m_m))%>%
    distinct()%>%
    select(-reach_length,-pt_wse_sd,-pt_wse,-pt_serial,-node_id)
 
  # print(paste0(output_directory,'reach/',rivername,'_','PT_reach_slope.csv'))
  write.csv(slope_df2,file=paste0(output_directory,'/reach/',rivername,'_','PT_reach_slope.csv'),row.names=FALSE)
  write.csv(reach_df,file=paste0(output_directory,'/reach/',rivername,'_','PT_reach_wse.csv'),row.names=FALSE)
  write.csv(node_df,file=paste0(output_directory,'/node/',rivername,'_','PT_node_wse.csv'),row.names=FALSE)
  
  
  
