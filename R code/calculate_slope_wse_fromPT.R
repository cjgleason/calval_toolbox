calculate_slope_wse_fromPT=function(keyfile,pt_files,SWORD_path,SWORD_reach,this_river_reach_ids){
  # keyfile='Willamette/WM_Key.csv'
  # pt_files=paste0('Willamette/Willamette munged pts/',list.files('Willamette/Willamette munged pts/'))
  # SWORD_path='na_sword_v11.nc'
  # SWORD_reach= read.csv('Willamette/Willamette nodes.csv')
  # this_river_reach_ids= as.numeric(as.character(unique(SWORD_reach$reach_id)))
  
  
  library(ncdf4)
  library(dplyr)
  #read in key file
  key_df=read.csv(keyfile)%>%
    transmute(pt_serial=PT_Serial,node_id=Node_ID,reach_id=Reach_ID,us_reach_id=US_Reach_ID,ds_reach_id=DS_Reach_ID)
  #read in SWORD
  
  # 
  SWORD_in=nc_open(SWORD_path,verbose=FALSE)
  reachids=ncvar_get(SWORD_in, 'reaches/reach_id',verbose=FALSE)
  reach_index= which(reachids %in% this_river_reach_ids)
  reach_length=ncvar_get(SWORD_in, 'reaches/reach_length',verbose=FALSE)[reach_index]
  
  reach_key=data.frame(us_reach_id=as.character(this_river_reach_ids),reach_length=reach_length) #since later we have explicit US and DS elevations with the same id, we lose the 
  #cardinal name of 'reach_id'. Can set this to us or ds.
  
  #read in pt files
  pt_df=do.call(rbind,lapply(pt_files,read.csv))%>%
    transmute(pt_serial,pt_time_UTC,pt_wse,pt_wse_sd)%>%
    left_join(key_df,by='pt_serial')
  
  #calculate node wse
  node_df=pt_df%>%
    group_by(node_id,pt_time_UTC)%>%
    summarise(mean_node_pt_wse_m=mean(pt_wse),mean_pt_wse_precision_m=0.001)%>% #JPL wants precision, not variance mean(pt_wse_sd)
    ungroup()%>%#based on grouping, it will repeat
    mutate(node_id=as.character(node_id))%>%
    distinct()#based on grouping, it will repeat
  
  #calculate reach wse
  reach_df=pt_df%>%
    group_by(reach_id,pt_time_UTC)%>%
    summarise(mean_reach_pt_wse_m=mean(pt_wse),mean_reach_pt_wse_precision_m=0.001)%>%# JPL wants precision, not variance mean(pt_wse_sd),
    ungroup()%>%#based on grouping, it will repeat
    mutate(reach_id=as.character(reach_id))%>%
    distinct()#based on grouping, it will repeat
  
  #calculate slope
  slope_df_us=pt_df%>%
    select(-node_id)%>%
    mutate(reach_id=as.character(reach_id),
           us_reach_id=as.character(us_reach_id),
           ds_reach_id=as.character(ds_reach_id))%>%
    group_by(us_reach_id,pt_time_UTC)%>%
    summarise(mean_us=mean(pt_wse),sd_us=sd(pt_wse,na.rm=T))#here we do want variance, as we are using multiple PTs and 
  #we need to characterize how much uncertainty we're adding to the US an DS boundary conditions
  
  slope_df_ds=pt_df%>%
    select(-node_id)%>%
    mutate(reach_id=as.character(reach_id),
           us_reach_id=as.character(us_reach_id),
           ds_reach_id=as.character(ds_reach_id))%>%
    group_by(ds_reach_id,pt_time_UTC)%>%
    summarise(mean_ds=mean(pt_wse),sd_ds=sd(pt_wse,na.rm=T))#here we do want variance, as we are using multiple PTs and 
  #we need to characterize how much uncertainty we're adding to the US an DS boundary conditions
  
  unique_reaches=unique(reach_df$reach_id)[!is.na(unique(reach_df$reach_id))]
  
  slope_calculator =function(this_reach,slope_df_ds,slope_df_us,reach_key){
    library(dplyr)
    
    slope_calc_df_us= filter(slope_df_us,us_reach_id==this_reach)
    slope_calc_df= filter(slope_df_ds,ds_reach_id==this_reach)%>%
      left_join(slope_calc_df_us,by='pt_time_UTC')%>%
      left_join(reach_key,by='us_reach_id')%>%
      mutate(slope=(mean_us-mean_ds)/reach_length)
    }
  

  
  final_slope=do.call(rbind,lapply(unique_reaches,slope_calculator,slope_df_ds=slope_df_ds,slope_df_us=slope_df_us,reach_key=reach_key))%>%
    ungroup()%>%
    transmute(reach_id=ds_reach_id, pt_time_UTC=pt_time_UTC,reach_PT_slope_m_m=slope,reach_PT_slope_precision_m=sqrt(sd_ds^2 +sd_us^2)/reach_length)%>%
    filter(!is.na(reach_PT_slope_m_m))
  
  
  write.csv(final_slope,'Willamette/SWORD products/reach/Willamette_PT_reach_slope.csv')
  write.csv(reach_df,'Willamette/SWORD products/reach/Willamette_PT_reach_wse.csv')
  write.csv(node_df,'Willamette/SWORD products/node/Willamette_PT_node_wse.csv')
}