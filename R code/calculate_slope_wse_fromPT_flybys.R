Sys.umask('002')
calculate_slope_wse_fromPT_flybys=function(keyfile,pt_files,SWORD_path,SWORD_reach,this_river_reach_ids,this_river_node_ids,
                                    alongstream_error,crossstream_error,measurement_error,output_directory,rivername){


# hubname='Brown'
# rivername='NS'
# continent='na'
# PT_key_file= 'SWOTCalVal_NS_KEY_20230525_20230613.csv' #WM
# utm_zone=13 #NS= 13
# setwd(paste0('/nas/cee-water/cjgleason/calval/Processed data/',hubname,'/'))
# working_dir=(paste0('/nas/cee-water/cjgleason/calval/Processed data/',hubname,'/'))
# domain_file=paste0(rivername,'_domain.csv')
# paste0('/nas/cee-water/cjgleason/calval/Processed data/',hubname,'/')
# PT_data_directory=paste0('/nas/cee-water/cjgleason/calval/Processed data/Brown/Flyby PT/reprocessed_2024_01_31/')
# PT_key_file= 'SWOTCalVal_NS_KEY_20230525_20230613.csv' #WM
# 
# 
#   read_keys=function(keyfile){
#     key_col_names = c("PT_Serial", "Label", "Baro_Comp", "Node_ID", "Reach_ID", "US_Reach_ID", 
#                       "DS_Reach_ID", "Lat_WGS84", "Long_WGS84", "Install_method",
#                       "Date_PT_Install", "Time_PT_Install_UTC", "Date_PT_Uninstall",
#                       "Time_PT_Uninstall_UTC", "Date_GNSS_Install", "Time_GNSS_Install_Start_UTC", 
#                       "Time_GNSS_Install_End_UTC", "GNSS_Offset_m", "Receiver_Install", "Original_Install_Log_File", 
#                       "Final_Install_Log_File", "Date_GNSS_Uninstall", "Time_GNSS_Uninstall_Start_UTC", 
#                       "Time_GNSS_Uninstall_End_UTC", "Receiver_Uninstall", "Original_Uninstall_Log_File", "Final_Uninstall_Log_File")
#     this_key= read.csv(keyfile,stringsAsFactors=FALSE, col.names = key_col_names, na.strings = c("","NA","na","NaN", " "))%>%
#       mutate(keyid=keyfile)%>%
#       mutate(pt_serial=as.integer(PT_Serial))
#   }
#  
#    keyfile= do.call(rbind,lapply(PT_key_file,read_keys))
#     pt_files= list.files(PT_data_directory, full.names = TRUE)
#     SWORD_path=SWORD_path=paste0('/nas/cee-water/cjgleason/calval/SWORD_15/netcdf/',continent,
#                                  '_sword_v15.nc')
#     SWORD_reach= read.csv(domain_file)
#     this_river_reach_ids= as.numeric(as.character(unique(SWORD_reach$Reach_ID)))
#     this_river_node_ids= as.numeric(unique(SWORD_reach$Node_ID[!is.na(SWORD_reach$Node_ID)]))
#     alongstream_error=0.05
#   crossstream_error=0.05
#   measurement_error=0.05
#   output_directory='/nas/cee-water/cjgleason/calval/Processed data/Brown/Data frames/reprocessed_2024_01_31/'
# 
#   
  #caclulate SWORD products from PTs
  library(ncdf4)
  library(dplyr)
  options(warn=-1)
  # detach(package:'plyr')
  
  #read in key file and add unique id (pt_id) column to get at PT serial moving nodes in river
  key_df=keyfile%>%
    transmute(pt_serial=as.integer(PT_Serial),pt_id = paste0(PT_Serial,'_',Node_ID),node_id=as.character(Node_ID),reach_id=as.character(Reach_ID),us_reach_id=as.character(US_Reach_ID),ds_reach_id=as.character(DS_Reach_ID))%>%
    filter(!is.na(pt_serial))
  #read in SWORD
  
  SWORD_in=nc_open(SWORD_path,verbose=FALSE)
  reachids=ncvar_get(SWORD_in, 'reaches/reach_id',verbose=FALSE)
  reach_index= which(reachids %in% this_river_reach_ids)
  reach_length=ncvar_get(SWORD_in, 'reaches/reach_length',verbose=FALSE)[reach_index]
  
  nodeids=ncvar_get(SWORD_in,'nodes/node_id',verbose=FALSE)
  node_index=which(nodeids %in% this_river_node_ids)
  p_dist_out=ncvar_get(SWORD_in,'nodes/dist_out',verbose=FALSE)[node_index]
  node_reachid=ncvar_get(SWORD_in, 'nodes/reach_id',verbose=FALSE)[node_index] #key field
  
  reach_length_df=data.frame(reach_id=as.character(reachids[reach_index]),reach_length=reach_length)
  node_dist_df=data.frame(node_id=as.character(nodeids[node_index]),p_dist_out=p_dist_out,reach_id=as.character(node_reachid))

  run_nodes=function(pt_file,key_df,measurement_error,alongstream_error,crossstream_error,node_dist_df){
    

    #read in pt files - create new pt_id field that is pt_serial and node id to prevent serial id confusion if geographically moved in river
    pt_df=read.csv(pt_file)%>%
      dplyr::select(pt_serial,pt_time_UTC,pt_with_flyby_wse_m,pt_with_correction_total_error_m,pt_with_correction_mean_offset_sd_m,Node_ID,flag)%>%
      mutate(pt_id = paste0(pt_serial,'_',Node_ID))%>%
      left_join(key_df,by='pt_id',relationship='many-to-many')
    
    #calculate node wse
    node_df=pt_df%>%
      group_by(node_id,pt_time_UTC)%>%      summarise(pt_id=first(pt_id),
                                                      pt_serial=pt_serial.x,
                                                      mean_node_pt_wse_m=mean(pt_with_flyby_wse_m),
                                                      mean_pt_wse_precision_m=alongstream_error+crossstream_error+measurement_error,
                                                      pt_with_correction_mean_offset_sd_m=pt_with_correction_mean_offset_sd_m,
                                                      pt_with_correction_total_error_m=pt_with_correction_total_error_m, flag=flag)%>% #JPL wants precision, not variance mean(pt_wse_sd)
      ungroup()%>%#based on grouping, it will repeat
      mutate(node_id=as.character(node_id))%>%
      left_join(node_dist_df,by='node_id')%>%
      distinct()
    # 12/20 added node ID to file name to make sure if PT serial is moved within river, it will not glom onto the wrong record 
    output_name=paste0(output_directory,'node/',rivername,'_',as.character(pt_df$pt_id[1]),'_PT_node_wse.csv')
    
    # This adds additional information from same PT in different key file to maintain PT record    
    if(file.exists(output_name)){
      holdit=read.csv(output_name)
      write_it=rbind(holdit,node_df)%>%distinct() #in csae the folder wasn't empty and we just duplicated something
      write.csv(write_it,output_name,col.names=TRUE,row.names=FALSE)  }else{
        write.csv(node_df,output_name,col.names=TRUE,row.names=FALSE)
      }#3dn file check
  }#end node function
  
  runit=lapply(pt_files,   run_nodes, 
               key_df=key_df,
               measurement_error=measurement_error,
               alongstream_error=alongstream_error,
               crossstream_error=crossstream_error,
               node_dist_df=node_dist_df)
  
  

  
  #wrapper to limit data read in
  read_in_pt_reach=function(pt_file,this_reach_id,key_df){
    
    
    data_in= read.csv(pt_file,header=TRUE)%>%
      mutate(PT_Serial=pt_serial)%>%
      left_join(key_df,by='pt_serial')%>%
      filter(reach_id==this_reach_id)
  }
  
  run_reaches=function(this_reach_id,reach_length_df,node_dist_df,key_df,pt_files)  {

    
    pt_df=do.call(rbind,lapply(pt_files,read_in_pt_reach,this_reach_id=this_reach_id,key_df=key_df))
    
    if(nrow(pt_df)==0){return(NA)}
    
    summarize_func=function(node_ids){
      output=paste(unique(node_ids),sep='_')}
    
    
    #calculate reach wse
    reach_df=pt_df%>%
      ungroup()%>%
      left_join(node_dist_df,by=c('reach_id','node_id'))%>%
      group_by(pt_time_UTC)%>%
      dplyr::summarise(reach_id=unique(reach_id),
                       mean_reach_pt_wse_m=mean(pt_with_flyby_wse_m),
                       ### add back alognstream crosstream if you want here, dropped on 1/29 for clarity ##########
                       nodelist=paste(unique(node_id),collapse='_'),
                       mean_p_dist_out=mean(p_dist_out))# JPL wants precision, not variance mean(pt_wse_sd),
    
    write.csv(reach_df,file=paste0(output_directory,'/reach/',rivername,'_',as.character(this_reach_id),'_PT_reach_wse.csv'),row.names=FALSE)
    
  }  #end run reaches
  
  runit=lapply(as.character(this_river_reach_ids),run_reaches,
               reach_length_df=reach_length_df,
               node_dist_df=node_dist_df,
               key_df=key_df,
               pt_files=pt_files)
  #     # }#end run reaches
  #slope redo starting 10/19
  
  #previous paradigm was reach based. Let's switch to PT based, sicne we want to just use the keyfile regardless of the geometry
  
  #we have the pt files
  slope_key=key_df%>%
    filter(!is.na(us_reach_id))
  
  
  #this gives us a dataframe where we have pts with reaches they belong to. take US reach ID - DS reach ID et viola
  
  read_in_pt_slope=function(pt_file,slope_key){
    data_in= read.csv(pt_file,header=TRUE)%>%
      mutate(PT_Serial=pt_serial)
    
    slope_key=slope_key%>%
      left_join(data_in,by='pt_serial')
  }
  
  #get all pt data at 15min for only those PTs called out as on a reach boundary in the key file
  slope_us=do.call(rbind,lapply(pt_files,read_in_pt_slope,slope_key))%>%
    #grab a mean of each time per identifed US reach boundary
    group_by(pt_time_UTC,us_reach_id)%>%
    summarize(mean_pt_wse_us_boundary_m=mean(pt_with_flyby_wse_m),
              # since there are two pts per boundary, but could be 1 or 3, we need a general solution as below
              # also, we want to account for the difference in US PTs, but since there are theoretically only 2, sd has no meaning
              total_error_pt_wse_us_boundary=sqrt( (max(mean_pt_wse_us_boundary_m)- min(mean_pt_wse_us_boundary_m))^2 +
                                                     sum( mean(pt_with_correction_total_error_m)^2)     )  ,
              pt_serials_us=paste(unique(pt_serial,sep="_")))%>%
    mutate(reach_id=us_reach_id)
  #the above dataframe has wses for only reachids identified as an upstream boundary in the keyfile. We preserve that as a static 'reachid' so we can later merge it with the downstream ids to generate slope.
  
  slope_ds=do.call(rbind,lapply(pt_files,read_in_pt_slope,slope_key))%>%
    #grab a mean of each time per identified DS reach boundary
    group_by(pt_time_UTC,ds_reach_id)%>%
    summarize(mean_pt_wse_ds_boundary_m=mean(pt_with_flyby_wse_m),
              total_error_pt_wse_ds_boundary=sqrt( (max(mean_pt_wse_ds_boundary_m)- min(mean_pt_wse_ds_boundary_m))^2 +
                                                     sum( mean(pt_with_correction_total_error_m)^2)     )  ,
              pt_serials_ds=paste(unique(pt_serial,sep="_")))%>%
    mutate(reach_id=ds_reach_id)  
  #the above dataframe has wses for only reachids identified as a downstream boundary in the keyfile. We preserve that as a static 'reachid' so we can join it with the other one above
  
  #join both dataframes together 
  slope_df= left_join(slope_us,slope_ds,by=c('pt_time_UTC','reach_id'))%>%
    #they've been joined on reachid, but they carry a 'us reach id', 'ds reach id', and wse mean and signa at each with them
    #this let's us do straight mutate math by group to calculate slope at all reaches simultaneously.
    
    #join in the reach length df to get SWORD lengths
    left_join(reach_length_df,by='reach_id')%>%
    
    #group by reach id and time to ensure the mtuate math operates correctly, although it shouldn't actually be needed as we've already reduced each time to a single wse value
    group_by(reach_id,pt_time_UTC)%>%
    
    #calcualte slope
    mutate(slope_m_m= (mean_pt_wse_ds_boundary_m- mean_pt_wse_us_boundary_m)/reach_length)%>%
    
    #calculate slope uncertainty
    mutate(slope_uncertainty_m_m = sqrt( total_error_pt_wse_us_boundary^2 +  total_error_pt_wse_ds_boundary^2  ) / reach_length )%>%
    
    #drop any NAs
    filter(!is.na(slope_m_m))%>%
    
    #these are now confusing, so drop them in the write out.
    select(-us_reach_id,-ds_reach_id)
  
  # print(head(slope_us))
  # print(head(slope_ds))
  
  write.csv(slope_df,file=paste0(output_directory,'/reach/',rivername,'_','PT_reach_slope.csv'),row.names=FALSE)
  #   
  
  
}# end of function