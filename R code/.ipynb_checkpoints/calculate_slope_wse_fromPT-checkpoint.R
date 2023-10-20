calculate_slope_wse_fromPT=function(keyfile,pt_files,SWORD_path,SWORD_reach,this_river_reach_ids,this_river_node_ids,
                                    alongstream_error,crossstream_error,measurement_error,output_directory,rivername){

    #caclulate SWORD products from PTs
  library(ncdf4)
  library(dplyr)
    options(warn=-1)
   # detach(package:'plyr')

  #read in key file
  key_df=keyfile%>%
    transmute(pt_serial=as.integer(PT_Serial),node_id=as.character(Node_ID),reach_id=as.character(Reach_ID),us_reach_id=as.character(US_Reach_ID),ds_reach_id=as.character(DS_Reach_ID))%>%
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
    
       #read in pt files
  pt_df=read.csv(pt_file)%>%
    dplyr::select(pt_serial,pt_time_UTC,pt_wse,pt_wse_sd,sigma_pt_correction_m)%>%
    left_join(key_df,by='pt_serial',relationship='many-to-many')
    
  #calculate node wse
  node_df=pt_df%>%
    group_by(node_id,pt_time_UTC)%>%      summarise(pt_serial=first(pt_serial),mean_node_pt_wse_m=mean(pt_wse),mean_pt_wse_precision_m=alongstream_error+crossstream_error+measurement_error,sigma_pt_correction_m=sigma_pt_correction_m)%>% #JPL wants precision, not variance mean(pt_wse_sd)
    ungroup()%>%#based on grouping, it will repeat
    mutate(node_id=as.character(node_id))%>%
    left_join(node_dist_df,by='node_id')%>%
     distinct()
     

  write.csv(node_df,file=paste0(output_directory,'node/',rivername,'_',as.character(pt_df$pt_serial[1]),'PT_node_wse.csv'),row.names=FALSE)
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
    dplyr::summarise(mean_reach_wse_m=mean(pt_wse),
                     reach_id=unique(reach_id),
              mean_reach_pt_wse_m=mean(pt_wse),
              mean_reach_pt_wse_precision_m=alongstream_error+crossstream_error+measurement_error,
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
     summarize(mean_pt_wse_us_boundary_m=mean(pt_wse),sd_pt_wse_us_boundary_m=sqrt(mean(sum(pt_wse_sd^2))), pt_serials_us=paste(unique(pt_serial,sep="_")))%>%
    mutate(reach_id=us_reach_id)
    #the above dataframe has wses for only reachids identified as an upstream boundary in the keyfile. We preserve that as a static 'reachid' so we can later merge it with the downstream ids to generate slope.
    
  slope_ds=do.call(rbind,lapply(pt_files,read_in_pt_slope,slope_key))%>%
       #grab a mean of each time per identified US reach boundary
        group_by(pt_time_UTC,ds_reach_id)%>%
        summarize(mean_pt_wse_ds_boundary_m=mean(pt_wse),sd_pt_wse_ds_boundary_m=sqrt(mean(sum(pt_wse_sd^2))), pt_serials_ds=paste(unique(pt_serial,sep="_")))%>%
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
    mutate(slope_uncertainty_m_m = sqrt((sd_pt_wse_ds_boundary_m^2+sd_pt_wse_us_boundary_m^2+alongstream_error^2 + measurement_error^2 + crossstream_error))/reach_length)%>%
    
    #drop any NAs
    filter(!is.na(slope_m_m))%>%
    
    #these are now confusing, so drop them in the write out.
    select(-us_reach_id,-ds_reach_id)
    
# print(head(slope_us))
# print(head(slope_ds))
  
    write.csv(slope_df,file=paste0(output_directory,'/reach/',rivername,'_','PT_reach_slope.csv'),row.names=FALSE)
#   
    
      
# #calculate slope
#   unique_reaches=this_river_reach_ids
    
#     #now we need a ptdf that includes the us and ds reaches for THIS REACH 
#     #since we're running on the reach level
    
# #wrapper to limit data read in
#     read_in_pt_reach_us_ds=function(pt_file,this_reach_id,key_df){
      
#     key_df=filter(key_df,reach_id==this_reach_id)
#         this_us_id=key_df$us_reach_id
#         this_ds_id=key_df$ds_reach_id
       
#     data_in= read.csv(pt_file,header=TRUE)%>%
#        mutate(PT_Serial=pt_serial)%>%
#        left_join(key_df,by='pt_serial')%>%
#        filter(reach_id==this_reach_id | any(reach_id==this_ds_id) | any(reach_id==this_us_id)) 
       
        
#       }
#      pt_df=do.call(rbind,lapply(pt_files,read_in_pt_reach_us_ds,this_reach_id=this_reach_id,key_df=key_df))
  
  
                              
#   slope_df= pt_df%>%
#     group_by(reach_id)%>%
#     mutate(upstream_node=max(node_id),downstream_node=min(node_id))%>%
#     group_by(node_id,pt_time_UTC)%>%
#     mutate(mean_pt_node = mean(pt_wse,na.rm=TRUE), sd_pt_node =sd(pt_wse,na.rm=TRUE))%>%
#     ungroup()
    
#    us_df=filter(slope_df,node_id == upstream_node)%>%
#      transmute(reach_id=reach_id,pt_time_UTC=pt_time_UTC,upstream_node=upstream_node,us_mean=mean_pt_node,us_sd=sd_pt_node)
#    #if there is only a single PT in the node, we need to set the sd manually to a small number (measurement uncertainty)
#    us_df$us_sd[is.na(us_df$us_sd)]= measurement_error
   
#    ds_df=filter(slope_df,node_id == downstream_node)%>%
#      transmute(reach_id=reach_id,pt_time_UTC=pt_time_UTC,downstream_node=downstream_node,ds_mean=mean_pt_node,ds_sd=sd_pt_node)
#    ds_df$ds_sd[is.na(ds_df$ds_sd)]= measurement_error
   
#   slope_df2=slope_df%>%
#     dplyr::select(-mean_pt_node,-sd_pt_node)%>%
#     left_join(us_df,by=c('reach_id','pt_time_UTC','upstream_node'),relationship ="many-to-many")%>%
#     left_join(ds_df,by=c('reach_id','pt_time_UTC','downstream_node'),relationship ="many-to-many")%>%
#     left_join(reach_length_df,by='reach_id')%>%
#     group_by(pt_time_UTC)%>%
#     mutate(slope_m_m= (us_mean- ds_mean)/reach_length, slope_uncertainty_m_m = (us_sd^2+ds_sd^2+alongstream_error^2 + measurement_error^2 + crossstream_error)/reach_length)%>%
#     filter(!is.na(slope_m_m))%>%
#     dplyr::select(reach_id,pt_time_UTC,slope_m_m,us_sd,ds_sd,slope_uncertainty_m_m,sigma_pt_correction_m)%>%
#     distinct()
    
 
    
#   write.csv(slope_df2,file=paste0(output_directory,'/reach/',rivername,'_',as.character(this_reach_id),'_PT_reach_slope.csv'),row.names=FALSE)
#   write.csv(reach_df,file=paste0(output_directory,'/reach/',rivername,'_',as.character(this_reach_id),'_PT_reach_wse.csv'),row.names=FALSE)
    

#   }#
  
 
    
}