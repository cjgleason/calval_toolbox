calculate_slope_wse_fromPT=function(keyfile,PT_files,SWORD_path,SWORD_reach,this_river_reach_IDs){
# keyfile='Willamette/WM_Key.csv'
# PT_files=paste0('Willamette/Willamette munged PTs/',list.files('Willamette/Willamette munged PTs/'))
# SWORD_path='na_sword_v11.nc'
# SWORD_reach= read.csv('Willamette/Willamette nodes.csv')
# this_river_reach_IDs= as.numeric(as.character(unique(SWORD_reach$reach_id)))


library(ncdf4)
#read in key file
key_df=read.csv(keyfile)%>%
  transmute(PT_serial=PT_Serial,node_id=Node_ID,reach_id=Reach_ID,US_reach_id=US_Reach_ID,DS_reach_id=DS_Reach_ID)
#read in SWORD

# 
# SWORD_in=nc_open(SWORD_path,verbose=FALSE)
# reachIDs=ncvar_get(SWORD_in, 'reaches/reach_id',verbose=FALSE)
# reach_index= which(reachIDs %in% this_river_reach_IDs)
# reach_length=ncvar_get(SWORD_in, 'reaches/reach_length',verbose=FALSE)[reach_index]

reach_length=10000

#read in PT files
PT_df=do.call(rbind,lapply(PT_files,read.csv))%>%
  select(PT_serial,PT_time_UTC,PT_wse,PT_wse_sd)%>%
  left_join(key_df,by='PT_serial')

#calculate node wse
node_df=PT_df%>%
  group_by(node_id,PT_time_UTC)%>%
  summarise(mean_PT_wse=mean(PT_wse),mean_PT_wse_sd=mean(PT_wse_sd),node_id=as.character(node_id))

#calculate reach wse
reach_df=PT_df%>%
  group_by(reach_id,PT_time_UTC)%>%
  summarise(mean_PT_wse=mean(PT_wse),mean_PT_wse_sd=mean(PT_wse_sd),reach_id=as.character(reach_id))

#calculate slope
slope_df_US=PT_df%>%
  select(-node_id)%>%
  mutate(reach_id=as.character(reach_id),US_reach_id=as.character(US_reach_id),DS_reach_id=as.character(DS_reach_id))%>%
  group_by(US_reach_id,PT_time_UTC)%>%
  summarise(mean_US=mean(PT_wse),sd_US=sd(PT_wse,na.rm=T))

slope_df_DS=PT_df%>%
  select(-node_id)%>%
  mutate(reach_id=as.character(reach_id),US_reach_id=as.character(US_reach_id),DS_reach_id=as.character(DS_reach_id))%>%
  group_by(DS_reach_id,PT_time_UTC)%>%
  summarise(mean_DS=mean(PT_wse),sd_DS=sd(PT_wse,na.rm=T))

unique_reaches=unique(reach_df$reach_id)[!is.na(unique(reach_df$reach_id))]

slope_calculator =function(this_reach,slope_df_DS,slope_df_US){
  slope_calc_df_US= filter(slope_df_US,US_reach_id==this_reach)
  slope_calc_df= filter(slope_df_DS,DS_reach_id==this_reach)%>%
    left_join(slope_calc_df_US,by='PT_time_UTC')%>%
    mutate(slope=(mean_US-mean_DS)/reach_length)}


final_slope=do.call(rbind,lapply(unique_reaches,slope_calculator,slope_df_DS=slope_df_DS,slope_df_US=slope_df_US))%>%
  ungroup()%>%
  transmute(reach_id=DS_reach_id, PT_time_UTC=PT_time_UTC,slope=slope,slope_sd=sqrt(sd_DS^2 +sd_US^2))%>%
  filter(!is.na(slope))


write.csv(final_slope,'Willamette/SWORD products/reach/Willamette_reach_PT_slope.csv')
write.csv(reach_df,'Willamette/SWORD products/reach/Willamette_reach_PT_wse.csv')
write.csv(node_df,'Willamette/SWORD products/node/Willamette_node_PT_wse.csv')
}