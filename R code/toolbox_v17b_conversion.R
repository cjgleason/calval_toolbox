### Script to add v17b columns to all domain files and all key files ###
library(dplyr)
continent = "NA"

v17b_node_trans = read.csv(paste0("/nas/cee-water/cjgleason/data/SWORD/SWORD_v17b_shp/shp/SWORD_v17b_to_v16_Translation/v17b/",continent,"_NodeIDs_v17b_vs_v16.csv"))%>%
  dplyr::select(-lon, -lat, -shift_flag, -boundary_flag, -boundary_percent, -dominant.reach, -v16.number.of.reaches)
v17b_reach_trans = read.csv(paste0("/nas/cee-water/cjgleason/data/SWORD/SWORD_v17b_shp/shp/SWORD_v17b_to_v16_Translation/v17b/",continent,"_ReachIDs_v17b_vs_v16.csv"))%>%
  dplyr::select(-lon, -lat, -boundary_flag, -boundary_percent, -dominant.reach, -v16.number.of.reaches)
### Domain files ###
site_id = c("Brown")#, "UMass", "UNC", "Brown")

domain_list = list.files(path=paste0("/nas/cee-water/cjgleason/calval/Processed data/",site_id), pattern = "*domain.csv", full.names=TRUE)
key_list = list.files(path=paste0("/nas/cee-water/cjgleason/calval/Processed data/",site_id), pattern = "*_KEY_", full.names=TRUE)
key_list = key_list[!grepl("_v17\\.csv$",key_list)]

for (domain in domain_list){
  domain_file = read.csv(domain, col.names=c("v16_node_id","v16_reach_id"))
  
  join = inner_join(domain_file, v17b_node_trans, by="v16_node_id")%>%
    dplyr::select(-v16_reach_id.y)%>%
    rename(v16_reach_id=v16_reach_id.x)
  
  write.csv(join, paste0(file_path_sans_ext(domain),"_v17b.csv"),row.names = FALSE)
}                        


for (key in key_list){
  key_file = read.csv(key)
  key_file = key_file %>%
    rename(v16_reach_id = Reach_ID, v16_node_id = Node_ID, v16_US_Reach_ID = US_Reach_ID, v16_DS_Reach_ID = DS_Reach_ID)
  
  key_join = inner_join(key_file, v17b_node_trans, by="v16_node_id")%>%
    dplyr::select(-v16_reach_id.y)%>%
    rename(v16_reach_id=v16_reach_id.x)
  
  if (nrow(key_join) == 0){next}
  
  key_join = key_join%>%
    left_join(v17b_reach_trans, by = c("v16_US_Reach_ID" = "v16_reach_id"))%>%
    rename(v17b_node_id = v17_node_id, v17b_US_Reach_ID = v17_reach_id.y, v17b_reach_id = v17_reach_id.x)
  
  key_join = key_join %>%
    left_join(v17b_reach_trans, by = c("v16_DS_Reach_ID" = "v16_reach_id")) %>%
    rename(v17b_DS_Reach_ID = v17_reach_id)
  
  write.csv(key_join, paste0(file_path_sans_ext(key),"_v17b.csv"),row.names=FALSE)
}
