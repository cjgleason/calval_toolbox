calculate_sope_wse_fromdrift=function(SWORD_path,drift_directory,PT_directory,output_directory,this_river_reach_IDs,this_river_node_IDs,
                                      utm_zone, buffer,rivername){
  
  library(ncdf4)
  library(sf)
  library(dplyr)
  library(rgdal)

LongLatToUTM<-function(x,y,zone){
  xy <- data.frame(ID = 1:length(x), X = x, Y = y)
  coordinates(xy) <- c("X", "Y")
  proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")  ## for example
  res <- spTransform(xy, CRS(paste("+proj=utm +zone=",zone," ellps=WGS84",sep='')))
  res=st_as_sf(res)
  return(data.frame(x=st_coordinates(res)[,1],y=st_coordinates(res)[,2]))
}


#get SWORD data---------------------------------------
SWORD_in=nc_open(SWORD_path,verbose=FALSE)

reachIDs=ncvar_get(SWORD_in, 'reaches/reach_id',verbose=FALSE)
reach_index= which(reachIDs %in% this_river_reach_IDs)

nodeIDs=ncvar_get(SWORD_in,'nodes/node_id',verbose=FALSE)
node_index= which(nodeIDs %in% this_river_node_IDs)
#-----------------------------------------------------

#Node variables-------
#overwriting with the index limits the RAM needed
node_x=ncvar_get(SWORD_in, 'nodes/x',verbose=FALSE)[node_index]

node_y=ncvar_get(SWORD_in, 'nodes/y',verbose=FALSE)[node_index]

node_max_width=ncvar_get(SWORD_in, 'nodes/max_width',verbose=FALSE)[node_index]

node_length=ncvar_get(SWORD_in, 'nodes/node_length',verbose=FALSE)[node_index]

node_reachid=ncvar_get(SWORD_in, 'nodes/reach_id',verbose=FALSE)[node_index] #key field


node_df=data.frame(lon=node_x,lat=node_y,node_ID=this_river_node_IDs,node_wmax=node_max_width,node_length=node_length,reach_id=node_reachid)

node_df= node_df%>%
  mutate(node_UTM_x=LongLatToUTM(node_df$lon,node_df$lat,utm_zone)[,1])%>%
  mutate(node_UTM_y=LongLatToUTM(node_df$lon,node_df$lat,utm_zone)[,2])



#---------------------

#Reach variables-------
#overwriting with the index limits the RAM needed
reach_x=ncvar_get(SWORD_in, 'reaches/x',verbose=FALSE)[reach_index]

reach_y=ncvar_get(SWORD_in, 'reaches/y',verbose=FALSE)[reach_index]

reach_x_max=ncvar_get(SWORD_in, 'reaches/x_max',verbose=FALSE)[reach_index]

reach_x_min=ncvar_get(SWORD_in, 'reaches/x_min',verbose=FALSE)[reach_index]

reach_y_max=ncvar_get(SWORD_in, 'reaches/y_max',verbose=FALSE)[reach_index]

reach_y_min=ncvar_get(SWORD_in, 'reaches/y_min',verbose=FALSE)[reach_index]

reach_length=ncvar_get(SWORD_in, 'reaches/reach_length',verbose=FALSE)[reach_index]


reach_df=data.frame(lon=reach_x,lat=reach_y,reach_id=this_river_reach_IDs,reach_xmax=reach_x_max,reach_xmin=reach_x_min,
                    reach_ymax=reach_y_max,reach_ymin=reach_y_min, reach_length=reach_length)





spatial_reach=st_as_sf(reach_df,coords=c('lon','lat'),remove=FALSE, crs='+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')%>%
  st_transform(6347)%>% #UTM 18N
  mutate(UTM_X=st_coordinates(.)[,1],UTM_Y=st_coordinates(.)[,2])

spatial_reach=spatial_reach%>%
  mutate(xmin=LongLatToUTM(spatial_reach$reach_xmin,spatial_reach$reach_ymin,18)[,1])%>%
  mutate(xmax=LongLatToUTM(spatial_reach$reach_xmax,spatial_reach$reach_ymin,18)[,1])%>%
  mutate(ymin=LongLatToUTM(spatial_reach$reach_xmin,spatial_reach$reach_ymin,18)[,2])%>%
  mutate(ymax=LongLatToUTM(spatial_reach$reach_xmin,spatial_reach$reach_ymax,18)[,2])
#---------------------

#centerline variables-------
cl_reach_IDs= ncvar_get(SWORD_in, 'centerlines/reach_id',verbose=FALSE)
cl_index=which(cl_reach_IDs %in% this_river_reach_IDs)

cl_x=ncvar_get(SWORD_in, 'centerlines/x',verbose=FALSE)[cl_index]
cl_y=ncvar_get(SWORD_in, 'centerlines/y',verbose=FALSE)[cl_index]
cl_id=ncvar_get(SWORD_in, 'centerlines/cl_id',verbose=FALSE)[cl_index]
cl_node_id=ncvar_get(SWORD_in, 'centerlines/node_id',verbose=FALSE)[cl_index]

cl_df=data.frame(reach_id=cl_reach_IDs[cl_index],lon=cl_x,lat=cl_y,cl_id=cl_id,node_ID=cl_node_id)%>%
  filter(!is.na(lat))%>%
  st_as_sf(coords=c('lon','lat'),remove=FALSE, crs='+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs') %>%
  st_transform(6347)%>% #UTM 18N
  mutate(cl_UTM_x=st_coordinates(.)[,1],cl_UTM_y=st_coordinates(.)[,2])

#---------------------

#calculate node wse----------------------------------
calc_node_wse=function(drift_file,node_df,cl_df){

  
  drift_in=readRDS(drift_file)
  
  node_cls=node_df%>%
    left_join(cl_df,by=c('node_ID','reach_id'))%>%
    group_by(node_ID)%>%
    filter(cl_id == min(cl_id) | cl_id == max(cl_id)) %>%
    mutate(node_angle= atan((cl_UTM_y[cl_id == min(cl_id)]-cl_UTM_y[cl_id == max(cl_id)])/(cl_UTM_x[cl_id == min(cl_id)]-cl_UTM_x[cl_id == max(cl_id)])))%>%
    select(-geometry)%>%
    #make a polygon geometry- 4 points, with the first point repeated for 5 total points
    mutate(point1_x= node_UTM_x +cos(node_angle)*node_length/2, 
           point1_y= node_UTM_y+sin(node_angle)*node_length/2,
           point2_x=node_UTM_x -cos(node_angle)*node_length/2,
           point2_y=node_UTM_y-sin(node_angle)*node_length/2,
           point3_x=node_UTM_x +cos(node_angle-(90*pi/180))*node_wmax/2,
           point3_y=node_UTM_y+sin(node_angle-(90*pi/180))*node_wmax/2,
           point4_x=node_UTM_x -cos(node_angle-(90*pi/180))*node_wmax/2,
           point4_y= node_UTM_y-sin(node_angle-(90*pi/180))*node_wmax/2) %>%
   mutate(node_box_x_max=max(c(point1_x,point2_x,point3_x,point4_x)),
          node_box_x_min=min(c(point1_x,point2_x,point3_x,point4_x)),
          node_box_y_max=max(c(point1_y,point2_y,point3_y,point4_y)),
          node_box_y_min=min(c(point1_y,point2_y,point3_y,point4_y)))%>%
    ungroup()
                        
  make_polys= function(node_df){

    polygon_list= st_polygon( list(rbind(c(node_df['node_box_x_min'],node_df['node_box_y_min']),
                                         
                              c(node_df['node_box_x_min'],node_df['node_box_y_max']),
                              c(node_df['node_box_x_max'],node_df['node_box_y_max']),
                              c(node_df['node_box_x_max'],node_df['node_box_y_min']),
                              c(node_df['node_box_x_min'],node_df['node_box_y_min']) )))
    
  }
  
  poly_list=apply(node_cls, 1,make_polys )
  
  spatial_node_cls=node_cls%>%
    mutate(poly_list=poly_list)

  spatial_node_cls=st_as_sf(spatial_node_cls,sf_column_name='poly_list')
  st_crs(spatial_node_cls)= '+proj=utm +zone=18 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs'
  
  plot(spatial_node_cls$poly_list[1:50],col='red')
  
  #ok, now we have a fully spatial object with polygons per node. There are two of them as an artifact of the centerline process, 
  #so let's trim one
  
  final_node_df= spatial_node_cls%>%
    group_by(node_ID)%>%
    filter(row_number()==1)
  
  #make drifts spatial
  spatial_drift=st_as_sf(drift_in,coords=c('UTM_x','UTM_y'),crs='+proj=utm +zone=18 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs')  

  points_in_node=st_intersection(spatial_drift,final_node_df)%>%
    group_by(node_ID)%>%
    summarize(node_wse_bar=mean(final_drift_height,na.rm=T),node_wse_sd=sd(final_drift_height,na.rm=T))
    

}
#----------------------------------------------------

#calculate reach wse and slope from the drifts-----------------------
calc_reach_stats=function(drift_file,spatial_reach, buffer,cl_df){
  drift_in=readRDS(drift_file)
  
  calc_wse_stats=function(reach_index, drift_in,spatial_reach){
    
    relevant_drift_index= which(drift_in$lat.x>spatial_reach$reach_ymin[reach_index] &
                                  drift_in$lat.x<spatial_reach$reach_ymax[reach_index] &
                                  drift_in$lon.x>spatial_reach$reach_xmin[reach_index] &
                                  drift_in$lon.x<spatial_reach$reach_xmax[reach_index])
    
    reach_wse_bar=mean(drift_in$final_drift_height[relevant_drift_index],na.rm=T)
    reach_wse_sd =sd(drift_in$final_drift_height[relevant_drift_index],na.rm=T)
    wse_time_start= min(drift_in$datetime.x[relevant_drift_index])
    wse_time_end=max(drift_in$datetime.x[relevant_drift_index])
    
    
    output=data.frame(reach_id=spatial_reach$reach_id[reach_index],wse_bar=reach_wse_bar,wse_sd=reach_wse_sd,wse_start= wse_time_start,wse_end= wse_time_end)
  }
  
  calc_slopes=function(reach_id_in,drift_in,spatial_reach, buffer,cl_df){
    
    # reach_id_in=73120000151
    
    cl_df=group_by(cl_df, reach_id)%>%
      filter(reach_id==reach_id_in)%>%
      filter(cl_id == min(cl_id) | cl_id == max(cl_id)) %>%
      mutate(xmin=cl_UTM_x - buffer, xmax=cl_UTM_x + buffer, ymin=cl_UTM_y - buffer, ymax=cl_UTM_y + buffer)
    
    
    slope_points_1= which(drift_in$UTM_x>cl_df$xmin[1] &
                            drift_in$UTM_x<cl_df$xmax[1]&
                            drift_in$UTM_y>cl_df$ymin[1] &
                            drift_in$UTM_y<cl_df$ymax[1])
    
    slope_points_2= which(drift_in$UTM_x>cl_df$xmin[2] &
                            drift_in$UTM_x<cl_df$xmax[2] &
                            drift_in$UTM_y>cl_df$ymin[2] &
                            drift_in$UTM_y<cl_df$ymax[2])
    
    reach_wse_start= mean(drift_in$final_drift_height[slope_points_1],na.rm=T)
    reach_wse_start_sd =sd(drift_in$final_drift_height[slope_points_1],na.rm=T)
    
    reach_wse_end= mean(drift_in$final_drift_height[slope_points_2],na.rm=T)
    reach_wse_end_sd =sd(drift_in$final_drift_height[slope_points_2],na.rm=T)
    
    slope_time_start= min(drift_in$datetime.x[c(slope_points_1,slope_points_2)])
    slope_time_end=max(drift_in$datetime.x[c(slope_points_1,slope_points_2)])
    
    reach_length=spatial_reach$reach_length[spatial_reach$reach_id==reach_id_in]
    
    reach_slope= abs( reach_wse_start-reach_wse_end) / reach_length
    reach_slope_uncertainty =sqrt( (reach_wse_start_sd/reach_wse_start)^2 + (reach_wse_end_sd/reach_wse_end)^2)
    
    # browse()
    
    if (any(  c(length(slope_points_1),length(slope_points_2))==0)){
      slope_time_start=NA
      slope_time_end=NA
    }
    output=data.frame(reach_id=reach_id_in,reach_slope=reach_slope, reach_slope_sd=reach_slope_uncertainty,
                      slope_time_start=slope_time_start, slope_time_end=slope_time_end)
  }
  
  
  
  
  reach_ids=unique(spatial_reach$reach_id)
  reach_indices=1:nrow(spatial_reach)
  
  output1 = do.call(rbind,lapply(reach_indices,calc_wse_stats,drift_in=drift_in,spatial_reach=spatial_reach))
  output2 = do.call(rbind,lapply(reach_ids,calc_slopes,drift_in=drift_in,spatial_reach=spatial_reach,buffer=buffer,cl_df=cl_df))
  
  output=full_join(output1,output2,by='reach_id')%>%
    mutate(reach_id=as.character(reach_id))
}
#---------------------------------------------------------------------


drifts = paste0(drift_directory,list.files(drift_directory))

node_wses=do.call(rbind,lapply(drifts,calc_node_wse,node_df=node_df,cl_df=cl_df))
reach_stats=do.call(rbind,lapply(drifts,calc_reach_stats,spatial_reach=spatial_reach,buffer=buffer,cl_df=cl_df))

saveRDS(node_wses,paste0(output_directory,rivername,'_node_wses.rds'))
saveRDS(reach_stats,paste0(output_directory,rivername,'_dritf_wse_slope.rds'))

}