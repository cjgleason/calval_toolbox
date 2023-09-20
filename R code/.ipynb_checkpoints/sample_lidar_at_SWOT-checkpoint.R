#experimental and untested as of 9/23

sample_lidar_at_SWOT= function(SWORD_path,this_river_node_IDs,this_river_reach_IDs,utm_zone,lidar_date,raster_path,output_path,river_name){

library(ncdf4)
library(sf)
library(rgdal)
library(raster)
library(dplyr)

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

node_cls=node_df%>%
  left_join(cl_df,by=c('node_ID','reach_id'))%>%
  group_by(node_ID)%>%
  filter(cl_id == min(cl_id) | cl_id == max(cl_id)) %>%
  mutate(node_angle= atan((cl_UTM_y[cl_id == min(cl_id)]-cl_UTM_y[cl_id == max(cl_id)])/(cl_UTM_x[cl_id == min(cl_id)]-cl_UTM_x[cl_id == max(cl_id)])))%>%
  dplyr::select(-geometry)%>%
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

spatial_node=node_cls%>%
  mutate(poly_list=poly_list)

spatial_node=st_as_sf(spatial_node,sf_column_name='poly_list')
st_crs(spatial_node)= '+proj=utm +zone=18 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs'

spatial_node=spatial_node%>%
  group_by(node_ID)%>%
  filter(row_number()==1)

spatial_node2=spatial_node%>%
  mutate(geometry=poly_list)%>%
  dplyr::select(node_ID,geometry)


#---------------------

#Reach variables-------
#overwriting with the index limits the RAM needed
reach_x=ncvar_get(SWORD_in, 'reaches/x',verbose=FALSE)[reach_index]

reach_y=ncvar_get(SWORD_in, 'reaches/y',verbose=FALSE)[reach_index]

reach_df=data.frame(lon=reach_x,lat=reach_y,reach_id=this_river_reach_IDs)

spatial_reach=st_as_sf(reach_df,coords=c('lon','lat'),remove=FALSE, crs='+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')%>%
  st_transform(6347)%>% #UTM 18N
  mutate(UTM_X=st_coordinates(.)[,1],UTM_Y=st_coordinates(.)[,2])

#need an outline of the reach. We will get one from the nodes


make_reach_polys=function(point3_x,point3_y,point4_x,point4_y){
  #up left bank, across, down right bank, back to start
   x_coords=c(point4_x,rev(point3_x),point4_x[1])
   y_coords=c(point4_y,rev(point3_y),point4_y[1])
   list(cbind(x_coords,y_coords))
}


reach_polygons=node_df%>%
  left_join(cl_df,by=c('node_ID','reach_id'))%>%
  group_by(node_ID)%>%
  filter(cl_id == min(cl_id) | cl_id == max(cl_id)) %>%
  mutate(node_angle= atan((cl_UTM_y[cl_id == min(cl_id)]-cl_UTM_y[cl_id == max(cl_id)])/(cl_UTM_x[cl_id == min(cl_id)]-cl_UTM_x[cl_id == max(cl_id)])))%>%
  dplyr::select(-geometry)%>%
  #make a polygon geometry- 4 points, with the first point repeated for 5 total points
  mutate(point3_x=node_UTM_x +cos(node_angle-(90*pi/180))*node_wmax/2,
         point3_y=node_UTM_y+sin(node_angle-(90*pi/180))*node_wmax/2,
         point4_x=node_UTM_x -cos(node_angle-(90*pi/180))*node_wmax/2,
         point4_y= node_UTM_y-sin(node_angle-(90*pi/180))*node_wmax/2) %>%
  #two node points as we have two centerline ends, get ride of one
  filter(row_number()==1)%>%
  ungroup()%>%
  group_by(reach_id)%>%
  arrange(cl_id)%>%
  mutate(poly_objects=make_reach_polys(point3_x,point3_y,point4_x,point4_y))%>%
  #now we have reach polygons that repeat per node. Keep only the first in each group
  filter(row_number()==1)%>%
  ungroup()

spatial_make_reach_polys=function(list_in){
  # point3_x=df_in['point3_x']
  # point3_y=df_in['point3_x']
  # point4_x=df_in['point3_x']
  # point4_y=df_in['point3_x']
  # x_coords=c(point3_x,point4_x,point3_x[1])
  # y_coords=c(point3_y,point4_y,point3_y[1])
  
  reach_poly_points=list(cbind(list_in[,1],list_in[,2]))
  st_polygon(reach_poly_points)
}


spatial_poly_list=lapply(reach_polygons$poly_objects, spatial_make_reach_polys)

spatial_reach_polygons=reach_polygons%>%
  mutate(poly_list=spatial_poly_list)

spatial_reach_polygons=st_as_sf(spatial_reach_polygons,sf_column_name='poly_list')
st_crs(spatial_reach_polygons)= '+proj=utm +zone=18 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs'

#---------------------

lidar_raster=raster(paste0(raster_path,'CTRiver_firstR.tif')) #already UTM. add auto check later

start_time=Sys.time()

node_mean_height=as.data.frame(extract(lidar_raster,spatial_node2, fun=mean, na.rm=TRUE, df=TRUE,sp=TRUE)@data)%>%
  transmute(node_ID=node_ID,wse_bar=CTRiver_firstR)

node_sd_height=as.data.frame(extract(lidar_raster,spatial_node2, fun=sd, na.rm=TRUE, df=TRUE,sp=TRUE)@data)%>%
  transmute(node_ID=node_ID,wse_sd=CTRiver_firstR)

node_stats=left_join(node_mean_height,node_sd_height,by='node_ID')%>%
  mutate(date=lidar_date)

reach_mean_height=as.data.frame(extract(lidar_raster,spatial_reach_polygons, fun=mean, na.rm=TRUE, df=TRUE,sp=TRUE)@data)%>%
  transmute(reach_ID=reach_id,wse_bar=CTRiver_firstR)

reach_sd_height=as.data.frame(extract(lidar_raster,spatial_reach_polygons, fun=max, na.rm=TRUE, df=TRUE,sp=TRUE)@data)%>%
  transmute(reach_ID=reach_id,wse_sd=CTRiver_firstR)

reach_stats=left_join(reach_mean_height,reach_sd_height,by='reach_ID')%>%
  mutate(date=lidar_date)

end_time=Sys.time()

end_time-start_time

saveRDS(node_stats,paste0(output_path,river_name,'node_wses.rds'))
saveRDS(reach_stats,paste0(output_path,river_name,'reach_wses.rds'))

}
