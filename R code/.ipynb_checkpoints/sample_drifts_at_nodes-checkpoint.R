
compare_reach_averaging =function(drift_file,
                                  utm_zone,
                                  scale_maxwidth,
                                  domain,
                                  this_river_node_ids,
                                  this_river_reach_ids,
                                  continent,
                                  buffer,
                                  SWORD_path,
                                  dataframe_file){


library(ncdf4)
library(sf)
library(dplyr)

LongLatToUTM<-function(x,y,zone){
  xy <- data.frame(id = 1:length(x), X = x, Y = y)
  xy <- st_as_sf(x = xy, 
                 coords = c("X", "Y"),
                 crs = "+proj=longlat +datum=WGS84")
###sp usage original###
  # st_coordinates(xy) <- c("X", "Y")
  # proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")  ## for example
  # res <- spTransform(xy, CRS( paste0('+proj=utm +zone=',utm_zone,' +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs')))
  res <- st_transform(xy, st_crs( paste0('+proj=utm +zone=',utm_zone,' +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs')))
  res=st_as_sf(res)
  return(data.frame(x=st_coordinates(res)[,1],y=st_coordinates(res)[,2]))
}


drift_in=read.csv(drift_file,header=TRUE,stringsAsFactors = FALSE)%>%
  filter(!is.na(gnss_Lon))%>%
  filter(!is.na(gnss_Lat))%>%
  mutate(UTM_x=LongLatToUTM(gnss_Lon,gnss_Lat,zone)[,1])%>%
  mutate(UTM_y=LongLatToUTM(gnss_Lon,gnss_Lat,zone)[,2])%>%
  mutate(gnss_time_UTC=as.POSIXct(gnss_time_UTC))#needed as when it gets written to csv it becomes not a posix object

#get SWORD data---------------------------------------
SWORD_in=nc_open(SWORD_path,verbose=FALSE)

reachids=ncvar_get(SWORD_in, 'reaches/reach_id',verbose=FALSE)
reach_index= which(reachids %in% this_river_reach_ids)

nodeids=ncvar_get(SWORD_in,'nodes/node_id',verbose=FALSE)
node_index= which(nodeids %in% this_river_node_ids)
#-----------------------------------------------------

#Node variables-------
#overwriting with the index limits the RAM needed
node_x=ncvar_get(SWORD_in, 'nodes/x',verbose=FALSE)[node_index]

node_y=ncvar_get(SWORD_in, 'nodes/y',verbose=FALSE)[node_index]

node_max_width=ncvar_get(SWORD_in, 'nodes/max_width',verbose=FALSE)[node_index]

node_length=ncvar_get(SWORD_in, 'nodes/node_length',verbose=FALSE)[node_index]

node_reachid=ncvar_get(SWORD_in, 'nodes/reach_id',verbose=FALSE)[node_index] #key field

node_nodeid=nodeids[node_index]


node_df=data.frame(lon=node_x,lat=node_y,node_id=node_nodeid,node_wmax=node_max_width,
                   node_length=node_length,reach_id=node_reachid)

node_df= node_df%>%
  mutate(node_UTM_x=LongLatToUTM(node_df$lon,node_df$lat,utm_zone)[,1])%>%
  mutate(node_UTM_y=LongLatToUTM(node_df$lon,node_df$lat,utm_zone)[,2])
#------------------------------------------------------

#centerline variables-------
cl_reach_ids= ncvar_get(SWORD_in, 'centerlines/reach_id',verbose=FALSE)
cl_index=which(cl_reach_ids %in% this_river_reach_ids)

cl_x=ncvar_get(SWORD_in, 'centerlines/x',verbose=FALSE)[cl_index]
cl_y=ncvar_get(SWORD_in, 'centerlines/y',verbose=FALSE)[cl_index]
cl_id=ncvar_get(SWORD_in, 'centerlines/cl_id',verbose=FALSE)[cl_index]
cl_node_id=ncvar_get(SWORD_in, 'centerlines/node_id',verbose=FALSE)[cl_index]
 

cl_df=data.frame(reach_id=cl_reach_ids[cl_index],lon=cl_x,lat=cl_y,cl_id=cl_id,node_id=cl_node_id)%>%
  filter(!is.na(lat))
  
cl_df=st_as_sf(cl_df,coords=c('lon','lat'),remove=FALSE, crs='+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')
  
cl_df=cl_df%>%
  mutate(cl_UTM_x=LongLatToUTM(cl_df$lon,cl_df$lat,utm_zone)[,1])%>%
  mutate(cl_UTM_y=LongLatToUTM(cl_df$lon,cl_df$lat,utm_zone)[,2])
#------------------------------------------------------

#Reach variables-------
#overwriting with the index limits the RAM needed
reach_x=ncvar_get(SWORD_in, 'reaches/x',verbose=FALSE)[reach_index]

reach_y=ncvar_get(SWORD_in, 'reaches/y',verbose=FALSE)[reach_index]

reach_x_max=ncvar_get(SWORD_in, 'reaches/x_max',verbose=FALSE)[reach_index] 

reach_x_min=ncvar_get(SWORD_in, 'reaches/x_min',verbose=FALSE)[reach_index] 

reach_y_max=ncvar_get(SWORD_in, 'reaches/y_max',verbose=FALSE)[reach_index] 

reach_y_min=ncvar_get(SWORD_in, 'reaches/y_min',verbose=FALSE)[reach_index] 

reach_length=ncvar_get(SWORD_in, 'reaches/reach_length',verbose=FALSE)[reach_index]

reach_reachid=reachids[reach_index]

reach_df=data.frame(lon=reach_x,lat=reach_y,reach_id=reach_reachid,reach_xmax=reach_x_max,reach_xmin=reach_x_min,
                    reach_ymax=reach_y_max,reach_ymin=reach_y_min, reach_length=reach_length)

spatial_reach=reach_df

spatial_reach=spatial_reach%>%
  mutate(reach_UTM_x=LongLatToUTM(spatial_reach$lon,spatial_reach$lat,utm_zone)[,1])%>%
  mutate(reach_UTM_y=LongLatToUTM(spatial_reach$lon,spatial_reach$lat,utm_zone)[,2])


spatial_reach=spatial_reach%>%
  mutate(xmin=-buffer + as.numeric(LongLatToUTM(spatial_reach$reach_xmin,spatial_reach$reach_ymin,utm_zone)[,1]))%>%
  mutate(xmax=buffer + as.numeric(LongLatToUTM(spatial_reach$reach_xmax,spatial_reach$reach_ymin,utm_zone)[,1]))%>%
  mutate(ymin=-buffer + as.numeric(LongLatToUTM(spatial_reach$reach_xmin,spatial_reach$reach_ymin,utm_zone)[,2]))%>%
  mutate(ymax=buffer + as.numeric(LongLatToUTM(spatial_reach$reach_xmin,spatial_reach$reach_ymax,utm_zone)[,2]))


make_reach_polys=function(spatial_reach){
  reach_polygon_list= st_polygon( list(rbind(c(spatial_reach['xmin'],spatial_reach['ymin']),
                                             
                                             c(spatial_reach['xmin'],spatial_reach['ymax']),
                                             c(spatial_reach['xmax'],spatial_reach['ymax']),
                                             c(spatial_reach['xmax'],spatial_reach['ymin']),
                                             c(spatial_reach['xmin'],spatial_reach['ymin']) )))}

reach_poly_list=apply(spatial_reach, 1,make_reach_polys )

spatial_reach=spatial_reach%>%
  mutate(geometry=reach_poly_list)

spatial_reach=st_as_sf(spatial_reach,sf_column_name='geometry')
st_crs(spatial_reach)= paste0('+proj=utm +zone=',utm_zone,' +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs')

#--------
  

 # points(node_df$lon,node_df$lat,col='red')

node_cls=node_df%>%
  left_join(cl_df,by=c('node_id','reach_id'))%>%
  group_by(node_id)%>%
  filter(cl_id == min(cl_id) | cl_id == max(cl_id)) %>%
  mutate(node_angle= atan((cl_UTM_y[cl_id == min(cl_id)]-cl_UTM_y[cl_id == max(cl_id)])/(cl_UTM_x[cl_id == min(cl_id)]-cl_UTM_x[cl_id == max(cl_id)])))%>%
  dplyr::select(-geometry)%>%
  #make a polygon geometry- 4 points, with the first point repeated for 5 total points
  mutate(point1_x=  cl_UTM_x[cl_id == min(cl_id)] -cos(node_angle-(90*pi/180))*node_wmax*scale_maxwidth, 
         point1_y=  cl_UTM_y[cl_id == min(cl_id)] +sin(node_angle-(90*pi/180))*node_wmax*scale_maxwidth,
         point2_x=  cl_UTM_x[cl_id == max(cl_id)] -cos(node_angle-(90*pi/180))*node_wmax*scale_maxwidth,
         point2_y=  cl_UTM_y[cl_id == max(cl_id)] +sin(node_angle-(90*pi/180))*node_wmax*scale_maxwidth,
         point3_x=  cl_UTM_x[cl_id == max(cl_id)] +cos(node_angle-(90*pi/180))*node_wmax*scale_maxwidth,
         point3_y=  cl_UTM_y[cl_id == max(cl_id)] -sin(node_angle-(90*pi/180))*node_wmax*scale_maxwidth,
         point4_x=  cl_UTM_x[cl_id == min(cl_id)] +cos(node_angle-(90*pi/180))*node_wmax*scale_maxwidth,
         point4_y=  cl_UTM_y[cl_id == min(cl_id)] -sin(node_angle-(90*pi/180))*node_wmax*scale_maxwidth) %>% 
  ungroup()


make_polys= function(node_df){
  
  polygon_list= st_polygon( list(rbind(c(node_df['point1_x'],node_df['point4_y']),
                                       c(node_df['point2_x'],node_df['point3_y']),
                                       c(node_df['point3_x'],node_df['point2_y']),
                                       c(node_df['point4_x'],node_df['point1_y']),
                                       c(node_df['point1_x'],node_df['point4_y']) )))
  
}
  
  poly_list=apply(node_cls, 1,make_polys )
  
  spatial_node_cls=node_cls%>%
    mutate(poly_list=poly_list)

  spatial_node_cls=st_as_sf(spatial_node_cls,sf_column_name='poly_list')
  st_crs(spatial_node_cls)= paste0('+proj=utm +zone=',utm_zone,' +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs')
  
  #ok, now we have a fully spatial object with polygons per node. There are two of them as an artifact of the centerline process, 
  #so let's trim one
  
  final_node_df= spatial_node_cls%>%
    group_by(node_id)%>%
    filter(row_number()==1)
  
  #ok. now, if we have a classified image we want to use for a node, we should use it instead
  #set implicitly by passing the argument
  
  geometry_saver=data.frame(transmute(final_node_df,geometry=poly_list))

  
  #make drifts spatial for point-in-polygon operation
  spatial_drift=st_as_sf(drift_in,coords=c('UTM_x','UTM_y'),
                         crs=paste0('+proj=utm +zone=',utm_zone,' +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs') ) 
  
  
  
  #try a nearest approach
  spatial_node_centers=st_as_sf(data.frame(node_id=as.character(node_nodeid),reach_id=as.character(node_reachid),
                                           node_x=LongLatToUTM(node_x,node_y,utm_zone)[,1],node_y=LongLatToUTM(node_x,node_y,utm_zone)[,2]),
           coords = c('node_x','node_y'),
           crs=paste0('+proj=utm +zone=',utm_zone,' +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs') ) 
  
  #now find nearest point
  # for A, B, returns index of B closes to each point in A
  index_of_closest_cl=st_nearest_feature(spatial_drift,spatial_node_centers)
  
  nearest_averaged=spatial_drift%>%
    mutate(closest_node_id=spatial_node_centers[index_of_closest_cl,]$node_id)%>%
    mutate(closest_reach_id=spatial_node_centers[index_of_closest_cl,]$reach_id)%>%
  group_by(closest_node_id)%>%
    filter(n()>5)%>%#nifty syntax to drop gropus smaller than 5
    summarize(node_wse_m=mean(gnss_wse,na.rm=TRUE),
              node_wse_precision_m=0.05,
              mean_time_UTC=mean(gnss_time_UTC),
              node_height_above_ellipsoid=mean(height_above_ellipsoid,na.rm=TRUE),
              reach_id=closest_reach_id[1],
              drift_id=drift_id[1],
              sample_points=n())%>%
    ungroup()%>% #we've averaged nodes, so now ungroup and repeat by reach
    group_by(reach_id)%>%
    summarize(closest_reach_wse_m=mean(node_wse_m,na.rm=TRUE),
              closest_reach_wse_sd=sd(node_wse_m,na.rm=TRUE),
              reach_wse_precision_m=0.05,
              closest_mean_time_UTC=mean(mean_time_UTC),
              reach_id=reach_id[1],
              drift_id=drift_id[1],
              closest_n_nodes_used=n())%>%
    st_drop_geometry()
  

  
 box_averaged=st_intersection(spatial_drift,final_node_df)%>%
    mutate(node_id=as.character(node_id))%>%
    mutate(reach_id=as.character(reach_id))%>%
    group_by(node_id)%>%
    filter(n()>5)%>%#nifty syntax to drop gropus smaller than 5
    summarize(node_wse_m=mean(gnss_wse,na.rm=TRUE),
                node_wse_precision_m=0.05,
                mean_time_UTC=mean(gnss_time_UTC),
                node_height_above_ellipsoid=mean(height_above_ellipsoid,na.rm=TRUE),
                reach_id=reach_id[1],
                drift_id=drift_id[1],
                sample_points=n())%>%
    ungroup()%>% #we've averaged nodes, so now ungroup and repeat by reach
    group_by(reach_id)%>%
    summarize(box_reach_wse_m=mean(node_wse_m,na.rm=TRUE),
              box_reach_wse_sd=sd(node_wse_m,na.rm=TRUE),
              box_reach_wse_precision_m=0.05,
              box_mean_time_UTC=mean(mean_time_UTC),
              reach_id=reach_id[1],
              drift_id=drift_id[1],
              box_n_nodes_used=n())%>%
    st_drop_geometry() #make it a regular dataframe so we can join
    

#now, grab the reach average from the dataframes to compare. Use the drift ID field as a key.
  # need to match both reach and drift id, and the drift id is buried in teh dataframe, so open
  # the reach file direct
  
  biased_drift=read.csv(dataframe_file)%>%
    mutate(reach_id=as.character(reach_id))%>%
    left_join(box_averaged,by=c('reach_id','drift_id'))%>%
    filter(!is.na(box_n_nodes_used))%>%
    left_join(nearest_averaged,by=c('reach_id','drift_id'))%>%
    mutate(box_vs_nearest=closest_reach_wse_m - box_reach_wse_m)%>%
    mutate(box_vs_point=mean_reach_drift_wse_m-box_reach_wse_m)%>%
    mutate(nearest_vs_point=mean_reach_drift_wse_m-closest_reach_wse_m)
  
  
 return(biased_drift)
  
 }#end function
  
 