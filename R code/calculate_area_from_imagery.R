


calculate_area_from_imagery=function(image,
                                     this_river_reach_ids,
                                     this_river_node_ids,
                                     rivercode,
                                     utm_zone,
                                     scale_maxwidth, 
                                     SWORD_path){


# load library 
library(raster,quietly = TRUE,warn.conflicts=FALSE)
library(rgdal, quietly = TRUE,warn.conflicts=FALSE)
library(ncdf4,quietly = TRUE,warn.conflicts=FALSE)
library(sf,quietly = TRUE,warn.conflicts=FALSE)
library(dplyr, quietly = TRUE,warn.conflicts=FALSE)
library(sp,quietly = TRUE,warn.conflicts=FALSE)


LongLatToUTM<-function(x,y,zone){
  xy <- data.frame(ID = 1:length(x), X = x, Y = y)
  coordinates(xy) <- c("X", "Y")
  proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")  ## for example
  res <- spTransform(xy, CRS(paste("+proj=utm +zone=",zone," ellps=WGS84",sep='')))
  res=st_as_sf(res)
  return(data.frame(x=st_coordinates(res)[,1],y=st_coordinates(res)[,2]))
}


#--------------------------------

#read in reaches and nodes, make spatial----------------------------------
SWORD_in= nc_open(SWORD_path,verbose=FALSE)

reachids=ncvar_get(SWORD_in, 'reaches/reach_id',verbose=FALSE)
reach_index= which(reachids %in% this_river_reach_ids)

nodeids=ncvar_get(SWORD_in,'nodes/node_id',verbose=FALSE)
node_index= which(nodeids %in% this_river_node_ids)
    
node_widths= ncvar_get(SWORD_in,'nodes/max_width',verbose=FALSE)[node_index]
reach_widths=ncvar_get(SWORD_in,'reaches/max_width',verbose=FALSE)[reach_index]
    
#centerline variables-------
cl_reach_ids= ncvar_get(SWORD_in, 'centerlines/reach_id',verbose=FALSE)
cl_index=which(cl_reach_ids %in% this_river_reach_ids)
    
#print(this_river_reach_ids)

cl_x=ncvar_get(SWORD_in, 'centerlines/x',verbose=FALSE)[cl_index]
cl_y=ncvar_get(SWORD_in, 'centerlines/y',verbose=FALSE)[cl_index]
cl_id=ncvar_get(SWORD_in, 'centerlines/cl_id',verbose=FALSE)[cl_index]
cl_node_id=ncvar_get(SWORD_in, 'centerlines/node_id',verbose=FALSE)[cl_index]

cl_df=data.frame(reach_id=cl_reach_ids[cl_index],lon=cl_x,lat=cl_y,cl_id=cl_id,node_id=cl_node_id)%>%
  filter(!is.na(lat))

cl_df=cl_df%>%
  mutate(cl_UTM_x=LongLatToUTM(cl_df$lon,cl_df$lat,utm_zone)[,1])%>%
  mutate(cl_UTM_y=LongLatToUTM(cl_df$lon,cl_df$lat,utm_zone)[,2])
  
cl_df=st_as_sf(cl_df,coords=c('cl_UTM_x','cl_UTM_y'),remove=FALSE, crs= paste0('+proj=utm +zone=',utm_zone,' +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs'))


#------------------------------------------------------

#spatial_node_df
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
  
  polygon_list= st_polygon( list(rbind(c(node_df['point1_x'],node_df['point1_y']),
                                       c(node_df['point2_x'],node_df['point2_y']),
                                       c(node_df['point3_x'],node_df['point3_y']),
                                       c(node_df['point4_x'],node_df['point4_y']),
                                       c(node_df['point1_x'],node_df['point1_y']) )))
  
}

poly_list=apply(node_cls, 1,make_polys )

spatial_node_df=node_cls%>%
  mutate(poly_list=poly_list)


spatial_node=st_as_sf(spatial_node_df,sf_column_name='poly_list')
st_crs(spatial_node)= paste0('+proj=utm +zone=',utm_zone,' +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs')




#spatial_reach
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
  mutate(xmin= as.numeric(LongLatToUTM(spatial_reach$reach_xmin,spatial_reach$reach_ymin,utm_zone)[,1]))%>%
  mutate(xmax= as.numeric(LongLatToUTM(spatial_reach$reach_xmax,spatial_reach$reach_ymin,utm_zone)[,1]))%>%
  mutate(ymin= as.numeric(LongLatToUTM(spatial_reach$reach_xmin,spatial_reach$reach_ymin,utm_zone)[,2]))%>%
  mutate(ymax= as.numeric(LongLatToUTM(spatial_reach$reach_xmin,spatial_reach$reach_ymax,utm_zone)[,2]))


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



# #read in image 
image_in=raster(image)
#crop to shapefile
    #first, crop to buffered centerline to eliminate non-river h20 if it exists
clbuffer=st_buffer(cl_df,max(reach_widths)*scale_maxwidth)
    
buffered_cl_image=mask(crop(image_in,clbuffer),clbuffer)
xcell=res(buffered_cl_image)[1]
ycell=res(buffered_cl_image)[2]
    

    
node_table=data.frame(node_area=unlist(lapply(extract(buffered_cl_image, spatial_node),sum,na.rm=T)),node_id=spatial_node$node_id)%>%
    mutate(node_area_m2=node_area*xcell*ycell)

reach_table=data.frame(reach_area=unlist(lapply(extract(buffered_cl_image, spatial_reach),sum,na.rm=T)),reach_id=spatial_reach$reach_id)%>%
    mutate(reach_area_m2=reach_area*xcell*ycell)    
    
    image_name=substr(sub('.*\\/', '', image),1,nchar(sub('.*\\/', '', image))-4)

write.csv(node_table,paste0(dir_output,rivername,'_',image_name,'_node_areas.csv'),row.names=FALSE)
write.csv(reach_table,paste0(dir_output,rivername,'_',image_name,'_reach_areas.csv'),row.names=FALSE)

    

    } #end total function



