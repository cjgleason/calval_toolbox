calculate_slope_wse_fromdrift=function(SWORD_path,drift_directory,PT_directory,output_directory,this_river_reach_ids,this_river_node_ids,
                                       utm_zone, buffer,rivername, photo_path=NULL){
  
  library(sf)
  library(dplyr)
  library(rgdal)
  library(ncdf4)
  library(stringr)

LongLatToUTM<-function(x,y,zone){
  xy <- data.frame(id = 1:length(x), X = x, Y = y)
  coordinates(xy) <- c("X", "Y")
  proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")  ## for example
  res <- spTransform(xy, CRS(paste("+proj=utm +zone=",zone," ellps=WGS84",sep='')))
  res=st_as_sf(res)
  return(data.frame(x=st_coordinates(res)[,1],y=st_coordinates(res)[,2]))
}


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





spatial_reach=st_as_sf(reach_df,coords=c('lon','lat'),remove=FALSE, crs='+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')

spatial_reach=spatial_reach%>%
  mutate(reach_UTM_x=LongLatToUTM(spatial_reach$lon,spatial_reach$lat,utm_zone)[,1])%>%
  mutate(reach_UTM_y=LongLatToUTM(spatial_reach$lon,spatial_reach$lat,utm_zone)[,2])

spatial_reach=spatial_reach%>%
  mutate(xmin=LongLatToUTM(spatial_reach$reach_xmin,spatial_reach$reach_ymin,utm_zone)[,1])%>%
  mutate(xmax=LongLatToUTM(spatial_reach$reach_xmax,spatial_reach$reach_ymin,utm_zone)[,1])%>%
  mutate(ymin=LongLatToUTM(spatial_reach$reach_xmin,spatial_reach$reach_ymin,utm_zone)[,2])%>%
  mutate(ymax=LongLatToUTM(spatial_reach$reach_xmin,spatial_reach$reach_ymax,utm_zone)[,2])
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

#calculate node wse----------------------------------
calc_node_wse=function(drift_file,node_df,cl_df,zone,photo_path){

  
  drift_in=read.csv(drift_file,header=TRUE,stringsAsFactors = FALSE)%>%
      mutate(UTM_x=LongLatToUTM(gnss_Lon,gnss_Lat,zone)[,1])%>%
      mutate(UTM_y=LongLatToUTM(gnss_Lon,gnss_Lat,zone)[,2])%>%
      mutate(gnss_time_UTC=as.POSIXct(gnss_time_UTC))#needed as when it gets written to csv it becomes not a posix object
  

  node_cls=node_df%>%
    left_join(cl_df,by=c('node_id','reach_id'))%>%
    group_by(node_id)%>%
    filter(cl_id == min(cl_id) | cl_id == max(cl_id)) %>%
    mutate(node_angle= atan((cl_UTM_y[cl_id == min(cl_id)]-cl_UTM_y[cl_id == max(cl_id)])/(cl_UTM_x[cl_id == min(cl_id)]-cl_UTM_x[cl_id == max(cl_id)])))%>%
    select(-geometry)%>%
    #make a polygon geometry- 4 points, with the first point repeated for 5 total points
    mutate(point1_x= node_UTM_x+cos(node_angle)*node_length/2, 
           point1_y= node_UTM_y+sin(node_angle)*node_length/2,
           point2_x=node_UTM_x -cos(node_angle)*node_length/2,
           point2_y=node_UTM_y- sin(node_angle)*node_length/2,
           point3_x=node_UTM_x +cos(node_angle-(90*pi/180))*node_wmax*2,
           point3_y=node_UTM_y+ sin(node_angle-(90*pi/180))*node_wmax*2,
           point4_x=node_UTM_x -cos(node_angle-(90*pi/180))*node_wmax*2,
           point4_y= node_UTM_y-sin(node_angle-(90*pi/180))*node_wmax*2) %>%
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
  st_crs(spatial_node_cls)= paste0('+proj=utm +zone=',zone,' +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs')
  

  plot(spatial_node_cls$poly_list[1:50],col='red')
  
  #ok, now we have a fully spatial object with polygons per node. There are two of them as an artifact of the centerline process, 
  #so let's trim one
  
  final_node_df= spatial_node_cls%>%
    group_by(node_id)%>%
    filter(row_number()==1)
  
  #ok. now, if we have a classified image we want to use for a node, we should use it instead
  #set implicitly by passing the argument
  

  if(!is.null(photo_path)){
    #find the node IDs in that photo path
    #REGEX form STR1(.*?)STR2 finds between them. the \\ is an escape for the .
    #str_extract_all(x4,"(?<=STR1).+(?=STR2)")
    photo_nodes=unlist(str_extract_all(list.files(photo_path,pattern= 'Node_*(.*?)*\\.shp'), '(?<=Node_).+(?=\\_water)'))
    
    shapefile_df=do.call(rbind,lapply(list.files(photo_path,pattern= 'Node_*(.*?)*\\.shp',full.names = TRUE),read_sf))%>%
    transmute(node_id= photo_nodes,
                geometry=geometry)
    
    row_index=which(final_node_df$node_id %in%  photo_nodes)
    
    final_node_df$poly_list[row_index]=shapefile_df$geometry
  }
  
  geometry_saver=data.frame(transmute(final_node_df,geometry=poly_list))

  
  #make drifts spatial for point-in-polygon operation
  spatial_drift=st_as_sf(drift_in,coords=c('UTM_x','UTM_y'),crs=paste0('+proj=utm +zone=',zone,' +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs') ) 
  points_in_node=st_intersection(spatial_drift,final_node_df)%>%
    group_by(node_id)%>%
    summarize(node_wse=mean(gnss_wse,na.rm=T),
              node_wse_precision_m=0.05,# JPL wants precision, not variance. sd(gnss_wse,na.rm=T),
              time=mean(gnss_time_UTC),
              reach_id=reach_id[1],
              drift_id=drift_id[1],
              node_box_x_max=node_box_x_max[1], 
              node_box_x_min=node_box_x_min[1], 
              node_box_y_max=node_box_y_max[1], 
              node_box_y_min=node_box_y_min[1]
              )%>%
   left_join(geometry_saver,by='node_id')
  
    

}
#------------------------------------------------------

#calculate reach wse and slope from the drifts-----------------------
calc_reach_stats=function(drift_file,spatial_reach, buffer,cl_df,zone,this_river_reach_ids){
  

  drift_in=read.csv(drift_file,header=TRUE,stringsAsFactors = FALSE)%>%
    mutate(UTM_x=LongLatToUTM(gnss_Lon,gnss_Lat,zone)[,1])%>%
    mutate(UTM_y=LongLatToUTM(gnss_Lon,gnss_Lat,zone)[,2])%>%
    mutate(gnss_time_UTC=as.POSIXct(gnss_time_UTC))#needed as when it gets written to csv it becomes not a posix object
  
  calc_wse_stats=function(reach_id_search, drift_in,spatial_reach,buffer,this_river_reach_ids){
    
    
    reach_id_filtered= filter(spatial_reach,reach_id==reach_id_search)
    relevant_drift_index= which(  drift_in$gnss_Lat>reach_id_filtered$reach_ymin &
                                  drift_in$gnss_Lat<reach_id_filtered$reach_ymax &
                                  drift_in$gnss_Lon>reach_id_filtered$reach_xmin &
                                  drift_in$gnss_Lon<reach_id_filtered$reach_xmax)
    
    filtered_drift=drift_in[relevant_drift_index,]
    
    if (nrow(filtered_drift)==0){
      output=data.frame(reach_id=reach_id_search,
                        wse_bar=NA,
                        wse_precision=NA,
                        wse_start= as.POSIXct('2000-01-01 12:00:00'),
                        wse_end= as.POSIXct('2000-01-01 12:00:00'),
                        slope= NA,
                        slope_precision=NA,
                        drift_id=drift_file)
      
      return(output)
    }

    reach_wse_bar_m=mean(drift_in$gnss_wse[relevant_drift_index],na.rm=T)
    reach_wse_precision_m =0.05 # JPL wants precision, not variance. sd(drift_in$gnss_wse[relevant_drift_index],na.rm=T)
    wse_time_start= min(drift_in$gnss_time_UTC[relevant_drift_index])
    wse_time_end=max(drift_in$gnss_time_UTC[relevant_drift_index])
    
    #take points within some distance of xmin, ymin, xmax, ymax to define the slope
    #we do not use the points in the middle
    #we have a 'buffer' variable for this
    
  this_cl=filter(cl_df,reach_id==reach_id_search)
  cl_start=this_cl[1,]
  cl_end=this_cl[nrow(this_cl),]
  
  cl_start_search_x1=cl_start$cl_UTM_x-buffer
  cl_start_search_x2=cl_start$cl_UTM_x+buffer
  cl_end_search_x1=cl_end$cl_UTM_x-buffer
  cl_end_search_x2=cl_end$cl_UTM_x+buffer
  
  cl_start_search_y1=cl_start$cl_UTM_y-buffer
  cl_start_search_y2=cl_start$cl_UTM_y+buffer
  cl_end_search_y1=cl_end$cl_UTM_y-buffer
  cl_end_search_y2=cl_end$cl_UTM_y+buffer
  
  cl_distance= reach_id_filtered$reach_length
 
  #now we can find the points that are within the drift file near the end points of the reach
  #this has the benefit of pulling in data from the reach above and below, if they exist, as it is 
  #a pure geospatial search. This aligns with JPL practice

  slope_start_index= which(  drift_in$UTM_x> cl_start_search_x1 &
                             drift_in$UTM_x< cl_start_search_x2 &
                             drift_in$UTM_y> cl_start_search_y1 &
                             drift_in$UTM_y< cl_start_search_y2 )  
  
  slope_end_index= which(      drift_in$UTM_x> cl_end_search_x1 &
                               drift_in$UTM_x< cl_end_search_x2 &
                               drift_in$UTM_y> cl_end_search_y1 &
                               drift_in$UTM_y< cl_end_search_y2 )  


  slope_start_elevations= drift_in[slope_start_index,]$gnss_wse
  slope_end_elevations= drift_in[slope_end_index,]$gnss_wse
  
  slope= (mean(slope_start_elevations)- mean(slope_end_elevations)) / cl_distance
  slope_sd= (sqrt(sd(slope_start_elevations)^2+sd(slope_end_elevations)^2)) / cl_distance

  if (is.na(slope_sd)){wse_bar=NA}#kluge that tells us the whole reach wasn't floated
    


    output=data.frame(reach_id=reach_id_search,
                      wse_bar=reach_wse_bar_m,
                      wse_precision=reach_wse_precision_m,
                      wse_start= wse_time_start,
                      wse_end= wse_time_end,
                      slope= slope,
                      slope_precision=slope_sd,
                      drift_id=drift_file)
    
    #print(output)
  }
  
  
  reach_ids=this_river_reach_ids
  
  output1 = do.call(rbind,lapply(reach_ids,calc_wse_stats,drift_in=drift_in,spatial_reach=spatial_reach,buffer=buffer))%>%
     filter(!is.na(wse_bar))
  

  
}
#------------------------------------------------------


drifts = paste0(drift_directory,list.files(drift_directory))

node_wses=do.call(rbind,lapply(drifts,calc_node_wse,node_df=node_df,cl_df=cl_df,zone=utm_zone,photo_path=photo_path))%>%
  mutate(node_id=format(node_id,scientific=FALSE))%>%
  mutate(geometry= geometry.x)%>%
  as.data.frame()%>%
  select(-geometry.x,-geometry.y,-poly_list)

node_geom=as.data.frame(node_wses)%>%
  select(node_id,node_box_x_min,node_box_x_max,node_box_y_min,node_box_y_max,geometry)

node_wses=as.data.frame(node_wses)%>%
  transmute(time_UTC=time,node_id=node_id,drift_id=drift_id,mean_node_pt_wse_m=node_wse,mean_node_pt_wse_precision_m=node_wse_precision_m)


reach_stats=do.call(rbind,lapply(drifts,calc_reach_stats,spatial_reach=spatial_reach,
                                 buffer=buffer,cl_df=cl_df,zone=utm_zone,this_river_reach_ids=this_river_reach_ids))%>%
  mutate(reach_id=format(reach_id,scientific = FALSE))%>%
  filter(!is.na(slope_precision))%>%
  transmute(reach_id=reach_id,mean_reach_drift_wse_m=wse_bar,mean_reach_drift_wse_precision_m=wse_precision,wse_drift_start_UTC=wse_start,
            wse_drift_end_UTC=wse_end, reach_drift_slope_m_m=slope,reach_drift_slope_precision_m=slope_precision,drift_id=drift_id)

write.csv(node_geom,paste0(output_directory,'node/',rivername,'_drift_node_geom.csv'))
write.csv(node_wses,paste0(output_directory,'node/',rivername,'_drift_node_wses.csv'))
write.csv(reach_stats,paste0(output_directory,'reach/',rivername,'_drift_reach_wse_slope.csv'))

 }