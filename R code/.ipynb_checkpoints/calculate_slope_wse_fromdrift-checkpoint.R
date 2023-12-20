calculate_slope_wse_fromdrift=function(SWORD_path,drift_directory,PT_directory,output_directory,this_river_reach_ids,this_river_node_ids,
                                       utm_zone, buffer,rivername, photo_path=NULL, reprocess_switch, core_count, scale_maxwidth){
  
    suppressWarnings({

#this code creates sword products from drifts

  library(sf)
  library(dplyr)
  # library(rgdal)
 # library(gdal)
  library(ncdf4)
  library(stringr)
    library(parallel)
    options("rgdal_show_exportToProj4_warnings"="none")

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

  
suppressWarnings({
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
    
    })#warning suppression

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

if(!is.null(photo_path)){
  #find the node IDs in that photo path
  #REGEX form STR1(.*?)STR2 finds between them. the \\ is an escape for the .
  #str_extract_all(x4,"(?<=STR1).+(?=STR2)")
  photo_reaches=unlist(str_extract_all(list.files(photo_path,pattern= 'Reach_*(.*?)*\\.shp'), '(?<=Reach_).+(?=\\_water)'))
  
  shapefile_df_reach=do.call(rbind,lapply(list.files(photo_path,pattern= 'Reach_*(.*?)*\\.shp',full.names = TRUE),read_sf))%>%
    transmute(reach_id= photo_reaches,
              geometry=geometry)
  
  reach_row_index=which(spatial_reach$reach_id %in%  photo_reaches)
  
  spatial_reach$geometry[reach_row_index]=shapefile_df_reach$geometry
}



#------------------------------------------------------
suppressWarnings({
#centerline variables-------
cl_reach_ids= ncvar_get(SWORD_in, 'centerlines/reach_id',verbose=FALSE)
cl_index=which(cl_reach_ids %in% this_river_reach_ids)

cl_x=ncvar_get(SWORD_in, 'centerlines/x',verbose=FALSE)[cl_index]
cl_y=ncvar_get(SWORD_in, 'centerlines/y',verbose=FALSE)[cl_index]
cl_id=ncvar_get(SWORD_in, 'centerlines/cl_id',verbose=FALSE)[cl_index]
cl_node_id=ncvar_get(SWORD_in, 'centerlines/node_id',verbose=FALSE)[cl_index]
    })#warnign suppresssion

cl_df=data.frame(reach_id=cl_reach_ids[cl_index],lon=cl_x,lat=cl_y,cl_id=cl_id,node_id=cl_node_id)%>%
  filter(!is.na(lat))
  
cl_df=st_as_sf(cl_df,coords=c('lon','lat'),remove=FALSE, crs='+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')
  
cl_df=cl_df%>%
  mutate(cl_UTM_x=LongLatToUTM(cl_df$lon,cl_df$lat,utm_zone)[,1])%>%
  mutate(cl_UTM_y=LongLatToUTM(cl_df$lon,cl_df$lat,utm_zone)[,2])
#------------------------------------------------------

#calculate node wse----------------------------------
calc_node_wse=function(drift_file,node_df,cl_df,zone,photo_path,scale_maxwidth){

    library(sf)
  library(dplyr)
  # library(rgdal)
  library(ncdf4)
  library(stringr)
    
      ### Need to point to the drift files in function?? ###  
   drift_in=read.csv(drift_file,header=TRUE,stringsAsFactors = FALSE)%>% 
    filter(!is.na(gnss_Lon))%>%
    filter(!is.na(gnss_Lat))%>%
      mutate(UTM_x=LongLatToUTM(gnss_Lon,gnss_Lat,zone)[,1])%>%
      mutate(UTM_y=LongLatToUTM(gnss_Lon,gnss_Lat,zone)[,2])%>%
      mutate(gnss_time_UTC=as.POSIXct(gnss_time_UTC))#needed as when it gets written to csv it becomes not a posix object
  
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
  st_crs(spatial_node_cls)= paste0('+proj=utm +zone=',zone,' +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs')
  

  #plot(spatial_node_cls$poly_list[1:50],col='red')
  
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
              node_height_above_ellipsoid=mean(height_above_ellipsoid,na.rm=TRUE),
              reach_id=reach_id[1],
              drift_id=drift_id[1]
              # node_box_x_max=node_box_x_max[1], 
              # node_box_x_min=node_box_x_min[1], 
              # node_box_y_max=node_box_y_max[1], 
              # node_box_y_min=node_box_y_min[1]
              )%>%
   left_join(geometry_saver,by='node_id')%>%
    mutate(drift_id=drift_file)
  
    
    


}
#------------------------------------------------------

#calculate reach wse and slope from the drifts-----------------------
calc_reach_stats=function(drift_file,spatial_reach, buffer,cl_df,zone,this_river_reach_ids){
  
  library(sf)
  library(dplyr)
  # library(rgdal)
  library(ncdf4)
  library(stringr)


    
      drift_in=read.csv(drift_file,header=TRUE,stringsAsFactors = FALSE)%>%
    filter(!is.na(gnss_Lon))%>%
    filter(!is.na(gnss_Lat))%>%
    mutate(UTM_x=LongLatToUTM(gnss_Lon,gnss_Lat,zone)[,1])%>%
    mutate(UTM_y=LongLatToUTM(gnss_Lon,gnss_Lat,zone)[,2])%>%
    mutate(gnss_time_UTC=as.POSIXct(gnss_time_UTC))#needed as when it gets written to csv it becomes not a posix object
  
  calc_wse_stats=function(reach_id_search, drift_in,spatial_reach,buffer){
    
    
    reach_id_filtered= filter(spatial_reach,reach_id==reach_id_search)
 
    
    spatial_drift=st_as_sf(drift_in,coords=c('UTM_x','UTM_y'),crs=paste0('+proj=utm +zone=',zone,' +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs') ) 
    points_in_reach=st_intersection(spatial_drift,reach_id_filtered)
    
    # old aspatial code
    # 
    # relevant_drift_index= which(  drift_in$gnss_Lat>reach_id_filtered$reach_ymin &
    #                               drift_in$gnss_Lat<reach_id_filtered$reach_ymax &
    #                               drift_in$gnss_Lon>reach_id_filtered$reach_xmin &
    #                               drift_in$gnss_Lon<reach_id_filtered$reach_xmax)
    # 
    # filtered_drift=drift_in[relevant_drift_index,]
    
    if (nrow(  points_in_reach)==0){
      
   
      output=data.frame(reach_id=reach_id_search,
                        wse_bar=NA,
                        wse_precision=NA,
                        ellipsoid_height=NA,
                        wse_start= '2000-01-01 12:00:00',
                        wse_end= '2000-01-01 12:00:00',
                        slope= NA,
                        slope_precision=NA,
                        drift_id=drift_file)
      
      return(output)
    }

    ##old aspatial code
    # reach_wse_bar_m=mean(drift_in$gnss_wse[relevant_drift_index],na.rm=T)
    # reach_wse_precision_m =0.05 # JPL wants precision, not variance. sd(drift_in$gnss_wse[relevant_drift_index],na.rm=T)
    # wse_time_start= min(drift_in$gnss_time_UTC[relevant_drift_index])
    # wse_time_end=max(drift_in$gnss_time_UTC[relevant_drift_index])
    

    reach_wse_bar_m=mean( points_in_reach$gnss_wse,na.rm=T)
    reach_height_above_ellipsoid_m=mean(points_in_reach$height_above_ellipsoid,na.rm=TRUE)
    reach_wse_precision_m =0.05 # JPL wants precision, not variance. sd(drift_in$gnss_wse[relevant_drift_index],na.rm=T)
    wse_time_start= min(points_in_reach$gnss_time_UTC,na.rm=T)
    wse_time_end=max(points_in_reach$gnss_time_UTC,na.rm=T)
    


    
    #take points within some distance of xmin, ymin, xmax, ymax to define the slope
    #we do not use the points in the middle
    #we have a 'buffer' variable for this
    
  this_cl=filter(cl_df,reach_id==reach_id_search)

  cl_start=this_cl[1,]
  cl_end=this_cl[nrow(this_cl),]
  
  # plot(drift_in$gnss_Lon,drift_in$gnss_Lat,col='green')
  # points(this_cl$lon,this_cl$lat,col='black')
  # points(cl_start$lon,cl_start$lat,col='red')
  # points(cl_end$lon,cl_end$lat,col='red')
  # points( points_in_reach$gnss_Lon,points_in_reach$gnss_Lat,col='blue')

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
  
      #this convention results in a negative slope, so swithing the start and end here to make a positive number
  slope= (mean(slope_end_elevations)- mean(slope_start_elevations)) / cl_distance
  slope_sd= (sqrt(sd(slope_start_elevations)^2+sd(slope_end_elevations)^2)) / cl_distance

      
      
  if (is.na(slope_sd)){reach_wse_bar_m=NA}#kluge that tells us the whole reach wasn't floated
  
    

 
    output1=data.frame(reach_id=reach_id_search,
                      wse_bar=reach_wse_bar_m,
                      ellipsoid_height=reach_height_above_ellipsoid_m,
                      wse_precision=reach_wse_precision_m,
                      wse_start= as.character(wse_time_start),
                      wse_end= as.character(wse_time_end),
                      slope= slope,
                      slope_precision=slope_sd,
                      drift_id=drift_file)


    
    return(output1)

    
  }
  
  output2 = lapply(this_river_reach_ids,calc_wse_stats,drift_in=drift_in,spatial_reach=spatial_reach,buffer=buffer)
 
  #print(output2)
 output3=do.call(rbind,output2)%>%
     filter(!is.na(wse_bar))


  
  # output2= calc_wse_stats(this_river_reach_ids[2],drift_in,spatial_reach,buffer)

# count=0
# output2=matrix(nrow=100,ncol=8)
#  for (reachid in this_river_reach_ids){
#   temp=calc_wse_stats(reachid,drift_in,spatial_reach,buffer
#   output2[nrow(temp),]
#   
# 
#  }

  return(output3)


}
#------------------------------------------------------

print(paste0(output_directory,'node/',rivername,'_drift_node_wses.csv'))

if(reprocess_switch ==0){
    existing_node_df=read.csv(paste0(output_directory,'node/',rivername,'_drift_node_wses.csv'))
    existing_reach_df=read.csv(paste0(output_directory,'reach/',rivername,'_drift_reach_wse_slope.csv'))
    
    potential_drifts=setdiff(sub( "\\..*","", list.files(drift_directory)),unique(existing_node_df$drift_id))

    #if there is nothing new, proceed to chuck it out
    if(identical(potential_drifts,character(0))){return(NA)}
    drifts=paste0(drift_directory,potential_drifts,'.csv') #we scrubbed the .csv when we pulled them for a check
    
} else {
    drifts = paste0(drift_directory,list.files(drift_directory))
}
  
    
 # CLUSTER=makeCluster(core_count)

node_wses=do.call(rbind,lapply(drifts,calc_node_wse,node_df=node_df,cl_df=cl_df,zone=utm_zone,photo_path=photo_path, scale_maxwidth=scale_maxwidth))%>%
  mutate(node_id=format(node_id,scientific=FALSE))%>%
  mutate(geometry= geometry.x)%>%
  as.data.frame()%>%
  dplyr::select(-geometry.x,-geometry.y,-poly_list)

 # driftno=core_count

#      print(driftno)  
#     print(drifts)
# node_wses=calc_node_wse(drifts[driftno],node_df=node_df,cl_df=cl_df,zone=utm_zone,photo_path=photo_path)%>%
#   mutate(node_id=format(node_id,scientific=FALSE))%>%
#   mutate(geometry= geometry.x)%>%
#   as.data.frame()%>%
#   dplyr::select(-geometry.x,-geometry.y,-poly_list)

node_geom=as.data.frame(node_wses)%>%
  dplyr::select(node_id,geometry)

    
    
node_wses=as.data.frame(node_wses)%>% transmute(time_UTC=time,node_id=node_id,drift_id=drift_id,mean_node_drift_wse_m=node_wse,mean_node_drift_wse_precision_m=node_wse_precision_m,ellipsoid_height_m=node_height_above_ellipsoid)

reach_geom=as.data.frame(spatial_reach)%>%
  dplyr::select(reach_id,geometry)

    
    

reach_stats=do.call(rbind,lapply(drifts,calc_reach_stats,spatial_reach=spatial_reach,              buffer=buffer,cl_df=cl_df,zone=utm_zone,this_river_reach_ids=this_river_reach_ids))%>%
  mutate(reach_id=format(reach_id,scientific = FALSE))%>%
  filter(!is.na(slope_precision))%>% transmute(reach_id=reach_id,mean_reach_drift_wse_m=wse_bar,mean_reach_drift_wse_precision_m=wse_precision,wse_drift_start_UTC=wse_start,wse_drift_end_UTC=wse_end, reach_drift_slope_m_m=slope,reach_drift_slope_precision_m=slope_precision,drift_id=drift_id,ellipsoid_height_m=ellipsoid_height)
    
    
# reach_stats=calc_reach_stats(drifts[driftno],spatial_reach=spatial_reach,              buffer=buffer,cl_df=cl_df,zone=utm_zone,this_river_reach_ids=this_river_reach_ids)%>%
#   mutate(reach_id=format(reach_id,scientific = FALSE))%>%
#   filter(!is.na(slope_precision))%>% transmute(reach_id=reach_id,mean_reach_drift_wse_m=wse_bar,mean_reach_drift_wse_precision_m=wse_precision,wse_drift_start_UTC=wse_start,wse_drift_end_UTC=wse_end, reach_drift_slope_m_m=slope,reach_drift_slope_precision_m=slope_precision,drift_id=drift_id,ellipsoid_height_m=ellipsoid_height)
     # stopCluster(CLUSTER)
         
        print(paste0(output_directory,'/reach/',rivername,'_drift_reach_wse_slope.csv'))

    if(reprocess_switch ==1){
write.csv(node_geom,paste0(output_directory,'/node/',rivername,'_drift_node_geom.csv'),append=FALSE,row.names=FALSE)
write.csv(reach_geom,paste0(output_directory,'/reach/',rivername,'_drift_reach_geom.csv'),append=FALSE,row.names=FALSE)
write.csv(node_wses,paste0(output_directory,'/node/',rivername,'_drift_node_wses.csv'),append=FALSE,row.names=FALSE)
write.csv(reach_stats,paste0(output_directory,'/reach/',rivername,'_drift_reach_wse_slope.csv'),append=FALSE,row.names=FALSE)} else {
        
write.csv(node_geom,paste0(output_directory,'/node/',rivername,'_drift_node_geom.csv'),append=TRUE,row.names=FALSE)
write.csv(reach_geom,paste0(output_directory,'/reach/',rivername,'_drift_reach_geom.csv'),append=TRUE,row.names=FALSE)
write.csv(node_wses,paste0(output_directory,'/node/',rivername,'_drift_node_wses.csv'),append=TRUE,row.names=FALSE)
write.csv(reach_stats,paste0(output_directory,'/reach/',rivername,'_drift_reach_wse_slope.csv'),append=TRUE,row.names=FALSE)}
        
        })

 }

