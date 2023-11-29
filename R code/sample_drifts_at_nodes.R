
library(ncdf4)
library(sf,lib.loc='/nas/cee-water/cjgleason/r-lib/')
  library(dplyr,lib.loc='/nas/cee-water/cjgleason/r-lib/')
  
  #library(stringr,lib.loc='/nas/cee-water/cjgleason/r-lib/')



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


utm_zone=18

scale_maxwidth=5

# OG sample
# drift_file= '/nas/cee-water/cjgleason/calval/Processed data/UMass/Munged drifts/reprocessed_2023_11_02/SWOTCalVal_CR_GNSS_L2_Rec1_20230525T134919_20230525T201724_20230530T224448_1.csv'
# install file
# drift_file='/nas/cee-water/cjgleason/calval/Processed data/UMass/Munged drifts/reprocessed_2023_11_01/SWOTCalVal_CR_GNSS_L2_Rec1_20230405T130005_20230405T204252_20230413T000135_1.csv'
# xx file
drift_file='/nas/cee-water/cjgleason/calval/Processed data/UMass/Munged drifts/reprocessed_2023_11_02/SWOTCalVal_CR_GNSS_L2_Rec1_20230526T132036_20230526T192852_20230530T232646_2.csv'

drift_in=read.csv(drift_file,header=TRUE,stringsAsFactors = FALSE)%>%
  filter(!is.na(gnss_Lon))%>%
  filter(!is.na(gnss_Lat))%>%
  mutate(UTM_x=LongLatToUTM(gnss_Lon,gnss_Lat,zone)[,1])%>%
  mutate(UTM_y=LongLatToUTM(gnss_Lon,gnss_Lat,zone)[,2])%>%
  mutate(gnss_time_UTC=as.POSIXct(gnss_time_UTC))#needed as when it gets written to csv it becomes not a posix object

#plot(drift_in$gnss_Lon,drift_in$gnss_Lat)

#open the key files   
read_keys=function(keyfile){
  this_key= read.csv(keyfile,stringsAsFactors=FALSE, na.strings = c("","NA","na","NaN", " "))%>%
    mutate(keyid=keyfile)%>%
    mutate(pt_serial=as.integer(PT_Serial))
}

 PT_key_file=paste0('/nas/cee-water/cjgleason/calval/Processed data/UMass/',c('SWOTCalVal_CR_Key_20230322_20230614.csv',
              'SWOTCalVal_CR_Key_20230516_20230613.csv')) #CT
 
master_key= do.call(rbind,lapply(PT_key_file,read_keys))

domain=read.csv('/nas/cee-water/cjgleason/calval/Processed data/UMass/CR_domain.csv')
this_river_reach_ids= unique(domain$Reach_ID)
this_river_node_ids= unique(domain$Node_ID)
continent='na'

SWORD_path=paste0('/nas/cee-water/cjgleason/calval/SWORD_15/netcdf/',continent,
                  '_sword_v15.nc')

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

buffer=500
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
  
  points_in_node=st_intersection(spatial_drift,final_node_df)%>%
    group_by(node_id)%>%
    mutate(node_id=as.character(node_id))%>%
    mutate(reach_id=as.character(reach_id))





   
       plot(st_geometry(points_in_node[1:575,]) ,axes=TRUE,graticule=TRUE)
       plot(st_geometry(final_node_df['node_id']),add=TRUE,col=sf.colors(alpha=0.2))
       plot(st_geometry(spatial_reach),add=TRUE,col=sf.colors(alpha=0.05))
       plot(points_in_node['node_id'],add=TRUE)

       
       
  count_it=points_in_node%>%
    group_by(node_id,reach_id)%>%
    summarize(points_per_node=n())%>%
    filter(points_per_node>5)%>%#floor%>%
    ungroup()%>%# summarize takes the first group, so we're good to drop and regroup by reach
    group_by(reach_id)%>%
    summarize(min_points_node_per_reach=min(points_per_node,na.rm=TRUE))%>%
    as.data.frame()%>%
    select(-geometry)
    

sample_by_group=function(this_reach_id,grouped_dataframe,count_it){
  
# this_reach_id=count_it$reach_id[1]
# grouped_dataframe=points_in_node

this_reach=grouped_dataframe%>%
  filter(reach_id==this_reach_id)%>%
  left_join(count_it,by='reach_id')%>%
  group_by(node_id)%>%
  filter(n()>5)%>%#nifty syntax to drop gropus smaller than 5
  slice_sample(n=filter(count_it,reach_id==this_reach_id)$min_points_node_per_reach) %>% #since we're joining one reahc at a time, they're equal
  summarize(node_wse_m=mean(gnss_wse,na.rm=TRUE),
            node_wse_precision_m=0.05,# JPL wants precision, not variance. sd(gnss_wse,na.rm=T),
            mean_time_UTC=mean(gnss_time_UTC),
            node_height_above_ellipsoid=mean(height_above_ellipsoid,na.rm=TRUE),
            reach_id=reach_id[1],
            drift_id=drift_id[1],
            sample_points=n())
  
}


node_average_from_sample=do.call(rbind,lapply(count_it$reach_id,sample_by_group,
                             grouped_dataframe=points_in_node,count_it=count_it))

   
  
   reach_avarege_from_sampled_node=node_average_from_sample%>%
     ungroup()%>%
     group_by(reach_id)%>%
     summarize(reach_average_wse_m=mean(node_wse_m,na.rm=TRUE),
               node_wse_precision_m=0.05,
               sample_points=sample_points[1],
               reach_wse_sd_m=sd(node_wse_m,na.rm=TRUE),
               mean_time_UTC=mean(mean_time_UTC),
               reach_id=reach_id[1],
               drift_id=drift_id[1],
               sample_points=sample_points[1])
    
   
   dataframe=read.csv('/nas/cee-water/cjgleason/calval/Processed data/UMass/Data frames/reprocessed_2023_11_02/reach/CR_drift_reach_wse_slope.csv')
   
test=filter(dataframe,wse_drift_start_UTC > '2023-05-26 15:00:00')%>%
  filter(wse_drift_end_UTC < '2023-05-26 23:50:00')%>%
  mutate(reach_id=as.character(reach_id))

 
print(reach_avarege_from_sampled_node$reach_average_wse_m[4])
    




 

