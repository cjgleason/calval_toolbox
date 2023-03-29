### Created on Nov 1, 2022 by Fangfang Yao (fangfang.yao@colorado.edu) ###
###Purpose: delineating water masks and calculating reach/node water areas for SWOT rivers using Pleiades images
#
#
###########
#utm_zone (based on imagery)


calculate_area_from_imagery=function(input_list,
                                     image_list,
                                     utm_zone,
                                     datetime,
                                     scale_maxwidth, 
                                     SWORD_path,
                                     water_index_threshold,
                                     dir_output){

print(input_list)
    this_river_reach_ids=input_list$reach
    this_river_node_ids=input_list$node
    reach_or_node=input_list$reach_or_node

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

if(reach_or_node=='node'){
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
  select(-geometry)%>%
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


spatial_node_df=st_as_sf(spatial_node_df,sf_column_name='poly_list')
st_crs(spatial_node_df)= paste0('+proj=utm +zone=',utm_zone,' +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs')

  spatial_object=spatial_node_df
}else{

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
  spatial_object=spatial_reach  

}


# #read in image and classify------
    
classify_water=function(Inputimagefile,spatial_object,
                        water_index_threshold){
    
    #print(Inputimagefile)
band2 = raster(Inputimagefile,band=2)
band4 = raster(Inputimagefile,band=4)

# xmin_col=colFromX(band2,extent(spatial_node_df)@xmin)
# xmax_col=colFromX(band2,extent(spatial_node_df)@xmax)
# ymin_row=rowFromY(band2,extent(spatial_node_df)@ymin)
# ymax_row=rowFromY(band2,extent(spatial_node_df)@ymax)
#this gives us a four element vector that starts at xmin ymin and goes to xmax ymax

band2_raster=crop(band2,spatial_object)
band4_raster=crop(band4,spatial_object)

ndwi = (band2_raster - band4_raster)/(band2_raster + band4_raster) 

#ndwi=mask(ndwi,spatial_object)
    
ndwi[ndwi[] >= water_index_threshold] = 1 # water pixel
ndwi[ndwi[] < water_index_threshold] = 0 # non-water pixel

labelled_ndwi=clump(ndwi)
#select the largest
label_counts=data.frame(raster::freq(labelled_ndwi))%>%
    filter(value!= 'NA')%>%
    filter(count==max(count))

labelled_ndwi[labelled_ndwi[]!=label_counts$value]=0
labelled_ndwi[labelled_ndwi[]==label_counts$value]=1
    
    image_name=substr(sub('.*\\/', '', Inputimagefile),1,nchar(sub('.*\\/', '', Inputimagefile))-4)
# export water mask in a raster format
writeRaster(labelled_ndwi, 
            paste0(dir_output,'raster/','mask_',this_river_reach_ids,'_',this_river_node_ids,'_', image_name,'.tif'),
           overwrite=TRUE)

# plot(labelled_ndwi)

total_cells=label_counts$count
x_size=res(labelled_ndwi)[1]
y_size=res(labelled_ndwi)[2]

water_area= total_cells*x_size*y_size



#write data frame
if(reach_or_node=='node'){
output=data.frame(node_id=this_river_node_ids,reach_id=this_river_reach_ids,water_area_m2=water_area,
                     image_name=image_name,datetime=datetime)
    }else{
    output=data.frame(reach_id=this_river_reach_ids,water_area_m2=water_area,
                     image_name=image_name,datetime=datetime)
    }


    
    } #end image function
    
output=do.call(rbind,lapply(image_list,classify_water,spatial_object= spatial_object,  water_index_threshold=water_index_threshold)    )
    

    } #end total function




