### Created on Nov 1, 2022 by Fangfang Yao (fangfang.yao@colorado.edu) ###
###Purpose: delineating water masks and calculating reach/node water areas for SWOT rivers using Pleiades images
#
#
###########
# load library 
library(raster)
library(rgdal)
library(rgeos)
library(ncdf4)
library(sf)
library(dplyr)
library(sp)
library(spatstat)

#path of input image 
Inputimagefile = '/Volumes/CIRES_8T/Willamette2022_Pleiades/WillametteROI/PHR_2022_07_25_Willamette_32610__shifted_to__GoogleAeroport32610.tif'
#path of input SWORD data (netcdf)
SWORD_path='/Volumes/CIRES_8T/SWORD/netcdf/na_sword_v14.nc'
#output dir
dir_output = '/Volumes/CIRES_8T/Willamette2022_Pleiades/Output/'

#selected reach and nodes to be processed
this_river_reach_IDs <- c(78220000231)
this_river_node_IDs <- c(78220000230031,78220000230041,78220000230051)
  #c(78339200060171,78339200060181)

#utm_zone (based on imagery)
utm_zone = 10
#image acquisition time (based on imagery)
image_time = '11:00'

#scale max width (in cases of under/overestimated or centerline offset)
scale_maxwidth = 3

##other parameter setting
#Threshold for water mapping
water_index_threshold = 0.2
#Offset the threshold to calculate water area uncertainty
ThresholdOffset_4_uncertainty = 0.1



##############################functions begin########################
#WGS lon,lat to UTM coordinates
LongLatToUTM<-function(x,y,zone){
  xy <- data.frame(ID = 1:length(x), X = x, Y = y)
  coordinates(xy) <- c("X", "Y")
  proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")  ## for example
  res <- spTransform(xy, CRS(paste("+proj=utm +zone=",zone," ellps=WGS84",sep='')))
  res=st_as_sf(res)
  return(data.frame(x=st_coordinates(res)[,1],y=st_coordinates(res)[,2]))
}
#normalize the range within (0-360) 
NormalizeDegree_0_360<-function(degree){
  if(degree<0){
    degree = degree + 360
  }
  if(degree>360){
    degree = degree - 360
  }
  return(degree)
}
#covert radian to degree
RadiantoDegree<-function(radian){
  degree=radian*180/3.1415926
  #normalize the range within (0-360) 
  degree=NormalizeDegree_0_360(degree)
  return(degree)
}
#covert degree to radian
DegreetoRadian<-function(degree){
  radian=degree*3.1415926/180
  return(radian)
}
#get the largest component of a binary image
GetTolpix_fromlargest_binaryimage<-function(rastertemp){
  tolpix = 0
  labelpix=0
  for(labeli in min(rastertemp[!is.na(rastertemp)]):max(rastertemp[!is.na(rastertemp)])){
    if(tolpix<(sum(rastertemp[rastertemp==labeli])/labeli)){
      tolpix = sum(rastertemp[rastertemp==labeli])/labeli
      labelpix = labeli
    }
  }
  return(c(tolpix,labelpix))
}
#sum(labelregions[labelregions==1])/1
#sum(labelregions[labelregions==2])/2
#sum(labelregions[labelregions==3])/3


##############################functions end########################

#create sub-folders for different outputs
#dir.create(paste0(dir_output,'ROI'))
#dir.create(paste0(dir_output,'Raster'))
#dir.create(paste0(dir_output,'Shapefile'))
#dir.create(paste0(dir_output,'CSV'))

dir_output_ROI = paste0(dir_output,'ROI')
dir_output_Raster = paste0(dir_output,'Raster')
dir_output_Shapefile = paste0(dir_output,'Shapefile')
dir_output_CSV = paste0(dir_output,'CSV')

# read Pleiades images 
Pleiades_image = stack(Inputimagefile)

#get SWORD data---------------------------------------
SWORD_in=nc_open(SWORD_path,verbose=FALSE)

reachIDs=ncvar_get(SWORD_in, 'reaches/reach_id',verbose=FALSE)

nodeIDs=ncvar_get(SWORD_in,'nodes/node_id',verbose=FALSE)
#-----------------------------------------------------

#data frame for output variable values of interest
Water_df = data.frame()

for (eachnode in this_river_node_IDs) {
  node_index= which(nodeIDs %in% eachnode)
  #for the section between current node and next node
  node_curr_next <- c(node_index,node_index+1)
  
  #Node variables-------
  node_x=ncvar_get(SWORD_in, 'nodes/x',verbose=FALSE)[node_curr_next]
  
  node_y=ncvar_get(SWORD_in, 'nodes/y',verbose=FALSE)[node_curr_next]
  
  node_max_width=ncvar_get(SWORD_in, 'nodes/max_width',verbose=FALSE)[node_curr_next]
  
  node_length=ncvar_get(SWORD_in, 'nodes/node_length',verbose=FALSE)[node_curr_next]
  
  node_reachid=ncvar_get(SWORD_in, 'nodes/reach_id',verbose=FALSE)[node_curr_next] #key field
  
  node_df=data.frame(lon=node_x,lat=node_y,node_wmax=node_max_width*scale_maxwidth,node_length=node_length,reach_ID=node_reachid)
  
  node_df= node_df%>%
    mutate(node_UTM_x=LongLatToUTM(node_df$lon,node_df$lat,utm_zone)[,1])%>%
    mutate(node_UTM_y=LongLatToUTM(node_df$lon,node_df$lat,utm_zone)[,2])
  
  node_angle_radian <- atan((node_df$node_UTM_y[1]-node_df$node_UTM_y[2])/(node_df$node_UTM_x[1]-node_df$node_UTM_x[2]))
  
  node_angle_degree <- RadiantoDegree(node_angle_radian)
  
  #get the ROI as a quadrilateral polygon
  Vertex_ne_y <- node_df$node_UTM_y[1]+node_df$node_wmax[1]*sin(DegreetoRadian(node_angle_degree+90))
  Vertex_ne_x <- node_df$node_UTM_x[1]+node_df$node_wmax[1]*cos(DegreetoRadian(node_angle_degree+90))
  
  Vertex_2_y <- node_df$node_UTM_y[1]+node_df$node_wmax[1]*sin(DegreetoRadian(node_angle_degree+270))
  Vertex_2_x <- node_df$node_UTM_x[1]+node_df$node_wmax[1]*cos(DegreetoRadian(node_angle_degree+270))
  
  node_angle_degree_next <- NormalizeDegree_0_360(node_angle_degree-180)
  
  Vertex_3_y <- node_df$node_UTM_y[2]-node_df$node_wmax[2]*sin(DegreetoRadian(node_angle_degree_next+270))
  Vertex_3_x <- node_df$node_UTM_x[2]-node_df$node_wmax[2]*cos(DegreetoRadian(node_angle_degree_next+270))
  
  Vertex_4_y <- node_df$node_UTM_y[2]-node_df$node_wmax[2]*sin(DegreetoRadian(node_angle_degree_next+90))
  Vertex_4_x <- node_df$node_UTM_x[2]-node_df$node_wmax[2]*cos(DegreetoRadian(node_angle_degree_next+90))
  
  #spatial_drift=st_as_sf(drift_in,coords=c('UTM_x','UTM_y'),crs=paste0('+proj=utm +zone=',zone,' +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs') ) 
  
  #store coords in a df
  NodeROI_x_coord <- c(Vertex_ne_x,Vertex_2_x,Vertex_3_x,Vertex_4_x)
  NodeROI_y_coord <- c(Vertex_ne_y,Vertex_2_y,Vertex_3_y,Vertex_4_y)
  NodeROI_xy_coord <- cbind(NodeROI_x_coord, NodeROI_y_coord)
  
  #conver the data frame to polygon
  NodeROI_poly = Polygon(NodeROI_xy_coord)
  NodeROI_polys = Polygons(list(NodeROI_poly),1)
  NodeROI_polys_spatial = SpatialPolygons(list(NodeROI_polys))
  plot(NodeROI_polys_spatial)
  
  proj4string(NodeROI_polys_spatial) = CRS(paste("+proj=utm +zone=",utm_zone," ellps=WGS84",sep=''))
  #proj4string(sps) = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  tempdata = data.frame(f=99.9)
  NodeROI_polys_spatialwref = SpatialPolygonsDataFrame(NodeROI_polys_spatial,tempdata)
  
  shapefile(x = NodeROI_polys_spatialwref, file = paste0(dir_output_ROI,'/','NodeID_',eachnode,'_ROI.shp'),overwrite=TRUE)
  
  # read ROI shapefile
  Node_ROI = readOGR(paste0(dir_output_ROI,'/','NodeID_',eachnode,'_ROI.shp'), layer = paste0('NodeID_',eachnode,'_ROI'))
  # reproject ROI to image projection (not necessary if the projections are the same)
  Node_ROI = spTransform(Node_ROI, crs(Pleiades_image))
  
  Imagefilename = basename(Inputimagefile)
  #find positions of '_'
  poss=which(strsplit(Imagefilename, "")[[1]]=="_")
  #locate date string
  strdate = substr(Imagefilename,poss[1]+1, poss[1]+10)
  # clip Pleiades image by each reach ROI
  masked_Pleiades_image <- mask(x = Pleiades_image, mask = Node_ROI)
  r_node = crop(masked_Pleiades_image, Node_ROI)
  # testing only (for normal running comment below and uncomment above)
  #r_node = crop(Pleiades_image, Node_ROI)
  
  # read the id of each read (to be changed to "SWORD_id")
  node_id_str = as.character(eachnode)
  
  # reach id
  reach_id_str = as.character(node_reachid[1])
  
  # set output paths
  out_shp = paste0(dir_output_Shapefile,'/Node_',node_id_str,'_watermask.shp')
  out_raster = paste0(dir_output_Raster,'/Node_',node_id_str,'_watermaskraster.tif')
  
  # calculate water index (using NDWI for now, may be updated later)
  # normalize between green band (2) and nir band (4)
  ndwi = (r_node[[2]] - r_node[[4]])/(r_node[[2]] + r_node[[4]]) 
  
  ##Water area mapping 
  #create a water mask
  r_binary_water = ndwi
  r_binary_water[r_binary_water >= water_index_threshold] = 1 # water pixel
  r_binary_water[r_binary_water < water_index_threshold] = 0 # non-water pixel
  
  #get each connected component (i.e., water body)
  #refer to https://gis.stackexchange.com/questions/348693/finding-regions-in-raster-with-rastertopolygons-r
  labelregions = clump(r_binary_water)
  #calculate water area; unit: m2
  waterarea = GetTolpix_fromlargest_binaryimage(labelregions)[1] * res(r_binary_water)[1] * res(r_binary_water)[2]
  
  ##Water area uncertainty
  r_binary_water_lower = ndwi
  r_binary_water_lower[r_binary_water_lower >= (water_index_threshold+ThresholdOffset_4_uncertainty)] = 1 # water pixel
  r_binary_water_lower[r_binary_water_lower < (water_index_threshold+ThresholdOffset_4_uncertainty)] = 0 # non-water pixel
  labelregions_lower = clump(r_binary_water_lower)
  waterarea_lower = GetTolpix_fromlargest_binaryimage(labelregions_lower)[1] * res(r_binary_water_lower)[1] * res(r_binary_water_lower)[2]
  r_binary_water_upper = ndwi
  r_binary_water_upper[r_binary_water_upper >= (water_index_threshold-ThresholdOffset_4_uncertainty)] = 1 # water pixel
  r_binary_water_upper[r_binary_water_upper < (water_index_threshold-ThresholdOffset_4_uncertainty)] = 0 # non-water pixel
  labelregions_upper = clump(r_binary_water_upper)
  waterarea_upper = GetTolpix_fromlargest_binaryimage(labelregions_upper)[1] * res(r_binary_water_upper)[1] * res(r_binary_water_upper)[2]
  # using std of water areas at various thresholds as the uncertainty
  waterarea_uncertainty = sd(c(waterarea,waterarea_lower,waterarea_upper))
  
  labelnode = GetTolpix_fromlargest_binaryimage(labelregions)[2]
  labelregions[labelregions[]!=labelnode]=0
  labelregions[labelregions[]!=0]=1
  #testing: to be commented
  #plot(labelregions)
  
  #sample to 10-m: 0.5m -> 10m with a factor of 20
  r_binary_water_10m = aggregate(labelregions, fact=20)
  r_binary_water_10m[r_binary_water_10m >= 0.5] = 1 # water pixel
  r_binary_water_10m[r_binary_water_10m < 0.5] = NA # non-water pixel
  #testing: to be commented
  #plot(r_binary_water_10m)
  
  #raster to polygon
  p_binary_water <- rasterToPolygons(r_binary_water_10m, na.rm = TRUE,dissolve=TRUE)
  # define the same projection
  crs(p_binary_water) <- crs(Node_ROI)
  
  #remove existing files if exists
  #file.remove(paste0(dir_output_Shapefile,'/Node_',node_id_str,'_watermask.shp'))
  #file.remove(paste0(dir_output_Shapefile,'/Node_',node_id_str,'_watermask.dbf'))
  #file.remove(paste0(dir_output_Shapefile,'/Node_',node_id_str,'_watermask.prj'))
  #file.remove(paste0(dir_output_Shapefile,'/Node_',node_id_str,'_watermask.shx'))
  # export shapefile
  #writeOGR(p_binary_water, dsn = dir_output,layer = paste0('Reach_',i,'_watermask'), driver="ESRI Shapefile", overwrite = TRUE)
  writeOGR(p_binary_water, dirname(out_shp),sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(out_shp)), "ESRI Shapefile", overwrite = TRUE)
  
  # export water mask in a raster format
  writeRaster(labelregions, out_raster,overwrite=TRUE)
  
  # save variable values to data frame
  Water_df = rbind(Water_df,data.frame(reach_id = reach_id_str,node_id = node_id_str,area_m2 = waterarea, area_precision_m2=waterarea_uncertainty,datetime =paste0(strdate,' ',image_time)))
}

##save data frame to a csv file
write.csv(Water_df, paste0(dir_output_CSV,'/Waterarea_by_node.csv'),row.names = FALSE)


##Same as above but at the reach scale
Water_df_reach = data.frame()
for (eachreach in this_river_reach_IDs) {
  #Reach variables-------
  reach_index= which(reachIDs %in% eachreach)
  #for the current reach
  reach_curr <- c(reach_index)
  #for the section between current node and next node
  #reach_curr_next <- c(reach_index,reach_index+1)
  #overwriting with the index limits the RAM needed
  reach_x=ncvar_get(SWORD_in, 'reaches/x',verbose=FALSE)[reach_curr]
  
  reach_y=ncvar_get(SWORD_in, 'reaches/y',verbose=FALSE)[reach_curr]
  
  reach_x_max=ncvar_get(SWORD_in, 'reaches/x_max',verbose=FALSE)[reach_curr]
  
  reach_x_min=ncvar_get(SWORD_in, 'reaches/x_min',verbose=FALSE)[reach_curr]
  
  reach_y_max=ncvar_get(SWORD_in, 'reaches/y_max',verbose=FALSE)[reach_curr]
  
  reach_y_min=ncvar_get(SWORD_in, 'reaches/y_min',verbose=FALSE)[reach_curr]
  
  reach_length=ncvar_get(SWORD_in, 'reaches/reach_length',verbose=FALSE)[reach_curr]
  
  reach_df=data.frame(lon=reach_x,lat=reach_y,reach_xmax=reach_x_max,reach_xmin=reach_x_min,
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
  
  
  #store coords in a df
  ReachROI_x_coord <- c(spatial_reach$xmax,spatial_reach$xmin,spatial_reach$xmin,spatial_reach$xmax)
  ReachROI_y_coord <- c(spatial_reach$ymax,spatial_reach$ymax,spatial_reach$ymin,spatial_reach$ymin)
  ReachROI_xy_coord <- cbind(ReachROI_x_coord, ReachROI_y_coord)
  
  #conver the data frame to polygon
  ReachROI_poly = Polygon(ReachROI_xy_coord)
  ReachROI_polys = Polygons(list(ReachROI_poly),1)
  ReachROI_polys_spatial = SpatialPolygons(list(ReachROI_polys))
  plot(ReachROI_polys_spatial)
  
  proj4string(ReachROI_polys_spatial) = CRS(paste("+proj=utm +zone=",utm_zone," ellps=WGS84",sep=''))
  #proj4string(sps) = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  tempdata = data.frame(f=99.9)
  ReachROI_polys_spatialwref = SpatialPolygonsDataFrame(ReachROI_polys_spatial,tempdata)
  
  shapefile(x = ReachROI_polys_spatialwref, file = paste0(dir_output_ROI,'/','ReachID',eachnode,'_ROI.shp'),overwrite=TRUE)
  
  # read ROI shapefile
  Reach_ROI = readOGR(paste0(dir_output_ROI,'/','ReachID',eachnode,'_ROI.shp'), layer = paste0('ReachID',eachnode,'_ROI'))
  # reproject ROI to image projection (not necessary if the projections are the same)
  Reach_ROI = spTransform(Reach_ROI, crs(Pleiades_image))
  
  Imagefilename = basename(Inputimagefile)
  #find positions of '_'
  poss=which(strsplit(Imagefilename, "")[[1]]=="_")
  #locate date string
  strdate = substr(Imagefilename,poss[1]+1, poss[1]+10)
  # clip Pleiades image by each reach ROI
  r_reach = crop(Pleiades_image, Reach_ROI)
  
  # reach id
  reach_id_str = as.character(eachreach)
  
  # set output paths
  out_shp = paste0(dir_output_Shapefile,'/Reach_',reach_id_str,'_watermask.shp')
  out_raster = paste0(dir_output_Raster,'/Reach_',reach_id_str,'_watermaskraster.tif')
  
  # calculate water index (using NDWI for now, may be updated later)
  # normalize between green band (2) and nir band (4)
  ndwi = (r_reach[[2]] - r_reach[[4]])/(r_reach[[2]] + r_reach[[4]]) 
  #plot(ndwi)
  ##Water area mapping 
  #create a water mask
  r_binary_water = ndwi
  r_binary_water[r_binary_water >= water_index_threshold] = 1 # water pixel
  r_binary_water[r_binary_water < water_index_threshold] = 0 # non-water pixel
  #get each connected component
  labelregions <- clump(r_binary_water)
  #calculate water area; unit: m2
  waterarea = GetTolpix_fromlargest_binaryimage(labelregions)[1] * res(r_binary_water)[1] * res(r_binary_water)[2]
  
  ##Water area uncertainty
  r_binary_water_lower = ndwi
  r_binary_water_lower[r_binary_water_lower >= (water_index_threshold+ThresholdOffset_4_uncertainty)] = 1 # water pixel
  r_binary_water_lower[r_binary_water_lower < (water_index_threshold+ThresholdOffset_4_uncertainty)] = 0 # non-water pixel
  labelregions_lower <- clump(r_binary_water_lower)
  waterarea_lower = GetTolpix_fromlargest_binaryimage(labelregions_lower)[1] * res(r_binary_water_lower)[1] * res(r_binary_water_lower)[2]
  r_binary_water_upper = ndwi
  r_binary_water_upper[r_binary_water_upper >= (water_index_threshold-ThresholdOffset_4_uncertainty)] = 1 # water pixel
  r_binary_water_upper[r_binary_water_upper < (water_index_threshold-ThresholdOffset_4_uncertainty)] = 0 # non-water pixel
  labelregions_upper <- clump(r_binary_water_upper)
  waterarea_upper = GetTolpix_fromlargest_binaryimage(labelregions_upper)[1] * res(r_binary_water_upper)[1] * res(r_binary_water_upper)[2]
  # using std of water areas at various thresholds as the uncertainty
  waterarea_uncertainty = sd(c(waterarea,waterarea_lower,waterarea_upper))
  
  labelnode = GetTolpix_fromlargest_binaryimage(labelregions)[2]
  labelregions[labelregions[]!=labelnode]=0
  labelregions[labelregions[]!=0]=1
  #testing: to be commented
  #plot(labelregions)
  
  #sample to 10-m: 0.5m -> 10m with a factor of 20
  r_binary_water_10m = aggregate(labelregions, fact=20)
  r_binary_water_10m[r_binary_water_10m >= 0.5] = 1 # water pixel
  r_binary_water_10m[r_binary_water_10m < 0.5] = NA # non-water pixel
  #testing: to be commented
  #plot(r_binary_water_10m)
  
  #raster to polygon
  p_binary_water <- rasterToPolygons(r_binary_water_10m, na.rm = TRUE,dissolve=TRUE)
  # define the same projection
  crs(p_binary_water) <- crs(Node_ROI)
  
  #remove existing files if exists
  #file.remove(paste0(dir_output_Shapefile,'/Node_',node_id_str,'_watermask.shp'))
  #file.remove(paste0(dir_output_Shapefile,'/Node_',node_id_str,'_watermask.dbf'))
  #file.remove(paste0(dir_output_Shapefile,'/Node_',node_id_str,'_watermask.prj'))
  #file.remove(paste0(dir_output_Shapefile,'/Node_',node_id_str,'_watermask.shx'))
  # export shapefile
  #writeOGR(p_binary_water, dsn = dir_output,layer = paste0('Reach_',i,'_watermask'), driver="ESRI Shapefile", overwrite = TRUE)
  writeOGR(p_binary_water, dirname(out_shp),sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(out_shp)), "ESRI Shapefile", overwrite = TRUE)
  
  # export water mask in a raster format
  writeRaster(labelregions, out_raster,overwrite=TRUE)
  
  # save variable values to data frame
  Water_df_reach = rbind(Water_df_reach,data.frame(reach_id = reach_id_str,area_m2 = waterarea, area_precision_m2=waterarea_uncertainty,datetime =paste0(strdate,' ',image_time)))
}
##save data frame to a csv file
write.csv(Water_df_reach, paste0(dir_output_CSV,'/Waterarea_by_reach.csv'),row.names = FALSE)






