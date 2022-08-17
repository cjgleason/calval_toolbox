library(readxl)

metadatain= read_excel('D:/OneDrive -\ University of Massachusetts/calval/Toolbox/calval_toolbox/Taylor data 7 12/CTR_Aug21_nodes_reaches.xlsx')


this_river_reach_IDs= unique(metadatain$reach_id)
this_river_node_IDs= unique(metadatain$node_id)



library(ncdf4)
library(sf)
library(dplyr)
library(rgdal)

SWORD_path='D:/OneDrive -\ University of Massachusetts/calval/Toolbox/calval_toolbox/Taylor data 7 12/na_sword_v11.nc'
drift_directory='D:/OneDrive -\ University of Massachusetts/calval/Toolbox/calval_toolbox/Taylor data 7 12/corrected drifts/'
PT_directory='D:/OneDrive -\ University of Massachusetts/calval/Toolbox/calval_toolbox/Taylor data 7 12/QAQC PTs/'

SWORD_in=nc_open(SWORD_path)

reachIDs=ncvar_get(SWORD_in, 'reaches/reach_id')
reach_index= which(reachIDs %in% this_river_reach_IDs)

nodeIDs=ncvar_get(SWORD_in,'nodes/node_id')
node_index= which(nodeIDs %in% this_river_node_IDs)

#Node variables-------
#overwriting with the index limits the RAM needed
node_x=ncvar_get(SWORD_in, 'nodes/x')[node_index]

node_y=ncvar_get(SWORD_in, 'nodes/y')[node_index]

node_max_width=ncvar_get(SWORD_in, 'nodes/max_width')[node_index]

node_length=ncvar_get(SWORD_in, 'nodes/node_length')[node_index]

node_reachid=ncvar_get(SWORD_in, 'nodes/reach_id')[node_index] #key field


node_df=data.frame(lon=node_x,lat=node_y,node_ID=this_river_node_IDs,node_wmax=node_max_width,node_length=node_length,reach_id=node_reachid)

#we're mixing meters and latlon, so need to make this a spatial object with a coordinate system to convert to UTM and then 
# we can add distances
spatial_node=st_as_sf(node_df,coords=c('lat','lon'),remove=FALSE, crs='+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')%>%
  st_transform(6347)%>% #UTM 18N
  mutate(UTM_X=st_coordinates(.)[,2],UTM_Y=st_coordinates(.)[,1])


#---------------------

#Reach variables-------
#overwriting with the index limits the RAM needed
reach_x=ncvar_get(SWORD_in, 'reaches/x')[reach_index]

reach_y=ncvar_get(SWORD_in, 'reaches/y')[reach_index]

reach_x_max=ncvar_get(SWORD_in, 'reaches/x_max')[reach_index]

reach_x_min=ncvar_get(SWORD_in, 'reaches/x_min')[reach_index]

reach_y_max=ncvar_get(SWORD_in, 'reaches/y_max')[reach_index]

reach_y_min=ncvar_get(SWORD_in, 'reaches/y_min')[reach_index]

reach_length=ncvar_get(SWORD_in, 'reaches/reach_length')[reach_index]


reach_df=data.frame(lon=reach_x,lat=reach_y,reach_id=this_river_reach_IDs,reach_xmax=reach_x_max,reach_xmin=reach_x_min,
                    reach_ymax=reach_y_max,reach_ymin=reach_y_min, reach_length=reach_length)

LongLatToUTM<-function(x,y,zone){
  xy <- data.frame(ID = 1:length(x), X = x, Y = y)
  coordinates(xy) <- c("X", "Y")
  proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")  ## for example
  res <- spTransform(xy, CRS(paste("+proj=utm +zone=",zone," ellps=WGS84",sep='')))
  res=st_as_sf(res)
  return(data.frame(x=st_coordinates(res)[,1],y=st_coordinates(res)[,2]))
}



spatial_reach=st_as_sf(reach_df,coords=c('lon','lat'),remove=FALSE, crs='+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')%>%
  st_transform(6347)%>% #UTM 18N
  mutate(UTM_X=st_coordinates(.)[,1],UTM_Y=st_coordinates(.)[,2])

spatial_reach=spatial_reach%>%
  mutate(xmin=LongLatToUTM(spatial_reach$reach_xmin,spatial_reach$reach_ymin,18)[,1])%>%
  mutate(xmax=LongLatToUTM(spatial_reach$reach_xmax,spatial_reach$reach_ymin,18)[,1])%>%
  mutate(ymin=LongLatToUTM(spatial_reach$reach_xmin,spatial_reach$reach_ymin,18)[,2])%>%
  mutate(ymax=LongLatToUTM(spatial_reach$reach_xmin,spatial_reach$reach_ymax,18)[,2])

spatial_reach$reach_id=as.numeric(levels(spatial_reach$reach_id))
#---------------------

#centerline variables-------
cl_reach_IDs= ncvar_get(SWORD_in, 'centerlines/reach_id')
cl_index=which(cl_reach_IDs %in% this_river_reach_IDs)

cl_x=ncvar_get(SWORD_in, 'centerlines/x')[cl_index]
cl_y=ncvar_get(SWORD_in, 'centerlines/y')[cl_index]

cl_df=data.frame(reach_id=cl_reach_IDs[cl_index],lon=cl_x,lat=cl_y)%>%
  filter(!is.na(lat))%>%
  st_as_sf(coords=c('lon','lat'),remove=FALSE, crs='+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs') %>%
  st_transform(6347)%>% #UTM 18N
  mutate(cl_UTM_x=st_coordinates(.)[,1],cl_UTM_y=st_coordinates(.)[,2])

#---------------------------


#now loop through the drifts to get the values----


#calculate node wse---------
calc_node_wse=function(drift_file, node_df){
  
  drift_in=readRDS(drift_file)
  
  
  
}

#calculate reach wse-----
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
      filter(row_number()==1 | row_number() ==n())%>% #since the CL is ordered, we take first and last per reach id here
      mutate(xmin=cl_UTM_x - buffer, xmax=cl_UTM_x + buffer, ymin=cl_UTM_y - buffer, ymax=cl_UTM_y + buffer)
    
    
    #now there are two buffered points per reach
    cl_df2=filter(cl_df,reach_id == 73120000151)
    
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

drifts = paste0(drift_directory,list.files(drift_directory))
buffer=500 #m
reach_stats=do.call(rbind,lapply(drifts,calc_reach_stats,spatial_reach=spatial_reach,buffer=buffer,cl_df=cl_df))
#------------------------




