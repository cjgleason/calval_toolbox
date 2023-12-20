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



# 
#        plot(st_geometry(points_in_node) ,axes=TRUE,graticule=TRUE)
#        plot(st_geometry(final_node_df['reach_id']),add=TRUE,col=sf.colors(alpha=0.2))
#        plot(st_geometry(spatial_reach),add=TRUE,col=sf.colors(alpha=0.05))
#        plot(points_in_node['node_id'],add=TRUE)




# return(list('node_average'=reach_avarege_from_sampled_node,
#             'plot'=p1))
# 


