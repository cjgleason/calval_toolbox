#setwd('D:\\OneDrive -\ University of Massachusetts\\calval\\Toolbox\\calval_toolbox\\Willamette\\')
setwd('C:\\Users\\colin\\Documents\\GitHub\\calval_toolbox\\Willamette\\')
library(ncdf4)


test=nc_open('rivertile.nc')

#names(test$var)

node_dist=ncvar_get(test,'nodes/node_dist')
node_wse=ncvar_get(test,'nodes/wse')
node_reachid=ncvar_get(test,'nodes/reach_id')
node_time=ncvar_get(test,'nodes/time_tai')
node_ids=ncvar_get(test,'nodes/node_id')

reachid=unique(node_reachid)[5]

this_node_dist=node_dist[which(node_reachid==reachid)]
this_node_wse=node_wse[which(node_reachid==reachid)]

node_ids=node_ids[which(node_reachid==reachid)]

nc_close(test)

#sample 5 nodes
point1=8
point5=(length(node_ids))
point2=sample(20:30,1)
point3=sample(45:55,1)
point4=sample(70:80,1)

average_height_true= mean(this_node_wse,na.rm=T)
average_height_sample= mean(    c(this_node_wse[point1],
                                  this_node_wse[point2],
                                  this_node_wse[point3],
                                  this_node_wse[point4],
                                  this_node_wse[point5] ), na.rm=T)

plot(node_ids,this_node_wse, ylim=c(60,75))
points(c(node_ids[point1],
         node_ids[point2],
         node_ids[point3],
         node_ids[point4],
         node_ids[point5]),
         c(this_node_wse[point1],
           this_node_wse[point2],
           this_node_wse[point3],
           this_node_wse[point4],
           this_node_wse[point5] )  ,col='black'  ,lwd=10   )
text(node_ids[point2],this_node_wse[point5] ,paste0('true mean: ',round(average_height_true,digits=2), ' m'))
text(node_ids[point2],this_node_wse[point4] ,paste0('PT mean: ',round(average_height_sample,digits=2), ' m'))

#now perturb the profile
this_node_wse_new = this_node_wse - 1 - runif(length(this_node_wse),0.0,0.40)

true_delta= mean(this_node_wse,na.rm=T)- mean(this_node_wse_new,na.rm=T)
sample_delta= mean(    c(this_node_wse[point1],
                         this_node_wse[point2],
                         this_node_wse[point3],
                         this_node_wse[point4],
                         this_node_wse[point5] ), na.rm=T)-

mean(    c(this_node_wse_new[point1],    this_node_wse_new[point2],
                                  this_node_wse_new[point3],
                                  this_node_wse_new[point4],
                                  this_node_wse_new[point5] ), na.rm=T)  

points(node_ids,this_node_wse_new, col='blue')
points(c(node_ids[point1],
         node_ids[point2],
         node_ids[point3],
         node_ids[point4],
         node_ids[point5]),
       c(this_node_wse_new[point1],
         this_node_wse_new[point2],
         this_node_wse_new[point3],
         this_node_wse_new[point4],
         this_node_wse_new[point5] )  ,col='blue'  ,lwd=10   )
text(node_ids[point4],this_node_wse[point2] ,paste0('true delta: ',round(true_delta,digits=2), ' m'),col='blue')
text(node_ids[point4],this_node_wse[point1] ,paste0('PT delta: ',round(sample_delta,digits=2), ' m'),col='blue')

points(c(node_ids[point1],
         node_ids[point2],
         node_ids[point3],
         node_ids[point4],
         node_ids[point5]),
       c(this_node_wse_new[point1]+runif(1,3,3.3),
         this_node_wse_new[point2]+runif(1,3,3.3),
         this_node_wse_new[point3]+runif(1,3,3.3),
         this_node_wse_new[point4]+runif(1,3,3.3),
         this_node_wse_new[point5]+runif(1,3,3.3))  ,col='green'  ,lwd=10   )


