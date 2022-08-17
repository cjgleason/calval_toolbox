
library(ncdf4)

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
node_x=ncvar_get(SWORD_in, 'nodes/x')
node_x=node_x[node_index]

node_y=ncvar_get(SWORD_in, 'nodes/y')
node_y=node_y[node_index]

node_max_width=ncvar_get(SWORD_in, 'nodes/max_width')
node_max_width=node_max_width[node_index]

node_length=ncvar_get(SWORD_in, 'nodes/node_length')
node_length=node_length[node_index]
#---------------------

#Reach variables-------
#overwriting with the index limits the RAM needed
reach_x=ncvar_get(SWORD_in, 'reaches/x')
reach_x=reach_x[reach_index]

reach_y=ncvar_get(SWORD_in, 'reaches/y')
reach_y=reach_y[reach_index]

reach_x_max=ncvar_get(SWORD_in, 'reaches/x_max')
reach_x_max=reach_x_max[reach_index]

reach_y_max=ncvar_get(SWORD_in, 'reaches/y_max')
reach_y_max=reach_y_max[reach_index]

reach_x_min=ncvar_get(SWORD_in, 'reaches/x_min')
reach_x_min=reach_x_min[reach_index]

reach_y_min=ncvar_get(SWORD_in, 'reaches/y_min')
reach_y_min=reach_y_min[reach_index]

#---------------------

#centerline variables-------
cl_reach_IDs= ncvar_get(SWORD_in, 'centerlines/reach_id')
cl_index=which(cl_reach_IDs %in% this_river_reach_IDs)

#---------------------------






