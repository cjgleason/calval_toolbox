setwd('D:\\OneDrive -\ University of Massachusetts\\calval\\Toolbox\\calval_toolbox\Willamette\\')
library(ncdf4)

test=nc_open('SWOT_L2_HR_RiverSP_001_013_NA_20190201T105055_20190201T105100_PGA0_01.nc')
#names(test$var)

node_dist=ncvar_get(test,'nodes/node_dist')
node_wse=ncvar_get(test,'nodes/wse')
node_reachid=ncvar_get(test,'nodes/reach_id')

