#compare reach WSE pt vs drift
reach_pt_wse=read.csv('Willamette/SWORD products/reach/Willamette_reach_pt_wse.csv')%>%
  select(-X)

reach_drift_wse=read.csv('Willamette/SWORD products/reach/Willamette_drift_wse_slope.csv')%>%
  select(-X)%>%
  inner_join(reach_pt_wse,by='reach_id')

#compare reach slope pt vs drift

#compare node WSE pt vs drift