source('/nas/cee-water/cjgleason/calval_toolbox/R code/correct_PT_via_flyby.R')

munged_PT_directory= '/nas/cee-water/cjgleason/calval/Processed data/UMass/Munged PT/reprocessed_2023_12_20/'
output_dir=          '/nas/cee-water/cjgleason/calval/Processed data/Umass/Flyby PT/reprocessed_2023_12_20/'
pt_file_in=list.files(munged_PT_directory)
munged_GNSS_directory='/nas/cee-water/cjgleason/calval/Processed data/UMass/Munged drifts/reprocessed_2023_12_20/'
gnss_sd_thresh=0.05 #
time_thresh=7.5*60
dist_thresh=200

lapply(pt_file_in,correct_PT_via_flyby,
       munged_GNSS_directory=munged_GNSS_directory,
       munged_PT_directory=munged_PT_directory,
       time_thresh=time_thresh,
       dist_thresh=dist_thresh,
       gnss_sd_thresh=gnss_sd_thresh,
       output_dir=output_dir)
