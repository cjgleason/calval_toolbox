# SWOT Calibration and Validation Hydrology Toolboxes

## Toolboxes for hydrology corrections of cal/val field data for comparison with SWOT satellite data.

To run the river toolboxes, the following inputs from the PO.DAAC repository are required:

**PT inputs:**
- PT data directory containing all PT L1 csv files that have been barologger corrected. First 11 rows of the csv are metadata, header on line 12 contains the following data:

| Variable	| Description |
|----------|--------------|
| Date	| m/dd/yyyy format |
| Time |	UTC 12 hour format |
| ms | 0 |
| Level	| meters |
| Temperature	| °C |

- PT key csv file that links PTs to SWORD reaches/nodes, install/uninstall GNSS drifts, and contains spatial information about the PT:

| Variable	| Description |
|----------|--------------|
| PT_Serial	| 7-digit PT serial number |
| Label	| Field label (e.g. PT1, BL1) |
| Baro_Comp | Barologger used to compensate PT level (e.g. BL1) |
| Node_ID	| SWORD node ID nearest PT |
| Reach_ID	| SWORD reach ID nearest PT |
| US_Reach_ID	| Upstream SWORD reach ID, relative to slope calculation |
| DS_Reach_ID	| Downstream SWORD reach ID, relative to slope calculation |
| Lat_WGS84	| Latitude |
| Long_WGS84 | Longitude |
| Install_method	| e.g. rebar, cinderblock |
| Date_PT_Install	| m/dd/yyyy format |
| Time_PT_Install_UTC | UTC 24 hour hh:mm format |
| Date_PT_Uninstall | m/dd/yyyy format |	
| Time_PT_Uninstall_UTC | UTC 24 hour hh:mm format |
| Date_GNSS_Install | m/dd/yyyy format |
| Time_GNSS_Install_Start_UTC | UTC 24 hour hh:mm:ss format |
| Time_GNSS_Install_End_UTC | UTC 24 hour hh:mm:ss format |
| GNSS_Offset_m	| Distance from receiver to water surface, meters |
| Receiver_Install | GNSS receiver used (e.g. Rec3) |
| Original_Install_Log_File | Septentrio filename (e.g. Rec3_20230310_001)|
| Final_Install_Log_File | SWOTCalVal_campaignshortname_GNSS_L0_instrumentname_startdatetime_enddatetime |
| Date_GNSS_Uninstall | m/dd/yyyy format |
| Time_GNSS_Uninstall_Start_UTC | UTC 24 hour hh:mm:ss format |
| Time_GNSS_Uninstall_End_UTC | UTC 24 hour hh:mm:ss format |
| Receiver_Uninstall | GNSS receiver used (e.g. Rec3) |
| Original_Uninstall_Log_File | Septentrio filename (e.g. Rec3_20230620_001)|
| Final_Uninstall_Log_File | SWOTCalVal_campaignshortname_GNSS_L0_instrumentname_startdatetime_enddatetime |

**GNSS inputs:**
-	GNSS drift data directory with all raw GNSS netCDF files containing:

| Variable	| Description |
|----------|--------------|
| latitude | Latitude of the GNSS antenna reference point with respect to the reference ellipsoid |
| longitude	| Longitude of the GNSS antenna reference point with respect to the reference ellipsoid |
| wse	| Water surface elevation relative to the provided model of the geoid with corrections for tidal effects (solid_earth_tide, load_tide_fes, and pole_tide) applied by subtracting from height_water. |
| time_tai	| Seconds since January 1 2000 at midnight without leap seconds |
| motioncode_flag	| Indicates motion of the platform: 0, 1, or 2. 2 indicates moving data |
| surfacetype_flag	| Indicates surface type of measurement, 10, 11, or 12. 12 indicates data over water |
| ellipsoid_semi_major_axis	| 
| ellipsoid_flattening |
| postion_3drss_formal_error | Three-dimensional root-sum-square formal error of position estimate |
| height_water | Height of the water level with respect to the reference ellipsoid |
| infoEventDescription | Description of noteworthy events (e.g. bridge or power line) that occurred during the campaign |
| infoEventStartTime |	Start time of noteworthy events in seconds in the UTC time scale since 1 Jan 2000 00:00:00 UTC |
| infoEventEndTime |	End time of noteworthy events in seconds in the UTC time scale since 1 Jan 2000 00:00:00 UTC |

**SWORD inputs:**
- SWORD data in the netCDF format brought in at the continental scale with centerline, node, and reach data. See the [SWORD Explorer](https://www.swordexplorer.com) for links to detailed dataset documentation.
- Domain csv file containing all SWORD reaches and nodes of interest.

**Initializing the toolboxes**

The river toolboxes are run on one field campaign at a time. First, the river of interest needs to be defined, along with the hubname associated with it (e.g. UC, UMass, UNC), the corresponding PT_key_file(s), utm_zone, and continent. A reach_end_buffer (which 'extends' reaches) and scale_maxwidth (how many SWORD widths wide each node box is) are also defined.

Empty directories named by processing date are created to store munged data outputs for PTs, GNSS drifts, and integrated SWORD products in addition to directories created for quality control flagged PT and GNSS drift output data. Depending on the reprocess_switch, dataframes may just be appended with additional data (0) or all reach and node products are recreated if there are major updates to toolbox processing (1). The directories are:
- Munged drifts/reprocessed_yyyy_mm_dd
- Munged PT/reprocessed_yyyy_mm_dd
- Data frames/reprocessed_yyyy_mm_dd
- Flagged PT/reprocessed_yyyy_mm_dd
- Flyby PT/reprocessed_yyyy_mm_dd

## Creating data frames from GNSS drifts

- First, list all the GNSS files returned from JPL processing that match the river of interest. There might be 'bad' duplicates of GNSS files, so filter by version date to pull in the most recent files.

- Run the create_GNSS_dataframe function. The function reads in the netCDF GNSS drifts sourced from PO.DAAC and creates a data frame in an xml format for cleaning and quality flagging with the fields listed in the GNSS inputs pulled in.

- Event code times are converted to POSIXct UTC. A one-minute buffer is added to the start and end time of the event codes that are powerlines or bridges.

- All of the GNSS TAI times format are converted to POSIXct UTC. Only data with the surface flag of 12 (water) and motion code of 2 (moving) are kept. Data with an uncertainty (position_3drss_formal_error) greater than 5cm are filtered out.

- ***Special case*** The Waimakariri River has static GNSS PT install files that need to be retained with a motion code of 0 (static). For just the Waimak, check if the GNSS file is listed in the keyfile. If it is, generate a dataframe as above, but without filtering motion or surface codes.

- Two information event dataframes are created: bad_info_df consisting of powerlines and bridge events, and good_info_df consisting of TP (turning points). Get ride of the bad data, and if the resulting data frame has no lines, the toolbox will print that the filename bonked.

- If there are no lines in the good_info_df, then we are done and the munged GNSS data frame is written to a csv in the GNSS output directory.

- If there are turning points, the GNSS file must be split at the beginning of the new good data. Split GNSS files following calval naming conventions, with an additional number indicating number of file splits at the end.

- Finally, the munged data frames are written to a csv in the GNSS output directory with the following fields:

| Variable	| Description |
|----------|--------------|
| gnss_Lat	| WGS84 |
| gnss_Lon	| WGS84 |
| gnss_wse	| Water surface elevation relative to the provided model of the geoid in meters |
| gnss_time_tai	| Seconds since January 1 2000 at midnight without leap seconds |
| gnss_uncertainty_m	| In meters, only values < 0.05 m |
| gnss_surf_flag	| Should be 12, indicating quality data over water |
| gnss_motion_flag	| Should be 2, indicating moving data collection |
| height_above_ellipsoid | Height of the water level with respect to the reference ellipsoid in meters |
| gnss_time_UTC	| yyyy-mm-dd hh:mm:ss 24 hour format |
| gnss_ellipsoid | ellipsoid_semi_major_axis, ellipsoid_flattening |
| drift_id	| Munged drifts/reprocessed_yyyy_mm_dd/SWOTCalVal_campaignshortname_GNSS_L2_instrumentname_startdatetime_enddatetime |

## Correct PTs to GNSS

**Only correct PTs to GNSS occupies**

Error thresholds for data:

**Distance threshold:** 150 meters, PT must be within a 150 m radius of a GNSS drift

**Time threshold:** 900 seconds (15 minutes), centered around PT collect, so 15 minutes before and after

**GNSS standard deviation:** 0.05 meters, how much variance is allowed in GNSS data when it is within the distance threshold

**Offset standard deviation:** 0.10 meters, how much variance is allowed in PT data from put in to take out. This is currently turned off

**Change threshold over 15 minutes:** 0.15 meters, does it change more than 15cm in 15 minutes? If so, that is a discontinuity in offset

**Dry threshold:** 0.10 meters, this is a raw PT level where anything below is considered a PT out of water

***These should eventually be specific to each river and are subject to change/should be up for debate!***

- First, check for un-munged csv PT L1 data in the PT data directory that have not already been processed. If there are, then run the correct_pt_to_gnss_only function.

- Read in the key file, fill blank cells with NA, and check that number of columns and names are correct.

- The correct_pt_to_gnss_only function reads in the raw PT files and the PT key files and IDs PTs based on their serial numbers. Check to make sure that the PTs in the key file exist and the csv files contain data. There are 11 lines of headers to skip when creating PT data frames in csv format. The PT data should log at 15min intervals and must be mutated to the standard POSIXct UTC time.

- The correct PT key file to join to the PT data is selected by the install and uninstall (in the water) time, which is always in the PT record. If the PT record is much shorter than expected, this is likely due to a mismatch between baro-corrected PT times and keyfile times.

- The last step in data tidying is to merge PT files with the PT key file with a left join based on PT serial number, filtered to times when the PT was in the water (install and uninstall dates, and above the dry PT threshold).

- Next up, come the actual corrections to the PT data to get water surface elevation. First, check to make sure the key file is properly joined to the PT data and there is data in the cleaned PT files, if not return NA. Also check to see if there are any shifts in the PT data (defined by the change_thresh_15_min), if there are flag the data with 1.

- Filter the PT data to the time of the GNSS install and uninstall, if both are available or just get the GNSS install data time. If there are no GNSS data files linked in the keyfile, bonk.

- Read in the munged L2 GNSS drift data and mutate the datetime into R’s POSIXct format. If an L2 GNSS file doesn't exist, check on push/pull GNSS from JPL.

- Do an inner join of the PT and GNSS data by datetime, applying the site-specific time threshold, data are limited to within the install and uninstall time frames. For example, with a threshold of 15 minutes, all GNSS data at 1 Hz within 30 minutes of every PT observation will be pulled.

- Multiple GNSS points go to one PT observation, so group the PT by occupy_id and create the offset correction of GNSS water surface elevation minus the PT level (both in meters).

- To create the PT corrections, PT level and the PT offset correction are added together and then joined to the PT data frame with a left join.

- Error is summarized in multiple ways. The PT correction total error is the standard deviation of the wse (boat bobbing) and the GNSS average error, propagated. The PT correction offset standard deviation and total error due to GNSS error are also summarized. The time difference between PT and GNSS measurements are also stored.

- At this point, there should be either 1 or 2 final offset corrections (depending on if there was both an install and uninstall). Check to make sure there are not more than 2 or 0 rows in the final_offset_df, if either statement is true return an error message.

- PT flag descriptions:
  - 0: all good
  - 1: shift in raw PT (15cm in 15min)
  - 10: no uninstall data
  - 1000: (install - uninstall) > threshold

- Finally, take the mean of the final_offset_df to get a singular PT offset and use a left join to add the offset and associated error estimates to the PT data. Apply the final PT offset to the pt_level to get PT wse filtered to when the PT was in the water.

- The last step is to print out the filename and ‘this file has passed all checks’ and write a csv of the final PT data saved to the QA QC PT output directory with the following fields:

| Variable	| Description |
|----------|--------------|
| pt_time_UTC	| yyyy-mm-dd hh:mm:ss format |
| pt_lat	| Latitude, WGS84 |
| pt_lon	| Longitude, WGS84 |
| pt_install_UTC | yyyy-mm-dd hh:mm:ss format |
| pt_uninstall_UTC | yyyy-mm-dd hh:mm:ss format |
| gnss_install_UTC_start | yyyy-mm-dd hh:mm:ss format |
| gnss_install_UTC_end | yyyy-mm-dd hh:mm:ss format |
| gnss_uninstall_UTC_start | yyyy-mm-dd hh:mm:ss format |
| gnss_uninstall_UTC_end | yyyy-mm-dd hh:mm:ss format |
| install_method	| e.g. rebar, cinderblock |
| pt_serial	| Seven digit ID number |
| pt_level | Distance below water surface, in meters |
| temperature | In °C |
| driftID_install | SWOTCalVal_campaignshortname_GNSS_L0_instrumentname_startdatetime_enddatetime |
| driftID_uninstall | SWOTCalVal_campaignshortname_GNSS_L0_instrumentname_startdatetime_enddatetime |
| datetime | yyyy-mm-dd hh:mm:ss, PT 15 min logging intervals |
| keyid | SWOTCalVal_campaignshortname_KEY_startdate_enddate.csv |
| Date_GNSS_Install | m/dd/yyyy |
| Date_GNSS_Uninstall | m/dd/yyyy |
| Reach_ID | SWORD reach id |
| Node_ID | SWORD node id |
| drift_id_install | Munged drifts/reprocessed_yyyy_mm_dd/SWOTCalVal_campaignshortname_GNSS_L2_instrumentname_startdatetime_enddatetime_processingdatetime_filenumber |
| drift_id_uninstall | Munged drifts/reprocessed_yyyy_mm_dd/SWOTCalVal_campaignshortname_GNSS_L2_instrumentname_startdatetime_enddatetime_processingdatetime_filenumber |
| pt_correction_total_error_m_install | standard deviation of the wse (boat bobbing) and the GNSS average error, propagated (m) |
| pt_correction_total_error_m_uninstall | standard deviation of the wse (boat bobbing) and the GNSS average error, propagated (m) |
| pt_correction_offset_sd_m_install | PT correction offset standard deviation (m) |
| pt_correction_offset_sd_m_uninstall | PT correction offset standard deviation (m) |
| pt_correction_gnss_average_error_m_install | average error due to GNSS error (m) |
| pt_correction_gnss_average_error_m_uninstall | average error due to GNSS error (m) |
| mean_dt_pt_gnss_offset_calc_install | Mean time difference from PT measurement to GNSS pings (s) |
| mean_dt_pt_gnss_offset_calc_uninstall | Mean time difference from PT measurement to GNSS pings (s) |
| pt_correction_m_install	| GNSS water surface elevation minus PT level (m) |
| pt_correction_m_uninstall	| GNSS water surface elevation minus PT level (m) |
| t_test_means_p_value | T test p vales for difference between install and uninstall offsets |
| in_out_diff | Difference between install and uninstall offset values (m) |
| pt_correction_mean_total_error_m | standard deviation of the wse (boat bobbing) and the GNSS average error, propagated and averaged for install and uninstall (m)| 
| pt_correction_mean_offset_sd_m | PT correction offset standard deviation, averaged for install and uninstall (m) |
| final_offset_m | PT offset correction, averaged for install and uninstall (m) |
| pt_wse_m | PT wse (m),  final_offset_m + pt_level |
| flag | 0 = good |

## Flyby offsets ##

- The correct_PT_via_flyby function takes PTs flagged as problematic from the correct_PT_to GNSS_occupy_only and attempts to correct PT level to wse by using GNSS data from passing drifts within a set time and distance threshold. Output is stored in Flyby PT/reprocessed_yyyy_mm_dd.

Error thresholds for data:

**GNSS standard deviation:** 0.05 meters, how much variance is allowed in GNSS data when it is within the distance threshold

**Time threshold:** 450 seconds (7.5 minutes), centered around PT collect, so 7.5 minutes before and after

**Distance threshold:** 150 meters, PT must be within a 150 m radius of a GNSS drift

**Minimum GNSS points for offset:** 5

- If the PT is flagged as 0 (good) or 1 (shift in raw data), do not run the correct_PT_via_flyby function and return a corrected PT dataframe with NA for flyby correction fields.

- Read in all of the river's GNSS data and return a list of all GNSS files that fall within the PT record by datetime. If there are multiple versions of a GNSS file, take the most recent reprocessing date.

- Create a bounding box of the GNSS max/min lat/lon and check to see if the PT is within the box. If so, join the PT and GNSS data with a difference inner join by datetime with a max distance based on the time threshold. A column with the seconds between the GNSS and PT measurements is added.

- Take the dataframe of PT paired with GNSS by time (pt_with_gnss_time) and calculate a distance vector with the pairwise difference between every row's GNSS and PT using a haversine. Join the distance vector to the pt_with_gnss_time dataframe and filter by the distance threshold.

- Calculate a flyby offset dataframe (gnss_wse-pt_level), making sure there is greater than the minimum number of GNSS points to create an offset. Take the mean of all the flyby offsets to get a flyby_pt_correction_m.

- Error is summarized in multiple ways. The flyby PT correction total error is the standard deviation of the wse (boat bobbing) and the GNSS average error, propagated. The flyby PT correction offset standard deviation and total error due to GNSS error are also summarized. Drift Ids used and number of GNSS pings are also stored in the offset_df.

- Next, possible problems with the flyby PT wse are handled by flagging the data. PT flag descriptions are:
  - 0: all good
  - 1: shift in raw PT (15cm in 15min)
  - 10: no uninstall data
  - 1000: (install - uninstall) > threshold
  - 10000: flybys don't agree with offset. All install, uninstall, flybys averaged
  - 100000: there aren't enough flybys. Not appropriate to do anything
  - 1000000: PT is likely settling. Linear fit was applied.
  - 10000000: flybys agree with OG file. Nothing changed.

- Check if the flyby_offset dataframe is empty, this occurs if there aren't enough GNSS pings (as defined by minimum number of GNSS pings threshold) to generate an offset. If it is, flag it with 100000.
- Next, filter for noise by checking that the flyby_pt_correction_total_error_m is less than the gnss_sd_thresh. If this empties the flyby_offset dataframe, also flag with 100000.
- Do the flybys agree with the occupy? Take the final offset from the occupies minus the mean flyby offset. If this difference is less than the gnss_sd_thresh there is agreement, flag with 10000000. Otherwise flag with 10000, flybys don't agree with offset - take the mean of the mean_flyby correction and the install_mean correction and recompute the flyby PT wse and error metrics with the average flyby and occupy correction.
- Check if the PT is settling by applying a linear fit to all flyby and occupy offset corrections over time. Check how monotonic the increase is with a difference vector ((n+1)-n). Add all of the differences into a 'monotic_sum.' If the slope and monotic_sum are both greater than 0 the PT is likely settling. Apply a linear fit (although we don't know it's linear...) to correct the PT, recompute error metrics taking linear fit into account, and flag it with 1000000.

- Create a final flyby_offsets dataframe saved to Flyby PT/reprocessed_yyyy_mm_dd with the following fields:

| Variable	| Description |
|----------|--------------|
| pt_time_UTC	| yyyy-mm-dd hh:mm:ss format |
| pt_lat	| Latitude, WGS84 |
| pt_lon	| Longitude, WGS84 |
| pt_install_UTC | yyyy-mm-dd hh:mm:ss format |
| pt_uninstall_UTC | yyyy-mm-dd hh:mm:ss format |
| gnss_install_UTC_start | yyyy-mm-dd hh:mm:ss format |
| gnss_install_UTC_end | yyyy-mm-dd hh:mm:ss format |
| gnss_uninstall_UTC_start | yyyy-mm-dd hh:mm:ss format |
| gnss_uninstall_UTC_end | yyyy-mm-dd hh:mm:ss format |
| install_method	| e.g. rebar, cinderblock |
| pt_serial	| Seven digit ID number |
| pt_level | Distance below water surface, in meters |
| temperature | In °C |
| driftID_install | SWOTCalVal_campaignshortname_GNSS_L0_instrumentname_startdatetime_enddatetime |
| driftID_uninstall | SWOTCalVal_campaignshortname_GNSS_L0_instrumentname_startdatetime_enddatetime |
| datetime | yyyy-mm-dd hh:mm:ss, PT 15 min logging intervals |
| keyid | SWOTCalVal_campaignshortname_KEY_startdate_enddate.csv |
| Date_GNSS_Install | m/dd/yyyy |
| Date_GNSS_Uninstall | m/dd/yyyy |
| Reach_ID | SWORD reach id |
| Node_ID | SWORD node id |
| drift_id_install | Munged drifts/reprocessed_yyyy_mm_dd/ SWOTCalVal_campaignshortname_GNSS_L2_instrumentname_startdatetime_enddatetime_processingdatetime_filenumber |
| drift_id_uninstall | Munged drifts/reprocessed_yyyy_mm_dd/ SWOTCalVal_campaignshortname_GNSS_L2_instrumentname_startdatetime_enddatetime_processingdatetime_filenumber |
| pt_correction_total_error_m_install | standard deviation of the wse (boat bobbing) and the GNSS average error, propagated (m) |
| pt_correction_total_error_m_uninstall | standard deviation of the wse (boat bobbing) and the GNSS average error, propagated (m) |
| pt_correction_offset_sd_m_install | PT correction offset standard deviation (m) |
| pt_correction_offset_sd_m_uninstall | PT correction offset standard deviation (m) |
| pt_correction_gnss_average_error_m_install | average error due to GNSS error (m) |
| pt_correction_gnss_average_error_m_uninstall | average error due to GNSS error (m) |
| mean_dt_pt_gnss_offset_calc_install | Mean time difference from PT measurement to GNSS pings (s) |
| mean_dt_pt_gnss_offset_calc_uninstall | Mean time difference from PT measurement to GNSS pings (s) |
| pt_correction_m_install	| GNSS water surface elevation minus PT level (m) |
| pt_correction_m_uninstall	| GNSS water surface elevation minus PT level (m) |
| t_test_means_p_value | T test p vales for difference between install and uninstall offsets |
| in_out_diff | Difference between install and uninstall offset values (m) |
| pt_correction_mean_total_error_m | standard deviation of the wse (boat bobbing) and the GNSS average error, propagated and averaged for install and uninstall (m)| 
| pt_correction_mean_offset_sd_m | PT correction offset standard deviation, averaged for install and uninstall (m) |
| final_offset_m | PT offset correction, averaged for install and uninstall (m) |
| pt_wse_m | PT wse (m),  final_offset_m + pt_level |
| flag | 0 = good |
| flyby_correction_m | GNSS water surface elevation minus PT level (m) derived from flybys |
| flyby_total_error_m | standard deviation of the wse (boat bobbing) and the GNSS average error, propagated (m) |
| pt_with_correction_mean_offset_sd_m | PT correction offset standard deviation (m) |
| pt_with_correction_total_error_m | ??? |
| pt_with_flyby_wse_m | PT wse (m) derived from flybys,  flyby_correction_m + pt_level |

## Calculate slopes and heights from drifts within nodes and reaches

- Read in the river-specific SWORD reach/node data from the domain_file and save all river reach and node IDs.

- Run the calculate_slope_wse_fromdrift function which creates SWORD products from drifts. First, set up a function to project latitude and longitude into UTMs.

- Bring in SWORD data in the netCDF format and index the reach and node IDs for the specific river.

For node variables, create a dataframe with the following fields:
| Variable	| Description |
|----------|--------------|
| lon	| Node longitude in UTM |
| lat	| Node latitude in UTM |
| node_id	| Unique node ID |
| node_wmax	| Max node width (m) |
| node_length	| Node length (m) |
| reach_id	| Reach ID that node falls in |

For reach variables, create a dataframe with the following fields:
| Variable	| Description |
|----------|--------------|
| lon	| Central reach longitude UTM |
| lat	| Central reach latitude UTM |
| reach_id	| Unique reach ID |
| reach_xmax	| Max longitude UTM |
| reach_xmin	| Min longitude UTM |
| reach_ymax	| Max latitude UTM |
| reach_ymin	| Min latitude UTM |
| reach_length | Length, in m |

- Mutate the reach dataframe into a spatial object bounded by reach_xmax, reach_xmin, reach_ymax, and reach_ymin and define the crs with the utm_zone defined when initializing the toolboxes.

- For both of these processes overwriting the original indexes limits the amount of RAM needed.

- If the photo_path is not null, find the associated node IDs with the imagery.

Bring in the SWORD centerlines in netCDF format for the specific river and create a spatial object with the following variables:
| Variable	| Description |
|----------|--------------|
| reach_id	| Reach ID for centerline |
| lon	| Centerline longitude UTM |
| lat	| Centerline latitude UTM |
| cl_id	| Centerline ID |
| cl_node_id	| Centerline node ID |

### Calculate node water surface elevation

- Read in the GNSS drift csv file and convert the latitude and longitude to UTMs and the time to POSIXct UTC, filter out any rows without a lat/lon.

- Join the centerline dataframe to the node dataframe using a left join by node and reach id and compute the angle that the centerline intersects each node at. Using that node angle, make a polygon geometry of four points centered around the node that are spaced out the maximum width and length of the node. Convert the points into a box for each node that is a spatial object.

- If there is a classified aerial image we want to use for a node, use it instead to create the overlapping node boxes.

- Make the GNSS drift data a spatial object and find the intersection between the node boxes and drift points. Group these points by node ID and then take the mean of all GNSS water surface elevation points within the boxes as the mean node water surface elevation (in meters). Compute the mean GNSS time for all drift data within the box and save the reach ID, drift ID, and node water surface elevation total error (m) as the GNSS wse sd and mean GNSS uncertainty, propagated.

### Calculate reach water surface elevation and slope from drifts

- Read in the GNSS drift csv file and convert the latitude and longitude to UTMs and the time to POSIXct UTC.

- Create an index of relevant drift data that that is within each reach, then for each reach calculate the mean GNSS water surface elevation relative to the geoid (m), the reach height above the ellipsoid (m), reach wse total error is the mean GNSS uncertainty of the points in the reach, water surface elevation start time, and water surface elevation end time.

- Search for centerline for points within the buffer distance at the top and bottom of each reach to define the slope and pull the length of each reach.

- Match GNSS drift data to the centerline points at the top and bottom of the reaches through a geospatial search.

- To calculate the slope, take the mean of the water surface elevations at the end of the reach over the buffer area minus the mean of the water surface elevations at the start of the reach over the buffer area divided by the length of the centerline (switch start and end here to get a positive number). The slope total error is the standard deviation of the slope start elevations, slope end elevations, slope start total error, and slope end total error, propagated and divided by the reach length.

- If the whole reach wasn’t floated, don’t compute the slope. If the slope doesn’t exist for a reach get rid of the water surface elevation, because water surface elevation should be averaged over an entire reach’s worth of data.

- From these calculations, there are three csv files saved into the node and reach output directories (Data frames/reprocessed_yyyy_mm_dd/node or Data frames/reprocessed_yyyy_mm_dd/reach):

Node water surface outputs saved as campaignshortname_drift_node_wses.csv:

| Variable	| Description |
|----------|--------------|
| time_UTC | yyyy-mm-dd hh:mm:ss UTC format |
| node_id	| Unique SWORD node ID |
| drift_id | Munged drifts/reprocessed_yyyy_mm_dd/ SWOTCalVal_campaignshortname_GNSS_L2_instrumentname_startdatetime_enddatetime_processingdatetime_filenumber |
| mean_node_drift_wse_m	| Node water surface elevation (m) |
| node_total_error_m	| Total error from GNSS pings (m) |
| ellipsoid_height_m | Node height relative to the ellipsoid (m) |

Node geometry is saved as campaignshortname_drift_node_geom.csv with node id and geometry.

Reach water surface elevation and slope as campaignshortname_drift_node_geom_drift_reach_wse_slope.csv:
| Variable	| Description |
|----------|--------------|
| reach_id	| Unique SWORD reach ID |
| mean_reach_drift_wse_m	| Water surface elevation (m) averaged to the reach scale |
| mean_reach_drift_wse_total_error_m	| Total GNSS error (m) |
| wse_drift_start_UTC	| yyyy-mm-dd hh:mm:ss UTC format |
| wse_drift_end_UTC	| yyyy-mm-dd hh:mm:ss UTC format |
| reach_drift_slope_m_m	| (end GNSS pings within buffer zone - start GNSS pings within buffer zone)/reach centerline length (m/m) |
| reach_drift_slope_precision_m	| Total error in slope computation (m) |
| drift_id	| Munged drifts/reprocessed_yyyy_mm_dd/ SWOTCalVal_campaignshortname_GNSS_L2_instrumentname_startdatetime_enddatetime_processingdatetime_filenumber |

### Calculate slopes and heights from PTs within nodes and reaches

- Bring in the munged PTs, the key file, and SWORD reaches and nodes for the river of interest from the domain_file. Next, run the calculate_slope_wse_fromPT_flybys function.

- Define sources of error:
  - alongstream error: 0.0001*200 m, error from downstream slope of a reach in a node (placeholder is 1e-4 slope over a 200m node)
  - crossstream error: 0.005 m, error from PT not representing cross stream elevation/noise in node
  - measurement error: 0.001 m, error from the PT measurment itself

- Make a dataframe with the key file and add an unique id (pt_id) column in case PT serial moves nodes in a river.

- Read in SWORD and create a reach_length_df with reach id and length and a node_dist_df with node id, p_dist_out (upstream distance in meters from the river system outlet), and reach id.

- Next, read in PT files and create new pt_id field (pt_serial & nod_id) to prevent confusion if PT was geographically moved in river. Join key file to PT df with a left join by pt_id.

- To calculate node water surface elevation, group the PT dataframe by node ID and find the mean water surface elevation (m) for each node. Set the mean PT water surface precision (m) to the sum of alongstream, crossstream, and measurement error. Also include the PT with correction mean offset standard deviation (m) and total error (m).

- To calculate reach water surface elevation, left join the reach dataframe to the PT dataframe and group by reach ID to find the mean water surface elevation (m) for each reach. 

???Set the mean PT water surface precision to 0.001 m, the error of the PT instruments.???

- To calculate slope, filter the key file to PTs that have data for us_reach_id and ds_reach_id and add the pt_id field. Get all PT data at the 15min intervals for the reach boundary PTs. Then seperately get the means of both the upstream and downstream reach boundary PT corrected water surface elevations (ideally there should be two PTs at reach boundaries). Total error for PT wse at each boudary is the square root of the (max mean pt wse minus the min mean pt wse)^2 plus the sum of the mean pt with correction total error^2. Join the upstream and downstream dataframes together by UTC time and reach ID. Also, join the reach length dataframe to get SWORD lengths.

- Slope (m/m) is the mean upstream water surface elevation minus the mean downstream water elevation divided by reach length, segmented by time. The slope uncertainty (m/m) is the total PT error at the upstream and downstream reach boundary, propagated and divided by reach length.

- From these calculations, there are three csv files saved into the node and reach output directories (Data frames/reprocessed_yyyy_mm_dd/node or Data frames/reprocessed_yyyy_mm_dd/reach):

PT node water surface elevation saved as campaignshortname_PTserial_nodeID_PT_node_wse.csv:

| Variable	| Description |
|----------|--------------|
| node_id	 | SWORD node ID |
| pt_time_UCT	| yyyy-mm-dd hh:mm:ss format |
| pt_id | PT serial_Node ID |
| pt_serial | 7 digit PT serial number |
| mean_node_wse_m	| Mean node water surface elevation (m) |
| mean_pt_wse_precision_m	| Sum of alongstream, crossstream, and measurement error (m) |
| pt_correction_sd | PT correction standard deviation (m) |
| flag | 100 |
| p_dist_out | Distance from river outlet calculated in SWORD (m) |
| reach_id | SWORD reach ID |

PT reach water surface elevation saved as campaignshortname_reachID_PT_reach_wse.csv:

| Variable	| Description |
|----------|--------------|
| pt_time_UCT	| yyyy-mm-dd hh:mm:ss UTC format |
| reach_id	| SWORD reach ID |
| mean_reach_wse_m	| Mean reach water surface elevation (m) |
| nodelist | List of nodes used to generate reach products |
| flaglist | PT error flags |
| mean_p_dist_out | mean distance to river outlet (as in SWORD) from nodes used in calculation (m) |

PT reach slope saved as campaignshortname_PT_reach_slope.csv:

| Variable	| Description |
|----------|--------------|
| pt_time_UCT	| yyyy-mm-dd hh:mm:ss UTC format |
| mean_pt_wse_us_boundary_m | Mean upstream boundary PT wse (m) |
| total_error_pt_wse_us_boudary | Total error from upstream boundary PTs |
| pt_serials_us | actually pt_ids (serial & node) |
| flag_us | PT error flags |
| reach_id	| SWORD reach ID |
| mean_pt_wse_ds_boundary_m | Mean downstream boundary PT wse (m) |
| total_error_pt_wse_ds_boudary | Total error from downstream boundary PTs |
| pt_serials_ds | actually pt_ids (serial & node) |
| flag_ds | PT error flags |
| slope_m_m	| Slope (m/m) |
| slope_uncertainty_m_m	| Slope uncertainty |

## Define what drift goes with what SWOT overpass

Define thresholds for linking GNSS drifts to SWOT overpasses:

**Time threshold:** Two hours

**Water surface elevation threshold:** Within 5cm

**Distance threshold:** Within 200m

***These should be specific to each river and are subject to change/should be up for debate!***

- Next, run the select_appropriate_drift function which matches GNSS drifts to SWOT granules. 

- Create a list of SWOT pass names to process. Next, average the start and end date times of the granule to get a mean SWOT granule time in UTC.

- The first check is if the SWOT time matches the drift time.

- Read in the node water surface elevation csv, convert time to POSTIXct, and calculate the time to SWOT overpass.

- Make an index of what drift node observations to keep and remove based on the time threshold.

- For node water surface elevations from drifts within the time threshold, add the time of the SWOT overpass and the SWOT pass ID to a direct match dataframe and save to a csv of SWOT drift pairs.

- For drifts outside of the time threshold get the drift data at 1 Hz.

- Pull the PT levels at the time of the SWOT overpass from the munged PT csv.

- Next, compare the drift node levels with the PT levels with a difference join based on latitude and longitude: (This process is slow.)

- Read in the PT key file. Join the drift and PT data using a nearest neighbor haversine geospatial left join. Compute the difference in water surface elevation between the GNSS and PT data. Then join the key file to the dataframe with a left join. Next, filter the data by the distance threshold.

- This dataframe has now found all drifts within the distance threshold of the PTs.

- Next, filter by water level within the water surface elevation threshold to find ‘good’ matches of PT and drift data.

Group by node and drift ID before computing summary statistics to be output in the final csv dataframe of SWOT drift pairs in Matched drifts/reprocessed_yyyy_mm_dd/ SWOT_L2_HR_RiverSP_Match_001_265_cycle_pass_continent_startdatetime_enddatetimematched__processdate_campaignshortname:

| Variable	| Description |
|----------|--------------|
| node_id	| SWORD node ID |
| drift_id	| Munged drifts/reprocessed_yyyy_mm_dd/ SWOTCalVal_campaignshortname_GNSS_L2_instrumentname_startdatetime_enddatetime_processingdatetime_filenumber |
| drift_pt_dist_km_bar	| Distance from drift to PT (km) |
| wse_difference_m_bar | Water surface elevation difference between drift and PT (m) |
| wse_difference_m_sd	| Standard deviation of water surface elevation difference (m) |
| swot_passid	| SWOT_L2_RiverSP_Match_cycle_pass_continent_startdatetime_enddatetime |
| swot_time_UTC	| yyyy-mm-dd hh:mm:ss format |


