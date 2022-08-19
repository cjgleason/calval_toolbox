# calval_toolbox
Toolboxes for hydro corrections of calval data

# correct_PT_to_GNSS

What it does:

1.	Looks at a raw PT file and checks to see if either of the ‘QAQC’ or ‘flagged’ folders contain a copy of it so that we can efficiently re-run without manual checking
2.	Joins that raw PT file to a GNSS drift based on a key to associate GNSS drifts with the PTs
3.	Filters for GNSS data within a specified time and distance threshold  of the PT set by the user for that particular river
4.	Calculates both a put in and pull out offset between the GNSS and the PT level
5.	Applies those offsets to PTs by closest time- essentially a nearest neighbor assignment
6.	Applies any field GNSS height offset corrections
7.	Automatically detects scrubs any leading or trailing out-of-water data
8.	Makes the following checks, all based on user thresholds to be tweaked by river
a.	GNSS standard deviation within the time/distance filter- are the GNSS data too noisy?
b.	GNSS data points- are there enough GNSS observations to make an offset?
c.	Offset consistency- did the PT offset change dramatically from put in to takeout? This indicates settling.
d.	Sudden shifts- are there shifts in the record other than at the beginning and end that indicate a suddent shift?
9.	If it passes all the tests, it places the PT with its new heights in the QAQC folder
a.	If it fails, it prints the data and adds a column with an error message that describes why it failed in a ‘flagged’ folder so a tech can look at it


# adjust_drift_via_PT

What it does:

1. Reads in QA/QCd PTs from above, along with GNSS drifts
2. Calcualtes the distance from every GNSS position @ 1Hz to all PTs less than a threshold
3. Calculates the difference between PT level @ SWOT overpass time to PT Level closest to each 1Hz position
4. Applies an IDW (power 2) to adust the height of the GNSS based on any number of PTs, where each adjustment is time varying based on #3
![alt text](https://github.com/cjgleason/calval_toolbox/blob/main/idw.gif?raw=true)

# calculate_slope_wse_fromdrift

What it does:

1. Reads in reach and node IDs you care about for a given river
2. Calculates wse (mean and sd) within a node by averaging within a box defined by SWORD, where the dimensions are node_length * max_width to pull all drift points

![alt text](https://github.com/cjgleason/calval_toolbox/blob/main/examplenodeboxes.JPG?raw=true)

2a. This is done by drift, so values have a timestamp
3. Calculates wse (mean and sd) within a reach by using SWORD's bounding box coordinates to pull all drift points within the reach
3a. This is done by drift, so value have a timestamp

