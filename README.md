# calval_toolbox
Toolboxes for hydro corrections of calval data

The 'toolbox_dev.R' is currently the main script to run.

At the moment, it runs a correction for PTs to get orthometric heights from a GNSS drift.

What it does is the following:

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
