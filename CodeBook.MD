Introduction:
=============

This code book contains information on variables, study design, and choices made in the run_analysis.R script.

Variables:
==========

* tidy_data: 10299 by 68 data frame containing subjectid, activity, mean, and standard deviation for each observation.

* tidy_mean: 180 by 68 data frame that contains mean of tidy data by subjectid and activity


###Column names for tidy_data and tidy_average:


* subjectid: factor with levels spanning 1 to 30. Each level corresponds to a different person.

* activity: factor with levels "walking”, "walking_upstairs”, "walking_downstairs”, "sitting”, "standing”, "laying" indicating activity performed by subject during measurement. 
 
*remaining 66 columns: The data in the remaining columns relate to the motion recorded by a smartphone worn on the hip of the subject during the activity. The data are described in detail in the README.txt and features_info.txt file included with the raw data set and have a variety of units. The names have been slightly modified from the raw versions found in features.txt (included with raw data) to be more descriptive (lines 57-66 in run_analysis.R). The empty brackets are stripped and "t" and "f" (indicating time and frequency) are replaced with "Time" and "Frequency" respectively. For the tidy_average, "Average" is also prefixed to each name to distinguish these quantities from those contained in tidy_data.


Study design:
=============

For information on how the data was collected, please see the README.txt and features_info.txt files included with the raw data. Alternatively, the data set website contains the same information.

    http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 


Choices made:
=============
 
* The script uses the dplyr package for some operations.

* The mean and standard deviation data were selected as on lines 58 and 63 of run_analysis.R. 

* Column names were updated to be descriptive (lines 57-66 in run_analysis.R).

