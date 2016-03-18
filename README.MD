Getting and Cleaning Data Course Project
========================================

###Summary: 
This repository contains an R script Run_analysis.R for tidying data from the UCI Human Activity Recognition Using Smartphones Data Set so that it can be used for further analysis. 

Raw data and further information about the data can be found at:

https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones



###Description:
The script run_analysis.R processes data from the UCI data set. The appropriate data from the UCI data set should be placed in the working directory. The script requires the dplyr package.
  

The script first loads and combines the training and test data sets into one data frame. It then determine which columns of the combined data contain means or standard deviations based on the names in features.txt. Then, the mean and standard deviation columns are combined with the appropriate subject and activity labels into a data frame tidy_data, and provided with suitably descriptive names. Next, the mean for each variable is computed per subject and per activity and stored in data frame tidy_mean. Finally, tidy_mean is saved in the working directory.

