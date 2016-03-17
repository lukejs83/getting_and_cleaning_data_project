Title: Getting and Cleaning Data Course Project

Summary: This repository contains a script for tidying data from the UCI Human Activity Recognition Using Smartphones Data Set so that it can be used for further analysis.

Raw data can be found at:
http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

Description: The script run_analysis.R processes data from the UCI data set. The script relies  on a particular directory structure, with the working directory containing a folder UCI HAR Dataset with subfolders test and train.  

The script first loads and combines the training and test data sets into one data frame. It then determine which columns of the combined data contain means or standard deviations. Then, the mean and standard deviation columns are combined with the appropriate subject and activity labels into a data frame tidy_data, and provided with suitably descriptive names. Finally, the mean for each variable is computed per subject and per activity and stored in data frame tidy_mean.

