#load packages
library(dplyr)

########
#Specify locations of as received data files. File should be unzipped into working directory.
#Data from: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
########

#test data set locations
subject_test_name <-"./UCI HAR Dataset/test/subject_test.txt"
X_test_name <-"./UCI HAR Dataset/test/X_test.txt"
y_test_name <-"./UCI HAR Dataset/test/y_test.txt"

#features file
features_key_file <- "./UCI HAR Dataset/features.txt"
activities_key_file <- "./UCI HAR Dataset/activity_labels.txt"

#training data set locations
subject_train_name <-"./UCI HAR Dataset/train/subject_train.txt"
X_train_name <-"./UCI HAR Dataset/train/X_train.txt"
y_train_name <-"./UCI HAR Dataset/train/y_train.txt"


########
#Read training and test data as well as features and activities labels
########

subject_test <- read.table(subject_test_name)
y_test <- read.table(y_test_name)
X_test <- read.table(X_test_name)

subject_train <- read.table(subject_train_name)
y_train <- read.table(y_train_name)
X_train <- read.table(X_train_name)

features_key <- read.table(features_key_file,stringsAsFactors = FALSE) 

activities_key <- read.table(activities_key_file,stringsAsFactors = FALSE)


############
# Combine data into one data frame
############

#combine test data set into one data frame
test <- cbind(subject_test, y_test,X_test)

#combine train data
train <- cbind(subject_train, y_train,X_train)

#combine test and training sets
data_merged <- rbind(train, test)

############
# Determine which columns contain mean or std and suitable name. Names are edited to make more descriptive.
############

#find column indices and names for mean and standard deviation. 
names_mean <- sub("\\()", "", grep("mean[^F]", features_key[,2],value = TRUE))
names_mean <- sub("^t","Time",names_mean)  
names_mean <- sub("^f","Frequency",names_mean) 
index_mean <- grep("mean[^F]", features_key[,2]) + 2  #add two to indices due to added subjectid and activity columns

names_std <- sub("\\()", "", grep("std", features_key[,2],value = TRUE)) #strip unneeded brackets
names_std <- sub("^t","Time",names_std) 
names_std <- sub("^f","Frequency",names_std) 
index_std <- grep("std", features_key[,2]) + 2  #add two to indices due to added subjectid and activity columns

############
#create data subset with only subjectid, activity, mean & std variables. 
############

tidy_data <- cbind(data_merged[,1:2],data_merged[,index_mean],data_merged[,index_std])

############
#name columns
#convert subjectid and activity to factor and give activities descriptive names
############

names(tidy_data) <- make.names(c("subjectid","activity",names_mean , names_std), unique = TRUE)

tidy_data$subjectid <- as.factor(tidy_data$subjectid)
tidy_data$activity <- as.factor(tidy_data$activity)
levels(tidy_data$activity) <- make.names(tolower(activities_key$V2))

########################
#compute mean by subject and activity and store in a second data frame
#########################

tidy_mean <- tidy_data %>% group_by(subjectid,activity) %>% summarise_each(funs(mean(., na.rm = TRUE)))
