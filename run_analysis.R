## The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set.
## One of the most exciting areas in all of data science right now is wearable computing - see for example this article .
##   http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones



## 'features_info.txt': Info on the features being measured

##  'features.txt': List of all features.

##  'activity_labels.txt': Activity Descriptors

##  'train/X_train.txt': Training set.

##  'train/y_train.txt': index pointer to activity labels.

##  'test/X_test.txt': Test set.

##  'test/y_test.txt': index pointer to activity labels

##  'train/subject_train.txt': Each row provides an identifer(1:30) representing the person who performed the activity
##  'train/subject_test.txt':  

##   Data for the project is downloaded from here:

##   https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip



## Create one R script called run_analysis.R that does the following.

## 1. Merges the training and the test sets to create one data set.
## 2. Extracts only the measurements on the mean and standard deviation for each measurement.
## 3. Uses descriptive activity names to name the activities in the data set
## 4. Appropriately labels the data set with descriptive variable names.
## 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.


##Set Environment Variables

setwd("~/DS-j19/datasciencecoursera/getting_and_cleaning_data/Course Project Assignment/UCI HAR Dataset")

dir_train_dat <- "~/DS-j19/datasciencecoursera/getting_and_cleaning_data/Course Project Assignment/UCI HAR Dataset/train"
dir_test_dat <- "~/DS-j19/datasciencecoursera/getting_and_cleaning_data/Course Project Assignment/UCI HAR Dataset/test"
dir_main <- "~/DS-j19/datasciencecoursera/getting_and_cleaning_data/Course Project Assignment/UCI HAR Dataset"


## Load  Libraries

library(dplyr)
library(tidyr)



#Read in Data  

## feature_labels =  the descriptors for WHAT is being observed
## activity_labels = the desciptors for the ACTIVITY( walking,runnig etc) that subject is enagged in while observation data is being recorded
## train_dat and test_dat = each row provides OBSERVATIONS on the 561 variables recorded
## each row in train_dat and test_dat represents the observations for a person underataking a specific activity at a particular point in time   
## subject_train and subject_test = the identifiers for the people WHO are being monitored 




feature_labels <- read.table(paste(dir_main ,"/", "features.txt",sep=""),col.names=c("index_f","feature"))


activity_labels <-  read.table(paste(dir_main ,"/", "activity_labels.txt",sep=""),col.names=c("index_a","activity"))


train_dat <- read.table(paste(dir_train_dat ,"/", "X_train.txt",sep=""), col.names=feature_labels[1:561,2])

test_dat <- read.table(paste(dir_test_dat,"/","X_test.txt",sep=""), col.names=feature_labels[1:561,2])

test_activity <- read.table(paste(dir_test_dat,"/","y_test.txt",sep=""), col.names=c("activity") )

train_activity <- read.table(paste(dir_train_dat,"/","y_train.txt",sep=""), col.names=c("activity"))

train_subject <- read.table(paste(dir_train_dat ,"/", "subject_train.txt",sep=""), col.names=c("subject")) 

test_subject <- read.table(paste(dir_test_dat ,"/", "subject_test.txt",sep=""), col.names=c("subject")) 


## Add Activity and Subject Identifiers to Test and Training Data Sets

test_dat <- cbind(test_dat, activity=test_activity, subject=test_subject)

train_dat <- cbind(train_dat,activity=train_activity, subject=train_subject)


## Since only interested in observations of mean and standard deviation 
## will subset from features just those labels
## then subset from Test and Train data just the columns of interest
pattern <- "mean|std"

focus_features <- feature_labels[grep(pattern, feature_labels$feature ),]

feature_cols_to_extract <- as.vector(focus_features$index_f) 

cols_to_extract <- append(feature_cols_to_extract,562:563)




test_dat <- test_dat[,cols_to_extract]

train_dat <- train_dat[,cols_to_extract]




## Merge Test and Training Data

merged_dat <- rbind(test_dat,train_dat)

merged_dat <- merge(activity_labels,merged_dat,by.x="index_a",by.y="activity")

## check for NAs
all(colSums(is.na(merged_dat))==0)

## Group Data for each subject and activity and
## Calculate mean for all observations across activities for every subject 
## Resulting in a grouped dataframe  with dimensions 180 x 82
## 180 rows = 6(#activities) * 30(#subjects), 82 columns = 82 observations (3 id factors, 79 measurements)

summary_mean_values <- merged_dat %>%
  group_by(activity,subject) %>%
  summarize_all(mean)

## Write out results in table format  to a text file

write.table(summary_mean_values, file = "wearables_analysis.txt", quote=FALSE, sep =" ",row.names=FALSE)





