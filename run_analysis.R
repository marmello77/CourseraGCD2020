################################################################################
##### COURSERA'S GETTING AND CLEANING DATA
##### Author: Marco Mello
##### October 2020
################################################################################


################################################################################
##### Setting the stage
################################################################################

#Set the working directory to where this script is
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Remove all previous objects
rm(list= ls())

#Load the packages to be used
library(tidyverse)
library(data.table)
library(lubridate)


################################################################################
##### Downloading and unzipping the data
################################################################################


#Create a directory for the data, if it does not already exist
if(!file.exists("./data")){dir.create("./data")}

#Download the zipped data from the course's website
fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileURL, destfile="./data/data.zip")

#Unzip the file
unzip(zipfile="./data/data.zip",exdir="./data")


################################################################################
##### Import the data sets
################################################################################


#Import the features
features <- read_table2('./data/UCI HAR Dataset/features.txt', col_names = c("number","signal"))
features

#Import the activity labels
activity_labels = read_table('./data/UCI HAR Dataset/activity_labels.txt', col_names = c("code", "activity"))
activity_labels

#Import the training data set
x_train <- read_table("./data/UCI HAR Dataset/train/X_train.txt", col_names = features$signal)
y_train <- read_table("./data/UCI HAR Dataset/train/y_train.txt", col_names = "code")
subject_train <- read_table("./data/UCI HAR Dataset/train/subject_train.txt", col_names = "subject")

#Check the training data
x_train
y_train
subject_train
range(subject_train)

#Import the testing data set
x_test <- read_table("./data/UCI HAR Dataset/test/X_test.txt", col_names = features$signal)
y_test <- read_table("./data/UCI HAR Dataset/test/y_test.txt", col_names = "code")
subject_test <- read_table("./data/UCI HAR Dataset/test/subject_test.txt", col_names = "subject")

#Check the testing data
x_test
y_test
subject_test
range(subject_test)


################################################################################
##### 1. Merge the training and the test sets to create the One Data Set
################################################################################


# One Data Set to rule them all, 
# One Data Set to find them,
# One Data Set to bring them all, 
# and in the darkness bind them
# (The Lord of R, by J RRRRRRR Tolkien)


#Merge the data step by step
Xdata <- rbind(x_train, x_test)
Ydata <- rbind(y_train, y_test)
Subject <- rbind(subject_train, subject_test)
DataMerged <- cbind(Subject, Ydata, Xdata)

#Check the merged data
DataMerged


################################################################################
##### 2. Extract only the measurements on the mean and standard deviation for each measurement
################################################################################


#Make the embryo of the tidy data set with only the requested variables, as descrived in "features_info.txt"
DataTidy <- DataMerged %>% select(subject, code, contains("mean"), contains("std"))


################################################################################
##### 3. Use descriptive activity names to name the activities in the data set
################################################################################


#Check the columns in the pre-tidy data set
names(DataTidy)
head(DataTidy)

#Check the labels you previously imported
activity_labels

#Create a new column with the activity names as described in "activity_labels.txt"
DataTidy$code <- activity_labels[DataTidy$code, 2]


################################################################################
##### 4. Appropriately label the data set with descriptive variable names
################################################################################


#Give the variables proper names
names(DataTidy)[2] = "activity"
names(DataTidy)<-gsub("Acc", "accelerometer", names(DataTidy))
names(DataTidy)<-gsub("Gyro", "gyroscope", names(DataTidy))
names(DataTidy)<-gsub("BodyBody", "body", names(DataTidy))
names(DataTidy)<-gsub("tBody", "timebody", names(DataTidy))
names(DataTidy)<-gsub("Mag", "bagnitude", names(DataTidy))
names(DataTidy)<-gsub("angle", "angle", names(DataTidy))
names(DataTidy)<-gsub("gravity", "gravity", names(DataTidy))
names(DataTidy)<-gsub("-mean()", "mean", names(DataTidy), ignore.case = TRUE)
names(DataTidy)<-gsub("-std()", "sd", names(DataTidy), ignore.case = TRUE)
names(DataTidy)<-gsub("-freq()", "frequency", names(DataTidy), ignore.case = TRUE)
names(DataTidy)<-gsub("^t", "time", names(DataTidy))
names(DataTidy)<-gsub("^f", "frequency", names(DataTidy))

#Check the names
names(DataTidy)


################################################################################
##### 5. From the data set in step 4, create a second, independent tidy data
##### set with the average of each variable for each activity and each subject
################################################################################


#Pipe everything to calculate the mean values and create a new data set
DataTidyAve <- DataTidy %>%
    group_by(subject, activity) %>%
    summarise_all(mean)

#Check the new data set
DataTidyAve
head(DataTidyAve)
names(DataTidyAve)
summary(DataTidyAve)

#Export the new data set as a TXT file
write.table(DataTidyAve, "DataTidyAve.txt", row.names = FALSE)