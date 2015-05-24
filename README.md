# GettingData_CourseProject
This repo contains CodeBook.md, README.md, run_analysis.R code, and link to *txt file with deliverable
---
title: "README.md"
author: "squirij"
date: "May 24, 2015"
output: html_document
keep_md: yes   
---

###Creating the tidy datafile
Data from the inertial data files were combined and summarized.  
For this project the following steps were taken to get to final deliverable - 
A Tidy Data Set with Mean of Measurements by Subject, Activity, and Measurement

The data was taken from two directories : 1) a directory with data for subjects in the training group and 2) a directory with data for subjects in the testing group.

For each record in the datasets below: 

* Triaxial acceleration from the accelerometer (total acceleration) and the estimated body  acceleration. 
* Triaxial Angular velocity from the gyroscope. 
* A 561-feature vector with time and frequency domain variables. 
* Its activity label. 
* An identifier of the subject who carried out the experiment.

Files include:
* train/subject_train.txt - list of subjects who carried out the experiments
* train/X_train.txt : Training dataset.
* train/y_train.txt : Training activity labels.
* test/subject_test.txt - list of subjects who carried out the experiments
* test/X_test.txt: Test set.
* test/y_test.txt: Test labels.

### Guide to create the tidy data file
### Step 1 on Assignment - To merge/create the initial tidy data file

All the data was downloaded and unzipped via the url:   

https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip   

Into the working directory :  //Desktop/CleanData/Data/UCI HAR Dataset  

Go to RStudio set wd and open needed library() for the project  

setwd("~/Desktop/CleanData/data")  
library(plyr)  
library(dplyr)  
library(reshape)  

Main object of merging the data is to get the data into the correct format (taken from David's chart on the Discussion Page)    
https://coursera-forum-screenshots.s3.amazonaws.com/fa/abaf9000be11e582e2e199dfe90c4f/Slide2.png 

###1.  Merging all data from the training set - subject, activity, measurements

subjectTrain <- read.table(file = "./UCI HAR Dataset/train/subject_train.txt")    
activityTrain <- read.table(file = "./UCI HAR Dataset/train/y_train.txt")  
train <- cbind(subjectTrain, activityTrain)  
names(train)<- c("Subject", "Activity")  
train <- mutate(train,Group = "Train")  ##to code these subjects as being from train group  
measurementTrain <- read.table(file = "./UCI HAR Dataset/train/x_train.txt", header=FALSE, na.strings="NA")  
train <- cbind(train, measurementTrain)  

###2.  Merging all data from the testing set - subject, activity, measurements
subjectTest <- read.table(file = "./UCI HAR Dataset/test/subject_test.txt")  
activityTest <- read.table(file = "./UCI HAR Dataset/test/y_test.txt")  
test <- cbind(subjectTest, activityTest)  
names(test)<- c("Subject", "Activity")  
test <- mutate(test,Group = "Test")   ## to code these subjects as being from test group   
measurementTest <- read.table(file = "./UCI HAR Dataset/test/x_test.txt", header=FALSE, na.strings="NA")  
test <- cbind(test, measurementTest)  

###3.  Testing that both files contain the same number of columns
dim(train)  
dim(test)  

###Both files have 564 columns (in the final file we should have 7352+2947= rows)

###Now merge the training data and the testing data together
comb <- rbind(train,test)  
dim(comb)  

### Cleaning of the data
This next set of code checks the data frame for any Missing Date, checks to ensure all 30
subjects are in the data frame, checks to see if there are equal number of records across all subjects all activities --- checking for replication, and then removes non-needed files from memory to speed processing

**Check for NAs to know if they should be removed when calculating statistics **

colSums(is.na(comb))

Results:  no missing data 

**are all 30 subjects represented in the combined data file?**

table(comb$Subject)

Results: yes there are 30 subjects in data file  


**do all the subjects have equal measures for all activities?**

table(comb$Activity, comb$Subject)

Results: the number of replicated activities by subject vary.. samples within each 
subject/activity are not equal

**table(comb$Activity(1-6), comb$Subject(1-30))**

    1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30
 1 95 59 58 60 56 57 57 48 52 53 59 50 57 59 54 51 61 56 52 51 52 46 59 58 74 59 57 54 53 65
 2 53 48 59 52 47 51 51 41 49 47 54 52 55 54 48 51 48 58 40 51 47 42 51 59 65 55 51 51 49 65
 3 49 47 49 45 47 48 47 38 42 38 46 46 47 45 42 47 46 55 39 45 45 36 54 55 58 50 44 46 48 62
 4 47 46 52 50 44 55 48 46 50 54 53 51 49 54 59 69 64 57 73 66 85 62 68 68 65 78 70 72 60 62
 5 53 54 61 56 56 57 53 54 45 44 47 61 57 60 53 78 78 73 73 73 89 63 68 69 74 74 80 79 65 59
 6 50 48 62 54 52 57 52 54 50 58 57 60 62 51 72 70 71 65 83 68 90 72 72 72 73 76 74 80 69 70

**to improve efficiency of macro - remove original training and test files from memory**

remove(measurementTest, measurementTrain, subjectTest, subjectTrain, test, train)
remove(activityTest, activityTrain)

#### Step 4 on Assignment - Label the data set with descriptive names

**get header row from features.txt**

header <- read.table(file = "./UCI HAR Dataset/features.txt")
header2 <- t(header[,2])
header2 <-as.vector(header2)
temp <- c("Subject", "Activity", "Group")
header3 <- c(temp,header2)

**add column names to combined training and testing data**

colnames(comb) <- header3

Results are 10299 rows and 564 columns 10299 data rows 

head(comb)
str(comb)

 
###Step 2 - Extract Measurements and get the mean and standard dev for each measurement
**measurements are different summary statistics of the raw inertia data, taking means of means arenot statistically correct but for assignment taking mean of mean will assumed to be correct**

**first, get data set from a wide format to a long format use melt - use only the measurement
fields to be able to get mean and std dev from measurement in next step**

g <- comb[,4:564]
g2 <- melt(g)     

**next by using dplyr summarise command get mean and standard deviation for each measurement**

combsum<- g2 %>% group_by(variable) %>%
  summarise(mean=mean(value), std dev=sd(value))

**to free memory remove melted data**
remove(g, g2)

###Step 3 - Add Descriptions for Activity Names to Data Set and included new variable name 

**get activity labels from the activity_labels.txt to merge the Activity Names with the Activity**
activlab <- read.table(file = "./UCI HAR Dataset/activity_labels.txt")
colnames(activlab) <- c("Activity", "ActivityName")

**add Activity Names to Combined File**

comb2 <- merge(comb, activlab, by.x="Activity", by.y="Activity")

###Step 5 - From Data Set Above create data set with average of for each subject, activity ####
###and measurement - Total n was added to show difference in sample size for each 
###subject/activity combination

**first need to melt measurement data by id = subject and activityname**

g<- melt(comb2, id.var=c("Subject", "ActivityName"), measure.var=(4:564), variable_name="variable")

tidydata2 <- g %>% 
              group_by(Subject, ActivityName, variable) %>%
              summarise(N=n(),Mean=mean(value))

**write/save table as *.txt file in working directory to submit to GitHub**

write.table(tidydata2, file = "./output_from_step5.txt", sep = " ", col.names = TRUE, row.names=FALSE)

### Description of the variables in the submitted tiny_data.txt file
**the txt file submitted has 85860 rows and 5 variables (Subject, Activity Name, 
Variable or Measurement, N (number of samples in the subject/activity combination), and 
Mean of the Variable)**

str(tidydata2)

Classes ‘grouped_df’, ‘tbl_df’, ‘tbl’ and 'data.frame':  85860 obs. of  5 variables:
 $ Subject     : int  1 1 1 1 1 1 1 1 1 1 ...
 $ ActivityName: Factor w/ 6 levels "LAYING","SITTING",..: 1 1 1 1 1 1 1 1 1 1 ...
 $ variable    : Factor w/ 477 levels "tBodyAcc-mean()-X",..: 1 2 3 4 5 6 7 8 9 10 ...
 $ N           : int  50 50 50 50 50 50 50 50 50 50 ...
 $ Mean        : num  0.2216 -0.0405 -0.1132 -0.9281 -0.8368 ...  
 - attr(*, "vars")=List of 2
  ..$ : symbol Subject
  ..$ : symbol ActivityName
 - attr(*, "drop")= logi TRUE
