## Run_Analysis.R will merge all data from the training set and the test sets into one tidy data set
## Next means and standard deviations for all measurements in dataset
## Will obtain descriptive activity names and combine them to the datasets
## Will label dataset with appropriate variable names
## Will create a second tidy data set with the average of each variable for each activity and for each subject
## For this analysis only the train data and test data are being used for calculation.  The inertial
## data is the raw data that was used to calculate the train and test data for each measurement
## Inertia data is not used in this analysis

## run_analysis.R initially programmed on 5/22/2015

###########################################################################################

setwd("~/Desktop/CleanData/data")
library(plyr)
library(dplyr)
library(reshape)

#### Step 1 - Merge the Training and Test Data Sets ####

# Merging all data from the training set 

subjectTrain <- read.table(file = "./UCI HAR Dataset/train/subject_train.txt")
activityTrain <- read.table(file = "./UCI HAR Dataset/train/y_train.txt")
train <- cbind(subjectTrain, activityTrain)
names(train)<- c("Subject", "Activity")
train <- mutate(train,Group = "Train")  ##to code these subjects as being from train group
measurementTrain <- read.table(file = "./UCI HAR Dataset/train/x_train.txt", header=FALSE, na.strings="NA")
train <- cbind(train, measurementTrain)

# Merging all data from the test sets of data

subjectTest <- read.table(file = "./UCI HAR Dataset/test/subject_test.txt")
activityTest <- read.table(file = "./UCI HAR Dataset/test/y_test.txt")
test <- cbind(subjectTest, activityTest)
names(test)<- c("Subject", "Activity")
test <- mutate(test,Group = "Test")   ## to code these subjects as being from test group 
measurementTest <- read.table(file = "./UCI HAR Dataset/test/x_test.txt", header=FALSE, na.strings="NA")
test <- cbind(test, measurementTest)

# ensure column widths the same for each file
dim(train)
dim(test)

# Both files have 564 columns (in the final file we should have 7352+2947 rows + 1 header row)

# Now merge the training data and the testing data together
comb <- rbind(train,test)
dim(comb)

## check for NAs to know if they should be removed when calculating statistics ##

colSums(is.na(comb))

## results:  no missing data 

# are all 30 subjects represented in the combined data file

table(comb$Subject)

# results yes there are 30 subjects in data file  


## do all the subjects have equal measures for all activities?

table(comb$Activity, comb$Subject)

## results: the number of replicated activities by subject vary.. samples within each 
# subject/activity are not equal

# table(comb$Activity(1-6), comb$Subject(1-30))

#    1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30
# 1 95 59 58 60 56 57 57 48 52 53 59 50 57 59 54 51 61 56 52 51 52 46 59 58 74 59 57 54 53 65
# 2 53 48 59 52 47 51 51 41 49 47 54 52 55 54 48 51 48 58 40 51 47 42 51 59 65 55 51 51 49 65
# 3 49 47 49 45 47 48 47 38 42 38 46 46 47 45 42 47 46 55 39 45 45 36 54 55 58 50 44 46 48 62
# 4 47 46 52 50 44 55 48 46 50 54 53 51 49 54 59 69 64 57 73 66 85 62 68 68 65 78 70 72 60 62
# 5 53 54 61 56 56 57 53 54 45 44 47 61 57 60 53 78 78 73 73 73 89 63 68 69 74 74 80 79 65 59
# 6 50 48 62 54 52 57 52 54 50 58 57 60 62 51 72 70 71 65 83 68 90 72 72 72 73 76 74 80 69 70

# to improve efficiency of macro - remove original training and test files from memory

remove(measurementTest, measurementTrain, subjectTest, subjectTrain, test, train)
remove(activityTest, activityTrain)

#### Step 4 - Label Data Set with Description Variable Names ####

# Now get header row from features.txt

header <- read.table(file = "./UCI HAR Dataset/features.txt")
header2 <- t(header[,2])
header2 <-as.vector(header2)
temp <- c("Subject", "Activity", "Group")
header3 <- c(temp,header2)

# add column names to combined training and testing data

colnames(comb) <- header3

# results are 10299 rows and 564 columns 10299 data rows 

head(comb)
str(comb)

#### Step 2 - Extract Measurements and get the mean and standard dev for each measurement####
#### measurements are different summary statistics of the raw inertia data, taking means of means are
#### is not statistically correct but for assignment taking mean of mean will assumed to be correct

# first, get data set from a wide format to a long format use melt - use only the measurement
# fields to be able to get mean and std dev from measurement in next step

g <- comb[,4:564]
g2 <- melt(g)     

# next by using dplyr summarise command get mean and standard deviation for each measurement

combsum<- g2 %>% group_by(variable) %>%
  summarise(mean=mean(value), std dev=sd(value))

# to free memory remove melted data
remove(g, g2)

#### Step 3 - Add Descriptions for Activity Names to Data Set and included new variable name ####

# get activity labels
activlab <- read.table(file = "./UCI HAR Dataset/activity_labels.txt")
colnames(activlab) <- c("Activity", "ActivityName")

# add Activity Names to Combined File

comb2 <- merge(comb, activlab, by.x="Activity", by.y="Activity")

#### Step 5 - From Data Set Above create data set with average of for each subject, activity ####
#### and measurement - Total n was added to show difference in sample size for each 
## subject/activity combination

# first need to melt measurement data by id = subject and activityname

g<- melt(comb2, id.var=c("Subject", "ActivityName"), measure.var=(4:564), variable_name="variable")

tidydata2 <- g %>% 
              group_by(Subject, ActivityName, variable) %>%
              summarise(N=n(),Mean=mean(value))

# save table as *.txt file in working directory to submit to GitHub

write.table(tidydata2, file = "./output_from_step5.txt", sep = " ", col.names = TRUE, row.names=FALSE)

str(tidydata2)
