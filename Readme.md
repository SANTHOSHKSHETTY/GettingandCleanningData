---
title: "Getting and Cleaning Data Course Project"
author: "SanthoshShetty"
date: "September 24, 2015"
output: html_document
---
# Background

A full description is available at the site where the data was obtained:

http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

Here are the data for the project:

https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

# Decription of the data

The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. These time domain signals (prefix ‘t’ to denote time) were captured at a constant rate of 50 Hz. and the acceleration signal was then separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ) – both using a low pass Butterworth filter.

The body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag).

A Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. (Note the ‘f’ to indicate frequency domain signals).


Description of abbreviations of measurements
1. leading t or f is based on time or frequency measurements.
2. Body = related to body movement.
3. Gravity = acceleration of gravity
4. Acc = accelerometer measurement
5. Gyro = gyroscopic measurements
6. Jerk = sudden movement acceleration
7. Mag = magnitude of movement
8. mean and SD are calculated for each subject for each activity for each mean and SD measurements.

The units given are g’s for the accelerometer and rad/sec for the gyro and g/sec and rad/sec/sec for the corresponding jerks.

These signals were used to estimate variables of the feature vector for each pattern:
‘-XYZ’ is used to denote 3-axial signals in the X, Y and Z directions. They total 33 measurements including the 3 dimensions - the X,Y, and Z axes.

    
- tBodyAcc-XYZ
- tGravityAcc-XYZ
- tBodyAccJerk-XYZ
- tBodyGyro-XYZ
- tBodyGyroJerk-XYZ
- tBodyAccMag
- tGravityAccMag
- tBodyAccJerkMag
- tBodyGyroMag
- tBodyGyroJerkMag
- fBodyAcc-XYZ
- fBodyAccJerk-XYZ
- fBodyGyro-XYZ
- fBodyAccMag
- fBodyAccJerkMag
- fBodyGyroMag
- fBodyGyroJerkMag

The set of variables that were estimated from these signals are:

1. mean(): Mean value
2. std(): Standard deviation

# DATA SET DETAILS

The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data.

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows. From each window, a vector of features was obtained by calculating variables from the time and frequency domain.



# High level design approach to prepare tidy data set

1. Wearable device records raw data
2. This has 128 readings per window ->the sliding windows of 2.56 sec
3. Define a column  for recording a window number -> a unique number for a given window that is 128 records are grouped using this window number to indicate that all 128 recording belonging to the same window. This column is WindowNum  -> This number is  unique to window
4. Calculated data for each window will also be identified by this column.
5. For this assignment , the Goal is to get the average value of mean and sd for all measures
6. Function are created to process raw and calculated data
6. As per scope of the assignment, Process only train calculated data and test calculated data
7. Merge train and test data
8. Create a filter for column to extract only mean and sd for all measures
9. Apply mean for all columns , groupded by subject and activity label
10. Processing of raw data is not in scope however a separate R code is provided in GITHIB if my assumption is wrong 
11. Processing of calculated data is run_analysis.R
12. run_analysis.R  will yield the tidy data set as per the goal of assignment



# DOWNLOAD DATA and IDENTIFY FILES FOR READING INTO R

-  meta data means features and activity labels -This is common to both test and train data
-  calculated data means all processed or estimated data for e.g. mean sd as detailed in begining
-  We have train and test data -both needs to be loaded for processing in next steps


```r
if(!file.exists("glddata.zip")) download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip","glddata.zip")

if(!file.exists("UCI HAR Dataset")) unzip("glddata.zip")

# META DATA
DATA_DIR<-"A:\\SKS\\R\\datasciencespecialization\\GCD\\UCI HAR Dataset\\"
activity<-"activity_labels.txt"  # activity label
feature<-"features.txt"          # calculated features

# TRAIN DATA
trn_sub_dir<-"train\\"
#Subject data
rawsubject<-"subject_train.txt"
#Calculated data
calc<-"X_train.txt"
#Activitiy data
rawactivity<-"y_train.txt"

# TEST DATA
test_sub_dir<-"test\\"
#Subject data
testrawsubject<-"subject_test.txt"
#Calculated data
testcalc<-"X_test.txt"
#Activitiy data
testrawactivity<-"y_test.txt"
```

#   Reading the train data


```r
# Read feaures -This will be part of the columns of tidy data set
fcol<-as.character(read.table(paste(DATA_DIR,feature,sep=""),header=F,sep=" ")[,2])  # features
# Read activity labels
actlbl<-as.character(read.table(paste(DATA_DIR,activity,sep=""),header=F,sep=" ")[,2])
# read estimated or calcullated variable  data


## TRAIN DATA FILES

calcdata<-readLines(paste(DATA_DIR,trn_sub_dir, calc,sep=""))
calcdf<-NULL
for ( i in 1: length(calcdata)) {
    temp<-as.numeric(unlist(strsplit(calcdata[1], " ")))  # spli by space,convert char to numebric
    temp<-temp[!is.na(temp)] # remove space/NA
    calcdf<-rbind(calcdf,temp)
}

calcdf<-as.data.frame(calcdf)
rownames(calcdf)<-NULL
# label the data
colnames(calcdf)<-fcol   # label each columns as that of features
# verify any NA 
sum(is.na(calcdf))    # should be zero this means no NA in clean data
```

```
## [1] 0
```

```r
# 

# read subject

subject<-read.table(paste(DATA_DIR,trn_sub_dir, rawsubject,sep=""))[,1]

length(subject)  #7352
```

```
## [1] 7352
```

```r
calcdf$subject<-subject   #  Adding subject to calculated data set


activity<-read.table(paste(DATA_DIR,trn_sub_dir, rawactivity,sep=""))[,1]

length(activity)  #7352
```

```
## [1] 7352
```

```r
calcdf$activity<-activity   #  Adding subject to calculated data set

# Assigning Window Number
windownum<-1:length(subject)
calcdf$windownum<-windownum

#replace acivity to labels
calcdf$activityLabel<-actlbl[calcdf$activity]
```

#  READ TEST DATA



```r
### TESTING FILES

tcalcdata<-readLines(paste(DATA_DIR,test_sub_dir, testcalc,sep=""))
tcalcdf<-NULL
for ( i in 1: length(tcalcdata)) {
  temp<-as.numeric(unlist(strsplit(tcalcdata[1], " ")))  # spli by space,convert char to numebric
  temp<-temp[!is.na(temp)] # remove space/NA
  tcalcdf<-rbind(tcalcdf,temp)
}

tcalcdf<-as.data.frame(tcalcdf)
rownames(tcalcdf)<-NULL
# label the data
colnames(tcalcdf)<-fcol   # label each columns as that of features
# verify any NA 
sum(is.na(tcalcdf))    # should be zero this means no NA in clean data
```

```
## [1] 0
```

```r
# 

# read subject

tsubject<-read.table(paste(DATA_DIR,test_sub_dir, testrawsubject,sep=""))[,1]

length(tsubject)  #2947
```

```
## [1] 2947
```

```r
tcalcdf$subject<-tsubject   #  Adding subject to calculated data set


tactivity<-read.table(paste(DATA_DIR,test_sub_dir, testrawactivity,sep=""))[,1]

length(tactivity)  #2947
```

```
## [1] 2947
```

```r
tcalcdf$activity<-tactivity   #  Adding subject to calculated data set

# Assigning Window Number
windownum<-(max(windownum)+1):(max(windownum)+length(tsubject))
tcalcdf$windownum<-windownum

#replace acivity to labels
tcalcdf$activityLabel<-actlbl[tcalcdf$activity]
```

# MERGE TRAINING AND TEST DATA


```r
mergedata<-rbind(calcdf,tcalcdf)
```


# EXTRACT ONLY MEAN AND SD


```r
## EXTRACT ONLY MEAN AND SD   fcol  variable has all the features

# get all means from the feature columns
m<-grep("mean",fcol,value=TRUE)
# get all means from the feature columns
s<-grep("std",fcol,value=TRUE)
# Extract columns having mean and std
fcolfilter<-c(m,s)

# Need all filter columns with subject and activity label

filtercols<-c("subject","activityLabel",fcolfilter)

tidymergedata<-mergedata[,filtercols]

sum(is.na(tidymergedata))  # to check any NA   this should be zero
```

```
## [1] 0
```

# Label columns as descriptive as below


- Leading t means time
- Leading f frequency measurements
- Body = related to body movement
- Gravity = acceleration of gravity
- Acc = accelerometer measurement
- Gyro = gyroscopic measurements
- Jerk = sudden movement acceleration
- Mag = magnitude of movement
    



```r
names(tidymergedata)<-gsub("std\\(\\)", "SD", names(tidymergedata))
names(tidymergedata)<-gsub("mean\\(\\)", "MEAN", names(tidymergedata))
names(tidymergedata)<-gsub("^t", "time", names(tidymergedata))
names(tidymergedata)<-gsub("^f", "frequency", names(tidymergedata))
names(tidymergedata)<-gsub("Acc", "Accelerometer", names(tidymergedata))
names(tidymergedata)<-gsub("Gyro", "Gyroscope", names(tidymergedata))
names(tidymergedata)<-gsub("Mag", "Magnitude", names(tidymergedata))
names(tidymergedata)<-gsub("BodyBody", "Body", names(tidymergedata))
```
#  Average of extracted mean and sd by subject and actvity labels


```r
tidymergedatamean<-aggregate(.~subject+activityLabel, data=tidymergedata, mean)
```


#  Write a text file


```r
write.table(tidymergedatamean, "tidymeandata.txt",row.name=FALSE)
```


