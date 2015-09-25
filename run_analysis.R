

if(!file.exists("glddata.zip")) 
  
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip","glddata.zip")

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
# Raw data
trn_raw_sub_dir<-"train\\Inertial Signals\\"
accx<-"body_acc_x_train.txt"
accy<-"body_acc_y_train.txt"
accz<-"body_acc_z_train.txt"
gyrox<-"body_gyro_x_train.txt"
gyroy<-"body_gyro_y_train.txt"
gyroz<-"body_gyro_z_train.txt"
accxt<-"total_acc_x_train.txt"
accyt<-"total_acc_y_train.txt"
acczt<-"total_acc_z_train.txt"
trainfiles<-c(accx,accy,accz,gyrox,gyroy,gyroz, accxt,accyt,acczt)
measureColumns<-substr(trainfiles,1,nchar(trainfiles)-10)

# TEST DATA
test_sub_dir<-"test\\"
#Subject data
testrawsubject<-"subject_test.txt"
#Calculated data
testcalc<-"X_test.txt"
#Activitiy data
testrawactivity<-"y_test.txt"
# Raw data
test_raw_sub_dir<-"test\\Inertial Signals\\"
taccx<-"body_acc_x_test.txt"
taccy<-"body_acc_y_test.txt"
taccz<-"body_acc_z_test.txt"
tgyrox<-"body_gyro_x_test.txt"
tgyroy<-"body_gyro_y_test.txt"
tgyroz<-"body_gyro_z_test.txt"
taccxt<-"total_acc_x_test.txt"
taccyt<-"total_acc_y_test.txt"
tacczt<-"total_acc_z_test.txt"

testfiles<-c(taccx,taccy,taccz,tgyrox,tgyroy,tgyroz, taccxt,taccyt,tacczt)
measureColumns<-substr(trainfiles,1,nchar(trainfiles)-10)


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

# 

# read subject

subject<-read.table(paste(DATA_DIR,trn_sub_dir, rawsubject,sep=""))[,1]

length(subject)  #7352

calcdf$subject<-subject   #  Adding subject to calculated data set


activity<-read.table(paste(DATA_DIR,trn_sub_dir, rawactivity,sep=""))[,1]

length(activity)  #7352

calcdf$activity<-activity   #  Adding subject to calculated data set

# Assigning Window Number
windownum<-1:length(subject)
calcdf$windownum<-windownum

#replace acivity to labels
calcdf$activityLabel<-actlbl[calcdf$activity]


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

# 

# read subject

tsubject<-read.table(paste(DATA_DIR,test_sub_dir, testrawsubject,sep=""))[,1]

length(tsubject)  #2947

tcalcdf$subject<-tsubject   #  Adding subject to calculated data set


tactivity<-read.table(paste(DATA_DIR,test_sub_dir, testrawactivity,sep=""))[,1]

length(tactivity)  #2947

tcalcdf$activity<-tactivity   #  Adding subject to calculated data set

# Assigning Window Number
windownum<-(max(windownum)+1):(max(windownum)+length(tsubject))
tcalcdf$windownum<-windownum

#replace acivity to labels
tcalcdf$activityLabel<-actlbl[tcalcdf$activity]



## EXTRACT ONLY MEAN AND SD   fcol  variable has all the features

# get all means from the feature columns
m<-grep("mean",fcol,value=TRUE)
# get all means from the feature columns
s<-grep("std",fcol,value=TRUE)
# Extract columns having mean and std
fcolfilter<-c(m,s)

# Need all filter columns with subject and activity label

filtercols<-c("subject","activityLabel",fcolfilter)

tidymergedata<-rbind(calcdf[,filtercols],tcalcdf[,filtercols])

sum(is.na(tidymergedata))  # to check any NA   should be zero

## label chanes 
names(tidymergedata)<-gsub("std\\(\\)", "SD", names(tidymergedata))
names(tidymergedata)<-gsub("mean\\(\\)", "MEAN", names(tidymergedata))
names(tidymergedata)<-gsub("^t", "time", names(tidymergedata))
names(tidymergedata)<-gsub("^f", "frequency", names(tidymergedata))
names(tidymergedata)<-gsub("Acc", "Accelerometer", names(tidymergedata))
names(tidymergedata)<-gsub("Gyro", "Gyroscope", names(tidymergedata))
names(tidymergedata)<-gsub("Mag", "Magnitude", names(tidymergedata))
names(tidymergedata)<-gsub("BodyBody", "Body", names(tidymergedata))

tidymergedatamean<-aggregate(.~subject+activityLabel, data=tidymergedata, mean)

write.table(tidymergedatamean, "tidymeandata.txt",row.name=FALSE)


