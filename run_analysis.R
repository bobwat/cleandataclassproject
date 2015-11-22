#Required packages for code
library(plyr)
library(dplyr)

#Create data folder on hard drive
if (!file.exists("data")) {
  dir.create("data")
}

#Obtain date download and write to data directory
dateDownloaded <-date()
write.table(dateDownloaded,file="./data/dateDownloaded.txt",row.names=FALSE,sep="\t")

#URL of folder of zipped files
url<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

#Download, unzip, and store files in ./data directory
download.file(url,destfile = "./data/phone.zip")
list.files <- unzip("./data/phone.zip", exdir = "data")

#Read required tables into R
features <- read.table("./data/UCI HAR Dataset/features.txt", quote="\"")
activity_labels <- read.table("./data/UCI HAR Dataset/activity_labels.txt", quote="\"")

subject_train <- read.table("./data/UCI HAR Dataset/train/subject_train.txt", quote="\"")
X_train <- read.table("./data/UCI HAR Dataset/train/X_train.txt", quote="\"")
y_train <- read.table("./data/UCI HAR Dataset/train/y_train.txt", quote="\"")

subject_test <- read.table("./data/UCI HAR Dataset/test/subject_test.txt", quote="\"")
X_test <- read.table("./data/UCI HAR Dataset/test/X_test.txt", quote="\"")
y_test <- read.table("./data/UCI HAR Dataset/test/y_test.txt", quote="\"")

#####################################################################################################
#Organized Training Data

#Name the Activity matching the Activity Code
names(y_train)[1] <- "act_code"
names(activity_labels)[1]<-"act_code"
combined<-merge(activity_labels, y_train, all=TRUE)
names(combined)[2]<-"activity"

#Rename column to "subject"
names(subject_train)<-"subject"

#Name the data column with the measurements
feat.vec<-as.vector(features$V2)
names(X_train)<-feat.vec

#Add column (mode) denoting test vs training
mode<-rep(c("train"),7352)
combined<-cbind(as.data.frame(mode),combined)

#Combine test data with information: mode(test vs train), subject, activity) 
train.data<-cbind(mode,subject_train$subject,combined$activity,X_train)
names(train.data)[1]<-"Mode"
names(train.data)[2]<-"Subject"
names(train.data)[3]<-"Activity"

##################################################################################
#Organize Test Data

#Name the Activity matching the Activity Code
names(y_test)[1] <- "act_code"
names(activity_labels)[1]<-"act_code"
combined.test<-merge(activity_labels, y_test, all=TRUE)
names(combined.test)[2]<-"activity"

#Rename column to "subject"
names(subject_test)<-"subject"

#Name the data column with the measurements
feat.vec<-as.vector(features$V2)
names(X_test)<-feat.vec

#Add column denoting test vs training
mode.test<-rep(c("test"),2947)
combined.test<-cbind(as.data.frame(mode.test),combined.test)

#Combine test data with information: mode(test vs train), subject, activity) 
test.data<-cbind(mode.test,subject_test$subject,combined.test$activity,X_test)
names(test.data)[1]<-"Mode"
names(test.data)[2]<-"Subject"
names(test.data)[3]<-"Activity"

##################################################################################

#Combine training and test data
combined.data<-rbind(train.data,test.data)
#Remove duplicated columns (none contain mean or standard deviation)
combined.data <- combined.data[ , !duplicated(colnames(combined.data))]

combined.data<-select(combined.data,1:3,contains("mean"),contains("std"))
##################################################################################
#Coumpute Means
#The means are the means of all of the columns of means (means of the means) and the
#means of the columns of standard deviations for the activities and the subjects.

#Coumpute Means of Each Activity
act<-aggregate(combined.data[,4:89],by=list(combined.data$Activity), mean)

#Compute Means of Each Subject
sub<-aggregate(combined.data[,4:89],by=list(combined.data$Subject), mean)
sub$Group.1<-factor(sub$Group.1)

#Create Tidy Data Set of Means for each Subject and each Activity
tidy.dataset<-rbind(act,sub)
names(tidy.dataset)[1]<-"Activity.or.Subject"

#################################################################################
#Write table to harddrive as tab delimited txt file: tidy.dataset.txt
write.table(tidy.dataset,file="./data/tidy.dataset.txt",row.names=FALSE,sep="\t")
