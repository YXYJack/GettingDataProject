packages=c("data.table","plyr","dplyr","reshape")
sapply(packages, require, character.only=TRUE, quietly=TRUE)

setwd("C:/Users/Mark/Desktop/datascience/Cleaning Data/Week4")
##Here are the data for the project:https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
##You should create one R script called run_analysis.R that does the following.
## 1Merges the training and the test sets to create one data set.
## 2Extracts only the measurements on the mean and standard deviation for each measurement.
## 3Uses descriptive activity names to name the activities in the data set
## 4Appropriately labels the data set with descriptive variable names.
## 5From the data set in step 4, creates a second, independent tidy data set with the average of 
##each variable for each activity and each subject.

## 1 Merge the training and the test sets to create one data set.
##first get the data
fileUrl<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
if (!file.exists("getdata-projectfiles-UCI HAR Dataset.zip")){
        download.file(fileUrl,destfile="getdata-projectfiles-UCI HAR Dataset.zip",mode="wb")}

## second load the datasets.  Looking at the .zip we need the following .txt files:
## y_train,y_test,X_test,X_train,subject_test,subject_train

temp<-"getdata-projectfiles-UCI HAR Dataset.zip"

##load the *_train data into data tables
y_train <- read.table(unz(temp, "UCI HAR Dataset/train/y_train.txt"))
X_train <- read.table(unz(temp, "UCI HAR Dataset/train/X_train.txt"))
subject_train <- read.table(unz(temp, "UCI HAR Dataset/train/subject_train.txt"))

##load the *_test data into data tables
y_test <- read.table(unz(temp, "UCI HAR Dataset/test/y_test.txt"))
X_test <- read.table(unz(temp, "UCI HAR Dataset/test/X_test.txt"))
subject_test <- read.table(unz(temp, "UCI HAR Dataset/test/subject_test.txt"))

##load  the feature names into a dataframe
fN<-read.table(unz(temp, "UCI HAR Dataset/features.txt"))
featureName<-dcast(melt(fN, id.vars = "V1"), variable ~ V1)
featureName[,1]<-NULL
##featureName[2,]<-NULL
##featureName<-as.character(featureName)


## third, merge the datasets in pairs:
## subject_test+subject_train
## y_test+y_train
## {subject_test+subject_train}+{y_test+y_train}
## X_test,X_train
## {X_test,X_train}+featurenames

dtSubject<-rbind(subject_test,subject_train)
setnames(dtSubject,"V1","subject")

dtActivity<-rbind(y_test,y_train)
data.table::setnames(dtActivity,"V1","activityNum")

dtSubject<-cbind(dtSubject,dtActivity)

dtX<-rbind(X_test,X_train)

## 4 Appropriately label the data set with descriptive variable names.
names(dtX)<-featureName[1,] ##rename the columns in X_*
dt<-cbind(dtSubject,dtX) ##this is the merged dataset satisfying Step 1 AND Step 4

## 2 Extract only the measurements on the mean and standard deviation for each measurement.
idGroup<-grepl("subject|activityNum|mean|std", colnames(dt))
dtExtract<-dt[ ,idGroup]  ##this is the dataset satisfying Step 2

## 3 use descriptive activity names to name the activities in the data set
##load  the activity names into a dataframe
lookup<-read.table(unz(temp, "UCI HAR Dataset/activity_labels.txt"))
colnames(lookup)<-c("activityNum","activityName")

dtExtractNamed<-merge(dtExtract, lookup, by="activityNum")
dtExtractNamed$activityNum<-NULL
dtExtractNamed<-dtExtractNamed[,c(1,81,2:80)] ##reorder the columns so the df is:
## subject activityName <featureName1..n>  This satisfies Step 3

## 5 From the data set in step 4, creates a second, independent tidy data set with the average of 
##each variable for each activity and each subject.
##Also, change appropriate field names to indicate this is a 'mean of means'
dtMean<-dtExtractNamed %>% group_by(subject,activityName) %>% summarise_each(funs(mean))
colnames(dtMean) <- paste("mean", colnames(dtMean), sep = "_")
names(dtMean)[names(dtMean)%in% c("mean_subject","mean_activityName")] <- c("subject","activityName")
##This satisfies Step 5

##write tidy dataset to file if it doesn't already exist
if(!file.exists("TidyDataSetForCleaningProject.txt")){
write.table(dtMean,file="TidyDataSetForCleaningProject.txt",quote = FALSE, 
            sep = "\t", row.names = FALSE)
}