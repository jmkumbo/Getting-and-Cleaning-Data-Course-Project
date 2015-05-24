getwd()
setwd("C:/Users/Admin/Desktop/Coursera/datascience_johnhopkins")
if(!file.exists("./Getting_and_cleaning_data_Project")){
              dir.create("./Getting_and_cleaning_data_Project")
      }
##fileUrl <- "http://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
##download.file(fileUrl,destfile="./Getting_and_cleaning_data_Project/Dataset.zip", mode="w")


##Unzip zipped file Dataset.zip
##unzip("./Getting_and_cleaning_data_Project/Dataset.zip",exdir="./Getting_and_cleaning_data_Project")

## UCI HAR Dataset folder contains files,: 
## path_rf <- file.path("./Getting_and_cleaning_data_Project" , "UCI HAR Dataset")
## files<-list.files(path_rf, recursive=TRUE)
## files

##Loading libraries
library(data.table)
library(dplyr)

##To read Supporting Metadata
featureNames <- read.table("./UCI HAR Dataset/features.txt")
activityLabels <- read.table("./UCI HAR Dataset/activity_labels.txt", header = FALSE)

##To format train data
##The data is split up into subject, activity and features

##To read train data
subjectTrain <- read.table("./UCI HAR Dataset/train/subject_train.txt", header = FALSE)
activityTrain <- read.table("./UCI HAR Dataset/train/y_train.txt", header = FALSE)
featuresTrain <- read.table("./UCI HAR Dataset/train/X_train.txt", header = FALSE)

##To read test data
subjectTest <- read.table("./UCI HAR Dataset/test/subject_test.txt", header = FALSE)
activityTest <- read.table("./UCI HAR Dataset/test/y_test.txt", header = FALSE)
featuresTest <- read.table("./UCI HAR Dataset/test/X_test.txt", header = FALSE)


##To merge the training and the test data sets to create one data set
subject <- rbind(subjectTrain, subjectTest)
activity <- rbind(activityTrain, activityTest)
features <- rbind(featuresTrain, featuresTest)

##To name the column names from the features file in variable featureNames
colnames(features) <- t(featureNames[2])

##To add activity and subject as a column to features
colnames(activity) <- "Activity"
colnames(subject) <- "Subject"
completeData <- cbind(features,activity,subject)


##Extraction of the measurements on the mean and standard deviation for each measurement
columnsWithMeanSTD <- grep(".*Mean.*|.*Std.*", names(completeData), ignore.case=TRUE)

##Addition of activity and subject columns
requiredColumns <- c(columnsWithMeanSTD, 562, 563)

##To look at the number of variables in completeData
dim(completeData)
extractedData <- completeData[,requiredColumns]

##To look at the number of variables in extractedData
dim(extractedData)


##Uses of descriptive activity names to name the activities in the data set

extractedData$Activity <- as.character(extractedData$Activity)
for (i in 1:6){
  extractedData$Activity[extractedData$Activity == i] <- as.character(activityLabels[i,2])
}
##To set the activity variable in the data as a factor
extractedData$Activity <- as.factor(extractedData$Activity)
 

##To look at variable names
names(extractedData)

### Appropriately labels the data set with descriptive variable names.

##Acc can be replaced with Accelerometer
##Gyro can be replaced with Gyroscope
##BodyBody can be replaced with Body
##Mag can be replaced with Magnitude
##Character 'f' can be replaced with Frequency
##Character 't' can be replaced with Time

names(extractedData)<-gsub("Acc", "Accelerometer", names(extractedData))
names(extractedData)<-gsub("Gyro", "Gyroscope", names(extractedData))
names(extractedData)<-gsub("BodyBody", "Body", names(extractedData))
names(extractedData)<-gsub("Mag", "Magnitude", names(extractedData))
names(extractedData)<-gsub("^t", "Time", names(extractedData))
names(extractedData)<-gsub("^f", "Frequency", names(extractedData))
names(extractedData)<-gsub("tBody", "TimeBody", names(extractedData))
names(extractedData)<-gsub("-mean()", "Mean", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-std()", "STD", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-freq()", "Frequency", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("angle", "Angle", names(extractedData))
names(extractedData)<-gsub("gravity", "Gravity", names(extractedData))


##To look at new variable names
names(extractedData)


##Part 5 - From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
##Set the subject variable in the data as a factor

extractedData$Subject <- as.factor(extractedData$Subject)
extractedData <- data.table(extractedData)

##To create tidyData as a set with average for each activity and subject
tidyData <- aggregate(. ~Subject + Activity, extractedData, mean)

##Order tidayData according to subject and activity
tidyData <- tidyData[order(tidyData$Subject,tidyData$Activity),]

##Write tidyData into a text file
write.table(tidyData, file = "Tidydata.txt", row.names = FALSE)