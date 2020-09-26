##############################################################################
#
# FILE
#   run_analysis.R
#
# THIS FILE FOR ANALYZE DATA FOR HOMEWORK FROM COURSERA
#

library(dplyr)



# 1)Getting data
############################################################################################################################################################

#download zip file from link in coursera course
zipUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zipFile <- "UCI HAR Dataset.zip"

if (!file.exists(zipFile)) {
  download.file(zipUrl, zipFile, mode = "wb")
}

#unzip file
dataPath <- "UCI HAR Dataset"
if (!file.exists(dataPath)) {
  unzip(zipFile)
}

# 2)Reading Data
############################################################################################################################################################

#reading training data
trainingSubjects <- read.table(file.path(dataPath, "train", "subject_train.txt"))
trainingValues <- read.table(file.path(dataPath, "train", "X_train.txt"))
trainingActivity <- read.table(file.path(dataPath, "train", "y_train.txt"))

#reading test data
testSubjects <- read.table(file.path(dataPath, "test", "subject_test.txt"))
testValues <- read.table(file.path(dataPath, "test", "X_test.txt"))
testActivity <- read.table(file.path(dataPath, "test", "y_test.txt"))

#reading features
features <- read.table(file.path(dataPath, "features.txt"), as.is = TRUE)
  ## note: feature names (in features[, 2]) are not unique
  ##       e.g. fBodyAcc-bandsEnergy()-1,8

#reading activities
activities <- read.table(file.path(dataPath, "activity_labels.txt"))
colnames(activities) <- c("activityId", "activityLabel")

# 3) Create Dataset and Edit
############################################################################################################################################################

humanActivity <- rbind(
  cbind(trainingSubjects, trainingValues, trainingActivity),
  cbind(testSubjects, testValues, testActivity)
)

#column names
colnames(humanActivity) <- c("subject", features[, 2], "activity")

# 4)Standard Deviation For Measurements
############################################################################################################################################################

#determine columns
columnsToKeep <- grepl("subject|activity|mean|std", colnames(humanActivity))

humanActivity <- humanActivity[, columnsToKeep]

#Change activity values
humanActivity$activity <- factor(humanActivity$activity, 
  levels = activities[, 1], labels = activities[, 2])

#column names
humanActivityCols <- colnames(humanActivity)

#remove some characters
humanActivityCols <- gsub("[\\(\\)-]", "", humanActivityCols)

humanActivityCols <- gsub("^f", "frequencyDomain", humanActivityCols)
humanActivityCols <- gsub("^t", "timeDomain", humanActivityCols)
humanActivityCols <- gsub("Acc", "Accelerometer", humanActivityCols)
humanActivityCols <- gsub("Gyro", "Gyroscope", humanActivityCols)
humanActivityCols <- gsub("Mag", "Magnitude", humanActivityCols)
humanActivityCols <- gsub("Freq", "Frequency", humanActivityCols)
humanActivityCols <- gsub("mean", "Mean", humanActivityCols)
humanActivityCols <- gsub("std", "StandardDeviation", humanActivityCols)

humanActivityCols <- gsub("BodyBody", "Body", humanActivityCols)

#using new label names
colnames(humanActivity) <- humanActivityCols

# 5)Cleaning and tidy dataset
############################################################################################################################################################

#grouping by subject and activity 
humanActivityMeans <- humanActivity %>% 
  group_by(subject, activity) %>%
  summarise_each(funs(mean))

#output "tidy_data.txt"
write.table(humanActivityMeans, "tidy_data.txt", row.names = FALSE, 
            quote = FALSE)
