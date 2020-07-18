## Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most 
## advanced algorithms to attract new users. The data linked to from the course 
## website represent data collected from the accelerometers from the Samsung Galaxy S smartphone.

library(data.table)
library(dplyr)


# Download the dataset
path <- getwd()
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url, file.path(path, "dataFiles.zip"))
unzip(zipfile = "dataFiles.zip")


## Read train and test sets
train <- fread(file.path(path, "UCI HAR Dataset/train/X_train.txt"))
test <- fread(file.path(path, "UCI HAR Dataset/test/X_test.txt"))

## 1. Merges the training and the test sets to create one data set.
combined <- rbind(train, test)


## 2. Extracts only the measurements on the mean and standard deviation for each measurement.
# Load features labels
featuresLabels <- fread(file.path(path, "UCI HAR Dataset/features.txt")
                        , col.names = c("index", "featureName"))

# Select features required (mean and standard deviation)
featuresSelected <- grep("(mean|std)\\(\\)", featuresLabels[, featureName])
measurements <- featuresLabels[featuresSelected, featureName]
measurements <- gsub('[()]', '', measurements)

combined <- combined[, featuresSelected, with = FALSE]

## 4. Appropriately labels the data set with descriptive variable names.
setnames(combined, colnames(combined), measurements)


## 3. Uses descriptive activity names to name the activities in the data set
trainActivities <- fread(file.path(path, "UCI HAR Dataset/train/Y_train.txt")
                         , col.names = c("Activity"))
trainSubjects <- fread(file.path(path, "UCI HAR Dataset/train/subject_train.txt")
                       , col.names = c("SubjectNum"))

testActivities <- fread(file.path(path, "UCI HAR Dataset/test/Y_test.txt")
                        , col.names = c("Activity"))
testSubjects <- fread(file.path(path, "UCI HAR Dataset/test/subject_test.txt")
                      , col.names = c("SubjectNum"))

activities <- rbind(trainActivities, testActivities)
subjects <- rbind(trainSubjects, testSubjects)

combined <- cbind(subjects, activities, combined)



# Load activity labels
activityLabels <- fread(file.path(path, "UCI HAR Dataset/activity_labels.txt")
                        , col.names = c("classLabels", "activityName"))

combined$Activity <- factor(combined$Activity, levels= activityLabels$classLabels, labels = activityLabels$activityName)


## 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.
combined$SubjectNum <- as.factor(combined$SubjectNum)

tidy <- combined %>% 
  group_by_(.dots=c("SubjectNum", "Activity"))  %>% 
  summarise_at(.vars=measurements, .funs=mean)


write.csv(tidy, "tidyData.txt", row.names = FALSE, quote=FALSE)
