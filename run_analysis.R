
##Merges the training and the test sets to create one data set.

X_test <- read.table(
        "./getting_data_quiz4/UCI HAR Dataset/test/X_test.txt",
        comment.char="")

y_test <- read.table(
        "./getting_data_quiz4/UCI HAR Dataset/test/y_test.txt",
        col.names=c("activity"))

subject_test <- read.table(
        "./getting_data_quiz4/UCI HAR Dataset/test/subject_test.txt",
        col.names=c("subject"))

str(X_test)
str(y_test)
str(subject_test)
test <- cbind(X_test, y_test, subject_test)


X_train <- read.table(
        "./getting_data_quiz4/UCI HAR Dataset/train/X_train.txt",
        comment.char="")

y_train <- read.table(
        "./getting_data_quiz4/UCI HAR Dataset/train/y_train.txt",
        col.names=c("activity"))

subject_train <- read.table(
        "./getting_data_quiz4/UCI HAR Dataset/train/subject_train.txt",
        col.names=c("subject"))

str(X_train)
str(y_train)
str(subject_train)
train <- cbind(X_train, y_train, subject_train)

data <- rbind(test, train)

##Extracts only the measurements on the mean and standard deviation for each measurement.

feature_list <- read.table(
        "./getting_data_quiz4/UCI HAR Dataset/features.txt", 
        col.names = c("id", "name"))
features <- c(as.vector(feature_list[, "name"]),
              "subject", "activity")
filtered_feature_ids <- grepl("mean|std|subject|activity", features) & 
        !grepl("meanFreq", features)
filtered_data = data[, filtered_feature_ids]

## Uses descriptive activity names to name the activities in the data set


activities <- read.table(
        "./getting_data_quiz4/UCI HAR Dataset/activity_labels.txt", 
        col.names=c("id", "name"))
for (i in 1:nrow(activities)) {
        filtered_data$activity[filtered_data$activity == activities[i, "id"]] 
        <- as.character(activities[i, "name"])
}

## Appropriately labels the data set with descriptive variable names.
filtered_feature_names <- features[filtered_feature_ids]
filtered_feature_names <- gsub("\\(\\)", "", filtered_feature_names)
filtered_feature_names <- gsub("Acc", "-acceleration", filtered_feature_names)
filtered_feature_names <- gsub("Mag", "-Magnitude", filtered_feature_names)
filtered_feature_names <- gsub("^t(.*)$", "\\1-time", filtered_feature_names)
filtered_feature_names <- gsub("^f(.*)$", "\\1-frequency", filtered_feature_names)
filtered_feature_names <- gsub("(Jerk|Gyro)", "-\\1", filtered_feature_names)
filtered_feature_names <- gsub("BodyBody", "Body", filtered_feature_names)
filtered_feature_names <- tolower(filtered_feature_names)
names(filtered_data) <- filtered_feature_names

## From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
library(dplyr)
tidy_data <- tbl_df(filtered_data) %>%
        group_by('subject', 'activity') %>%
        summarise_each(funs(mean)) %>%
        gather(measurement, mean, -activity, -subject)
write.table(tidy_data, file="tidy_data.txt", row.name=FALSE)
