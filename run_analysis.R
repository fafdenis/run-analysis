
---------------------------
# This file does the following:
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
# 3. Uses descriptive activity names to name the activities in the  data set
# 4. Appropriately labels the data set with descriptive variable names.
# 5. From the data set in step 4, creates a second, independent tidy data set 
# with the average of each variable for each activity and each subject.

---------------------------
# Load libraries
library(dplyr)
library(stringr)
library(reshape2)
library(tidyr)

---------------------------
# Read in all data 
x_test <- read.csv("./UCI HAR Dataset/test/X_test.txt", sep = "", header = FALSE)
y_test <- read.csv("./UCI HAR Dataset/test/y_test.txt", sep = "", header = FALSE)
subject_test <- read.csv("./UCI HAR Dataset/test/subject_test.txt", sep = "", header = FALSE)

x_train <- read.csv("./UCI HAR Dataset/train/X_train.txt", sep = "", header = FALSE)
y_train <- read.csv("./UCI HAR Dataset/train/y_train.txt", sep = "", header = FALSE)
subject_train <- read.csv("./UCI HAR Dataset/train/subject_train.txt", sep = "", header = FALSE)

features <- read.csv("./UCI HAR Dataset/features.txt", sep = "", header = FALSE)
activity_labels <- read.csv("./UCI HAR Dataset/activity_labels.txt", sep = "", header = FALSE)

# Use lowercase for activity levels
levels(activity_labels$V2) <- tolower(levels(activity_labels$V2))

---------------------------
# Merge datasets into a single dataframe
test <- data.frame(subject_test, y_test, x_test)
train <- data.frame(subject_train, y_train, x_train)
rundata <- rbind(train, test)

---------------------------
# Create a character vector using the values in the second column of features
column_names <- as.vector(features[, 2])

# Name columns in rundata using the character vector column_names
colnames(rundata) <- c("subject", "activity", column_names)

# Remove duplicated columns to avoid error message "duplicate column name"
rundata <- rundata[ , !duplicated(colnames(rundata))]

# Select only the columns that contain mean and std
rundata <- select(rundata, contains("subject"), contains("activity"), 
		  contains("mean"), contains("std"), -contains("Freq"), 
		  -contains("angle"))

# Match activity labels to activity codes
rundata$activity <- as.character(activity_labels[
			match(rundata$activity, activity_labels$V1), 'V2'])

# Clean-up column names
colnames(rundata) <- gsub("-", "_", colnames(rundata))
colnames(rundata) <- gsub("\\(\\)", "", colnames(rundata))
colnames(rundata) <- gsub("BodyBody", "Body", colnames(rundata))
colnames(rundata) <- gsub("^f", "f_", colnames(rundata))
colnames(rundata) <- gsub("^t", "t_", colnames(rundata))

# Calculate average of each variable for each activity and each subject
rundata2 <- rundata %>% 
		group_by(subject, activity) %>% 
		summarise_each(funs(mean))

# Reshape data from wide to long
long_data <- melt(rundata2, c("subject","activity"))

# Split variable column into 4 variables
long_data <- separate(long_data, variable, 
		      into = c("domain", "feature", "measure", "axis"), 
		      fill = "right", remove = TRUE)

# Rename levels in domain column
long_data <- mutate(long_data, domain = 
		    	ifelse(domain %in% c("t"), "timed", "filtered"))

# Rename levels in axis column
long_data <- mutate(long_data, axis = 
		    	ifelse(axis %in% c("X"), "x", 
		    	       ifelse(axis %in% c("Y"), "y",
		    	              ifelse(axis %in% c("Z"), "z", "NA"))))

# Rename levels in measure column
long_data <- mutate(long_data, measure = 
		    	ifelse(measure %in% c("std"), "sd", "mean"))

# Spread measure into two columns -- mean, sd
long_data <- spread(long_data, measure, value)

# Create an independent tidy data set
run_analysis <- long_data

# Print first 8 observations of tidy dataset
run_analysis %>% head(8)

# Create a text file with the tidy run analysis data
write.table(run_analysis,"./run_analysis.txt", sep = "", row.name = FALSE) 

