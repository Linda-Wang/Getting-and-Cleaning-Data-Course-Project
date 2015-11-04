# Getting and Cleaning Data Course Project
## The Course Project for Getting and Cleaning Data
## Author: Linda Wang
## Date: 11/08/2015

Project Description:
Create one R script called run_analysis.R that does the following:
1. Merges the training and the test sets to create one data set.
2. Extracts only the measurements on the mean and standard deviation for each measurement. 
3. Uses descriptive activity names to name the activities in the data set
4. Appropriately labels the data set with descriptive variable names. 
5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

Overview of the Data:
1. Data used for the project: 
data collected from the accelerometers from the Samsung Galaxy S smartphone.
2. Data after cleaning-up: 
an output with only the average of each measurement on the mean and standard deviation for each measurement by activity and subject, 
with training and test data merged together, and with descriptive activity and variable names.
(See the R script (code) for notes on how "measurements on the mean and standard deviation" is defined.)

Notes on Variables:
(See notes toward the end of this code book on descriptive variable names; for this part here before moving to the script, no deccriptive ones will be mentioned.)
Time domain data come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ;
acceleration signal was then separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ);
body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ);
magnitude of these three-dimensional signals were calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag);
Fast Fourier Transform (FFT) was applied to some of these signals producing frequency domain signals (fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag);
'-XYZ' is used to denote 3-axial signals in the X, Y and Z directions; mean() (mean value), std() (standard deviation), and meanFreq() (weighted average of the frequency components to obtain a mean frequency) are estimated from the above signals.

Before Writing/Running the Code:
it is recommended to take a look at each of the data files by trying to import them into R
download the data from the link provided in the course web page and unzip, and
put all of the individual txt files in a single folder, and
set this folder above to be the working directory.

To merge the sets (Step 1), first note that:
1. the training set (X_train.txt) has 7352 obervations (rows), and 561 variables (columns);
2. the test set (X_test.txt) has 2947 observations (rows), and 561 variables (columns);
3. y_train.txt contains labels for the 6 activities, and there are 7352 of them;
4. y_test.txt contains labels for the 6 activities, and there are 2947 of them;
5. column names of the sets can be "mapped" by the file features.txt;
6. subject_train.txt is a list of 7352 participant observations (labels from 1 to 30); 2947 for subject_test.txt;
7. activity_labels.txt relates the descriptive names to the number id's of each the 6 activities. 

Overview of Operations:

library(data.table)
-- Because later some data.table functions will be used, first load the data.table package.

x_train <- read.table("X_train.txt")
y_train <- read.table("y_train.txt")
subject_train <- read.table("subject_train.txt")
x_test <- read.table("X_test.txt")
y_test <- read.table("y_test.txt")
subject_test <- read.table("subject_test.txt")
features <- read.table("features.txt")
activities <- read.table("activity_labels.txt")
-- Import all of the data that will be used: 8 files mentioned above; 
-- assign them to the descriptive variables.

merged_data_full <- as.data.frame(rbind(cbind(y_train, subject_train, x_train), cbind(y_test, subject_test, x_test)))
var_names <- c("Subject", "Activity", as.vector(features[,2]))
colnames(merged_data_full) <- var_names
-- Add the participant id's and activity id's as the first 2 columns to each of the sets using cbind(), 
-- and rbind() the "complemented" training and test sets; 
-- then assign the merged set column names, where the first two for subjects and activities will be manually typed, 
-- and the column names for the 561 measurement elements are from the second column of features.txt.

merged_data_partial <- merged_data_full[, grepl("Activity|Subject|std\\(\\)|mean\\(\\)", colnames(merged_data_full))]
col_activity <- merged_data_partial[,1]
col_subject <- merged_data_partial[,2]
-- Because we want only the means and standard deviations for each measurement, use grepl() to select the ones desired (by "partially matching the column names"); 
-- call the "partial collection of data sets" "merged_data_partial";
-- save the columns of activities and subjects for later.
-- (The above is when excluding "meanFreq" (assuming the "weighted average is not a 'true' mean"); to include "meanFreq", use: grepl("Activity|Subject|std|mean",colnames(merged_data_full)).)

merged_data_sort <- merged_data_partial[order(as.vector(col_activity),as.vector(col_subject)),]
-- Sort merged_data_partial by activities, then by subjects, in ascending order. 

merged_data_split <- split(merged_data_sort,as.vector(merged_data_sort[,1]))
-- Split the data by activity (i.e. the first column of itself).

output <- data.frame()
for (i in 1:length(names(merged_data_split))){
  act_group <- as.data.frame(matrix(as.vector(unlist(merged_data_split[i])),ncol=ncol(merged_data_sort),byrow=FALSE))
  act_group <- data.table(act_group)
  group_avg_by_subj <- as.data.frame(act_group[,lapply(.SD,mean),by=V2])
  output <- rbind(output,group_avg_by_subj)}
-- With a for loop, compute the means for each of the activity-based subset of data, and rbind the results into a data frame.

activiy_descrp <- factor(as.vector(output[,2]), labels=as.vector(activities[,2]))
output[,2] <- activiy_descrp
colnames(output) <- colnames(merged_data_sort)
colnames(output) <- sub("BodyBody","Body",colnames(output))
colnames(output) <- sub("^t","TimeDomain",colnames(output))
colnames(output) <- sub("^f","FreqDomain",colnames(output))
colnames(output) <- gsub("Acc","Acceleration",colnames(output))
colnames(output) <- gsub("Mag","Magnitude",colnames(output))
output <- output[,c(2,1,3:ncol(output))]
-- Create a factor vector of activities, and label the output (tidy data) with descriptive activity names in the second column;
-- label the columns appropriately, correct error/typos, and use descriptive names (- notations standard as "std" stay, but less obvious ones like above get changed);
-- Reorganize the output data so that activities appear first, then subjects.

write.table(as.data.frame(output), file="Means by Activity & Subject - Getting & Cleaning Data Course Project.txt", row.name=FALSE)
-- Write the tidy data out to a text file.
-- (Depending on how "measurements on the mean and standard deviation" is defined (i.e. whether weighted average (meanFreq) is treated as an average/a mean), two versions of the tidy data (66 vs 79 measurements) are included in the repo.)
