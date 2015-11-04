# run_analysis.R
# Course Project for Getting & Cleaning Data
# Linda Wang @ 11082015

# Download the zipped data and unzip the files; put all of them in a folder; set the folder to be the working directory.

library(data.table)

x_train <- read.table("X_train.txt")
y_train <- read.table("y_train.txt")
subject_train <- read.table("subject_train.txt")
x_test <- read.table("X_test.txt")
y_test <- read.table("y_test.txt")
subject_test <- read.table("subject_test.txt")
features <- read.table("features.txt")
activities <- read.table("activity_labels.txt")

merged_data_full <- as.data.frame(rbind(cbind(y_train, subject_train, x_train), cbind(y_test, subject_test, x_test)))
var_names <- c("Subject", "Activity", as.vector(features[,2]))
colnames(merged_data_full) <- var_names

merged_data_partial <- merged_data_full[, grepl("Activity|Subject|std\\(\\)|mean\\(\\)", colnames(merged_data_full))]
# The above is when excluding "meanFreq" (assuming the "weighted average is not a 'true' mean")
# To include "meanFreq", use: grepl("Activity|Subject|std|mean",colnames(merged_data_full))
col_activity <- merged_data_partial[,1]
col_subject <- merged_data_partial[,2]

merged_data_sort <- merged_data_partial[order(as.vector(col_activity),as.vector(col_subject)),]

merged_data_split <- split(merged_data_sort,as.vector(merged_data_sort[,1]))

output <- data.frame()
for (i in 1:length(names(merged_data_split))){
  act_group <- as.data.frame(matrix(as.vector(unlist(merged_data_split[i])),ncol=ncol(merged_data_sort),byrow=FALSE))
  act_group <- data.table(act_group)
  #Optional for double-checking: write.table(as.data.frame(act_group), file="part-i.txt", row.name=FALSE)
  group_avg_by_subj <- as.data.frame(act_group[,lapply(.SD,mean),by=V2])
  #Optional for double-checking: write.table(group_avg_by_subj, file="avg-i.txt", row.name=FALSE)
  output <- rbind(output,group_avg_by_subj)
}

activiy_descrp <- factor(as.vector(output[,2]), labels=as.vector(activities[,2]))
output[,2] <- activiy_descrp
colnames(output) <- colnames(merged_data_sort)
colnames(output) <- sub("BodyBody","Body",colnames(output))
colnames(output) <- sub("^t","TimeDomain",colnames(output))
colnames(output) <- sub("^f","FreqDomain",colnames(output))
colnames(output) <- gsub("Acc","Acceleration",colnames(output))
colnames(output) <- gsub("Mag","Magnitude",colnames(output))
output <- output[,c(2,1,3:ncol(output))]

write.table(as.data.frame(output), file="Means by Activity & Subject - Getting & Cleaning Data Course Project.txt", row.name=FALSE)
