
#function to convert numbers to their corresponding activity#

to_activity <- function(x){
  if (x == 1){
    return ("WALKING")
  }
  else if (x == 2){
    return ("WALKING_UPSTAIRS")
  }
  else if (x == 3){
    return ("WALKING_DOWNSTAIRS")
  }
  else if (x == 4){
    return ("SITTING")
  }
  else if (x == 5){
    return ("STANDING")
  }
  else if (x == 6){
    return ("LAYING")
  }
}

#load necessary libraries#

library(dplyr)

#download the file from the Internet, unzip it and retrieve the tables we need#

fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
if(!file.exists("data")){
  dir.create("data")
}
download.file(fileURL,destfile = "./data/UCIdataset.zip")
unzip("./data/UCIdataset.zip", files = "UCI HAR Dataset/test/X_test.txt")
col_names <- read.table(unzip("./data/UCIdataset.zip", files = "UCI HAR Dataset/features.txt"))
test <- read.table(unzip("./data/UCIdataset.zip", files = "UCI HAR Dataset/test/X_test.txt"))
test_lable <- read.table(unzip("./data/UCIdataset.zip", files = "UCI HAR Dataset/test/y_test.txt"))
test_subject <- read.table(unzip("./data/UCIdataset.zip", files = "UCI HAR Dataset/test/subject_test.txt"))
train <- read.table(unzip("./data/UCIdataset.zip", files = "UCI HAR Dataset/train/X_train.txt"))
train_lable <- read.table(unzip("./data/UCIdataset.zip", files = "UCI HAR Dataset/train/y_train.txt"))
train_subject <- read.table(unzip("./data/UCIdataset.zip", files = "UCI HAR Dataset/train/subject_train.txt"))

#rename columns#
names(test_lable) <- "activity"
names(test_subject) <- "subject_num"
names(train_lable) <- "activity"
names(train_subject) <- "subject_num"
names(test) <- col_names$V2
names(train) <- col_names$V2
#merge them to form a united data set#

test_all <- data.frame(cbind(test_subject,test_lable, test))
# find the columns to keep#
col_to_keep <- grep(".+mean.+|.+std.+",names(test_all))
train_all <- data.frame(cbind(train_subject,train_lable, train))
test_all <- test_all[,c(1,2,col_to_keep)]
train_all <- train_all[,c(1,2,col_to_keep)]
merged_data <- rbind(test_all, train_all)

#label the activities#
merged_data$activity <- lapply(merged_data$activity, to_activity)

#new data set: group by activities and subjects and find the mean for each variables#
act_sub_grouped_mean <- merged_data %>% 
                group_by(activity, subject_num)%>%
                summarise_all(mean)

#write to a file#
act_sub_grouped_mean$activity <- as.character(act_sub_grouped_mean$activity) 
write.table(act_sub_grouped_mean, file = "./data/tidy_data.txt", row.name=FALSE)
