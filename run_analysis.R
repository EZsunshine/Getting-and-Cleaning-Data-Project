library(dplyr)

#DOWNLOAD DATA
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,"UCI HAR Dataset.zip",method="curl")

if(!file.exists("UCI HAR Dataset")) {
  unzip("UCI HAR Dataset.zip")
}
dateDownloaded <- now()

#READ DATA
#READ TRAINING DATA
train_subject<-read.table("./UCI HAR Dataset/train/subject_train.txt")
training_set<-read.table("./UCI HAR Dataset/train/X_train.txt")
training_labels<-read.table("./UCI HAR Dataset/train/y_train.txt")

#READ TEST DATA
test_subject<-read.table("./UCI HAR Dataset/test/subject_test.txt")
test_set<-read.table("./UCI HAR Dataset/test/X_test.txt")
test_labels<-read.table("./UCI HAR Dataset/test/y_test.txt")

#READ FEATURES
features<-read.table("./UCI HAR Dataset/features.txt",as.is=TRUE)

#READ ACTIVITY LABELS
activity_labels<-read.table("./UCI HAR Dataset/activity_labels.txt")

# 1.MERGE TRAINING DATA AND TEST DATA
fdata<-rbind(cbind(train_subject,training_set,training_labels),
             cbind(test_subject,test_set,test_labels))

colnames(fdata)<-c("subject",features[, 2],"activity")

rm(train_subject,training_set,training_labels,test_subject,test_set,test_labels)

# 2.Extracts only the measurements on the mean and standard deviation for each measurement
extract <- grepl("subject|activity|mean|std", colnames(fdata))
fdata<-fdata[, extract]

# 3.Uses descriptive activity names to name the activities in the data set
fdata$activity<-factor(fdata$activity, 
                       levels = activities[, 1], labels = activities[, 2])

# 4.Appropriately labels the data set with descriptive variable names
fdatacol<-colnames(fdata)
fdatacol <- gsub("^f", "frequencyDomain", fdatacol)
fdatacol <- gsub("^t", "timeDomain", fdata)
fdatacol <- gsub("Acc", "Accelerometer", fdatacol)
fdatacol <- gsub("Gyro", "Gyroscope", fdatacol)
fdatacol <- gsub("Mag", "Magnitude", fdatacol)
fdatacol <- gsub("BodyBody", "Body", fdatacol)
colnames(fdata) <-fdatacol

# 5.From the data set in step 4, creates a second, 
#independent tidy data set with the average of each variable for each activity and each subject.
means <- fdata %>% 
  group_by(subject, activity) %>%
  summarise_all(funs(mean))

write.table(means, "tidy_data.txt", row.names = FALSE, 
            quote = FALSE)



