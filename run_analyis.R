library(dplyr)

X_test <- read.table("UCI HAR Dataset/test/X_test.txt") 
y_test <- read.table("UCI HAR Dataset/test/y_test.txt") 
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt") 

X_train <- read.table("UCI HAR Dataset/train/X_train.txt") 
y_train <- read.table("UCI HAR Dataset/train/y_train.txt") 
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt") 

activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt") 
features <- read.table("UCI HAR Dataset/features.txt") 

# dim()
# head() - specify columns for X_test and X_train since there are 561 columns

# the second column of features contains the column names of X_test / X_train
colnames(X_test) <- features[,2]
colnames(X_train) <- features[,2]

# the test data can be completed by cbinding subject, activity (y_test) 
# and measured data (X_test)
test_data <- cbind(subject_test, y_test, X_test)
# the column names for the first and second column have to be added
colnames(test_data)[1] <- "subject"
colnames(test_data)[2] <- "activity"

# the train data can be completed by cbinding subject, activity (y_train) 
# and measured data (X_train)
train_data <- cbind(subject_train, y_train, X_train)
# the column names for the first and second column have to be added
colnames(train_data)[1] <- "subject"
colnames(train_data)[2] <- "activity"

# test_data and train_data are rbinded to from the dataset data
data <- rbind(test_data, train_data)

# to show only the measurements about the mean and standard deviation a logical
# operation is used to extract these columns from the data
# also the subject and activity columns are extracted
data <- data[, grepl("subject|activity|mean\\(\\)|std\\(\\)", names(data))]

# change content of activity column from numbers to informative text
data <- mutate(data, activity = case_when(
        activity == activity_labels[1,1] ~ activity_labels[1,2],
        activity == activity_labels[2,1] ~ activity_labels[2,2],
        activity == activity_labels[3,1] ~ activity_labels[3,2],
        activity == activity_labels[4,1] ~ activity_labels[4,2],
        activity == activity_labels[5,1] ~ activity_labels[5,2],
        activity == activity_labels[6,1] ~ activity_labels[6,2]))

# to find the average per activity per subject of all measured variables in 
# this dataset the data is first grouped by these variables and consequently
# the average is calculated
data_grouped <- group_by(data, subject, activity)
data_grouped_mean <- summarise_at(data_grouped, 
                vars("tBodyAcc-mean()-X":"fBodyBodyGyroJerkMag-std()"), mean) 

# change the column names of variable columns so it is clear that means are shown
colnames(data_grouped_mean)[3:68] <- paste("Mean", 
                colnames(data_grouped_mean)[3:68], sep = ":")

# output txt file
write.table(data_grouped_mean, file = "data_grouped_mean.txt", row.name=FALSE)
