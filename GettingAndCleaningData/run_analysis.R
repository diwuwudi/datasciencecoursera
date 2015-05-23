### First we need to open a series of files
### NOTE that the path here only works on a WINDOWS system because of "\\"
### Get the current working directory where all the files are located
path = getwd()

### Get path to folder "test" and folder "train"
path_test = paste(path, "\\test", sep = "")
path_train = paste(path, "\\train", sep = "")

### Open "X_test.txt" as test
test <- read.table(paste(path_test, "\\X_test.txt", sep = ""))

### Open "subject_test.txt" as sub_test
sub_test <- read.table(paste(path_test, "\\subject_test.txt", sep = ""))

### Open "y_test.txt" as act_test
act_test <- read.table(paste(path_test, "\\y_test.txt", sep = ""))

### Open "X_train.txt" as train
train <- read.table(paste(path_train, "\\X_train.txt", sep = ""))

### Open "subject_train.txt" as sub_train
sub_train <- read.table(paste(path_train, "\\subject_train.txt", sep = ""))

### Open "y_test.txt" as act_train
act_train <- read.table(paste(path_train, "\\y_train.txt", sep = ""))

### Open "activity_labels" as act_label
act_label <- read.table(paste(path, "\\activity_labels.txt", sep = ""))

### Open "features.txt" as colnames
colnames <- read.table(paste(path, "\\features.txt", sep = ""))

### Change the colnames of dataset "train" and "test"
colnames = t(colnames)
colnames = colnames[-1,]
colnames(test) <- c(colnames)
colnames(train) <- c(colnames)

### Merge train and test data
new <- rbind(test, train)

### Extract only the measurements on the mean and standard deviation for each measurement
a = NULL
for (i in 1:561) {
  if (grepl("mean()", names(new)[i]) == TRUE) {
    a[i] = i
  }
  else if (grepl("std()", names(new)[i]) == TRUE) {
    a[i] = i
  }
}

### remove all the NA values from a
a <- a[!is.na(a)]
col_index <- c(a)

### keep all the mean and std columns
mean_std_data <- new[, col_index]

### extract the column names and check whether they truly are mean and standard 
### deviation values by viewing names(mean_std_data)

### Column names like "fBodyAcc-meanFreq()-Y" are not mean values so they are 
### excluded from the mean_std_data data frame. That gives us 66 columns 
### with 33 mean and 33 std.

mean_std_data <- mean_std_data[, -c(47,48,49, 56,57,58,65,66,67,70,73,76,79)]

### Then we need to add rownames (subject number and activity name)

sub <- rbind(sub_test, sub_train)
colnames(sub) <- c("Subject")
act <- rbind(act_test, act_train)
colnames(act) <- c("Activity")
data2 <- cbind(act, mean_std_data)
data2 <- cbind(sub, data2)

### Then we need to change activity number into activity names
act_label <- act_label[, -1]
act_label <- as.vector(act_label)

for (i in 1:10299) {
  if (data2[i,2] ==1) {
    data2[i,2] <- act_label[1]
  }
  else if (data2[i,2] ==2) {
    data2[i,2] <- act_label[2]
  }
  else if (data2[i,2] ==3) {
    data2[i,2] <- act_label[3]
  }
  else if (data2[i,2] ==4) {
    data2[i,2] <- act_label[4]
  }
  else if (data2[i,2] ==5) {
    data2[i,2] = act_label[5]
  }
  else {
    data2[i,2] <- act_label[6]
  }
}

### then we calculate the required average figures 
### and order the final result by subject number
mean <- aggregate(data2, by = list(data2$Subject, data2$Activity), FUN = "mean")
mean <- mean[, -c(3,4)]
colnames(mean)[1] <- "Subject"
colnames(mean)[2] <- "Activity"
mean <- mean[order(mean$Subject, rev(mean$Activity), decreasing = FALSE), ]

### mean is the final cleaned dataset ordered by subject, which has 180 observations 
### and 68 variables (each subject has 6 activity records)

### At last we create a "mean.txt" file in the current directory
write.table(mean, file = paste(path, "\\mean.txt", sep = ""), row.names = FALSE, sep = "\t")

### The "mean.txt" is a tab delimited txt tile





