# coursera
# getting & cleaning data
# course project
# depends on these packages:
# dpyr
# tidyr

# submit these
# 1 a tidy dataset
# 2 a link to a Github repository with your script performing the analysis

# You should create one R script called 'run_analysis.R' that does this:
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each
#    measurement. 
# 3. Uses descriptive activity names to name the activities in the data set.
# 4. Appropriately labels the data set with descriptive variable names.
# 5. From the data set in step 4, creates a second, independent tidy data set
#    with the average of each variable for each activity and each subject.

# GitHub repo should contain these:
# 1 README.md
#   -- explains how the 'run_analysis.R' script works
# 2 CodeBook.md
#   -- describes the variables, the data and any transformations or work done
#   -- to clean up the data
# 3 run_analysis.R
#   -- 1    merges training and test sets to create one data set
#   -- 2    extracts only the measurements on the mean and standard deviation
#           for each measurement
#   -- 3    gives descriptive activity names to the activies in the data set
#   -- 4    gives descriptive names for the variables in the data set
#   -- 5    creates a 2nd, independent tidy data set with the average of each
#           variable for each activity and each subject

library("curl")

work.dir <- getwd()

# the data for this project is here:
file.url <- file.path(
    "https://d396qusza40orc.cloudfront.net",
    "getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
)
my.zip.file <- file.path(work.dir, "UCI_HAR_Dataset.zip")

download.file(
    url = file.url, destfile = my.zip.file, method = "curl", quiet = TRUE
)

dateDownloaded <- date()
show(dateDownloaded)

if (!file.exists(my.zip.file)) stop("the data file does not exist")

# the archive's directory structure is not preserved
# all files are deposited in the current extraction directory
unzip(zipfile = my.zip.file, junkpaths = TRUE)

# TASK 1. Merge the training and the test sets to create one data set.

library(dplyr)

# TRAIN data set
# number of observations in TRAIN set
n.train <- 7352 

# read TRAIN measurements from 'X_train.txt' into DATA FRAME
train.df <- tbl_df(
    read.table(
        file = "X_train.txt", header = FALSE, sep = "", colClasses = "numeric",
        nrows = n.train, stringsAsFactors = FALSE
    )
)

# read TRAIN subject identifiers from 'subject_train.txt' into VECTOR
subj.train <- scan(file = "subject_train.txt", what = 0, nmax = n.train)

# read TRAIN activity class labels from 'y_train.txt' into VECTOR
act.train <- scan(file = "y_train.txt", what = 0, nmax = n.train)

# assemble TRAIN data set
train.set <- train.df %>%
    mutate(subject = subj.train) %>%
    mutate(activity = act.train)

# TEST data set    
# number of observations in TEST set
n.test <- 2947

# read TEST measurements from 'X_text.txt' into DATA FRAME  
test.df <- tbl_df(
    read.table(
        file = "X_test.txt", header = FALSE, sep = "", colClasses = "numeric",
        nrows = n.test, stringsAsFactors = FALSE
    )
)

# read TEST subject identifiers from 'subject_test.txt' into VECTOR
subj.test <- scan(file = "subject_test.txt", what = 0, nmax = n.test)

# read TEST activity class class labels from 'y_train.txt' into VECTOR
act.test <- scan(file = "y_test.txt", what = 0, nmax = n.test)

# assemble TEST data set
test.set <- test.df %>%
    mutate(subject = subj.test) %>%
    mutate(activity = act.test)

# merge TRAIN and TEST data into one data set
merged.set <- bind_rows(train.set, test.set)

# this completes TASK 1

# TASK 2. Extract only the measurements on the mean and standard deviation for
# each measurement. 

n.feat <- 561

# get feature labels from 'features.txt' file
feats <- read.table(
    file = "features.txt", header = FALSE, sep = "", nrows = n.feat,
    colClasses = c("numeric", "character"), stringsAsFactors = FALSE
)[, 2, drop = TRUE]

# the measurements on the mean include '-mean(' in their names
# the measurements on the standard deviation include '-std(' in their names
# column numbers to keep from 'merged.df'

msd.idx <- grep("-(mean|std)\\(", feats)

msd.df <- select(merged.set, subject, activity, num_range("V", msd.idx))

# this completes TASK 2

# TASK 3. Give descriptive activity names to the activies in the data set.

n.actis <- 6

# get activity class labels and names from the 'activity_labels.txt' file
# 'activity_labels.txt' is a text file where each row links class labes with 
# their activity name

activity.code <- 1:n.actis

activity.name <- read.table(
    file = "activity_labels.txt", header = FALSE, sep = "", nrows = n.actis,
    colClasses = c("numeric", "character"), stringsAsFactors = FALSE
)[, 2, drop = TRUE]

names(activity.code) <- activity.name

msd.df$activity <- activity.name[match(msd.df$activity, activity.code)]

# this completes TASK 3

# 4. Give descriptive names for the variables in the data set
old.vars <- paste0("V", msd.idx)    # existing column names to rename
var.labels <- feats[msd.idx]    # labels for 66 variables in msd.df dataset

new.vars <- gsub("-", ".", var.labels)
new.vars <- gsub("\\(|\\)", "", new.vars)

names(msd.df)[3:ncol(msd.df)] <- new.vars

# this completes TASK 4

# this completes TASK 5
# 5. Creates a 2nd, independent tidy data set with the average of each
#           variable for each activity and each subject

tidy.df <- msd.df %>%
    group_by(activity, subject) %>%
    summarise_each(funs(mean))

# write out tidy dataset
write.table(tidy.df, file = "tidy.dataset.txt", row.name = FALSE)


