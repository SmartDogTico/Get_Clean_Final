## Set working directory of the project
setwd("Z:/Cursos/CURSO_DATA_SCIENCE/3_GETTING_CLEANING_DATA/project/UCI HAR Dataset")
## Istall dlyr and reshape2 to use into the scrypt
install.packages("dplyr")
install.packages("reshape2")
## Open the packages dplyr and reshape2
library("dplyr")
library("reshape2")
## Move the working directory to a variable
dir<-"Z:/Cursos/CURSO_DATA_SCIENCE/3_GETTING_CLEANING_DATA/project/UCI HAR Dataset"
## creation of function to load a file into the workspace
carga_archivo <- function(nombre_archivo, ...) {
        file.path(..., nombre_archivo) %>%
                read.table(header=FALSE)
        
}
##Load a file in train
carga_train_file<-function(nombre_archivo){
        carga_archivo(nombre_archivo,dir,"train")
}

##Load a file in test
carga_test_file<-function(nombre_archivo){
        carga_archivo(nombre_archivo,dir,"test")
}

## In this part, uses list of activity values to describe the test and training labels
## In: pab .. label dataset
## Will Return the original dataset with better column name and values
describe_lbl_pab <- function(pab) {
        names(pab) <- activity_col  
        pab$Activity <- factor(pab$Activity, levels = activity_lbl$V1, labels = activity_lbl$V2)
        pab
}

## Here, the program takes the results of feature tests and assoc columns with the individual feat. 
## Parameters: pab .. activity dataset
## Will Return the original dataset with columns indicating which feature it describes
describe_act_pab <- function(pab) {
        col_names <- gsub("-", "_", features$V2)
        col_names <- gsub("[^a-zA-Z\\d_]", "", col_names)
        names(pab) <- make.names(names = col_names, unique = TRUE, allow_ = TRUE)
        pab
}

## The code adjusts the column name in the data set identifying test participants
describe_sub_pab <- function(pab) {
        names(pab) <- subject_col
        pab
}

## Know go and download and extract a zip file with datasets given by COursera
if (!file.exists(dir)) {
        source_url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
        dest_file <- "Dataset.zip"
        download.file(source_url, destfile = dest_file, method = "curl")
        unzip(dest_file)
        if (!file.exists(dir)) 
                stop("Please check the dataset because doesn't have the expected structure.")
}

## Try another columns names for better understand
subject_col <- "Subject"
activity_col <- "Activity"

## Get the features as understandible column names for better visualization
features <- carga_archivo("features.txt", dir)

## Now try to Load activity labels into the wspace.
activity_lbl <- carga_archivo("activity_labels.txt", dir)


## Now put the descriptive activity names to name the activities in the data set
#### First of all: Training data
train_set <- carga_train_file("X_train.txt") %>% describe_act_pab
train_lbl <- carga_train_file("y_train.txt") %>% describe_lbl_pab
train_sub <- carga_train_file("subject_train.txt") %>% describe_sub_pab

#### And then: Test data
test_set <- carga_test_file("X_test.txt") %>% describe_act_pab
test_lbl <- carga_test_file("y_test.txt") %>% describe_lbl_pab
test_sub <- carga_test_file("subject_test.txt") %>% describe_sub_pab

## Next step is to merge the training and the test sets to create only one dataset as required.
## Later you have to extract only the measurements on the mean and standard deviation for each measurement of the new dataset.
merge_data <- rbind(
        cbind(train_set, train_lbl, train_sub),
        cbind(test_set, test_lbl, test_sub)
) %>%
        select(
                matches("mean|std"), 
                one_of(subject_col, activity_col)
        )

## Finally, you must create a second, independent tidy data set with the average of each variable for each activity and each subject
id_cols <- c(subject_col, activity_col)
tidy_data <- melt(
        merge_data, 
        id = id_cols, 
        measure.vars = setdiff(colnames(merge_data), id_cols)
) %>%
        dcast(Subject + Activity ~ variable, mean)

## Write a new .txt file called "new_tidy_data.txt"
write.table(tidy_data, file = "new_tidy_data.txt", sep = ",", row.names = FALSE)
