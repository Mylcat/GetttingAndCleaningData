## Extract files from the URL

getZipFileFromURL<-function(val){
  
  # Download the file from the internet and store in the filename getdata_projectfiles_UCI_HAR_Dataset.zip in the data folder
  download.file(url = val,destfile ="data/getdata_projectfiles_UCI_HAR_Dataset.zip" ,cacheOK = TRUE)
  
  # Extract the zip file data/getdata_projectfiles_UCI_HAR_Dataset.zipinto the directory data
  unzip(zipfile = "data/getdata_projectfiles_UCI_HAR_Dataset.zip", exdir = "data")
  
  # set the working directory to the extracted path //data//UCI HAR Dataset
  projectDir<-getwd()
  workingDir<-paste(projectDir,"//data//UCI HAR Dataset",sep = "")
  setwd(workingDir)
  print(getwd())
  
  result<-run_analysis()
  write.table(result,file = "result.txt",row.names = FALSE )
  print(result)
  # set back the working directory
  setwd(projectDir)
  print(getwd())
}                


run_analysis <- function() {
        
        ## Naming convention for the testData holders is test_Data_%fileName% excluding the file name.
        ## Test data importing.
        test_Data_X_test<-read.table("test/X_test.txt",header=FALSE)
        test_Data_Y_test<-read.table("test/y_test.txt",header=FALSE)
        test_Data_subject_test<-read.table("test/subject_test.txt",header=FALSE)
        
        ## Naming convention for the trainingData holders is train_Data_%fileName% excluding the file name.
        ## Training Data Importing
        train_Data_X_train<-read.table("train/X_train.txt",header=FALSE)
        train_Data_Y_train<-read.table("train/y_train.txt",header=FALSE)
        train_Data_subject_train<-read.table("train/subject_train.txt",header=FALSE)
        
        ## 1 Merge the training and the test sets to create one data set.
        ## Combining the X test and train, Y test and train data and row identification into X and Y data
        X_full<-rbind(test_Data_X_test, train_Data_X_train)
        Y_full<-rbind(test_Data_Y_test, train_Data_Y_train)
        subject_full<-rbind(test_Data_subject_test, train_Data_subject_train)
        ## Now the data frames are joined, columns names to be set from features.txt
        features <- read.table("features.txt")
        colnames(X_full)<-features[,2]
        
        ## 2 Extract only the measurements on the mean and standard deviation for each measurement
        ## columns with the desired measurements are labeled using mean() and std() so using grepl on the column names
        ## looking for partial matches will flag them. '|' will create a vector that is true if either is matched.
        rightcols<- grepl("mean()",colnames(X_full)) | grepl("std()",colnames(X_full))
        
        ## new columns in a data frame :
        X_mean_std <- X_full[,rightcols]
        
        ## 3 Uses descriptive activity names to name the activities in the data set
        ## 4 Appropriately labels the data set with descriptive activity names.
        activities<-read.table("activity_labels.txt")
        ## translating Y_full into human readable names converting it to a factor.
        Y_factor <- as.factor(Y_full[,1])
        
        ## mapvalues from the plyr package is used.
        library(plyr)
        Y_factor <- mapvalues(Y_factor,from = as.character(activities[,1]), to = as.character(activities[,2]))
        ## Y_factor is now a factor with the 6 named levels, the same length as the height of X_mean_std, so it can be added
        ## using cbind, putting it first for ease.
        X_mean_std <- cbind(Y_factor, X_mean_std)
        ## Setting the name of the new first column to "activity" 
        colnames(X_mean_std)[1] <- "activity"
        ## column of subject IDs for later so I'll repeat the process with subject_full
        X_mean_std <- cbind(subject_full, X_mean_std)
        colnames(X_mean_std)[1] <- "subject"
        ## X_mean_std should now be a data frame with the subject ids in the first column the activity name in the second
        ## and then all the columns of variables that contained mean() and std() in their names.
        ## 5 Creates a second, independent tidy data set with the average of each variable for each activity and each
        ## subject.
        ## The goal is to take the average for each column of all values where subject and activity are the same, and to
        ## sort the resulting data so the first six rows are each activity for subject one, then the six for subject two etc.
        ## This can be done using the reshape functions introduced in the lecture 
        
        library(reshape2)
        X_melt<- melt(X_mean_std,id.vars=c("subject","activity"))
        Xav_tidy <- dcast(X_melt, subject + activity ~ ..., mean)
        ## tidy dataset required is returned
        return(Xav_tidy)
}


library("plyr")
library("dplyr")

## Set the working directory to the projectdirectory
#setwd("E:/Narayana/work/RProjects/GetttingAndCleaningData")

## URL to extract the file from Internet
path<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

getZipFileFromURL(path)

