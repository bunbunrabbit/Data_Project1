# Data_Project1
The goal is to prepare tidy data that can be used for later analysis. This repo explains how the script works and how they are connected.  

One of the most exciting areas in all of data science right now is wearable computing - see for example this article . Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained: 

http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 

Data is from  

https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

script run_analysis.R  does the following. 
1. Merges the training and the test sets to create one data set.
2. Extracts only the measurements on the mean and standard deviation for each measurement. 
3. Uses descriptive activity names to name the activities in the data set
4. Appropriately labels the data set with descriptive variable names. 
5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

Input files:

* testXFile <- "X_test.txt"
* testYFile <- "Y_test.txt"
* testSubjectFile <- "subject_test.txt"
* trainXFile <- "X_train.txt"
* trainYFile <- "Y_train.txt"
* trainSubjectFile <- "subject_train.txt"
* activitiesFile <- "activity_labels.txt"
* featuresFile <- "features.txt"

Output file:

* tidydata.txt
 

