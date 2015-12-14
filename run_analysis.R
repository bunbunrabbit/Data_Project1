## Data Project 1
## 
## Goal - to prepare tidy data that can be used for later analysis based on the data at
##    http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
##
## 1. Merges the training and the test sets to create one data set. 
## 2. Extracts only the measurements on the mean and standard deviation 
##      for each measurement. 
## 3. Uses descriptive activity names to name the activities in the data set 
## 4. Appropriately labels the data set with descriptive variable names. 
## 5. From the data set in step 4, creates a second, independent tidy data set 
##      with the average of each variable for each activity and each subject.

##Set working directory
setwd("~/Coursera/RData/RWorking/Data_Project1")

##Set up variables for data files
testXFile <- ".\\UCI HAR Dataset\\test\\X_test.txt"
testYFile <- ".\\UCI HAR Dataset\\test\\Y_test.txt"
testSubjectFile <- ".\\UCI HAR Dataset\\test\\subject_test.txt"
trainXFile <- ".\\UCI HAR Dataset\\train\\X_train.txt"
trainYFile <- ".\\UCI HAR Dataset\\train\\Y_train.txt"
trainSubjectFile <- ".\\UCI HAR Dataset\\train\\subject_train.txt"
activitiesFile <- ".\\UCI HAR Dataset\\activity_labels.txt"
featuresFile <- ".\\UCI HAR Dataset\\features.txt"

activitiesdf <- read.table(activitiesFile) ## 1-6 

##Merges the training and the test sets to create one data set.
alldata <- rbind(read.table(testXFile ),
                        read.table(trainXFile ))
Ydf <- rbind(read.table(testYFile),
                        read.table(trainYFile))
Subjectdf <- rbind(read.table(testSubjectFile),
                        read.table(trainSubjectFile))

##Extracts only the measurements on the mean and standard deviation for each measurement. 
featuresdf <- read.table(featuresFile ) ## 561 observations
reg <- "(std|mean[^F])"
featuresdf <- featuresdf[grepl(reg, featuresdf[,2]),]
alldata <- alldata[,featuresdf[,1]]

##Update to human readable names
featuresdf[,2] <- gsub("^tB","timedB",featuresdf[,2])
featuresdf[,2] <- gsub("^tG","timedG",featuresdf[,2])
featuresdf[,2] <- gsub("^fB","frequencyB",featuresdf[,2])
featuresdf[,2] <- gsub("Jerk-","JerkSignal-",featuresdf[,2])
featuresdf[,2] <- gsub("Acc","Acceleration",featuresdf[,2])
featuresdf[,2] <- gsub("Acceleration-","AccelerationSignal-",featuresdf[,2])
featuresdf[,2] <- gsub("Gyro","Gyroscope",featuresdf[,2])
featuresdf[,2] <- gsub("Gyroscope-","GyroscopeSignal-",featuresdf[,2])
featuresdf[,2] <- gsub("Mag-","Magnitude-",featuresdf[,2])

##Appropriately labels the data set with descriptive variable names. 
colnames(alldata) <- featuresdf[,2]

##Uses descriptive activity names to name the activities in the data set
colnames(activitiesdf)[1] <- "activityID"
colnames(activitiesdf)[2] <- "activity"
colnames(Ydf)[1] <- "activityID"
colnames(Subjectdf)[1] <- "subjectID"

##Bind activity and subject
Ydf <- cbind(Ydf, Subjectdf)
Ydf$activitysubjectID <- paste(Ydf[,"subjectID"],Ydf[,"activityID"],sep=".")

##create lookup for activity subject ID
activitiessubjectdf <- unique(Ydf[,c(1:3)])
activitiessubjectdf <- merge(activitiessubjectdf, activitiesdf,by="activityID", sort=TRUE)

##Add activity subject ID lookup column to alldata
alldata$activitysubjectID <- Ydf[,"activitysubjectID"]

##Creates a second, independent tidy data set with the average of each variable 
##  for each activity and each subject.
tidydata <- do.call("rbind", 
    by(alldata, alldata[,"activitysubjectID"], simplify = TRUE,
            function(x) colMeans(x[,as.vector(featuresdf[,2])])
    ))

##update subject ID and activity in tidydata -- rowname holds the activitysubjectID
tidydata <- as.data.frame(cbind(tidydata,"activitysubjectID" = rownames(tidydata)))
tidydata <- merge(activitiessubjectdf[,c("subjectID","activity","activitysubjectID")], 
                tidydata, by="activitysubjectID")

##Export tidy data
write.table(tidydata,file="tidydata.txt",row.names=FALSE)
