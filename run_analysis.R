testXFile <- ".\\UCI HAR Dataset\\test\\X_test.txt"
testYFile <- ".\\UCI HAR Dataset\\test\\Y_test.txt"
testSubjectFile <- ".\\UCI HAR Dataset\\test\\subject_test.txt"
trainXFile <- ".\\UCI HAR Dataset\\train\\X_train.txt"
trainYFile <- ".\\UCI HAR Dataset\\train\\Y_train.txt"
trainSubjectFile <- ".\\UCI HAR Dataset\\train\\subject_train.txt"
activitiesFile <- ".\\UCI HAR Dataset\\activity_labels.txt"
featuresFile <- ".\\UCI HAR Dataset\\features.txt"

featuresdf <- read.table(featuresFile ) ## 561 observations
activitiesdf <- read.table(activitiesFile) ## 1-6 

##Merges the training and the test sets to create one data set.
alldata <- rbind(read.table(testXFile ),
                        read.table(trainXFile ))
Ydf <- rbind(read.table(testYFile),
                        read.table(trainYFile))
subjectdf <- rbind(read.table(testSubjectFile),
                        read.table(trainSubjectFile))

##Extracts only the measurements on the mean and standard deviation for each measurement. 
reg <- "std|mean"
featuresdf <- featuresdf[grep(reg, featuresdf[,2]),]
alldata <- alldata[,featuresdf[,1]]

##Appropriately labels the data set with descriptive variable names. 
names(alldata) <- featuresdf[,2]

##Uses descriptive activity names to name the activities in the data set
names(activitiesdf)[1] <- "activityID"
names(activitiesdf)[2] <- "activity"
names(Ydf)[1] <- "activityID"
names(subjectdf)[1] <- "subjectID"

##Bind activity and subject
Ydf <- cbind(Ydf, Subjectdf)
Ydf$activitysubjectID <- paste(Ydf[,"subjectID"],Ydf[,"activityID"],sep=".")

##create lookup for activity subject ID
activitiessubjectdf <- unique(Ydf[,c(1:3)])
activitiessubjectdf <- merge(activitiessubjectdf, activitiesdf,by="activityID", sort=TRUE)

##Add activity subject ID lookup column to alldata
alldata$activitysubjectID <- Ydf[,"activitysubjectID"]

##From the data set in step 4, creates a second, independent tidy data 
##  set with the average of each variable for each activity and each subject.
tidydata <- do.call("rbind", 
    by(alldata, alldata[,"activitysubjectID"], simplify = TRUE,
      function(x) colMeans(x[,1:79])
    ))

##update subject ID and activity in tidydata -- rowname holds the activitysubjectID
tidydata <- as.data.frame(cbind(tidydata,"activitysubjectID" = rownames(tidydata)))
tidydata <- merge(activitiessubjectdf[,c("subjectID","activity","activitysubjectID")], 
                tidydata, by="activitysubjectID")

##Export tidy data
write.table(tidydata,file="tidydata.txt",row.names=FALSE)

##verification tests
##splitdf <- split(alldata,alldata$activitysubjectID)
