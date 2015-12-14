
#tidydata.txt Code Book

The *tidydata.txt* file contains data that was downloaded and reformatted from the following site

https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

The resulting data file contains 1 row for every combination of subject, identified by subjectID and Activity. The resulting values are the calculated Mean of all of the datapoints for that variable in the combined test and training datasets.  Only the variables describing the mean and the standard deviation are included.  

For more details, please see the README.TXT file including in the download above.

**File Format**: The data file is a space delimitted file with each datafield in ""

The following fields are in the resulting file

Column Name | Format | Description
------------|--------|--------------
activitysubjectID | string | Concatenated string of the subject ID and activity ID
subjectID | integer | Unique ID for subject in test. Value between 1 and 30.
activity | string | One of WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING
timedBodyAccelerationSignal-mean()-X | decimal | Mean of all values in test and traing for subject/activity. See features_info.txt
timedBodyAccelerationSignal-mean()-Y | decimal | Mean of all values in test and traing for subject/activity. See features_info.txt
timedBodyAccelerationSignal-mean()-Z | decimal | Mean of all values in test and traing for subject/activity. See features_info.txt
timedBodyAccelerationSignal-std()-X | decimal | Mean of all values in test and traing for subject/activity. See features_info.txt
timedBodyAccelerationSignal-std()-Y | decimal | Mean of all values in test and traing for subject/activity. See features_info.txt
timedBodyAccelerationSignal-std()-Z | decimal | Mean of all values in test and traing for subject/activity. See features_info.txt
timedGravityAccelerationSignal-mean()-X | decimal | Mean of all values in test and traing for subject/activity. See features_info.txt
timedGravityAccelerationSignal-mean()-Y | decimal | Mean of all values in test and traing for subject/activity. See features_info.txt
timedGravityAccelerationSignal-mean()-Z | decimal | Mean of all values in test and traing for subject/activity. See features_info.txt
timedGravityAccelerationSignal-std()-X | decimal | Mean of all values in test and traing for subject/activity. See features_info.txt
timedGravityAccelerationSignal-std()-Y | decimal | Mean of all values in test and traing for subject/activity. See features_info.txt
timedGravityAccelerationSignal-std()-Z | decimal |Mean of all values in test and traing for subject/activity. See features_info.txt
timedBodyAccelerationJerkSignal-mean()-X | decimal | Mean of all values in test and traing for subject/activity. See features_info.txt
timedBodyAccelerationJerkSignal-mean()-Y | decimal | Mean of all values in test and traing for subject/activity. See features_info.txt
timedBodyAccelerationJerkSignal-mean()-Z | decimal | Mean of all values in test and traing for subject/activity. See features_info.txt
timedBodyAccelerationJerkSignal-std()-X | decimal | Mean of all values in test and traing for subject/activity. See features_info.txt
timedBodyAccelerationJerkSignal-std()-Y | decimal | Mean of all values in test and traing for subject/activity. See features_info.txt
timedBodyAccelerationJerkSignal-std()-Z | decimal | Mean of all values in test and traing for subject/activity. See features_info.txt
timedBodyGyroscopeSignal-mean()-X | decimal | Mean of all values in test and traing for subject/activity. See features_info.txt
timedBodyGyroscopeSignal-mean()-Y | decimal | Mean of all values in test and traing for subject/activity. See features_info.txt
timedBodyGyroscopeSignal-mean()-Z | decimal | Mean of all values in test and traing for subject/activity. See features_info.txt
timedBodyGyroscopeSignal-std()-X | decimal | Mean of all values in test and traing for subject/activity. See features_info.txt
timedBodyGyroscopeSignal-std()-Y | decimal | Mean of all values in test and traing for subject/activity. See features_info.txt
timedBodyGyroscopeSignal-std()-Z | decimal | Mean of all values in test and traing for subject/activity. See features_info.txt
timedBodyGyroscopeJerkSignal-mean()-X | decimal | Mean of all values in test and traing for subject/activity. See features_info.txt
timedBodyGyroscopeJerkSignal-mean()-Y | decimal | Mean of all values in test and traing for subject/activity. See features_info.txt
timedBodyGyroscopeJerkSignal-mean()-Z | decimal | Mean of all values in test and traing for subject/activity. See features_info.txt
timedBodyGyroscopeJerkSignal-std()-X | decimal | Mean of all values in test and traing for subject/activity. See features_info.txt
timedBodyGyroscopeJerkSignal-std()-Y | decimal | Mean of all values in test and traing for subject/activity. See features_info.txt
timedBodyGyroscopeJerkSignal-std()-Z | decimal | Mean of all values in test and traing for subject/activity. See features_info.txt
timedBodyAccelerationMagnitude-mean() | decimal | Mean of all values in test and traing for subject/activity. See features_info.txt
timedBodyAccelerationMagnitude-std() | decimal | Mean of all values in test and traing for subject/activity. See features_info.txt
timedGravityAccelerationMagnitude-mean() | decimal | Mean of all values in test and traing for subject/activity. See features_info.txt
timedGravityAccelerationMagnitude-std() | decimal | Mean of all values in test and traing for subject/activity. See features_info.txt timedBodyAccelerationJerkMagnitude-mean() | decimal | Mean of all values in test and traing for subject/activity. See features_info.txt
timedBodyAccelerationJerkMagnitude-std() | decimal | Mean of all values in test and traing for subject/activity. See features_info.txt
timedBodyGyroscopeMagnitude-mean() | decimal | Mean of all values in test and traing for subject/activity. See features_info.txt
timedBodyGyroscopeMagnitude-std() | decimal | Mean of all values in test and traing for subject/activity. See features_info.txt
timedBodyGyroscopeJerkMagnitude-mean() | decimal | Mean of all values in test and traing for subject/activity. See features_info.txt
timedBodyGyroscopeJerkMagnitude-std() | decimal | Mean of all values in test and traing for subject/activity. See features_info.txt
frequencyBodyAccelerationSignal-mean()-X | decimal | Mean of all values in test and traing for subject/activity. See features_info.txt
frequencyBodyAccelerationSignal-mean()-Y | decimal | Mean of all values in test and traing for subject/activity. See features_info.txt
frequencyBodyAccelerationSignal-mean()-Z | decimal | Mean of all values in test and traing for subject/activity. See features_info.txt
frequencyBodyAccelerationSignal-std()-X | decimal | Mean of all values in test and traing for subject/activity. See features_info.txt
frequencyBodyAccelerationSignal-std()-Y | decimal | Mean of all values in test and traing for subject/activity. See features_info.txt
frequencyBodyAccelerationSignal-std()-Z | decimal | Mean of all values in test and traing for subject/activity. See features_info.txt
frequencyBodyAccelerationJerkSignal-mean()-X | decimal | Mean of all values in test and traing for subject/activity. See features_info.txt
frequencyBodyAccelerationJerkSignal-mean()-Y | decimal | Mean of all values in test and traing for subject/activity. See features_info.txt
frequencyBodyAccelerationJerkSignal-mean()-Z | decimal | Mean of all values in test and traing for subject/activity. See features_info.txt
frequencyBodyAccelerationJerkSignal-std()-X | decimal | Mean of all values in test and traing for subject/activity. See features_info.txt
frequencyBodyAccelerationJerkSignal-std()-Y | decimal | Mean of all values in test and traing for subject/activity. See features_info.txt
frequencyBodyAccelerationJerkSignal-std()-Z | decimal | Mean of all values in test and traing for subject/activity. See features_info.txt
frequencyBodyGyroscopeSignal-mean()-X | decimal | Mean of all values in test and traing for subject/activity. See features_info.txt
frequencyBodyGyroscopeSignal-mean()-Y | decimal | Mean of all values in test and traing for subject/activity. See features_info.txt
frequencyBodyGyroscopeSignal-mean()-Z | decimal | Mean of all values in test and traing for subject/activity. See features_info.txt
frequencyBodyGyroscopeSignal-std()-X | decimal | Mean of all values in test and traing for subject/activity. See features_info.txt
frequencyBodyGyroscopeSignal-std()-Y | decimal | Mean of all values in test and traing for subject/activity. See features_info.txt
frequencyBodyGyroscopeSignal-std()-Z | decimal | Mean of all values in test and traing for subject/activity. See features_info.txt
frequencyBodyAccelerationMagnitude-mean() | decimal | Mean of all values in test and traing for subject/activity. See features_info.txt
frequencyBodyAccelerationMagnitude-std() | decimal | Mean of all values in test and traing for subject/activity. See features_info.txt
frequencyBodyBodyAccelerationJerkMagnitude-mean() | decimal | Mean of all values in test and traing for subject/activity. See features_info.txt
frequencyBodyBodyAccelerationJerkMagnitude-std() | decimal | Mean of all values in test and traing for subject/activity. See features_info.txt
frequencyBodyBodyGyroscopeMagnitude-mean() | decimal | Mean of all values in test and traing for subject/activity. See features_info.txt
frequencyBodyBodyGyroscopeMagnitude-std() | decimal | Mean of all values in test and traing for subject/activity. See features_info.txt
frequencyBodyBodyGyroscopeJerkMagnitude-mean() | decimal | Mean of all values in test and traing for subject/activity. See features_info.txt
frequencyBodyBodyGyroscopeJerkMagnitude-std() | decimal | Mean of all values in test and traing for subject/activity. See features_info.txt



