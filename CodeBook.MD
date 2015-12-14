
#tidydata.txt Code Book

The *tidydata.txt* file contains data that was downloaded and reformatted from the following site

https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

The resulting data file contains 1 row for every combination of subject, identified by subjectID and Activity. The resulting values are the calculated Mean of all of the datapoints for that variable in the combined test and training datasets.  Only the variables describing the mean and the standard deviation are included.  

For more details, please see the README.TXT file including in the download above.

**File Format**: The data file is a space delimmitted file with each datafield in ""

The following fields are in the resulting file

Column Name | Format | Description
------------|--------|--------------
activitysubjectID | string | Concatenated string of the subject ID and activity ID
subjectID | integer | Unique ID for subject in test. Value between 1 and 30.
activity | string | One of WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING
timedBodyAccelerationSignal-mean()-X | decimal |
timedBodyAccelerationSignal-mean()-Y | decimal |
timedBodyAccelerationSignal-mean()-Z | decimal |
timedBodyAccelerationSignal-std()-X | decimal |
timedBodyAccelerationSignal-std()-Y | decimal |
timedBodyAccelerationSignal-std()-Z | decimal |
timedGravityAccelerationSignal-mean()-X | decimal |
timedGravityAccelerationSignal-mean()-Y | decimal |
timedGravityAccelerationSignal-mean()-Z | decimal |
timedGravityAccelerationSignal-std()-X | decimal |
timedGravityAccelerationSignal-std()-Y | decimal |
timedGravityAccelerationSignal-std()-Z | decimal |
timedBodyAccelerationJerkSignal-mean()-X | decimal |
timedBodyAccelerationJerkSignal-mean()-Y | decimal |
timedBodyAccelerationJerkSignal-mean()-Z | decimal |
timedBodyAccelerationJerkSignal-std()-X | decimal |
timedBodyAccelerationJerkSignal-std()-Y | decimal |
timedBodyAccelerationJerkSignal-std()-Z | decimal |
timedBodyGyroscopeSignal-mean()-X | decimal |
timedBodyGyroscopeSignal-mean()-Y | decimal |
timedBodyGyroscopeSignal-mean()-Z | decimal |
timedBodyGyroscopeSignal-std()-X | decimal |
timedBodyGyroscopeSignal-std()-Y | decimal |
timedBodyGyroscopeSignal-std()-Z | decimal |
timedBodyGyroscopeJerkSignal-mean()-X | decimal |
timedBodyGyroscopeJerkSignal-mean()-Y | decimal |
timedBodyGyroscopeJerkSignal-mean()-Z | decimal |
timedBodyGyroscopeJerkSignal-std()-X | decimal |
timedBodyGyroscopeJerkSignal-std()-Y | decimal |
timedBodyGyroscopeJerkSignal-std()-Z | decimal |
timedBodyAccelerationMagnitude-mean() | decimal |
timedBodyAccelerationMagnitude-std() | decimal |
timedGravityAccelerationMagnitude-mean() | decimal |
timedGravityAccelerationMagnitude-std() | decimal |
timedBodyAccelerationJerkMagnitude-mean() | decimal |
timedBodyAccelerationJerkMagnitude-std() | decimal |
timedBodyGyroscopeMagnitude-mean() | decimal |
timedBodyGyroscopeMagnitude-std() | decimal |
timedBodyGyroscopeJerkMagnitude-mean() | decimal |
timedBodyGyroscopeJerkMagnitude-std() | decimal |
frequencyBodyAccelerationSignal-mean()-X | decimal |
frequencyBodyAccelerationSignal-mean()-Y | decimal |
frequencyBodyAccelerationSignal-mean()-Z | decimal |
frequencyBodyAccelerationSignal-std()-X | decimal |
frequencyBodyAccelerationSignal-std()-Y | decimal |
frequencyBodyAccelerationSignal-std()-Z | decimal |
frequencyBodyAccelerationJerkSignal-mean()-X | decimal |
frequencyBodyAccelerationJerkSignal-mean()-Y | decimal |
frequencyBodyAccelerationJerkSignal-mean()-Z | decimal |
frequencyBodyAccelerationJerkSignal-std()-X | decimal |
frequencyBodyAccelerationJerkSignal-std()-Y | decimal |
frequencyBodyAccelerationJerkSignal-std()-Z | decimal |
frequencyBodyGyroscopeSignal-mean()-X | decimal |
frequencyBodyGyroscopeSignal-mean()-Y | decimal |
frequencyBodyGyroscopeSignal-mean()-Z | decimal |
frequencyBodyGyroscopeSignal-std()-X | decimal |
frequencyBodyGyroscopeSignal-std()-Y | decimal |
frequencyBodyGyroscopeSignal-std()-Z | decimal |
frequencyBodyAccelerationMagnitude-mean() | decimal |
frequencyBodyAccelerationMagnitude-std() | decimal |
frequencyBodyBodyAccelerationJerkMagnitude-mean() | decimal |
frequencyBodyBodyAccelerationJerkMagnitude-std() | decimal |
frequencyBodyBodyGyroscopeMagnitude-mean() | decimal |
frequencyBodyBodyGyroscopeMagnitude-std() | decimal |
frequencyBodyBodyGyroscopeJerkMagnitude-mean() | decimal |
frequencyBodyBodyGyroscopeJerkMagnitude-std() | decimal |



