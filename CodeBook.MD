
#tidydata.txt Code Book

The *tidydata.txt* file contains data that was downloaded and reformatted from the following site

https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

The resulting data file contains 1 row for every combination of subject, identified by subjectID and Activity. The resulting values are the calculated Mean of all of the datapoints for that variable in the combined test and training datasets.  Only the variables describing the mean and the standard deviation are included.  

For more details, please see the README.TXT file including in the download above.

**File Format**: The data file is a space delimmitted file with each datafield in ""

The following fields are in the resulting file

Column Name | Format | Description
------------|--------|--------------
activitysubjectID
subjectID
activity | string | one of 
    * WALKING, 
    * WALKING_UPSTAIRS, 
    * WALKING_DOWNSTAIRS, 
    * SITTING, 
    * STANDING
    * LAYING

timedBodyAccelerationSignal-mean()-X | |
timedBodyAccelerationSignal-mean()-Y | |
timedBodyAccelerationSignal-mean()-Z | |
timedBodyAccelerationSignal-std()-X | |
timedBodyAccelerationSignal-std()-Y | |
timedBodyAccelerationSignal-std()-Z | |
timedGravityAccelerationSignal-mean()-X | |
timedGravityAccelerationSignal-mean()-Y | |
timedGravityAccelerationSignal-mean()-Z | |
timedGravityAccelerationSignal-std()-X | |
timedGravityAccelerationSignal-std()-Y | |
timedGravityAccelerationSignal-std()-Z | |
timedBodyAccelerationJerkSignal-mean()-X | |
timedBodyAccelerationJerkSignal-mean()-Y | |
timedBodyAccelerationJerkSignal-mean()-Z | |
timedBodyAccelerationJerkSignal-std()-X | |
timedBodyAccelerationJerkSignal-std()-Y | |
timedBodyAccelerationJerkSignal-std()-Z | |
timedBodyGyroscopeSignal-mean()-X | |
timedBodyGyroscopeSignal-mean()-Y | |
timedBodyGyroscopeSignal-mean()-Z | |
timedBodyGyroscopeSignal-std()-X | |
timedBodyGyroscopeSignal-std()-Y | |
timedBodyGyroscopeSignal-std()-Z | |
timedBodyGyroscopeJerkSignal-mean()-X | |
timedBodyGyroscopeJerkSignal-mean()-Y | |
timedBodyGyroscopeJerkSignal-mean()-Z | |
timedBodyGyroscopeJerkSignal-std()-X | |
timedBodyGyroscopeJerkSignal-std()-Y | |
timedBodyGyroscopeJerkSignal-std()-Z | |
timedBodyAccelerationMagnitude-mean() | |
timedBodyAccelerationMagnitude-std() | |
timedGravityAccelerationMagnitude-mean() | |
timedGravityAccelerationMagnitude-std() | |
timedBodyAccelerationJerkMagnitude-mean() | |
timedBodyAccelerationJerkMagnitude-std() | |
timedBodyGyroscopeMagnitude-mean() | |
timedBodyGyroscopeMagnitude-std() | |
timedBodyGyroscopeJerkMagnitude-mean() | |
timedBodyGyroscopeJerkMagnitude-std() | |
frequencyBodyAccelerationSignal-mean()-X | |
frequencyBodyAccelerationSignal-mean()-Y | |
frequencyBodyAccelerationSignal-mean()-Z | |
frequencyBodyAccelerationSignal-std()-X | |
frequencyBodyAccelerationSignal-std()-Y | |
frequencyBodyAccelerationSignal-std()-Z | |
frequencyBodyAccelerationJerkSignal-mean()-X | |
frequencyBodyAccelerationJerkSignal-mean()-Y | |
frequencyBodyAccelerationJerkSignal-mean()-Z | |
frequencyBodyAccelerationJerkSignal-std()-X | |
frequencyBodyAccelerationJerkSignal-std()-Y | |
frequencyBodyAccelerationJerkSignal-std()-Z | |
frequencyBodyGyroscopeSignal-mean()-X | |
frequencyBodyGyroscopeSignal-mean()-Y | |
frequencyBodyGyroscopeSignal-mean()-Z | |
frequencyBodyGyroscopeSignal-std()-X | |
frequencyBodyGyroscopeSignal-std()-Y | |
frequencyBodyGyroscopeSignal-std()-Z | |
frequencyBodyAccelerationMagnitude-mean() | |
frequencyBodyAccelerationMagnitude-std() | |
frequencyBodyBodyAccelerationJerkMagnitude-mean() | |
frequencyBodyBodyAccelerationJerkMagnitude-std() | |
frequencyBodyBodyGyroscopeMagnitude-mean() | |
frequencyBodyBodyGyroscopeMagnitude-std() | |
frequencyBodyBodyGyroscopeJerkMagnitude-mean() | |
frequencyBodyBodyGyroscopeJerkMagnitude-std() | |



