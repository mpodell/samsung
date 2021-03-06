# SAMSUNG

## Script

The script provided defines the function "readSet" intended to read either the train or test dataset.

1. Read features' names.
2. Read activity labels.
3. Read and merge train and test datasets.
4. Re-label records with descriptive activity names.
5. Extract measurements on the mean and std dev for each measurement.
6. Calculate average of measurements for each activity and each subject.
7. Clean output obtained in previous step.
8. Write "output.txt" file in parent directory (same level as "UCI HAR Dataset" folder).

**NOTES:**

- It is assumed that "UCI HAR Dataset" folder with Samsung dataset is in the working directory.
- Regarding step 5, all features containing either "mean" or "std" strings in their names were extracted (79 features).
- Regarding step 7, a single column is provided for the Activity.Subject information (e.g. "LAYING.1"). 


## Data

The following dataset is not included in this repo. To get it, please click [here](https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip)

For each record it is provided:

- Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration.
- Triaxial Angular velocity from the gyroscope. 
- A 561-feature vector with time and frequency domain variables. 
- Its activity label. 
- An identifier of the subject who carried out the experiment.

The dataset includes the following files:

- **'README.txt'**
- **'features_info.txt':** Shows information about the variables used on the feature vector.
- **'features.txt':** List of all features.
- **'activity_labels.txt':** Links the class labels with their activity name.
- **'train/X_train.txt':** Training set.
- **'train/y_train.txt':** Training labels.
- **'test/X_test.txt':** Test set.
- **'test/y_test.txt':** Test labels.

The following files are available for the train and test data, whose descriptions are equivalent. 

- **'train/subject_train.txt':** Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30. 
- **'train/Inertial Signals/total_acc_x_train.txt':** The acceleration signal from the smartphone accelerometer X axis in standard gravity units 'g'. Every row shows a 128 element vector. The same description applies for the 'total_acc_x_train.txt' and 'total_acc_z_train.txt' files for the Y and Z axis. 
- **'train/Inertial Signals/body_acc_x_train.txt':** The body acceleration signal obtained by subtracting the gravity from the total acceleration. 
- **'train/Inertial Signals/body_gyro_x_train.txt':** The angular velocity vector measured by the gyroscope for each window sample. The units are radians/second. 

**NOTES:** 

- Features are normalized and bounded within [-1,1].
- Each feature vector is a row on the text file.
- For more information about this dataset visit: [UCI](http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones#)

### License:

Use of this dataset in publications must be acknowledged by referencing the following publication [1] 

[1] Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. Human Activity Recognition on Smartphones using a Multiclass Hardware-Friendly Support Vector Machine. International Workshop of Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012

This dataset is distributed AS-IS and no responsibility implied or explicit can be addressed to the authors or their institutions for its use or misuse. Any commercial use is prohibited.

Jorge L. Reyes-Ortiz, Alessandro Ghio, Luca Oneto, Davide Anguita. November 2012.
