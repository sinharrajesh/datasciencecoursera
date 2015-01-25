## run_analysis.R README.md


### Intent


This script processes the average of mean & standard deviation of accelerometer and gyroscope data collected using smartphone attached to 30 subjects during Activities of Daily life. The averages are computed and shown by subject and the activity type. The tidy data set is written to {working_dir}/tidydatadir/tidydata.txt file
  
### Source Data


Jorge L. Reyes-Ortiz, Davide Anguita, Alessandro Ghio, Luca Oneto. 
Smartlab - Non Linear Complex Systems Laboratory 
DITEN - UniversitÃ  degli Studi di Genova, Genoa I-16145, Italy. 
activityrecognition '@' smartlab.ws 
www.smartlab.ws 

Data available here:
https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

With a full description here:

http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 

### How to run


Before running

* packages plyr, reshape2 and futile.logger are installed as the script just loads them using library(package-name) call
* The scripts starts with downloading from the source location rather than assuming it to be available in working directory
* Change your working directory in the run_Analysys.R script and check you have write permissions on it
* If you want console log, change the line no 78 to flog.appender(appender.console(), name=logger.name)

call source("run_Analysis.R") and it should load the functions and run by itself. By default the trace will be generated in a file called trace.out in your working directory


### How it works


1. The script downloads the data from the source URL.  Knowing that it is a zip file it unzips it in the rawdatadirectory
2. The script then merges the traing and test data for observations - i.e. X_test.txt with X_train.txt file to create X_Full.txt pulling these files from right locations. It also merges acitivities code data for these observations as present in y_test.txt and y_train.txt and the subject identifiers for these observations as contained in subject_test and subject_test.txt in right order.
These are then read into one dataframe each (X.Full. y.Full and subject.Full).
3. The column names from X.Full data frame are read from features.txt file and then applied using colnames 
4. The script will remove all columns from the observations data except for the ones which contain "-mean()" or "-std()" in their names. This is accomplished using grepl and regex for two patterns to be evaluated with OR condition which yields a logical vector to eliminate the columns that we do not want from observations data (X.Full)
5. The script then reads the activity labels master data from activities_labels.txt. For the activity labels for each observation provided in Y.Full, it converts from numeric vector to facttors with the corresponding text activity label found in the master data read from activities_labels.txt. To map this, it uses revalue function from plyr library.
6. The script then attaches the subject and the activity data using cBind to observations data and assigns them appropriate column names
7. To calculate the average of all observations based on subject and activity grouping, it uses melt and dcast functions of reshape2 library.
8. Tidy data is written to tidatadir/tidydata.txt file and all temporary files are removed.
