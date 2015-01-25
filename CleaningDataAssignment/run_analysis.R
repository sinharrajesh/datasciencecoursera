## run_analysis.R
## Assignment : Getting and Cleaning Date
## Note to reviewer - 
## (a) there are some extra steps for downloads and unzipping and then working off the unzipped
## directory rather than current directory
## (b) tidy data set is written to workingdir/tidydata/tidydata.txt
## (c) trace can be written to console by setting appender.console() instead of appender.file()
## 

library(futile.logger)
library(reshape2)
library(plyr)

## function: downloadAndSetUp
## Downloads the zip file in rawdata directory of current working dir
## extracts the files and places them in rawdata directory
## Returns the path to the unzipped file directory
downloadAndSetUp <- function() {
        ## change this as needed
        dir.name <- "rawdata"
        file.url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
       
        
        flog.trace("checking if %s exists else creating it in %s", dir.name, getwd(), name=logger.name)
        
        if (!file.exists(dir.name)) {
                flog.info("creating %s directory in %s", dir.name, getwd(), name=logger.name)
                dir.create(dir.name)
        }

        
        zipfile.destname <- paste(getwd(),"/",dir.name,"/","zipfile.zip", sep="");
        flog.trace("checking if zipfile %s is already available", zipfile.destname, name=logger.name)
        if (!file.exists(zipfile.destname)) {
                flog.info("downloading the data file as %s", zipfile.destname, name=logger.name);
                x<- download.file(url=file.url, destfile=zipfile.destname, method="curl");
                flog.info("Downloaded file with return code %d", x, name=logger.name);
        }
        
        
        downloaddir.name <- paste(getwd(),"/", dir.name,"/", "UCI HAR Dataset", sep="")
        flog.trace("checking if zip file has already been expanded in %s", downloaddir.name, name=logger.name)
        
        if (!file.exists(downloaddir.name)) {
                destination <- paste(getwd(),"/",dir.name, sep="")
                flog.trace("unzip the download file in %s", destination, name=logger.name);
                x<- unzip(zipfile.destname, exdir=destination);
                flog.trace("unzipped file and created following files:", x, capture=TRUE, name=logger.name);
        }

        flog.trace("Raw data files extracted and available in %s", downloaddir.name, name=logger.name);
        
        return (downloaddir.name)
} 

# function to copy and merge the train and test data files before we load them for working
copyInitFiles <- function(dir.names, file.skeleton.names) {
        
        for (i in dir.names) {
                for (j in file.skeleton.names) {
                        name1 <- paste(rawdata.dir,"/", i,"/",j,"_",i,".txt", sep="" )
                        name2 <- paste(tidydata.dir,"/",j,"_full.txt", sep="")
                        if (i == "train") {
                                flog.info("file copy %s to %s", name1, name2, name=logger.name)
                                file.copy(name1, name2, overwrite = TRUE)
                        } else if (i == "test") {
                                flog.info("file append %s to %s", name1, name2, name=logger.name)
                                file.append(name2, name1)
                        }
                }
        }
}

## Start the execution
## Basic stuff
logger.name <- "my.logger"
flog.threshold(TRACE, name=logger.name)
flog.appender(appender.file("trace.out"), name=logger.name)
setwd("~/gitrepos/datasciencecoursera/CleaningDataAssignment"); # Change this as needed

# if required ensure raw data is in extracted and available in the right place
rawdata.dir <- downloadAndSetUp()
flog.info("Raw data is in %s", rawdata.dir, name=logger.name)

# create an output directory for tidy data if it does not exists
tidydata.dir <- paste(getwd(),"/tidydatadir", sep="")
if (!file.exists(tidydata.dir)) {
        flog.trace("Creating tidy data directory %s", tidydata.dir, name=logger.name)
        dir.create(tidydata.dir)
}

# Part-1 Merge the train and test data into one
# files to merge are train/X_train.txt with test/X_test.txt train/y_train.txt and test/y_train.txt and
# train/subject_train.txt with test/subject_test.txt 
dir.names <- c("train", "test")
file.skeleton.names <- c("X","y", "subject")
file.names <- copyInitFiles(dir.names, file.skeleton.names)

X.DataFile <- paste(tidydata.dir,"/X_Full.txt", sep="");
y.DataFile <- paste(tidydata.dir,"/Y_Full.txt", sep="");
subject.DataFile <- paste(tidydata.dir,"/subject_Full.txt", sep="");


# read these files into memory
X.Full <- read.table(X.DataFile);
y.Full <- read.table(y.DataFile);
subject.Full <- read.table(subject.DataFile);


# read the features labels and assign them to X data file
flog.trace("Reading features labels and then assigning the column names to X.Full DT", name=logger.name)
features.Lables <- read.table(paste(rawdata.dir,"/features.txt", sep=""))
colnames(X.Full) <- features.Lables[,2]

# find only those labels which have mean and standard deviation in them
tidy.colVector <- grepl("-mean()|-std()", colnames(X.Full))
flog.trace("made the tidy vector by grepl on mean and std column names", name=logger.name)

X.Full <- X.Full[,tidy.colVector]
flog.trace("The total number of columns in X is now %d", length(X.Full), name=logger.name)

# Read the activity Labels and convert to the values required
activity.Lables <- read.table(paste(rawdata.dir,"/activity_labels.txt", sep=""), stringsAsFactors=F)
y.Full <- as.factor(y.Full[,1])
y.Full <- revalue(y.Full,c("1" = activity.Lables[1,2], 
                           "2" = activity.Lables[2,2], 
                           "3" = activity.Lables[3,2], 
                           "4" = activity.Lables[4,2], 
                           "5" = activity.Lables[5,2], 
                           '6' = activity.Lables[6,2]))
flog.trace("Converted the activity Lables before cBind of the same", name=logger.name)

# Create one DF with all in one go with subject and activity and the mean and std dev columns 
X.Full <- cbind( subject.Full,y.Full, X.Full)
colnames(X.Full)[1:2] <- c("Subject","Activity")
flog.trace("Bound and assigned names to X - %s", colnames(X.Full), name=logger.name)

# Now apply the melt and dcast magic
flog.info("Melting the DT on subject and Activity", name=logger.name)
x.melted <- melt(X.Full, id.vars=c("Subject","Activity"))
flog.info("DCast with mean", name=logger.name)
x.final.tidy <- dcast(x.melted, Subject + Activity ~ ..., mean )

# write to TXT file in tinydata.dir
flog.info("Writing to the file tidydata.txt in %s", tidydata.dir, name=logger.name)
write.table(x.final.tidy, file=paste(tidydata.dir,"/tidydata.txt",sep=""), row.name=FALSE) 

# remove the unnecessary files
flog.info("cleaning up - removing files %s, %s, %s", X.DataFile, y.DataFile, subject.DataFile, name=logger.name)
file.remove(X.DataFile, y.DataFile, subject.DataFile)