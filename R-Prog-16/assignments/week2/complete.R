complete <- function (directory, id = 1:332) {
        ## directory is a character vector of lenght 1 indicating the location of the files
        ## id is an integer vector indicating the monitor ID numbers to be used

        ## return a data frame of the form
        ##      id nobs

        ## create an empty output data frame
        df <- data.frame(id=numeric(), nobs=numeric() )

        ## define the columns and types for each data frame which will be read
        columnNames <- c("Date", "sulfate", "nitrate", "ID")
        columnClasses <- c("Date", "numeric", "numeric", "numeric")

        ## read files and create a temp data frame for each file
        for (i in id) {
                file <- paste(directory, "/", formatC(i, width=3, flag="0"), ".csv", sep="")
                tempdf <- read.csv(file,
                                    colClasses = columnClasses,
                                   col.names = columnNames)
                df[nrow(df)+1, ] <- c(i, nrow(tempdf[complete.cases(tempdf), ]) )
        }

        ## Output the data frame populated
        df
}

