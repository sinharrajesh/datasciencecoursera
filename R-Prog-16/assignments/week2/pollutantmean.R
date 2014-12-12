pollutantmean <- function(directory, pollutant, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'pollutant' is a character vector of length 1 indicating
        ## the name of the pollutant for which we will calculate the
        ## mean; either "sulfate" or "nitrate".

        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used

        ## Return the mean of the pollutant across all monitors list
        ## in the 'id' vector (ignoring NA values)

        ## define the columns and types for each data frame which will be read
        columnNames <- c("Date", "sulfate", "nitrate", "ID")
        columnClasses <- c("Date", "numeric", "numeric", "numeric")

        ## read files and create a temp data frame for each file
        sum <- 0
        ninstances <- 0
        for (i in id) {
                file <- paste(directory, "/", formatC(i, width=3, flag="0"), ".csv", sep="")
                tempdf <- read.csv(file,
                                   colClasses = columnClasses,
                                   col.names = columnNames)
                sum <- sum + colSums(tempdf[pollutant], na.rm = TRUE)
                ninstances <- ninstances + nrow(tempdf[complete.cases(tempdf[, pollutant]), ])
        }
        if (ninstances > 0 )
                avg = round(sum/ninstances, 3)
        else
                avg = NA

        #output the average
        avg

}

