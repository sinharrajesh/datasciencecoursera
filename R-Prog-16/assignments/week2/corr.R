corr <- function(directory, threshold = 0) {
        correlation = numeric()
        observed <- complete(directory)
        interestedFilePrefix <- observed[observed$nobs >= threshold, ]$id
        columnNames <- c("Date", "sulfate", "nitrate", "ID")
        columnClasses <- c("Date", "numeric", "numeric", "numeric")
        for (i in interestedFilePrefix) {
                file <- paste(directory, "/", formatC(i, width=3, flag="0"), ".csv", sep="")
                tempdf <- read.csv(file,
                                   colClasses = columnClasses,
                                   col.names = columnNames)
                temp2 <- tempdf[complete.cases(tempdf), ]
                correlation <- c(correlation, cor(temp2$sulfate, temp2$nitrate))
        }
        correlation
}
