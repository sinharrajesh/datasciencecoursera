# filename: plot1.R
# Author: Rajesh Sinha
# Answers Q1

# Have total emissions from PM2.5 decreased in the United States from 1999 to 
# 2008? Using the base plotting system, make a plot showing the total PM2.5 
# emission from all sources for each of the years 1999, 2002, 2005, and 2008.
#
# Note to reviewers
# 1. plot1.R and plot2.R uses common function for plotting 
# 2. all plot*.R use the same functions for reading files etc. Not implemented Caching
# 3. EBImage is used to open up and display the file in browser after generation of png file
# 4. To run use plot1() or plot2() or plot3() as appropriate
# 5. There is a kludge to cache the data - not nice but saves time on repeated executions
# 

library(futile.logger) # for logging
library(dplyr)         # all bread and butter operations
library(EBImage)       # display the plots after the thing is done


plot1  <- function()  {
    # Uses Base Plotting system to find out how the total emissions of PM2.5 decreased from
    # 1999 to 2008
    
    all.data <- setEnvAndReadData("Exploratory Data Analysis Part-2")
    nei <- all.data$nei.data
    
    pm25 <- nei %>% group_by(year) %>% summarize(total.pm25 = sum(Emissions)) 
    
    filename<- "./plot1.png"
    png(filename=filename, width=600, height=600, units="px")
    plotPM25(pm25, "US")
    dev.off()
    
    #show-off hopefully correct
    display(readImage(files=filename, type="png"))
    
}



plotPM25 <- function(pm25.by.year, coverage) {
    # common plotting function for question-1 and question-2. Accepts summarized data for 
    # US or a specific city and generates a base plot (line graph) with points with the
    # emission values plotted and also puts a sub-title of whether anything increased or
    # decreased
    
    
    # some kludge so that we can put things for subtitle correctly about whether
    # emissions increased or decreased and by how much
    # if only R-MARKDOWN was allowed for this assignment ;-)
    
    pm.1999 <- pm25.by.year %>% filter(year==1999) %>% select(total.pm25)
    pm.2008 <- pm25.by.year %>% filter(year==2008) %>% select(total.pm25)
    flog.info("PM2.5 in 1999 and 2008 are %f %f", pm.1999, pm.2008)
    
    what.h <- ifelse(pm.1999 > pm.2008, "decreased by ", 
                     ifelse( pm.1999 < pm.2008, "increased by ", 
                             "remained the same by "))
    inc = as.character(round(abs(pm.2008 - pm.1999), digits=2))
    init="The Total "
    tons="tons between 1999 and 2008 in" 
    base="in Tons from 1999 to 2008 for "
    
    # Use bquote instead of expression as I cannot get vars to be substituted with paste inside expression()
    
    sub.title <- bquote(.(init)~PM[2.5]*~has~.(what.h)~.(inc)~.(tons)~.(coverage))
    main.title <- bquote(.(init)~PM[2.5]*~.(base)~.(coverage))
    
    # some of the labels do not show properly is x-axis limits are less so made it from 1998 to 2008 instead of default
    with(pm25.by.year, {
        plot(year, total.pm25, xlim=c(1998,2009), type="l", 
             xlab="Year", ylab=expression(paste('PM'[2.5],"Emission (in Tons)")))
        points(year, total.pm25, col="dark red", pch=21)
        text(year, total.pm25, round(total.pm25,1), cex=1, pos=4, col="dark blue") 
        title(main=main.title, col.main="brown",  sub= sub.title, col.sub="dark green")
    })
    
}

setEnvAndReadData <- function(assignment.id) {
    # Utility function
    env.List <- readEnv(assignment.id)
    setwd(env.List$wd);
    return(readData(env.List))
}

readEnv <- function(assignment.id) {
    # These are the settings that you can override esp wd
    
    if (assignment.id == "Exploratory Data Analysis Part-2") {
        nei.file.name<- "summarySCC_PM25.rds"
        scc.file.name<- "Source_Classification_Code.rds"
        downloadfile.name<- "exdata%2Fdata%2FNEI_data.zip"
        downloadfile.url<- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
        working.directory<- "~/courses/John Hopkins DS specialization/ExploratoryDataAnalysis"
    } else {
        stop("This is an unknown assignment")
    }
    return(list(nei.file.name=nei.file.name, 
                scc.file.name=scc.file.name, 
                downloadfile.name=downloadfile.name,
                wd=working.directory, 
                downloadfile.url=downloadfile.url))
    
}

readData <- function(env.List) {
    # This function reads the data from NEI and SCC files and if not available
    # downloads then checks if the zip files exists, unzips it and puts them in the # right location.
    # Args
    #   Input: working directory
    #          name of NEI file
    #          name of SCC file
    #          fileURL of zip file
    #   Returns:
    #          List with SCC and NEI Data
    #   TODO: 
    #          unzip file correctly
    #          download files if no zip file available
    #          Better caching
    
    nei.file.name=env.List$nei.file.name
    scc.file.name=env.List$scc.file.name
    
    # some poor implementation of caching and repeatedly loading data was a pain
    if (exists("SCC") & exists("NEI")){
        flog.info("we have NEI and SCC in our global env - lets reuse");
        
    } else if (file.exists(nei.file.name) & file.exists(scc.file.name)) {
        flog.info("Reading scc and nei data as files exist %s", 
                  nei.file.name)
        SCC <<- readRDS(scc.file.name) # OK - Dont Flame - Poor Implementation
        NEI <<- readRDS(nei.file.name) # OK - Dont Flame - Poor Implementation
        flog.info("Finished reading files")
    } 
    
    return(list(scc.data=SCC, nei.data=NEI))
}
