# filename: plot5.R
# Author: Rajesh Sinha
# Answers Q5
# How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?#
# Note to reviewers
# 1. all plot*.R use the same functions for reading files etc. Not implemented Caching properly - a kludge is used
# 2. EBImage is used to open up and display the file in browser after generation of png file
# 3. To run use plot5()
# 

library(data.table)    # for running on steroids
library(futile.logger) # for logging
library(dplyr)         # all bread and butter operations
library(EBImage)       # display the plots after the thing is done
library(ggplot2)       # 


plot5 <- function() {
    # How have the emissions from Vehicle sources changed?
    
    all.data <- setEnvAndReadData("Exploratory Data Analysis Part-2")
    scc <- all.data$scc.data
    
    # couple of ways to do this
    # Data.Category == "onroad"
    # EI.Sector grep with "*[Vv]ehicles - every vehicle will have a motor, duh?
    # Lets combine them to get it
    try1 <- SCC %>% filter(grepl("*vehicles*", EI.Sector, ignore.case=T)) %>% select(SCC, EI.Sector, Data.Category)
    try2 <- SCC %>% filter(grepl("Onroad", Data.Category)) %>% select(SCC, EI.Sector, Data.Category)
    of.interest <- union(try1, try2)
    
    # Got about 1138 total SCC to match
    flog.info("The SCCs picked up are %d in number", nrow(of.interest))
    
    scc.table <- data.table(of.interest, key="SCC")
    nei.table <- data.table(all.data$nei.data, key="SCC")
    
    # set the pipeline rolling (most ineffec)
    op.table <- nei.table %>% filter(fips=="24510") %>% inner_join(scc.table) %>% group_by(year) %>% summarize(total.pm25 = sum(Emissions)) %>% select(year, total.pm25)
    
    #Make the message line
    pm.1999 <- op.table %>% filter(year==1999) %>% select(total.pm25)
    pm.2008 <- op.table %>% filter(year==2008) %>% select(total.pm25)
    difference <- as.character(round(pm.1999 - pm.2008, digits=2))
    line1 <- paste("The emissions from Motor Vehicles has declined by\n",
                   difference, " tons in Baltimore City from 1999 to 2008.", sep="")
    
    
    #The core plot
    filename<- "./plot5.png"
    png(filename=filename, width=600, height=600, units="px")
    g<- ggplot(op.table, aes(x=year, y=total.pm25)) + 
        geom_line() +
        geom_point(shape=21, size=4, fill="red") +
        xlab("Year") +
        ylab(expression("Total PM"[2.5]*" Emission (in Tons)")) +
        ggtitle(expression("Total PM"[2.5]*" Emissions in Tons by Year from Motor Vehicles in Baltimore City"))+
        annotate("text", x=2002, y=300, label=line1, hjust=0,size=5, fontface="italic",  colour="darkred") +
        geom_text(aes(label=round(total.pm25, digits=1)), size=4, fontface="italic", vjust=-1.5)
    print(g)
    dev.off()
    
    display(readImage(files=filename, type="png"))
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
        flog.info("we have NEI and SCC in our global env - lets reuse")
        
    } else if (file.exists(nei.file.name) & file.exists(scc.file.name)) {
        flog.info("Reading scc and nei data as files exist %s", 
                  nei.file.name)
        SCC <<- readRDS(scc.file.name) # OK - Dont Flame - Poor Implementation
        NEI <<- readRDS(nei.file.name) # OK - Dont Flame - Poor Implementation
        flog.info("Finished reading files")
    } 
    
    return(list(scc.data=SCC, nei.data=NEI))
}
