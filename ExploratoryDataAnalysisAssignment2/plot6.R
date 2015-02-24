# filename: plot6.R
# Author: Rajesh Sinha
# Answers Q6
# Compare emissions from motor vehicle sources in Baltimore City with 
# emissions from motor vehicle sources in Los Angeles County, California 
# (fips == "06037"). Which city has seen greater changes over time in 
# motor vehicle emissions?
# Note to reviewers
# 1. all plot*.R use the same functions for reading files etc. Not implemented Caching properly - a kludge is used
# 2. EBImage is used to open up and display the file in browser after generation of png file
# 3. To run use plot6()
# 

library(data.table)    # for running on steroids
library(futile.logger) # for logging
library(dplyr)         # all bread and butter operations
library(EBImage)       # display the plots after the thing is done
library(ggplot2)       # 


plot6 <- function() {
    all.data <- setEnvAndReadData("Exploratory Data Analysis Part-2")
    scc <- all.data$scc.data
    
    # we do what we did in plot5 
    try1 <- SCC %>% filter(grepl("*vehicles*", EI.Sector, ignore.case=T)) %>% select(SCC, EI.Sector, Data.Category)
    try2 <- SCC %>% filter(grepl("Onroad", Data.Category)) %>% select(SCC, EI.Sector, Data.Category)
    of.interest <- union(try1, try2)
    
    # Got about 1138 total SCC to match
    flog.info("The SCCs picked up are %d in number", nrow(of.interest))
    
    scc.table <- data.table(of.interest, key="SCC")
    nei.table <- data.table(all.data$nei.data, key="SCC")
    
    # set the pipeline rolling (most ineffec)
    op.table <- nei.table %>% filter( (fips=="24510") | (fips=="06037")) %>% inner_join(scc.table) %>% group_by(fips, year) %>% summarize(total.pm25 = sum(Emissions)) %>% select(fips, year, total.pm25)
    
    # Prepare for LA  message to be annotated 
    la.change <- round(op.table[fips=="06037" & year==2008,]$total.pm25 - op.table[fips=="06037" & year==1999,]$total.pm25, digits=2)
    la.change.percent <- abs(round((la.change/op.table[fips=="06037" & year==1999,]$total.pm25) * 100, digits=2))
    la.up <- ifelse(la.change >=0, "increase", "decrease")
    la.change <- abs(la.change)
    
    # Prepare for Baltimore message to be annotated
    bt.change <- round(op.table[fips=="24510" & year==2008,]$total.pm25 - op.table[fips=="24510" & year==1999,]$total.pm25, digits=2)
    bt.change.percent <- abs(round((bt.change/op.table[fips=="24510" & year==1999,]$total.pm25) * 100, digits=2))
    bt.up <- ifelse(bt.change >=0, "increase", "decrease")
    bt.change <- abs(bt.change)


    flog.info("LA %f %f %s  BT %f %f %s", la.change, la.change.percent, la.up, bt.change, bt.change.percent, bt.up)
    op.table <- mutate(op.table, county=as.factor(ifelse(fips=="06037","Los Angelese County", "Baltimore City")))
    la.line <- paste("Los Angeles County - ", la.change.percent,"% ",la.up, "(",la.change," tons).", sep="")
    bt.line <- paste("Baltimore - ", bt.change.percent,"% ",bt.up, "(",bt.change," tons).", sep="")
    
    filename<- "./plot6.png"
    png(filename=filename, width=600, height=600, units="px")
    g<- ggplot(op.table, aes(x=year, y=total.pm25, fill=fips)) + 
        geom_line() +
        geom_point(shape=21, size=4) +
        xlab("Year") +
        ylab(expression("Total PM"[2.5]*" Emission in Tons")) +
        ggtitle("Comparison of Total Emissions From Motor\n Vehicle Sources in Baltimore City\n and Los Angeles County from 1999 to 2008") +
        scale_fill_discrete(name = "Group", label = c("Los Angeles County","Baltimore City")) +
        annotate("text", x=2000, y=3750, label=la.line, hjust=0,size=5, fontface="italic",  colour="darkred") +
        annotate("text", x=2000, y=800, label=bt.line, hjust=0,size=5, fontface="italic",  colour="darkred") +
        geom_text(aes(label=round(total.pm25, digits=0)), size=4, fontface="italic", vjust=1.5)
        
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
