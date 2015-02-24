# filename: plot3.R
# Author: Rajesh Sinha
# Answers Q3
# Of the four types of sources indicated by the type (point, nonpoint, onroad, 
# nonroad) variable, which of these four sources have seen decreases in 
# emissions from 1999–2008 for Baltimore City? Which have seen increases in 
# emissions from 1999–2008? Use the ggplot2 plotting system to make a plot 
# answer this question.
# Note to reviewers
# 1. all plot*.R use the same functions for reading files etc. Not implemented Caching but for a kludge
# 2. EBImage is used to open up and display the file in browser after generation of png file
# 3. To run use plot3()
# 

library(futile.logger) # for logging
library(dplyr)         # all bread and butter operations
library(EBImage)       # display the plots after the thing is done
library(ggplot2)       # 


plot3 <- function() {
    # Finds out which of the 4 sources type have seen decrease or increase between 1999 to 2008 in Baltimore City
    # using ggplot2 
  all.data <- setEnvAndReadData("Exploratory Data Analysis Part-2")
  nei <- all.data$nei.data
  pm25.by.type <- nei %>% filter(fips=="24510") %>% 
                    group_by(year,type) %>% summarize(total.pm25 = sum(Emissions)) %>% mutate(type=as.factor(type))    
    
  line1 <- "Emissions from all source-types\n except Point Sources have seen decrease"
  filename<- "./plot3.png"
  png(filename=filename, width=600, height=600, units="px")

  g <-ggplot(pm25.by.type, aes(x=year, y=total.pm25, fill=type)) + 
      geom_line() + geom_point(shape=21, size=4) +  xlab("Year")  +
      ylab(expression("Total PM"[2.5]*" Emission in Tons"))  +
      ggtitle(expression("Total PM"[2.5]*" Emissions in Tons by Year and Source for Baltimore City")) + 
      scale_fill_discrete(name = "Source-Type")  +
      annotate("text", x=2002, y=2000, label=line1, hjust=0,size=5, fontface="italic",  colour="darkred") 

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
