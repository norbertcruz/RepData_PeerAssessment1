analysis <- function() {  ## completes all assignment requirements
    
    library(dplyr)
    
    library(ggplot2)
    
    ## Load Scripts and Data
    
    source("./scripts/byDate.R")
    
    source("./scripts/byInterval.R")
    
    source('./scripts/modData.R')
    
    if (!file.exists("activity.csv")) unzip("activity.zip")
    
    data <- read.csv("activity.csv")
    
    cleanData <- filter(data, !is.na(data$steps))
    
    
    ## Total Number of Steps Taken per Day (ignoring NAs)
    
    resultsByDate <- byDate(cleanData, 1)
    
    names(resultsByDate) <- c("mean", "median")
    
    ## Average Daily Activity Pattern
    
    resultsByInterval <- byInterval(cleanData)
    
    maxInterval <- resultsByInterval[[1]]
    
    stepsByInterval <- data.frame(steps = resultsByInterval$steps, 
                                  interval = resultsByInterval$interval)
    
    
    ##  Replacing Missing Values + Total Number of Steps Revisited
    
    missValues <- length(filter(data, is.na(data$steps))$steps)
    
    newData <- modData(data, stepsByInterval)
    
    byDateMod <- byDate(newData, 2)
    
    names(byDateMod) <- c("mean", "median")
    
    stepsByDay <- cbind(resultsByDate, byDateMod)
    
    
    ## Activity Pattern in Weekdays and Weekends
    
    sepData <- mutate(newData, day = weekdays(as.Date(date)))
    
    weekends <- c("Saturday", "Sunday")
    
    dayType = NULL
    
    for (i in sepData$day) {
        
        if (i %in% weekends) type = "weekend"
        
        else type = "weekday"
        
        dayType <- c(dayType, type)
        
    }
    
    sepData <- mutate(sepData, dayType = as.factor(dayType))
    
    sepData <- select(sepData, -c(date, day))
    
    sepData <- group_by(sepData, interval, dayType)
    
    sepData <- summarise_each(sepData, funs(mean))
    
    png()
    
    dev.copy(png, file = "./figures/timeSeries2.png")
    
        ggplot(sepData, aes(interval, steps)) + 
            
            geom_line() + facet_wrap(~ dayType, nrow = 2) + 
            
            ggtitle("Average Daily Activity Pattern") + 
            
            xlab("Time Interval (minutes)") + ylab("Steps")
        
    dev.off()
    
}