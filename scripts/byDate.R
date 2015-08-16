byDate <- function(data, file) {  ## performs analysis by Day
    
    dataByDate <- group_by(data, date)
    
    dataByDate <- select(dataByDate, -interval)
    
    stepsSum <- summarise_each(dataByDate, funs(sum))
    
    meanByDate <- mean(stepsSum$steps)
    
    medianByDate <- median(stepsSum$steps)
    
    maxSteps <- max(stepsSum$steps)
    
    bins <- seq(0, 22500, 2500) ## based on maxSteps value
    
    png()
    
        if (file == 1) {
        
            dev.copy(png, file = "./figures/histogram1.png")
        
        } else dev.copy(png, file = "./figures/histogram2.png")
    
        hist(stepsSum$steps, breaks = bins, ylim = c(0, 30), xaxt = 'n', 
         
            main = "Total Number of Steps Taken per Day", xlab = "Steps")
    
        axis(1, at = bins, labels = bins)
        
        abline(v = meanByDate, col = "red", lwd = 2)
        
        abline(v = medianByDate, col = "blue", lwd = 2, lty = 3)
    
    dev.off()
    
    results <- c(meanByDate, medianByDate)
    
    return (results)
    
}