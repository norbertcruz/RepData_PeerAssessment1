byInterval <- function(data) {  ## performs analysis by interval
    
    dataByInterval <- group_by(data, interval)
    
    dataByInterval <- select(dataByInterval, -date)
    
    stepsByInterval <- summarise_each(dataByInterval, funs(mean))
    
    maxByInterval <- max(stepsByInterval$steps)
    
    maxInterval <- stepsByInterval$interval[stepsByInterval$steps == maxByInterval]
    
    bins <- seq(0, max(unique(stepsByInterval$interval)), 90)
    
    png()
    
    dev.copy(png, file = "./figures/timeSeries1.png")
    
        par(las = 2)
    
        plot(stepsByInterval$interval, stepsByInterval$steps, type = "l", main = "Average Daily Activity Pattern",
            
             xlab = "Time Interval (minutes)", ylab = "Steps", xaxt = 'n')
        
        axis(1, at = bins, labels = bins)
        
        abline(v = maxInterval, col = "red", lty = 3)
        
        abline(v = maxInterval+5, col = "blue", lty = 3)
    
    dev.off()
    
    results <- c(maxInterval, stepsByInterval)
    
    return(results)
    
}