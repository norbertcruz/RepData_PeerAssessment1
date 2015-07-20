byInterval <- function(data) {  ## performs analysis by interval
    
    dataByInterval <- group_by(data, interval)
    
    dataByInterval <- select(dataByInterval, -date)
    
    stepsByInterval <- summarise_each(dataByInterval, funs(mean))
    
    maxByInterval <- max(stepsByInterval$steps)
    
    maxInterval <- stepsByInterval$interval[stepsByInterval$steps == maxByInterval]
    
    bins <- 1:length(stepsByInterval$interval)
    
    png()
    
    dev.copy(png, file = "./figures/timeSeries1.png")
    
    with(stepsByInterval, 

        plot(interval, steps, type = "l", main = "Average Daily Activity Pattern",
            
             xlab = "Time Interval (minutes)", ylab = "Steps", xaxt = 'n'),
        
        axis(1, at = bins, labels = bins)
        
        )
    
    dev.off()
    
    results <- c(maxInterval, stepsByInterval)
    
    return(results)
    
}