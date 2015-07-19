modData <- function(data, stepsByInterval) {  ## replaces missing values
    
    newData <- data
    
    for (i in unique(newData$date)) {
        
        for (j in unique(newData$interval)) {
            
            if (is.na(newData[newData$date == i & newData$interval == j, "steps"])) {
                
                newData[newData$date == i & newData$interval == j, "steps"] <- 
                    
                    stepsByInterval[stepsByInterval$interval == j, "steps"]
                
            }  ## if ends
            
        }  ## 2nd for ends
        
    }  ## 3rd for ends
    
    return(newData)
    
}