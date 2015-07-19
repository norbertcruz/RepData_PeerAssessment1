# Reproducible Research: Peer Assessment 1
Norbert J. Cruz Lebron  

## Overview

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit.  But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The task was completed by separating each part of the analysis into smaller scripts of code (functions).

## Loading and preprocessing the data

The data and scripts needed are loaded. The data is filtered to remove missing values. The cleaned data is used when missing values can be ignored.


```r
    library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
    library(ggplot2)

    source("./scripts/byDate.R")
    
    source("./scripts/byInterval.R")
    
    source('./scripts/modData.R')
    
    if (!file.exists("activity.csv")) unzip("activity.zip")
    
    data <- read.csv("activity.csv")
    
    cleanData <- filter(data, !is.na(data$steps))
```

Here is a description of the cleaned data.


```
## 'data.frame':	15264 obs. of  3 variables:
##  $ steps   : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 2 2 2 2 2 2 2 2 2 2 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```


## What is mean total number of steps taken per day?

The cleaned data is passed to the function byDate to generate a histogram of the number of steps taken per day. The mean and median values of steps per day are calculated. 


```r
    resultsByDate <- byDate(cleanData, 1)
    
    names(resultsByDate) <- c("mean", "median")
```

Histogram:

![Histogram](./figures/histogram1.png)

## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
