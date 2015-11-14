# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
Use the following packages for generating the document:


```r
library(ggplot2)
library(knitr)
library(scales)
```

Unzip and read the dataset

```r
activitydata <- read.csv(unz("activity.zip", "activity.csv"), colClasses = c("numeric", "Date", "numeric"))
```

## What is mean total number of steps taken per day?

Calculate number of steps taken per day and plot.

```r
stepsPerDay <- aggregate(activitydata$steps ~ activitydata$date, data = activitydata , FUN = sum, na.action = na.omit)

qplot(stepsPerDay$`activitydata$steps`, data = stepsPerDay, binwidth=2000, xlab = "Number of steps", ylab = "Count")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

```r
mean(stepsPerDay$`activitydata$steps`)
```

```
## [1] 10766.19
```

```r
median(stepsPerDay$`activitydata$steps`)
```

```
## [1] 10765
```


## What is the average daily activity pattern?

Calculate number of steps per interval over all days and plot.


```r
stepsPerInterval <- aggregate(activitydata$steps ~ activitydata$interval, data = activitydata , FUN = mean, na.action = na.omit)

plot(stepsPerInterval$`activitydata$interval`, stepsPerInterval$`activitydata$steps`, type = "l", xlab = "Interval", ylab = "Mean number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

Display maximum number of steps taken and the interval at which it happened

```r
maximumSteps <- stepsPerInterval[which.max(stepsPerInterval$`activitydata$steps`), ]
intervalAtWhichMaximumStepsTaken <- maximumSteps$`activitydata$interval`
maximumStepsTakenAtAnyGivenInterval <- maximumSteps$`activitydata$steps`
intervalAtWhichMaximumStepsTaken
```

```
## [1] 835
```

```r
maximumStepsTakenAtAnyGivenInterval
```

```
## [1] 206.1698
```

## Imputing missing values
Find number of missing rows

```r
missingRows <- is.na(activitydata$steps)
sum(missingRows)
```

```
## [1] 2304
```

Replace missing values with the mean of interval

```r
stepsPerIntervalList <- aggregate(x=list(steps=activitydata$steps), by=list(interval=activitydata$interval), FUN=mean, na.rm=TRUE)

replaceWithMeanOfInterval <- function(steps, interval) {
  filled <- NA
  if (!is.na(steps)) {
    filled <- c(steps)
  }
  else {
    filled <- (stepsPerIntervalList[stepsPerIntervalList$interval==interval, "steps"])
  }
  return(filled)
}
completeActivityData <- activitydata
completeActivityData$steps <- mapply(replaceWithMeanOfInterval, completeActivityData$steps, completeActivityData$interval)
```

Plot steps per day and find the new mean and median

```r
completeStepsPerDay <- aggregate(completeActivityData$steps ~ completeActivityData$date, data = completeActivityData , FUN = sum, na.action = na.omit)
qplot(completeStepsPerDay$`completeActivityData$steps`, data = completeStepsPerDay, binwidth=2000, xlab = "Number of steps", ylab = "Count")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png) 

```r
mean(completeStepsPerDay$`completeActivityData$steps`)
```

```
## [1] 10766.19
```

```r
median(completeStepsPerDay$`completeActivityData$steps`)
```

```
## [1] 10766.19
```

With this method of imputing missing data the mean remain the same as expected because we replace the missing values with the mean of the intervals but the median changes and is the same as the mean.

## Are there differences in activity patterns between weekdays and weekends?
Create new factor variable called 'typeOfDay'

```r
completeActivityData$typeOfDay <- as.numeric(format(completeActivityData$date, "%u"))
completeActivityData[completeActivityData$typeOfDay > 5, "typeOfDay"] <- "weekend"
completeActivityData[completeActivityData$typeOfDay != "weekend", "typeOfDay"] <- "weekday"
completeActivityData$typeOfDay <- as.factor(completeActivityData$typeOfDay)
```

Seperate weekday and weekend steps taken per interval 

```r
weekdaysData <- completeActivityData[completeActivityData$typeOfDay == "weekday", ]
weekendData <- completeActivityData[completeActivityData$typeOfDay == "weekend", ]

weekdaySteps <- split(weekdaysData$steps, weekdaysData$interval)
weekendSteps <- split(weekendData$steps, weekendData$interval)
```

Find mean and plot

```r
meanStepsPerWeekdayInterval <- sapply(weekdaySteps, mean)
meanStepsPerWeekendInterval <- sapply(weekendSteps, mean)

intervals <- unique(completeActivityData$interval)

par(mfcol=c(2,1))

plot(intervals, meanStepsPerWeekdayInterval, type="l", main="Mean number of steps per weekday interval", xlab="Interval", ylab="Number of steps", lwd=2)

plot(intervals, meanStepsPerWeekendInterval, type="l", main="Mean number of steps per weekend interval", xlab="Interval", ylab="Number of steps", lwd=2)
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png) 
