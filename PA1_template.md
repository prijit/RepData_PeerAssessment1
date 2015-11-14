# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
The following packages are used to produce this report:


```r
library(ggplot2)
library(knitr)
library(scales)
```

Read the dataset and calculate number of steps per day

```r
activitydata <- read.csv(unz("activity.zip", "activity.csv"), colClasses = c("numeric", "Date", "numeric"))
```

## What is mean total number of steps taken per day?

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


```r
stepsPerInterval <- aggregate(activitydata$steps ~ activitydata$interval, data = activitydata , FUN = mean, na.action = na.omit)

plot(stepsPerInterval$`activitydata$interval`, stepsPerInterval$`activitydata$steps`, type = "l", xlab = "Interval", ylab = "Mean number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

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

```r
missingRows <- is.na(activitydata$steps)
sum(missingRows)
```

```
## [1] 2304
```

```r
stepsPerIntervalList <- aggregate(x=list(steps=activitydata$steps), by=list(interval=activitydata$interval),
                      FUN=mean, na.rm=TRUE)

replaceWithMeanOfInterval <- function(steps, interval) {
  filled <- NA
  if (!is.na(steps))
    filled <- c(steps)
  else
    filled <- (stepsPerIntervalList[stepsPerIntervalList$interval==interval, "steps"])
  return(filled)
}
completeActivityData <- activitydata
completeActivityData$steps <- mapply(replaceWithMeanOfInterval, completeActivityData$steps, completeActivityData$interval)

completeStepsPerDay <- aggregate(completeActivityData$steps ~ completeActivityData$date, data = completeActivityData , FUN = sum, na.action = na.omit)
qplot(completeStepsPerDay$`completeActivityData$steps`, data = completeStepsPerDay, binwidth=2000, xlab = "Number of steps", ylab = "Count")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

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


## Are there differences in activity patterns between weekdays and weekends?

```r
completeActivityData$typeOfDay <- as.numeric(format(completeActivityData$date, "%u"))
completeActivityData[completeActivityData$typeOfDay > 5, "typeOfDay"] <- "weekend"
completeActivityData[completeActivityData$typeOfDay != "weekend", "typeOfDay"] <- "weekday"
completeActivityData$typeOfDay <- as.factor(completeActivityData$typeOfDay)

datWeekdays <- completeActivityData[completeActivityData$typeOfDay == "weekday", ]
datWeekends <- completeActivityData[completeActivityData$typeOfDay == "weekend", ]

datSplitWeekdays <- split(datWeekdays$steps, datWeekdays$interval)
datSplitWeekends <- split(datWeekends$steps, datWeekends$interval)

# Find the average for each interval

meanStepsPerWeekdayInterval <- sapply(datSplitWeekdays, mean)
meanStepsPerWeekendInterval <- sapply(datSplitWeekends, mean)

intervals <- unique(completeActivityData$interval)

par(mfcol=c(2,1))
plot(intervals, meanStepsPerWeekdayInterval, type="l",
     main="Average number of steps per interval across all weekdays", 
     xlab="Interval", ylab="Average steps across all weekdays", 
     lwd=2)
plot(intervals, meanStepsPerWeekendInterval, type="l",
     main="Average number of steps per interval across all weekends", 
     xlab="Interval", ylab="Average steps across all weekends", 
     lwd=2)
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png) 
