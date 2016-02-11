---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

## Loading and preprocessing the data

```r
library(knitr)
library(plyr)
library(dplyr)
library(lubridate)
library(Hmisc)
unzip("activity.zip")
## Read in data
activityDataRaw <- read.csv("activity.csv")
activityDataTidy <- activityDataRaw
##Normalize interval variable to a continous interval variable in minutes
activityDataTidy$interval <- (activityDataRaw$interval %/% 100 * 60) + (activityDataRaw$interval %% 100)
```

## What was the mean total number of steps taken per day?

```r
dailySteps <- activityDataTidy %>%
  group_by(date) %>%
  summarise(sum(steps, na.rm=TRUE))

hist(dailySteps$sum, main = paste("Histogram of Total Number of Steps Taken Each Day"), xlab="Total Number of Steps per Day", breaks=10)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

```r
meanSteps <- mean(dailySteps$sum, na.rm = TRUE)
medianSteps <- median(dailySteps$sum, na.rm = TRUE)
```

**Mean Daily Steps**

The **mean number of daily steps** is **9354.2295082.**

**Median Daily Steps**

The **median number of daily steps** is **10395.**

## What is the average daily activity pattern?

```r
meanStepsPerInterval <- activityDataTidy %>%
  group_by(interval) %>%
  summarise(mean(steps, na.rm = TRUE))

plot(meanStepsPerInterval$interval, meanStepsPerInterval$mean, type="l", ylab="Mean Number of Steps", xlab="Time of day [min] - 5 min intervals", main="Mean Number of Steps per 5 Minute Interval throughout the Day")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

```r
maxIndex <- which.max(meanStepsPerInterval$mean)
maxInterval <- meanStepsPerInterval$interval[maxIndex] 
```

The 5-minute interval, on average across all the days in the dataset, containing the maximum number of steps is: 515 - 520 min of the day

## Inputing missing values

```r
missingObservations <- sum(is.na(activityDataTidy$steps))
```

In total there were 2304 missing values in the original dataset.  

These observations were substituted with the mean number of steps per interval, as calculated ignoring the missing values.

This substitution produces an adjusted set.  

```r
activityDataTidy2 <- activityDataTidy
activityDataTidy2$steps <- impute(activityDataTidy2$steps,mean)

dailySteps2 <- activityDataTidy2 %>%
  group_by(date) %>%
  summarise(sum(steps, na.rm=TRUE))

hist(dailySteps2$sum, main = paste("Histogram of Total Number of Steps Taken Each Day"), xlab="Total Number of Steps per Day", breaks=10)
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

```r
meanSteps2 <- mean(dailySteps2$sum, na.rm = TRUE)
medianSteps2 <- median(dailySteps2$sum, na.rm = TRUE)
```

In consideration of the new data set, the following data is returned.

**Mean Daily Steps**

The **mean number of daily steps** is **1.0766189 &times; 10<sup>4</sup>.**

**Median Daily Steps**

The **median number of daily steps** is **1.0766189 &times; 10<sup>4</sup>.**

Making adjustments for the missing values has dropped both the median and mean for the set.

## Are there differences in activity patterns between weekdays and weekends?

```r
 activityDataTidy2$dayofweek <- as.factor(weekdays(ymd(activityDataTidy2$date)))
 activityDataTidy2$dayofweek <- revalue(activityDataTidy2$dayofweek, c("Monday"="weekday","Tuesday"="weekday","Wednesday"="weekday","Thursday"="weekday","Friday"="weekday", "Saturday"="weekend", "Sunday"="weekend"))

dailySteps3 <- activityDataTidy2 %>%
  group_by(dayofweek, interval) %>%
  summarise(mean(steps)) 

  xyplot(dailySteps3$mean ~ interval | dayofweek, dailySteps3, 
         type="l",
         ylab="Mean Number of Steps", 
         xlab="Time of day [min] - 5 min intervals", 
         main="Average Number of Steps per 5 Minute Interval throughout the Day",
         layout=c(1,2))
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 
