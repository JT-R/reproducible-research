---
title: "Reproducible Research course - Assignment 1"
author: "Jakub Tomaszewski"
date: "Friday, July 18, 2014"
output: html_document
---



This is my Assignment 1 report in the Reproducible Research course on Coursera.

Loading of the dataset into R workspace. Data source: https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip.

```r
library(data.table)
dataset <- read.csv("activity.csv")
```

Conversion of dataset to data.table type to increase the speed of processing:


```r
dataset <- data.table(dataset)
dataset$date <- as.POSIXct(dataset$date)
dataset$steps <- as.numeric(dataset$steps)
setkey(dataset,date)
sum_of_steps <- dataset[,list(steps=sum(steps)),by=list(date)]
print(head(sum_of_steps))
```

```
##          date steps
## 1: 2012-10-01    NA
## 2: 2012-10-02   126
## 3: 2012-10-03 11352
## 4: 2012-10-04 12116
## 5: 2012-10-05 13294
## 6: 2012-10-06 15420
```

Histogram of the total number of steps taken per day:


```r
hist(sum_of_steps$steps,main="Total number of steps taken per day",
     xlab="Total number of steps",breaks=10)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

Mean and median total number of steps taken per day:


```r
mean_of_steps <- mean(sum_of_steps$steps,na.rm=TRUE)
print(mean_of_steps)
```

```
## [1] 10766
```


```r
median_of_steps <- median(sum_of_steps$steps,na.rm=TRUE)
print(median_of_steps)
```

```
## [1] 10765
```
Time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days:


```r
means_of_steps_by_intervals <- dataset[,list(steps=mean(steps,na.rm=TRUE)),by=list(interval)]
plot(x=means_of_steps_by_intervals$interval,y=means_of_steps_by_intervals$steps,
     type="l",xlab="5-minute interval",ylab="Averaged number of steps",
     main="Plot of the number of steps in 5-minute intervals averaged by days")
grid()
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7.png) 

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
maximum_number_of_steps <- means_of_steps_by_intervals[which.max(steps)]
print(maximum_number_of_steps)
```

```
##    interval steps
## 1:      835 206.2
```

Imputation of missing values

Number of missing values:


```r
number_of_missing <- sum(is.na(dataset$steps))
print(number_of_missing)
```

```
## [1] 2304
```

Filling of missing values by the average number of steps in the given day:


```r
dataset_imputed <- dataset # Creating a local copy of dataset for imputation
intervals <- unique(dataset$interval) # Vector of unique dates in the dataset
for(i in 1:length(intervals)){
    tmp <- dataset_imputed[interval==intervals[i]]$steps
    dataset_imputed[interval==intervals[i]]$steps <- replace(tmp,is.na(tmp),mean(tmp,na.rm=TRUE)) 
}
```

Histogram of the total number of steps taken per day (after imputation)

Comparison with distribution of total number of steps in non-imputed dataset:


```r
sum_of_steps_imputed <- dataset_imputed[,list(steps=sum(steps)),by=list(date)]
par(mfrow=c(1,2))
hist(sum_of_steps$steps,main="Total number of steps taken\n per day",
     xlab="Total number of steps",breaks=10)
hist(sum_of_steps_imputed$steps,main="Total number of steps taken\n per day\n (after imputation by intervals)",
     xlab="Total number of steps",breaks=10)
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11.png) 

Mean and median total number of steps taken per day (after imputation):


```r
mean_of_steps_imputed <- mean(sum_of_steps_imputed$steps,na.rm=TRUE)
print(mean_of_steps_imputed)
```

```
## [1] 10766
```


```r
median_of_steps_imputed <- median(sum_of_steps_imputed$steps,na.rm=TRUE)
print(median_of_steps_imputed)
```

```
## [1] 10766
```

As we see, distribution of the data filled by the average number of steps in the given day is different in comparison to the raw data. Imputation caused that the mean and median values of total number of steps taken per day are equal (because there was so many missing values replaced by the mean).

Weekdays factor variable creation:


```r
weekday <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", 
            "Friday", "Saturday")[as.POSIXlt(dataset_imputed$date)$wday + 1]
dataset_imputed$weektime <- ifelse(weekday %in% c("Saturday","Sunday"),"weekend","weekday")
head(dataset_imputed)
```

```
##      steps       date interval weektime
## 1: 1.71698 2012-10-01        0  weekday
## 2: 0.33962 2012-10-01        5  weekday
## 3: 0.13208 2012-10-01       10  weekday
## 4: 0.15094 2012-10-01       15  weekday
## 5: 0.07547 2012-10-01       20  weekday
## 6: 2.09434 2012-10-01       25  weekday
```

Mean and median total number of steps taken per day (after imputation) by weekday/weekend:


```r
sum_of_steps_imputed_by_weektime <- dataset_imputed[,list(steps=sum(steps)),by=list(weektime,date)]
means_of_steps_imputed_by_weektime <- sum_of_steps_imputed_by_weektime[,list(steps=mean(steps)),by=list(weektime)]
print(means_of_steps_imputed_by_weektime)
```

```
##    weektime steps
## 1:  weekday 10256
## 2:  weekend 12202
```


```r
medians_of_steps_imputed_by_weektime <- sum_of_steps_imputed_by_weektime[,list(steps=median(steps)),by=list(weektime)]
print(medians_of_steps_imputed_by_weektime)
```

```
##    weektime steps
## 1:  weekday 10765
## 2:  weekend 11646
```

Time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (for imputed data):


```r
means_of_steps_by_intervals_imputed <- dataset_imputed[,list(steps=mean(steps,na.rm=TRUE)),by=list(weektime,interval)]
library(lattice)
xyplot(steps~interval|weektime,data=means_of_steps_by_intervals_imputed, type="l")
```

![plot of chunk unnamed-chunk-17](figure/unnamed-chunk-17.png) 


