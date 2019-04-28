---
title: "Reproducible Research Project 1"
author: "Avila Xochihuatl Brayan Alexis"
date: "28/04/2019"
output:
  md_document:
    variant: markdown_github
---

## Introduction
It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the “quantified self” movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The data for this assignment can be downloaded from the course web site:

* Dataset: [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip) 

The variables included in this dataset are:

steps: Number of steps taking in a 5-minute interval (missing values are coded as 𝙽𝙰) </br>
date: The date on which the measurement was taken in YYYY-MM-DD format </br>
interval: Identifier for the 5-minute interval in which measurement was taken </br>
The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset. 

## Loading and preprocessing the data
Unzip data to obtain a csv file.

```{r}
getwd()
setwd("C:/Users/sagak/Desktop/Coursera/Reproducible Research/Project-1")
```

## Reading csv Data into Data.Table. 
```{r}
actDT<-read.csv("activity.csv")
```

## What is mean total number of steps taken per day?

1. Calculate the total number of steps taken per day

```{r}
total_steps <- aggregate(steps ~ date, data = actDT, FUN = sum, na.rm = TRUE)
head(total_steps,10)
```

2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day. 

```{r}
actDT$date <- as.Date(actDT$date, "%Y-%m-%d")
act_date <- as.Date(actDT$date, "%Y-%m-%d")
hist(total_steps$steps,main="Total Steps per Day",xlab="Number of Steps per Day",
     ylab = "Interval",col="blue",breaks=50)
```

3. Calculate and report the mean and median of the total number of steps taken per day
```{r}
msteps<- mean(total_steps$steps)
msteps
medsteps<- median(total_steps$steps)
medsteps
summary(total_steps)
```

## What is the average daily activity pattern?

1. Make a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```{r}
fivemin <- aggregate(steps ~ interval, data = actDT, FUN = mean, na.rm = TRUE)
plot(x = fivemin$interval,y = fivemin$steps,type="l",
     col="blue",xlab="5-minute Intervals",ylab = "Average Steps Taken ~ Days",
     main="Average Daily Activity Pattern")
```

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```{r}
maxsteps <- fivemin$interval[which.max(fivemin$steps)]
maxsteps
```


## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with 𝙽𝙰s)

```{r}
impute <- sum(is.na(actDT$steps))
impute
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```{r}
act2<- actDT
nas <- is.na(act2$steps)
avg_interval <- tapply(act2$steps, act2$interval, mean, na.rm=TRUE, simplify = TRUE)
act2$steps[nas] <- avg_interval[as.character(act2$interval[nas])]
names(act2)
sum(is.na(act2))
```

4. Make a histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```{r}
total_steps2 <- aggregate(steps ~ date, data = act2, FUN = sum, na.rm = TRUE)
head(total_steps2)
hist(total_steps2$steps,main = "Total Steps per Day (no-NA)",
     xlab = "Number of Steps per Day",ylab = "Interval",col="blue",breaks=50)
summary(total_steps)
summary(total_steps2)
```

Type of Estimate | Mean_Steps | Median_Steps
--- | --- | ---
First Part (with na) | 10765 | 10765
Second Part (fillin in na with median) | 9354.23 | 10395

## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```{r}
act2$day<- weekdays(act2$date)
act2$day_category<- sapply(act2$date, function(x) {
if (weekdays(x) == "sábado" | weekdays(x) =="domingo")
{y <- "Weekend"} else{y <- "Weekday"}
y})
head(act2)
head(act2,100)
total_steps2 <- aggregate(steps ~ day_category, data = act2, FUN = sum)
total_steps2
```

2. Make a panel plot containing a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```{r}
library(ggplot2)
activity_by_date <- aggregate(steps~interval + day_category, act2, mean, na.rm = TRUE)
plot<- ggplot(activity_by_date, aes(x = interval , y = steps, color = day_category)) +
geom_line() +
labs(title = "Average daily steps by type of date", x = "Interval", y = "Average number of steps") +
facet_wrap(~day_category, ncol = 1, nrow=2)
plot
```
