---
title: "Peer Assessment 1 of course 'Reproducible Research'"
author: "Zakharenko Marina"
date: "Sunday, March 15, 2015"
output: html_document
---

Loading the data

```r
data <- read.csv("./activity/activity.csv")
```

###What is mean total number of steps taken per day?

Calculating the total number of steps taken per day

```r
total_steps <- aggregate(steps ~ date, data, sum)
```

Calculating the mean and median of the total number of steps taken per day

```r
mean_steps <- as.integer(round(mean(total_steps$steps)))
median_steps <- as.integer(round(median(total_steps$steps)))
```


The mean of the total number of steps is 10766.
The median of the total number of steps is 10765.

Drawing a histogram of the total number of steps taken each day

```r
lab1 <- paste("Mean   - ", mean_steps)
lab2 <- paste("Median - ", median_steps)
library(ggplot2)
g <- ggplot(total_steps, aes(x=steps))
g <- g + geom_histogram(binwidth=1000, colour="black", fill="white") + 
    geom_vline(aes(xintercept=mean(steps, na.rm=T)),color="red", linetype="dashed", size=1.5) +
    geom_vline(aes(xintercept=median(steps, na.rm=T)),color="green", size=1) +
    labs(title = "The total number of steps taken each day") +
    geom_text(aes(15000, 9, label = lab1, color="red")) +
    geom_text(aes(15000, 8.5, label = lab2, color="green")) +
    theme(legend.position = "none")

print(g)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

###What is the average daily activity pattern?

Calculating the average number of steps taken, averaged across all days

```r
aver_steps <- aggregate(steps ~ interval, data, mean)
```

Converting the 5-minute interval in POSIXct format

```r
aver_steps$date <- formatC(aver_steps$interval, width = 4, format = "d", flag = "0")
aver_steps$date <- as.POSIXct(paste("20150311", aver_steps$date), format="%Y%m%d %H%M")
```

Calculating the 5-minute interval, containing the maximum number of steps


```r
max_interval <- aver_steps$date[aver_steps$steps == max(aver_steps$steps)]
max_interval_time <- substr(as.character(max_interval),12,16)
```

5-minute interval from 08:35 contains the maximum number of steps

Drawing a time series plot

```r
library(scales)
line_max_interval <- as.numeric(max_interval)
g <- ggplot(aver_steps, aes(date, steps))
g <- g +
    geom_line() +
    scale_x_datetime(labels = date_format("%H:%M")) +
    labs(title = "The average daily activity pattern") +
    theme(plot.title = element_text(face = "bold", size = 14)) +
    xlab("Interval") + ylab("Average number of steps") +
    geom_vline(aes(xintercept = line_max_interval), 
               color = "red", linetype = "dashed") +
    geom_text(aes(max_interval, 0, label = max_interval_time,
                  size = 8, color = "red")) +
    theme(legend.position = "none")


print(g)
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png) 
