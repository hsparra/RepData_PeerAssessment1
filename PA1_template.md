# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data

```r
options(scipen = 1, digits = 7)
library(data.table)
data <- read.csv("activity.csv")
data$day <- as.Date(data$date, "%Y-%m-%d")
data$weekday <- weekdays(data$day)
weekday.list <- unique(data$weekday)
day.type <- c(rep("Weekday", 5), rep("Weekend", 2))
data$dayType <- day.type[match(data$weekday, weekday.list)]
```

## Remove the NA values

```r
data.noNA <- data[!is.na(data$steps),]
```


## What is mean total number of steps taken per day?
Sum the steps for each day.

```r
total.steps.by.day <- xtabs(steps ~ day, data=data.noNA)
```

Create a histogram of the mean number of steps per day.

```r
hist(total.steps.by.day, main="Histogram of Total Number of Steps per Day", xlab="")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

Calculate the mean of the number of steps per day.

```r
mean(total.steps.by.day)
```

```
## [1] 10766.19
```

Calculate the median of the number of steps per day.

```r
median(as.vector(total.steps.by.day))
```

```
## [1] 10765
```

## What is the average daily activity pattern?
Take the average number of steps by the interval across days.

```r
library(plyr)
avgsteps <- ddply(data.noNA, .(interval), summarize, avg=mean(steps))
with(avgsteps, plot(interval, avg, type="l", 
                                   main="Average Daily Activity Pattern", 
                                   xlab="Interval (5 minute increment)",
                                   ylab="Average Number of Steps"))
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7.png) 

Interval with the maximum number of steps on average across the day.

```r
avgsteps[avgsteps$avg == max(avgsteps$avg),"interval"]
```

```
## [1] 835
```

## Imputing missing values
Total number of mission values in original data.

```r
sum(is.na(data$steps))
```

```
## [1] 2304
```

For simplicity replace missing values with the average daily number of steps for that interval.
Round the average number of steps to a whole value since there are not partial steps.

```r
nareplaced <- merge(data, avgsteps, by="interval")
na.idx <- which(is.na(nareplaced$steps))
nareplaced$steps[na.idx] <- round(nareplaced[na.idx,"avg"],0)
```


Find the number of steps per day for new data set.

```r
intervalsteps <- ddply(nareplaced, .(day), summarize, steps=sum(steps))
```

Histogram with the total number of steps taken per day

```r
hist(intervalsteps$steps, main="Total Number of Steps per Day by 5-Minute Interval (NAs Replaced)", xlab="")
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12.png) 

Calculate the mean of the number of steps per day.

```r
mean(intervalsteps$steps)
```

```
## [1] 10765.64
```

Calculate the median of the number of steps per day.

```r
median(intervalsteps$steps)
```

```
## [1] 10762
```

### Brief discussion of changes due to replacing NA values
The overall shape of the distribution remained the same with a higher peak.
The mean was almost the same which makes sense given that the rounded average of each interval was used to fill in the 
corresponding interval with a missing value.
The median moved only slightly due to the addition of additional values since the mean and median of the observations with step values were very close. 

## Are there differences in activity patterns between weekdays and weekends?

Convert the day type variable created above during the initial processing to a factor. Compute average steps per interval using new data then plot the data to compare weekdays with weekends.

```r
library(ggplot2)
nareplaced$dayType <- factor(nareplaced$dayType)
intervalsteps <- aggregate(steps ~ interval + dayType, data=nareplaced, FUN=mean)
ggplot(intervalsteps, aes(interval, steps)) + geom_line() + xlab("Interval") + ylab("Number of steps") + facet_grid(dayType ~ .)
```

![plot of chunk unnamed-chunk-15](figure/unnamed-chunk-15.png) 
