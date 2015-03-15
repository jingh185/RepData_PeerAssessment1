# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
Load the data (i.e. read.csv())

Process/transform the data (if necessary) into a format suitable for your analysis



```r
setwd ("~/Coursera_Rcode/Reproduciable_research/RepData_PeerAssessment1/")
data <- read.csv("activity.csv")
#remove NA
cleanData <- na.omit(data)
```

## What is mean total number of steps taken per day?
For this part of the assignment, you can ignore the missing values in the dataset.

1. Calculate the total number of steps taken per day


```r
totalSteps <- aggregate(x = cleanData[c("steps")], 
                        FUN = sum, 
                        by = list(group.date = cleanData$date))
```

2. Make a histogram of the total number of steps taken each day


```r
hist(totalSteps$steps, col="red", xlab = "Total Number of Steps per day" , 
        main = "Total number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

3. Calculate and report the mean and median of the total number of steps taken per day

```r
mean(totalSteps$steps, na.rm = TRUE)
```

```
## [1] 10766.19
```

```r
median(totalSteps$steps, na.rm = TRUE)
```

```
## [1] 10765
```


## What is the average daily activity pattern?
1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
#prepare data
library(plyr)
cleanData <- na.omit(data)
avgSteps <- ddply(cleanData, ~interval, summarise, mean=mean(steps))

#make a plot
library(ggplot2)
qplot(x=interval, y=mean, data = avgSteps,  geom = "line",
      xlab="5 Minutes Interval",
      ylab="Number of Steps",
      main="Average Number of steps across all days"
)
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
avgSteps[which.max(avgSteps$mean),]
```

```
##     interval     mean
## 104      835 206.1698
```


## Imputing missing values
Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
nrow(data[(is.na(data$steps)),])
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


```r
missData <- data[(is.na(data$steps)),]
#using mean for that 5 mins interval
missData$steps <- round(tapply(X=cleanData$steps,INDEX=cleanData$interval,FUN=mean))
```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
newData <- rbind(missData, cleanData)
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?



```r
#calculate the new total number of steps
newTotalSteps <- aggregate(x = newData[c("steps")], 
                        FUN = sum, 
                        by = list(group.date = newData$date))

#plot histogram
hist(newTotalSteps$steps, col="red", xlab = "Number of Steps per day" , 
        main = "Total number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png) 

```r
#mean
mean(newTotalSteps$steps)
```

```
## [1] 10765.64
```

```r
#median
median(newTotalSteps$steps)
```

```
## [1] 10762
```

The mean 10765.64 and median 10762 of the new dataset is different to the mean 10766.19 ad median 10765 of the original dataset. The histogram diagram pattern are very similar but the freqency counts increased.

## Are there differences in activity patterns between weekdays and weekends?
For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
dayType <- data.frame(sapply(X = newData$date, FUN = function(day) {
        if (weekdays(as.Date(day)) %in% c("Monday", "Tuesday", "Wednesday","Thursday", "Friday")) {
                day <- "weekday"
        } else {
                day <- "weekend"
        }
}))

finalData <- cbind (newData, dayType)
#rename last column
colnames(finalData)[4] <- "dayType"
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
plotData <- aggregate(finalData[c("steps")], 
                        list(type = finalData$dayType, interval = finalData$interval), mean)

library(ggplot2)
qplot(interval, steps, data = plotData, facets = type~., geom = c("line"), method = "lm"
      , color = type , ylab = "Number of steps", xlab = "Interval",
      main = "Average number of steps across weekdays or weekends")
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png) 
