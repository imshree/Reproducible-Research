# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

## Assuming the activity.csv is copied in current working directory


```r
activity_data <- read.csv("activity.csv")
```

## Processing/transforing the data

## Let’s convert dates from strings to actual dates


```r
activity_data$date <- as.Date(activity_data$date)
```

## What is mean total number of steps taken per day?

## Calculate the total number of steps taken per day


```r
library(ggplot2)
activityByDate <- aggregate(steps ~ date, activity_data, sum)
qplot(steps, data = activityByDate, binwidth = 1000, color = I('black'), fill = I('blue'), main = 'Total number of steps per day', xlab = 'Number of steps')
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

## Calculate and report the mean and median of the total number of steps taken per day


```r
summary(activityByDate$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    8841   10760   10770   13290   21190
```

## What is the average daily activity pattern?

## Make a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
averageDay <- aggregate(steps ~ interval, activity_data, mean)
qplot(interval, steps, data = averageDay, geom = 'line', color = I('blue'), main = 'Average daily activity', xlab = '5-min interval', ylab='Average number of steps')
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

## Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
averageDay[which.max(averageDay$steps), ]
```

```
##     interval    steps
## 104      835 206.1698
```


## Imputing missing values

## Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with 𝙽𝙰s)


```r
sum(is.na(activity_data$steps))
```

```
## [1] 2304
```


## Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

## I am filling NA values with the mean for that interval across all days

## Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
fullActivity <- activity_data
fullActivity$steps[is.na(fullActivity$steps)] <- averageDay[averageDay$interval == fullActivity$interval[is.na(fullActivity$steps)], 'steps']
```

## Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
fullActivityByDate <- aggregate(steps ~ date, fullActivity, sum)
qplot(steps, data = fullActivityByDate, binwidth = 1000, color = I('black'), fill = I('blue'), main='Total number of steps per day(without NA)', xlab='Number of steps')
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png) 

## Now let’s compare the two histograms with NA and without NA


```r
library(gridExtra)
```

```
## Warning: package 'gridExtra' was built under R version 3.1.3
```

```r
q1 <- qplot(steps, data = activityByDate, binwidth = 1000, color = I('black'), fill=I('blue'), main='With NA', xlab='Number of steps')
q2 <- qplot(steps, data = fullActivityByDate, binwidth = 1000, color = I('black'), fill=I('blue'), main='Without NA', xlab='Number of steps')
grid.arrange(q1,q2,nrow=1,ncol=2)
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png) 

## The plots look pretty much the same, supposedly because the data were missing for whole days and we filled it will the ‘average day’ data. Therefore it didn’t impact the distribution.


## Are there differences in activity patterns between weekdays and weekends?

## Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
library(chron)
fullActivity$datType <- factor(is.weekend(fullActivity$date), labels = c('weekday','weekend'))
```

## Make a panel plot containing a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
fullActivity$dayType <- factor(is.weekend(fullActivity$date), labels = c('weekday','weekend'))
fullAverageDay <- aggregate(steps ~ interval + dayType, fullActivity, mean)
qplot(interval, steps, data = fullAverageDay, geom = 'line', facets = dayType ~ ., color = I('blue'), xlab='Interval', ylab='Number od steps')
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png) 


