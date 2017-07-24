# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
data <- read.csv(unz("activity.zip", "activity.csv"))
```

## What is mean total number of steps taken per day?
For this part of the assignment, you can ignore the missing values in the dataset.

Calculate the total number of steps taken per day
Make a histogram of the total number of steps taken each day


```r
aggdata <-aggregate(steps~date, data=data, FUN=sum, na.rm=TRUE)

hist(aggdata$steps, main="Histogram for Total number of steps a day", 
     xlab="Number of steps", )
```

![](./PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

Calculate and report the mean and median of the total number of steps taken per day
The Mean and median number of steps taken each day are part of the summary

```r
summary(aggdata) 
```

```
##          date        steps      
##  2012-10-02: 1   Min.   :   41  
##  2012-10-03: 1   1st Qu.: 8841  
##  2012-10-04: 1   Median :10765  
##  2012-10-05: 1   Mean   :10766  
##  2012-10-06: 1   3rd Qu.:13294  
##  2012-10-07: 1   Max.   :21194  
##  (Other)   :47
```


## What is the average daily activity pattern?
Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
avgInterval <- tapply(data$steps, data$interval, mean, na.rm = T)
plot(avgInterval, type = "l", main = "interval activity", xlab = "interval", ylab = "average number of steps")
```

![](./PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
which.max(avgInterval)
```

```
## 835 
## 104
```


## Imputing missing values
Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
complete<-complete.cases(data)
length(complete[complete==FALSE])
```

```
## [1] 2304
```
The strategy for filling in all of the missing steps is to take the mean of the 5-minute interval.
Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
imputeNAdata <- data
for (i in unique(data$interval)) {
        imputeNAdata$steps[is.na(imputeNAdata$steps) & imputeNAdata$interval == i] <- round(mean(imputeNAdata$steps[data$interval == i], na.rm = T))
}
```

Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 

```r
aggImpute <-aggregate(steps~date, data=imputeNAdata, FUN=sum, na.rm=TRUE)
hist(aggImpute$steps, main="Histogram Total number of steps a day (imputed data)", 
     xlab="Number of steps", )
```

![](./PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->
Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
summary(aggImpute)
```

```
##          date        steps      
##  2012-10-01: 1   Min.   :   41  
##  2012-10-02: 1   1st Qu.: 9819  
##  2012-10-03: 1   Median :10762  
##  2012-10-04: 1   Mean   :10766  
##  2012-10-05: 1   3rd Qu.:12811  
##  2012-10-06: 1   Max.   :21194  
##  (Other)   :55
```

```r
totalStepsDifference <- sum(aggImpute$steps) - sum(aggdata$steps)
totalStepsDifference
```

```
## [1] 86096
```

Because we imputed the missing data with average values the mean value stays the same at 10766 steps a day. The median is 3 lower. The total sum of steps is higher when imputed, so we expect the estimated number of steps each day to be higher.   


## Are there differences in activity patterns between weekdays and weekends?
For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
isWeekend <- lapply(X=imputeNAdata$date, FUN=function (x) { weekdays(as.Date(x)) %in% c("zaterdag", "zondag") })
```
Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
imputeNAdata$isWeekend<-isWeekend 
weekendSteps <- tapply(imputeNAdata$steps[imputeNAdata$isWeekend == TRUE], imputeNAdata$interval[imputeNAdata$isWeekend == TRUE], mean)
nonWeekendSteps <- tapply(imputeNAdata$steps[imputeNAdata$isWeekend == FALSE], imputeNAdata$interval[imputeNAdata$isWeekend == FALSE], mean)

par(mfrow = c(2,1))
plot(nonWeekendSteps, type = "l", main = "weekday"
     ,xlab = "interval"
     ,ylab = "average number of steps")

plot(weekendSteps, type = "l", main = "weekend"
     ,xlab = "interval"
     ,ylab = "average number of steps")
```

![](./PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->
