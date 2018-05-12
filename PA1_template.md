---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---




## Loading and preprocessing the data


```r
data <- read.table(unz("activity.zip", "activity.csv"), header=T, quote="\"", sep=",")
```


## What is mean total number of steps taken per day?



```r
totalByDay<-aggregate(list(steps=data$steps), by=list(date=data$date),  FUN=sum, na.rm=TRUE, na.action=NULL)

mean(totalByDay$steps,na.rm=TRUE)
```

```
## [1] 9354.23
```

```r
median(totalByDay$steps,na.rm=TRUE)
```

```
## [1] 10395
```



## What is the average daily activity pattern?

```r
# Calc avg steps per day
avgByDayAndInterval<-aggregate(list(steps=data$steps), by=list(date=data$date,interval=data$interval),  FUN=mean, na.rm=TRUE, na.action=NULL)

avgFor5min<-subset(avgByDayAndInterval, avgByDayAndInterval$interval==5)

max<-avgFor5min[which.max(avgFor5min$steps),]$date

plot(steps ~ date, avgFor5min, type="l")
abline(v=max)
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
max
```

```
## [1] 2012-10-10
## 61 Levels: 2012-10-01 2012-10-02 2012-10-03 2012-10-04 ... 2012-11-30
```




## Imputing missing values

```r
sum(is.na(data$steps))
```

```
## [1] 2304
```

```r
uniqueDates<-unique(data$date)

dataImputed<-data

# Impute using daily average
dataImputed$steps[is.na(dataImputed$steps)]<- with(dataImputed, ave(steps, uniqueDates, FUN = function(x) mean(x, na.rm = TRUE)))[is.na(dataImputed$steps)]

totalImputedByDay<-aggregate(list(steps=dataImputed$steps), by=list(date=dataImputed$date),  FUN=sum, na.rm=TRUE, na.action=NULL)

hist(totalImputedByDay$steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
mean(totalImputedByDay$steps,na.rm=TRUE)
```

```
## [1] 10768.21
```

```r
median(totalImputedByDay$steps,na.rm=TRUE)
```

```
## [1] 10765
```

Both mean and median have increased

## Are there differences in activity patterns between weekdays and weekends?


```r
dataImputed$weekpart <- ifelse(weekdays(as.Date(totalImputedByDay$date))%in%c("Saturday","Sunday"),"weekend", "weekday")

dataImputed$weekpart <- as.factor(dataImputed$weekpart)

totalImputedByInterval <- aggregate(list(steps=dataImputed$steps), by=list(interval=dataImputed$interval, weekpart=dataImputed$weekpart),  FUN=mean, na.rm=TRUE, na.action=NULL)

qplot(interval, steps, data=totalImputedByInterval, group=1, facets = .~weekpart,geom="line")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

