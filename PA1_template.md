---
title: "courseassignment_1"
author: "Harsh Ekambe"
date: "10/07/2020"
output: 
  html_document: 
    keep_md: yes
---

**COURSE ASSIGNMENT**

First we download the data from the url,unzip it,read it and process it.


```r
fileurl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileurl,destfile = "./activity.zip",method = "curl")
unzip("activity.zip")
DATA <- read.csv("activity.csv",sep=",")
DATA <- transform(DATA,date=as.Date(DATA$date,"%Y-%m-%d"))
```

Now we will answer all the questions one by one 

1)Histogram of the total number of steps taken each day?


```r
stepsperday <- aggregate(steps~date,DATA,sum)
hist(stepsperday$steps,col="blue",main="Histogram of the total number of steps taken each day",xlab="Steps")
```

![](PA1_template_files/figure-html/histogram code-1.png)<!-- -->

2)Mean and median number of steps taken each day?


```r
stepsperday <- aggregate(steps~date,DATA,sum)
mean(stepsperday$steps)
```

```
## [1] 10766.19
```

```r
median(stepsperday$steps)
```

```
## [1] 10765
```

3)Time series plot of the average number of steps taken?


```r
stepsperinterval <- aggregate(steps~interval,DATA,mean)
plot(stepsperinterval$interval,stepsperinterval$steps,type="l",main="Average steps across Interval",xlab = "Interval",ylab = "Steps")
```

![](PA1_template_files/figure-html/time series plot-1.png)<!-- -->

4)The 5-minute interval that, on average, contains the maximum number of steps?


```r
stepsperinterval[which.max(stepsperinterval$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```

5)Code to describe and show a strategy for imputing missing data?

for missing data we need to find how many missing values are there first and then we are going to fill them by the mean steps per day on basis of interval.Here is the complete code how to do it and we have created a new datasets by these values called as newactivity


```r
stepsaverage <- aggregate(steps~interval,DATA,mean)

fillNA <- numeric()
for(i in 1:nrow(DATA)){
  obs<- DATA[i,]
  if(is.na(obs$steps)){
    steps <- subset(stepsaverage,interval==obs$interval)$steps
  }
  else{
    steps <- obs$steps
    
  }
  fillNA <- c(fillNA,steps)
}

newactivity <- DATA
newactivity$steps <- fillNA
```

6)Histogram of the total number of steps taken each day after missing values are imputed


```r
newstepsperday <- aggregate(steps~date,newactivity,sum)

hist(newstepsperday$steps,col="blue",breaks = 20,main = "Histogram of total steps per day after imputing",xlab = "total steps per day")
```

![](PA1_template_files/figure-html/Histogram with new dataset-1.png)<!-- -->

7)Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends?

for this we need to add a variable in the newdataset which is a factor variable of two levels weekday or weekend.To assign the corresponding, we extracted all the dates columns into a single variable and run a for loop to assign all the values and readded them to newdataset by using dplyr package.Here is the code to do it and we have used lattice plotting package to plot.


```r
d <- newactivity$date
day <- weekdays(d)

for(i in 1:length(day)){
  if(day[i]=="Saturday"){
    day[i]="weekend"
  }
  else if(day[i]=="Sunday"){
    day[i]="weekend"
  }
  else {
    day[i]="weekday"
  }
}




library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
newactivity <- mutate(newactivity,day=day)
newactivity <- transform(newactivity,day=as.factor(day))
averagesteps <- aggregate(steps~interval+day,newactivity,mean)

library(lattice)
xyplot(steps~interval|day,averagesteps,type="l",layout=c(1,2))
```

![](PA1_template_files/figure-html/Panel plot-1.png)<!-- -->

We have answered all the questions asked in the above assignment




