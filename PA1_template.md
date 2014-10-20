# Reproducible Research: Peer Assessment 1
Ricardo Gutiérrez (@kyeeh)  

### Introduction

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.


### Preparing R environment
This actions are required to perform the markdown document:

1. Set Working directory.
2. Load libraries required.
3. Define Markdown options for this document.


```r
setwd("~/Documents/Coursera/dss/5.- Reproducible Research/Projects/RepData_PeerAssessment1")
library(ggplot2)
library(knitr)
library(plyr)
opts_chunk$set(echo = TRUE, results = 'hold')
```


### Loading and preprocessing the data
Data Loading starts reading *CVS* input file located into `data/`folder, the 
working directory is defined as well. Preprocessing consists in remove `NA` 
observations using the following code: 


```r
setwd("~/Documents/Coursera/dss/5.- Reproducible Research/Projects/RepData_PeerAssessment1")
data <- read.csv("data/activity.csv", na.strings = 'NA')
```

This is a little sample of the data loaded:

```r
head(data,10)
```

```
##    steps       date interval
## 1     NA 2012-10-01        0
## 2     NA 2012-10-01        5
## 3     NA 2012-10-01       10
## 4     NA 2012-10-01       15
## 5     NA 2012-10-01       20
## 6     NA 2012-10-01       25
## 7     NA 2012-10-01       30
## 8     NA 2012-10-01       35
## 9     NA 2012-10-01       40
## 10    NA 2012-10-01       45
```

### What is mean total number of steps taken per day?
For this part of the assignment, missing values will be ignored in the dataset. 
In order to answer the question, the first steep is to preparate the data for 
calculations, then let's make a histogram of the total number of steps taken 
each day: 

```r
sbd <- aggregate(steps ~ date, data = data, FUN=sum,na.rm=TRUE)
ggplot(sbd,aes(x=steps)) + geom_histogram(binwidth = 3000, colour = "black", 
  fill = "orange") + labs(title = "Steps Taken per Day", x="Number of Steeps",
  y="Frequency")
```

![plot of chunk unnamed-chunk-4](./PA1_template_files/figure-html/unnamed-chunk-4.png) 

Now, let's calculate the mean and median for this data set:

```r
ma <- mean(sbd$steps,na.rm=TRUE)
md <- median(sbd$steps,)
```
Results:

* Mean = **10766.189**
* Median = **10765**

### What is the average daily activity pattern?
In order to show the daily activity pattern, let's plot the average number of 
steps taken daily against the interval number. The first steep is to preparate 
the data for ploting, then let's make a line chart of the average of steps taken 
daily: 

```r
sbi <- aggregate(steps ~ interval, data = data, FUN=mean,na.rm=TRUE)
ggplot(sbi,aes(x = interval, y = steps)) + geom_line(colour = "orange") +
    labs(title = "Average of Steps taken Daily", x = "Interval", 
    y = "Number of steps")
```

![plot of chunk unnamed-chunk-6](./PA1_template_files/figure-html/unnamed-chunk-6.png) 

Now, let's calculate the 5-minute interval that contains the maximum
number of steps:

```r
max_interval <- sbi[which.max(sbi$steps),]
```
With the folowing results:

* The **835<sup>th</sup>** interval has maximum **
206** steps.

### Imputing missing values

#### 1. Total number of missing values:

Let's calculate missing values for steps with `is.na()` method to check whether
the value is mising in the steps vector.


```r
mv <- sum(is.na(data$steps))
```

The amount of ***missing values*** is **2304**.

#### 2. Strategy for filling in all of the missing values in the dataset

To populate missing values, let's replace them with the mean value at the same 
interval across days. This action in order to avoid bias into some calculations 
or summaries of the data.


```r
data_filled <- adply(data, 1, function(x) if (is.na(x$steps)) {
    x$steps = round(sbi[sbi$interval == x$interval, 2])
    x
} else {
    x
})
```

In order to validate that everythin is ok, let's check if there any missing 
values remaining or not:

```r
sum(is.na(data_filled$steps))
```

```
## [1] 0
```

Zero output shows that there are ***NO MISSING VALUES***.

Let's see the data filled in the following histogram:

```r
sbd_filled <- aggregate(steps ~ date, data = data_filled, FUN=sum)
ggplot(sbd_filled,aes(x=steps)) + geom_histogram(binwidth = 3000, 
  colour = "black", fill = "orange") + labs(title = "Steps Taken per Day", 
  x="Number of Steeps", y="Frequency")
```

![plot of chunk unnamed-chunk-11](./PA1_template_files/figure-html/unnamed-chunk-11.png) 

Now, let's calculate the mean and median for this data set:

```r
maf <- mean(sbd_filled$steps,na.rm=TRUE)
mdf <- median(sbd_filled$steps,)
```

**Do these values differ from the estimates from the first part of the 
assignment?**

Yes, with a minimal difference for mean and median:

* Previows Mean = **10765.639** | 
Current Mean = **10766.189**
* Previows Median = **10762** | 
Current Median = **10765**

**What is the impact of imputing missing data on the estimates of the total 
daily number of steps?**

Comparing both histograms we observe t-student distribution differenced by an 
increasment in the max value or peak, after populate the data in this exercise.

### Are there differences in activity patterns between weekdays and weekends?

The dataset with the filled-in missing values will be usedfor this part. To do 
this comparison let's follow the next steps:

1. Create a new factor variable in the dataset with two levels – “weekday” and 
“weekend” subsetting the data by weekday or weekend day.

```r
sbd_filled <- aggregate(steps ~ date + interval, data = data_filled, FUN=sum)
data_weekend <- subset(sbd_filled, weekdays(as.Date(date)) %in% c("Saturday", "Sunday"))
data_weekday <- subset(sbd_filled, !weekdays(as.Date(date)) %in% c("Saturday", "Sunday"))
```

2. Obtain the average steps per interval for each dataset.

```r
data_weekend <- aggregate(steps ~ interval, data_weekend, mean)
data_weekday <- aggregate(steps ~ interval, data_weekday, mean)
```

3. Plot datasets for comparison.

```r
# Let's add a label for ploting
data_weekend <- cbind(data_weekend, day = rep("weekend"))
data_weekday <- cbind(data_weekday, day = rep("weekday"))
# Combine the subsets and a specify the levels
data_week <- rbind(data_weekend, data_weekday)
levels(data_week$day) <- c("Weekend", "Weekday")
# Draw
ggplot(data_week, aes(x = interval, y = steps)) + geom_line(colour = "orange") + 
  facet_grid(day ~ .) + labs(x = "Interval", y = "Number of steps")
```

![plot of chunk unnamed-chunk-15](./PA1_template_files/figure-html/unnamed-chunk-15.png) 

We can observe that activity on weekends tends to be more than activity on weekdays. This could be due to people spend work hours at office sitted the mayor of the time instead of weekends where the people make several activities in several spaces.
