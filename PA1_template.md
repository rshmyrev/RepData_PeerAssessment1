# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
# load library
library(dplyr)
library(ggplot2)

# unzip and load data
unzip('activity.zip')
df <- read.csv(file = "activity.csv",
               colClasses = c('integer', 'Date', 'integer'))
```


## What is mean total number of steps taken per day?

```r
total_steps_per_day <- df %>%
    group_by(date) %>%
    summarise(total_steps = sum(steps, na.rm = TRUE))

qplot(total_steps, data = total_steps_per_day,
      main = 'Total number of steps per day',
      xlab = 'Total number of steps', ylab = 'Days')
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

Mean of the total number of steps taken per day =
``9354``

Median of the total number of steps taken per day =
``10395``

## What is the average daily activity pattern?

```r
average_steps_per_interval <- df %>%
    group_by(interval) %>%
    summarise(average_steps = mean(steps, na.rm = TRUE))

qplot(interval, average_steps, data = average_steps_per_interval, geom = 'line',
      main = 'Average number of steps taken, averaged across all days',
      xlab = '5-minute interval', ylab = 'Average number of steps')
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

```r
average_steps_per_interval_sorted <- average_steps_per_interval %>%
    arrange(desc(average_steps))
```

``835``-th
5-minute interval contains the maximum number of steps 
(``206.1698113``)

## Imputing missing values

```r
total_na <- sum(is.na(df))
```
Total number of missing values in the dataset =
``2304``

## Are there differences in activity patterns between weekdays and weekends?
