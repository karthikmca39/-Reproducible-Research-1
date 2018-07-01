---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---




## Loading and preprocessing the data
<!---
### Reading in the data
-->
We begin by extracting the raw text file from the zip archive.


```r
if (!file.exists(DATA_DIR)) dir.create(DATA_DIR)
if (!file.exists(file.path(DATA_DIR, 'activity.csv'))) {
  unzip(RAW_DATA_ARCHIVE, exdir=DATA_DIR)
}
```

We then read in the data using ``read.csv`` with default settings.


```r
data <- tibble::as.tibble(read.csv(file.path(DATA_DIR, 'activity.csv')))
```

After reading in the data we inspect the first few rows in the data set.


```r
print(data)
```

```
## # A tibble: 17,568 x 3
##    steps date       interval
##    <int> <fct>         <int>
##  1    NA 2012-10-01        0
##  2    NA 2012-10-01        5
##  3    NA 2012-10-01       10
##  4    NA 2012-10-01       15
##  5    NA 2012-10-01       20
##  6    NA 2012-10-01       25
##  7    NA 2012-10-01       30
##  8    NA 2012-10-01       35
##  9    NA 2012-10-01       40
## 10    NA 2012-10-01       45
## # ... with 17,558 more rows
```

We can see that the date column has been read in as a factor, so we convert its
values into proper ``Date`` objects.


```r
library(dplyr)
library(lubridate)
data <- mutate(data, date = ymd(date))
class(data$date)
```

```
## [1] "Date"
```

## What is the mean total number of steps taken per day?

In order to answer this question, we begin by calculating the total daily
steps through a summation over each day's 5-minute intervals. We choose to
ignore missing values for now as their influence on the analysis'
results will be examined in a later section.


```r
total_daily_steps <- with(data, tapply(steps, date, sum, na.rm = TRUE))
```

It is now straightforward to plot a histogram to examine the results visually.




```r
library(ggplot2)
ggplot(data=data.frame(total_daily_steps), aes(x=total_daily_steps)) +
  geom_histogram(binwidth=5000, center=5000/2, aes(col=I("white"))) +
  labs(title='Histogram of total daily steps', x='Total daily steps') +
  theme(plot.title = element_text(hjust = 0.5))
```

![](PA1_template_files/figure-html/hist_without_nas-1.png)<!-- -->

Here we calculate the mean and median of the total number of steps per day.


```r
c(mean=mean(total_daily_steps), median=median(total_daily_steps))
```

```
##     mean   median 
##  9354.23 10395.00
```


## What is the average daily activity pattern?

To visualize the average daily activity pattern we must first compute the
average number of steps per 5-minute interval across all days.


```r
avg_daily_activity <- {data %>%
                       group_by(interval) %>%
                       summarise(avg_steps=mean(steps, na.rm = TRUE))}
head(avg_daily_activity)
```

```
## # A tibble: 6 x 2
##   interval avg_steps
##      <int>     <dbl>
## 1        0    1.72  
## 2        5    0.340 
## 3       10    0.132 
## 4       15    0.151 
## 5       20    0.0755
## 6       25    2.09
```

We can now use the data computed in the previous step to build the desired
time-series plot.


```r
ggplot(avg_daily_activity, aes(x=interval, y=avg_steps)) + geom_line() +
  labs(title='Average Daily Activity Pattern') +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(y = 'Number of steps')
```

![](PA1_template_files/figure-html/plot_daily_activity_pattern-1.png)<!-- -->

From the plot above, it can be seen that the 5-minute interval that contains
the most steps (on average) must be close to, but less than 875. We can 
confirm this programmatically as follows:


```r
avg_daily_activity[which.max(avg_daily_activity$avg_steps), 'interval'][[1]]
```

```
## [1] 835
```

## Imputing missing values

The presence of missing days may introduce bias into some calculations or
summaries of the data, so we will assess this risk by first checking the total
number of missing values in the data set.


```r
sum(is.na(data$steps))
```

```
## [1] 2304
```

While the number of missing values in the data set seems to be significant
(2304), we will be more rigorous and
compute what proportion of the observations is missing.


```r
mean(is.na(data$steps))
```

```
## [1] 0.1311475
```

Because the proportion of missing values is indeed significant
(13.11%), we will impute them using
the mean number of steps per 5-minute interval, i.e., the data previously used
to build the daily activity pattern ``avg_daily_activity``.

Next, we use the imputation strategy described above to create a new data set
``data_imp`` that is equal to the original data set but with the missing data
filled in.


```r
library(plyr)
data_imp <- join(data, avg_daily_activity, by = 'interval')
na_rows <- is.na(data_imp$steps)
data_imp[na_rows, ]$steps <- data_imp[na_rows,]$avg_steps
data_imp <- subset(data_imp, select = -avg_steps)
head(data_imp)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```

We now use the imputed data set to rebuild the histogram of total daily steps. 



```r
total_daily_steps_imp <- with(data_imp, tapply(steps, date, sum))
ggplot(data=data.frame(total_daily_steps_imp), aes(x=total_daily_steps_imp)) +
  geom_histogram(binwidth=5000, center=5000/2, aes(col=I("white"))) +
  labs(title='Histogram of total daily steps (with imputation)', 
       x='Total daily steps') +
  theme(plot.title = element_text(hjust = 0.5))
```

![](PA1_template_files/figure-html/hist_imputed-1.png)<!-- -->

Comparing the histogram above with the one obtained previously, we can see that
imputation yields a more symmetrical and Gaussian-like distribution for the
total daily steps. This observation suggests that the strategy used earlier for
handling missing values may have caused the estimates of the total daily number
of steps to be biased towards zero. 

Finally, we compute the mean and median total number of daily steps from the
imputed data and compare them with the estimates obtained in the first part
of the assignment.


```r
imputed<-c(mean=mean(total_daily_steps_imp), median=median(total_daily_steps_imp))
original<-c(mean=mean(total_daily_steps), median=median(total_daily_steps))
rbind(imputed, original)
```

```
##              mean   median
## imputed  10766.19 10766.19
## original  9354.23 10395.00
```

From the table above, we see that both the mean and median estimates computed
from the imputed data are greater than those obtained without imputation.
Moreover, we can also see that the mean and median values are now identical,
which supports the observation that the distribution looks more symmetric.

## Are there differences in activity patterns between weekdays and weekends?

We begin by extending the dataset with a new factor variable ``day_type`` with
two levels - "weekday" and "weekend" - indicating whether a given date is a
weekday or weekend day.


```r
data_imp$day_type <- weekdays(data_imp$date, TRUE) %in% c('Sat','Sun')
data_imp$day_type <- factor(data_imp$day_type,
                            labels=c('FALSE'='weekday', 'TRUE'='weekend'))
head(data_imp)
```

```
##       steps       date interval day_type
## 1 1.7169811 2012-10-01        0  weekday
## 2 0.3396226 2012-10-01        5  weekday
## 3 0.1320755 2012-10-01       10  weekday
## 4 0.1509434 2012-10-01       15  weekday
## 5 0.0754717 2012-10-01       20  weekday
## 6 2.0943396 2012-10-01       25  weekday
```

Next, we use the ``day_type`` factor to build a panel plot containing the
average daily activity patterns for weekdays and weekends.


```r
avg_daily_activity_day_type <- {data_imp %>%
                                group_by(interval, day_type) %>%
                                summarise(steps=mean(steps))}
g <- ggplot(avg_daily_activity_day_type, aes(x=interval, y=steps)) + geom_line()
g <- g + facet_grid(day_type ~ .)
g <- g + labs(title='Average Daily Activity Patterns for Weekdays and Weekends') +
         theme(plot.title = element_text(hjust = 0.5))
g <- g + labs(y = 'Number of steps')
print(g)
```

![](PA1_template_files/figure-html/panel_plot_weekdays_vs_weekends-1.png)<!-- -->

From the plot above, we can see that there are noticeable differences between
the activity patterns for weekdays and weekends.
