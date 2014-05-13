Peer Assignment #1
========================================================

### Loading and preprocessing the data

```r
setwd("C:/Users/hwildasi/Desktop/Coursera")
active <- read.csv("activity.csv")
```


### What is the mean total number of steps taken per day?

```r
day <- tapply(active$steps, active$date, sum)
day_mean <- mean(day, na.rm = TRUE)
day_median <- median(day, na.rm = TRUE)
hist(day, main = "Histogram of Total Number of Daily Steps", xlab = "Total Number of Daily Steps")
```

![plot of chunk Calculate Mean Per Day](figure/Calculate_Mean_Per_Day.png) 


The mean number of steps per day is 1.0766 &times; 10<sup>4</sup> and the median number of steps per day is 10765.

### What is the average daily activity pattern?


```r
steps_int <- aggregate(steps ~ interval, active, mean, na.rm = TRUE)
plot(steps_int$interval, steps_int$steps, type = "l", main = "Plot of Mean Steps per 5-minute Interval", 
    xlab = "5-minute Interval", ylab = "Mean Number of Steps")
```

![plot of chunk Time Serids](figure/Time_Serids.png) 

```r
max <- steps_int[which.max(steps_int$steps), 1]
```


The 835 5-minute interval contains the maximum number of steps on average across all the days.

### Inputing missing values


```r
steps <- active$steps  #Create a vector of steps
miss <- is.na(steps)  #Find missing/non-missing
nmiss <- steps[miss]  #Only keep missing values
num_miss <- length(nmiss)  #Count number of elements in vector
```


A total of 2304 values are missing in the dataset.

For records with a missing value for the number of steps. Impute the value as the average for that time interval across all days and compute corresponding histogram.

```r
names(steps_int)[2] <- "steps_avg"  #rename steps to steps_avg
active_imp <- merge(active, steps_int, by = "interval")
active_imp$stepsi <- max(active_imp$steps, active_imp$steps_avg)
active_imp$stepsi <- ifelse(is.na(active_imp$steps), active_imp$steps_avg, active_imp$steps)
```



```r
dayi <- tapply(active_imp$stepsi, active_imp$date, sum)
dayi_mean <- mean(dayi, na.rm = TRUE)
dayi_median <- median(dayi, na.rm = TRUE)
hist(dayi, main = "Histogram of Total Number of Daily Steps - Missing Values Imputed", 
    xlab = "Total Number of Daily Steps")
```

![plot of chunk Histogram with imputed data](figure/Histogram_with_imputed_data.png) 

```r
comp_mean <- ifelse(dayi_mean == day_mean, "equal", ifelse(dayi_mean > day_mean, 
    "greater", "lesser"))
comp_median <- ifelse(dayi_median == day_median, "equal", ifelse(dayi_median > 
    day_median, "greater", "lesser"))
diff_mean <- abs(day_mean - dayi_mean)
diff_median <- abs(day_median - dayi_median)
diff_sum <- ifelse(diff_mean < 10 & diff_median < 10, "insignificant (<10 steps per day)", 
    ifelse(diff_mean < 500 & diff_median < 500, "minimal (<500 steps per day)", 
        "Significant (>500 steps per day)"))
```


The mean number of steps per day using imputed values is 1.0766 &times; 10<sup>4</sup> and the median number of steps per day is 1.0766 &times; 10<sup>4</sup>. In comparison to the values generated excluding missing values the mean is equal and the median is greater. The change in the mean and median values is insignificant (<10 steps per day).

### Are there differences in activity patterns between weekdays and weekends?

```r
# Create Factor Levels for Weekday and Weekend
active_imp$daten <- as.Date(active_imp$date, "%Y-%m-%d")
active_imp$weekday <- weekdays(active_imp$daten)
active_imp$weekdayf <- ifelse(active_imp$weekday == "Saturday" | active_imp$weekday == 
    "Sunday", "weekend", "weekday")

# Sum across weekday/weekend and plot data
stepsi_int <- aggregate(stepsi ~ interval * weekdayf, active_imp, mean, na.rm = TRUE)
stepsi_int$interval <- as.numeric(stepsi_int$interval)
library(lattice)
xyplot(stepsi_int$stepsi ~ stepsi_int$interval | stepsi_int$weekdayf, type = "l", 
    main = "Plot of Mean Steps per 5-minute Interval - Weekend vs. Weekday", 
    xlab = "5-minute Interval", ylab = "Mean Number of Steps", layout = c(1, 
        2))
```

![plot of chunk Weekdays](figure/Weekdays.png) 


