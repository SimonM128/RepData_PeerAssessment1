Reproducible Research: Peer Assessment 1
============================================

```{r setoptions, echo=TRUE}
library(knitr)
opts_chunk$set(echo = TRUE)
```

## Loading and pre-processing the data

The data (`activity.zip`) is in the forked repository. First, it is unzipped, then read in, and the `date` column is converted to the `Date` class.

```{r loaddata}
if (!file.exists("activity.csv")) {
  unzip("activity.zip")
}
activity <- read.csv("activity.csv", stringsAsFactors = FALSE)
activity$date <- as.Date(activity$date, format = "%Y-%m-%d")

str(activity)
summary(activity)
```

## What is the mean total number of steps taken per day?

*Missing values are ignored for this part*

```{r totalsteps}
total_steps <- aggregate(steps ~ date, data = activity, FUN = sum, na.rm = TRUE)
head(total_steps)
```

Histogram of the total steps taken each day:

```{r hist_original}
hist(total_steps$steps,
     breaks = 20,
     col = "chocolate",
     main = "Total number of steps taken per day",
     xlab = "Steps per day")
```

Mean and median of the total steps taken per day:

```{r mean_median}
mean_steps   <- mean(total_steps$steps)
median_steps <- median(total_steps$steps)
mean_steps
median_steps
```

The mean is `r format(mean_steps, big.mark = ",")` steps per day and the median is `r format(median_steps, big.mark = ",")` steps per day.

## What is the average daily activity pattern?

```{r interval_pattern}
interval_avg <- aggregate(steps ~ interval, data = activity, FUN = mean, na.rm = TRUE)

plot(interval_avg$interval, interval_avg$steps,
     type = "l",
     col = "darkred",
     main = "Average daily activity pattern",
     xlab = "5-minute interval",
     ylab = "Average number of steps (across all days)")
```

The 5-minute interval that contains the maximum average number of steps:

```{r max_interval}
max_row <- interval_avg[which.max(interval_avg$steps), ]
max_row
```

Interval `r max_row$interval` has the highest average, at about `r round(max_row$steps, 1)` steps.

## Imputing missing values

Total number of missing values (rows with `NA`) in the dataset:

```{r count_na}
sum(is.na(activity$steps))
```

Each missing value is replaced with the mean number of steps for that same 5-minute interval, averaged across all days. A new dataset, `activity_imputed`, is created with all `NA`s filled in.

```{r impute}
activity_imputed <- activity
na_idx <- is.na(activity_imputed$steps)

# look up the interval mean for each missing row
activity_imputed$steps[na_idx] <- interval_avg$steps[
        match(activity_imputed$interval[na_idx], interval_avg$interval)]

# check: no missing values remain
sum(is.na(activity_imputed$steps))
```

Histogram of the total number of steps per day after imputing NA values:

```{r hist_imputed}
daily_steps_imputed <- aggregate(steps ~ date, data = activity_imputed, FUN = sum)

hist(daily_steps_imputed$steps,
     breaks = 20,
     col = "cadetblue",
     main = "Total number of steps taken per day (imputed data)",
     xlab = "Steps per day")
```

```{r mean_median_imputed}
mean_imputed   <- mean(daily_steps_imputed$steps)
median_imputed <- median(daily_steps_imputed$steps)

data.frame(
  dataset = c("Original", "Imputed"),
  mean    = c(mean_steps, mean_imputed),
  median  = c(median_steps, median_imputed)
)
```

### Do these values differ from the first part?

The mean is unchanged after imputation (`r format(mean_steps, nsmall = 2)` vs `r format(mean_imputed, nsmall = 2)`). That makes sense, because the missing values fall on whole days, and each of those days was filled using the average daily pattern, so every imputed day adds up to roughly the overall mean. The median shifts slightly (`r format(median_steps, nsmall = 2)` to `r format(median_imputed, nsmall = 2)`) and equals the mean, since several days now have a total equal to it.

### Impact of imputation

Imputation adds more days to the histogram, and because every imputed day lands close to the mean daily total, the central bin gets taller. The estimates of the daily totals are hardly biased by this approach, but the variability in the data is understated.

## Are there differences in activity patterns between weekdays and weekends?

Using the dataset with imputed values, a new factor variable `day_type` is then created with levels "weekday" and "weekend".

```{r weekday_factor}
day_names <- weekdays(activity_imputed$date)
activity_imputed$day_type <- factor(
  ifelse(day_names %in% c("Saturday", "Sunday"), "weekend", "weekday"),
  levels = c("weekday", "weekend")
)

table(activity_imputed$day_type)
```

Panel plot of the average number of steps per 5-minute interval, for weekdays and weekends:

```{r panel_plot, fig.height=7}
interval_daytype <- aggregate(steps ~ interval + day_type,
                              data = activity_imputed, FUN = mean)

library(lattice)
xyplot(steps ~ interval | day_type,
       data = interval_daytype,
       type = "l",
       layout = c(1, 2),
       xlab = "Interval",
       ylab = "Number of steps")
```

Weekdays show a clear spike in the morning, followed by lower activity for the remainder of the day. Weekend activity begins later and is distributed more evenly, with higher levels from midday into the evening.
