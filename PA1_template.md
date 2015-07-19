#Peer Assessment 1
library(ggplot2)
library(scales)
library(Hmisc)

#===Load the data
setwd("C:/Users/wcai/Documents/My Files/PBR Cohort 4")
activity_data <- read.csv("activity.csv")

#===Process/transforms the data (if necessary) into a format suitable for your analysis

#Extracts time
time <- formatC(activity_data$interval/100, 2, format = "f")
#Reformatting to time to hrs mins sec
activity_data$date.time <- as.POSIXct(paste(activity_data$date, time), format = "%Y-%m-%d %H.%M", 
                                 tz = "GMT")
activity_data$time <- format(activity_data$date.time, format = "%H:%M:%S")
activity_data$time <- as.POSIXct(activity_data$time, format = "%H:%M:%S")

#===What is mean total number of steps taken per day?

#Calculates the total number of steps taken per day
Steps_subset <- tapply(activity_data$steps, activity_data$date, sum, na.rm = TRUE)

#Makes a histogram of the total number of steps taken each day
qplot(Steps_subset, xlab = "Total steps", ylab = "Observations")

#Calculates and report the mean and median of the total number of steps taken per day
mean(Steps_subset)
##output: [1] 9354.23
median(Steps_subset)
##output: 10395

#===What is the average daily activity pattern?

#Creates daily pattern subset of 5 min intervals
avg_steps <- tapply(activity_data$steps, activity_data$time, mean, na.rm = TRUE)
daily_pattern <- data.frame(time = as.POSIXct(names(avg_steps)), avg_steps = avg_steps)

#Makes a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
ggplot(daily_pattern, aes(time, avg_steps)) + geom_line() + xlab("Time of day") + 
  ylab("Mean number of steps") + scale_x_datetime(labels = date_format(format = "%H:%M"))

#Which five minute interval has the highest mean number of steps?
format(daily_pattern[which.max(daily_pattern$avg_steps), "time"], format = "%H:%M")
##output: [1] "08:35"

#===Imputing missing values

#Calculates and reports the total number of missing values in the datase
summary(activity_data$steps)
##output: NA's 2304

#Use the mean for that day, or the mean for that 5-minute interval
#Creates new NA imputed data set
activity_imputeNA <- activity_data
activity_imputeNA$steps <- with(activity_imputeNA, impute(steps, mean))

#Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.
qplot(activity_imputeNA, xlab = "Total steps", ylab = "Observations")

#===Are there differences in activity patterns between weekdays and weekends?

#Creates function for testing days of the week
day_week <- function(date) {
  if (weekdays(date) %in% c("Saturday", "Sunday")) {
    return("weekend")
  } else {
    return("weekday")
  }
}

#Creates a new factor variable in the dataset with two levels - "weekday" and "weekend"
day_week <- sapply(activity_imputeNA$date.time, day_week)
activity_imputeNA$day_week <- as.factor(day_week)

#Make a panel plot containing a time series plot
avg_steps_impute <- tapply(activity_imputeNA$steps, 
                           interaction(activity_imputeNA$time,
                                       activity_imputeNA$day_week), mean, na.rm = TRUE)
day_week_step <- data.frame(time = as.POSIXct(names(avg_steps_impute)), avg_steps_impute = avg_steps_impute, 
                            day_week = as.factor(c(rep("weekday", 288), rep("weekend", 288))))

ggplot(day_week_step, aes(time, avg_steps_impute)) + geom_line() + xlab("Time of day") + 
  ylab("Mean number of steps") + scale_x_datetime(labels = date_format(format = "%H:%M")) + 
  facet_grid(. ~ day_week)
