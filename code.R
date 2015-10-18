# read in the data ------------------------------------------------------------
data <- read.csv("activity.csv", header = TRUE, sep = ",")
head(data)

# -----------------------------------------------------------------------------
# What is the mean total number of steps taken per day?
# plot histogram, calculate mean, median
sumsteps <- tapply(data$steps, data$date, sum, na.rm = TRUE)
hist(sumsteps, main = "Histogram of Number of Steps Take Each Day",
xlab = "Number of Steps Taken", ylab = "Count of Days", 
ylim = c(0,30))

mean_steps <- round(mean(sumsteps), digits = 0)
median_steps <- round(median(sumsteps), digits = 0)
print(paste("Mean number of steps per day:", mean_steps))
print(paste("Median number of steps per day:", median_steps))


# -----------------------------------------------------------------------------
# plot time graph
num_days <- length(unique(data$date))
#timesteps <- tapply(data$steps, interval_mins, sum, na.rm = TRUE)
timesteps <- tapply(data$steps, data$interval, sum, na.rm = TRUE)
timesteps <- timesteps / num_days
print(head(timesteps, 30))
plot(names(timesteps), timesteps, type = "l", 
     xlab = "Time Interval in 5-Minute Steps Across the Day", 
     ylab = "Average Number of Steps", ylim = c(0, 200),
     main = "Steps per 5-Minute Interval Averaged Across All Days")

# -----------------------------------------------------------------------------
# finding the time slot with maximum steps
print(max(timesteps))
max_value <- timesteps == max(timesteps)
max_time <- names(timesteps[max_value])
print(max_time)

# -----------------------------------------------------------------------------
# Imputing missing values
imputed_data <- data
for(i in 1:nrow(data)){
  if(is.na(imputed_data$steps[i])){
    row_name <- as.character(imputed_data$interval[i])
    imputed_value <- timesteps[row_name]
    imputed_data$steps[i] <- imputed_value
    }
  }

# -----------------------------------------------------------------------------
# Plot histogram with imputed data set
imputed_sumsteps <- tapply(imputed_data$steps, imputed_data$date, sum)
hist(imputed_sumsteps, main = "Histogram of Number of Steps Take Each Day (with imputed data values)",
     xlab = "Number of Steps Taken", ylab = "Count of Days", 
     ylim = c(0,30))
  
# -----------------------------------------------------------------------------
# calculated and compare means, medians
mean_imputed_steps <- round(mean(imputed_sumsteps), digits = 0)
median_imputed_steps <- round(median(imputed_sumsteps), digits = 0)
print(paste("Mean number of steps per day:", mean_imputed_steps))
print(paste("Median number of steps per day:", median_imputed_steps))

# -----------------------------------------------------------------------------
# Are there differences in activity patterns between weekdays and weekends?

# find which records are for weekends and how namy weekend/week days there are
data_weekdays <- weekdays(as.Date(imputed_data$date))
is_weekend <- data_weekdays == "Saturday" | data_weekdays == "Sunday"
num_weekend_days <- length(unique(imputed_data$date[is_weekend]))
num_weekdays <- length(unique(imputed_data$date[!is_weekend]))

# append a factored column to imputed_data indicating weekend or weekday
factor_weekend <- as.character(c())
factor_weekend[is_weekend] <- "weekend"
factor_weekend[!is_weekend] <- "weekday"
imputed_data <- mutate(imputed_data, factor_weekend) 
imputed_data$factor_weekend <- factor(imputed_data$factor_weekend)

# create an aggregate - steps by day, by weekend/weekday, averaged by the 
imputed_data$interval <- factor(imputed_data$interval)
plot_data <- aggregate(imputed_data$steps, list(imputed_data$interval, imputed_data$factor_weekend), FUN = "sum")
names(plot_data) <- c("interval","factor_weekend", "steps")
for(i in 1:nrow(plot_data)){
  if(plot_data$factor_weekend[i] == "weekday"){plot_data$steps[i] <- plot_data$steps[i]/num_weekdays}
  else {plot_data$steps[i] <- plot_data$steps[i]/num_weekend_days}
}

panelplot <- lattice::xyplot(steps ~ interval| factor_weekend, data = plot_data, type = 'l', col = 'black',
                             xlab = "Interval",
                             ylab = "Number of steps")
print(panelplot)


#xyplot(