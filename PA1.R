setwd("/Users/nicklukianoff/Desktop/Reproducing")

#1. Load the activity data
fit_data <- read.csv("activity.csv")

#2. Process/transform the data (if necessary) into a format suitable for your analysis
fit_data <- subset(fit_data,!is.na(fit_data[,1]))

#1. Calculate the total number of steps taken per day
day_sum <- 0
daily_sum <- 0
for (a in 1:53) {
        for (b in 1:288) {
                day_sum <- day_sum + fit_data[a*b,1]
                }
        daily_sum[a] <- day_sum
        day_sum <- 0
}

#2. Histogram of the total number of steps taken each day
hist(daily_sum)

#3. Calculate and report the mean and median of the total number of steps taken per day
print(mean(daily_sum))
print(median(daily_sum))

#1. Plot mean of steps taken per time interval
fit_mean <- aggregate(fit_data[,1], list(fit_data[,3]), mean)
plot(fit_mean[,2],fit_mean[,1],type = "l")

#2. Which 5-minute interval contains the maximum number of steps
print(fit_mean[match(max(fit_mean[,2]),fit_mean[,2]),1])

#1. Calculate and report the total number of missing values in the dataset
print(nrow(subset(fit_data,is.na(fit_data[,1]))))

#2. Devise a strategy for filling in all of the missing values in the dataset.
#For each time interval, 37 is close to the average step amount overall.
#My strategy is to replace all NA values with the number 37.

#3. Replace NA values with 37, place into another data set
fit_data2 <- fit_data
fit_data2[is.na(fit_data2)] <- 37

#4. Histogram of the total number of steps taken each day
fit_total <- aggregate(fit_data2[,1], list(fit_data2[,2]), sum)
hist(fit_total[,2], main = "Histogram of Total Steps Taken", xlab = "Steps Taken")

#4. Calculate and report the mean and median total number of steps taken per day
print(mean(fit_total[,2]))
print(median(fit_total[,2]))

#4. The numbers differ slightly from the first part of this assignment,
#but only slightly, since the filler value that I used to replace the NA
#values was close to the actual average value.
#However, the total number of steps is far greater, since we are now
#counting values instead of NA's for several thousand rows.


#1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend”
fit_data2$Day <- weekdays(as.Date(fit_data2[,2]))
fit_data2$Day <- sub("Monday", "weekday", fit_data2$Day)
fit_data2$Day <- sub("Tuesday", "weekday", fit_data2$Day)
fit_data2$Day <- sub("Wednesday", "weekday", fit_data2$Day)
fit_data2$Day <- sub("Thursday", "weekday", fit_data2$Day)
fit_data2$Day <- sub("Friday", "weekday", fit_data2$Day)
fit_data2$Day <- sub("Saturday", "weekend", fit_data2$Day)
fit_data2$Day <- sub("Sunday", "weekend", fit_data2$Day)

#2. Make a panel plot containing a time series plot
fit_mean2 <- aggregate(fit_data2, list(fit_data2[,4],fit_data2[,3]), data = fit_data2$steps, mean)
fit_plot1 <- subset(fit_mean2, fit_mean2[,1] == "weekday")
fit_plot2 <- subset(fit_mean2, fit_mean2[,1] == "weekend")

#Set up 2 panels, and plot to the screen
par(mfcol = c(2, 1))
plot(fit_plot1[,2],fit_plot1[,3], xlab = "Interval", ylab = "Number of steps", main = "Weekday", type = "l")
plot(fit_plot2[,2],fit_plot2[,3], xlab = "Interval", ylab = "Number of steps", main = "Weekend", type = "l")

