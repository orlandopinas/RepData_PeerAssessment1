#Downloand data 
if(!file.exists(".data")){dir.create("./data")}
Url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(Url, destfile = "./data/activity.zip", method = "curl")

#Unzip file 
unzip("activity.zip")

#Read and load CSV file into a data frame
activity <- read.csv("activity.csv", header = TRUE)
activity$date <- as.Date(activity$date)


#Load libraries
library(ggplot2)
library(dplyr)


#Part 1---What is mean total number of steps taken per day?

##1.Calculate the total number of steps taken per day
stepsperday <- activity %>% 
        group_by(date) %>% 
        summarize(sumsteps = sum(steps, na.rm = TRUE))
head(steps)

##2.Make a histogram of the total number of steps taken each day
hist(stepsperday$sumsteps, 
     main = "Histogram of daily steps", 
     col = "green", 
     xlab = "Steps",
     ylim = c(0,30))

##3.Calculate and report the mean and median of the total number of steps taken per day
mean.na <- round(mean(stepsperday$sumsteps, na.rm = TRUE), digits = 2)
median.na <- median(stepsperday$sumsteps, na.rm = TRUE)

print(paste("The mean is: ", mean.na))

print(paste("The median is: ", median.na))

#Part 2---What is the average daily activity pattern?

##1. Make a time series plot (i.e.type = "l") of the 5-minute interval (x-axis)
#and the average number of steps taken, averaged across all days (y-axis)

stepsperinterval <- activity %>% 
        group_by(interval) %>% 
        summarize(meansteps = mean(steps, na.rm = TRUE))

plot(stepsperinterval$meansteps ~ stepsperinterval$interval, 
     col = "red", 
     type = "l", 
     xlab = "Intervals(5 minutes)", 
     ylab = "Average number of steps", 
     main = "Steps by Time interval")

##2. Which 5-minute interval, on average across all the days in the dataset, 

print(paste("Average steps for that interval: ", max(stepsperinterval$meansteps)))

print(paste("Interval containing the most steps on average: ", 
            stepsperinterval$interval[which.max(stepsperinterval$meansteps)])) 

#Part 3---Imputing missing values

##1. Calculate and report the total number of missing values in the dataset
print(paste("The total number of rows with NA is: ", sum(is.na(activity$steps))))

##2. Devise a strategy for filling in all of the missing values in the dataset.
#The strategy does not need to be sophisticated.

"The NA values from the activity data frame are replaced with the data from the averages 
of each 5 min interval that we calculated in previous steps."
"First, in the activity data frame we will look for the rows that contain NA values,
then according to the time interval (5min) to which they correspond, 
they will be replaced by the average values calculated in the stepsperinterval data frame."

##3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

activitynoNA <- activity 
for(i in 1:nrow(activity)){
        if(is.na(activity$steps[i])){
                activitynoNA$steps[i] <- stepsperinterval$meansteps[activitynoNA$interval[i]
                                                                     == stepsperinterval$interval]
        }
}

##4. Make a histogram of the total number of steps taken each day and Calculate and report
#the mean and median total number of steps taken per day. Do these values differ from the
#estimates from the first part of the assignment? What is the impact of imputing missing data
#on the estimates of the total daily number of steps?

stepsperdaynoNA <- activitynoNA %>% 
        group_by(date) %>% 
        summarize(sumsteps = sum(steps, na.rm = TRUE))

hist(stepsperdaynoNA$sumsteps, 
     main = "Histogram of Daily steps", 
     col = "green", 
     xlab = "Steps")

meannoNA <- mean(stepsperdaynoNA$sumsteps)
mediannoNA <- median(stepsperdaynoNA$sumsteps)
print(paste("The mean is: ", meannoNA))
print(paste("The median is: ", mediannoNA))

#Part 4---Are there differences in activity patterns between weekdays and weekends?

##1. Create a new factor variable in the dataset with two levels - "weekday" and
#"weekend" indicating whether a given date is a weekday or weekend day.
activityw <- activitynoNA
activityw$date <- as.Date(activityw$date)
activityw$day <-ifelse(weekdays(activityw$date) %in% c("sábado", "domingo"), 
                       "weekend", "weekday")
#2. Make a panel plot containing a time series plot (i.e. type="l") of the 5-minute
#interval (x-axis) and the average number of steps taken, averaged across all weekday days
#or weekend days (y-axis).
activityweekday <- filter(activityw, activityw$day == "weekday")
activityweekend <- filter(activityw, activityw$day == "weekend")

activityweekday <- activityweekday %>%
        group_by(interval) %>%
        summarize(steps = mean(steps))
activityweekday$day <- "weekday"

activityweekend <- activityweekend %>% 
        group_by(interval) %>%
        summarize(steps = mean(steps))
activityweekend$day <- "weekend"

weekdayweekend <- rbind(activityweekday, activityweekend)
weekdayweekend$day <- as.factor(weekdayweekend$day)

g <- ggplot(weekdayweekend, aes(interval, steps))
g + geom_line() + facet_grid(day~.) + labs(y = "Average Number of Steps") + labs(x ="Interval") +
        ggtitle("Average number of steps", subtitle = "Weekday vs Weekend")



