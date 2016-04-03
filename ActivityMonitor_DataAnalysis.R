library(dplyr)
library(ggplot2)
library(mice)
library(lubridate)

rawData <- read.csv("activity.csv",header=TRUE, sep=",")
rawData <- mutate(rawData, date = as.Date(date))
TotalSteps <- group_by(rawData, date) %>% 
              summarize(StepsPerDay = sum(steps)) %>%
              mutate(DateRecorded = as.Date(date)) %>%
              select(-(date))
# invoking graphical device png
png("plot1.png", width=480, height=480)
with(TotalSteps,plot(DateRecorded,StepsPerDay,type="h",lwd=5,col="green"))
title(main="Histogram - Total Steps Per Day")
dev.off()


#Mean of Total Number of Steps taken per day
MeanSteps <- mean(TotalSteps$StepsPerDay,na.rm=TRUE)

# Median of total number of steps taken per day
MedainSteps <- median(TotalSteps$StepsPerDay,na.rm=TRUE)

# making a time series pattern of the 5-minute interval (x-axis) and the average number
# of steps taken, across all days (y-axis)

# First creating a data-frame AverageSteps
png("plot2.png", width=480, height=480) # invoking graphics device
AverageSteps <- group_by(rawData, interval) %>% summarize(MeanSteps = mean(steps,na.rm=TRUE))
ggplot(AverageSteps, aes(interval,MeanSteps)) + geom_line()
dev.off()

# Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

filter(AverageSteps, MeanSteps == max(MeanSteps))

# Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with 𝙽𝙰s)

TotalNA <- sum(is.na(rawData))

### Devise a strategy for filling in all of the missing values in the dataset. 
### The strategy does not need to be sophisticated. For example, you could use the mean/median for ### that day, or the mean for that 5-minute interval, etc.

rawDataCopy <- rawData
pos <- which(is.na(rawDataCopy$steps))
mean_vec <- rep(mean(rawDataCopy$steps, na.rm=TRUE), times=length(pos))
rawDataCopy[pos, "steps"] <- mean_vec
AggregateData <- aggregate(rawDataCopy$steps, by=list(rawData$date), FUN=sum)
names(AggregateData) <- c("DateRecorded", "TotalStepsPerDay")

### Make a histogrm of the total number of steps taken each day after imputing missed data

png("plot3.png", width=480, height=480) ### initiate graphics device
with(AggregateData,plot(as.Date(DateRecorded),TotalStepsPerDay,type="h",lwd=5,col="green"))
title(main="Histogram - Total Steps Per Day after imputing data")
dev.off()

### Mean of total steps after imputing data
imputedMeanSteps <- mean(AggregateData$TotalStepsPerDay) 

### Meadin total number of steps after imputing data
Median_Steps <- median(AggregateData$TotalStepsPerDay) 


## Are there differences in activity patterns between weekdays and weekends?

### step 1: Creating a factor variable in the dataset with two levels - "weekday" and "weekend"

completedData <- mutate(rawDataCopy, DateRecorded = as.Date(date)) %>%
  select(-(date)) 
completedData$Day <- weekdays(completedData$DateRecorded,abbreviate=FALSE) 
completedData <- transform(completedData,Day = ifelse(Day %in% c('Monday','Tuesday','Wednesday','Thursday','Friday'),'Weekday','Weekend'))

### Make a panel plot containing a time series plot of the 5-minute interval(x-axis) and the
### average number of steps taken, averaged across all weekday days and weekend days.
png("plot4.png", width=1200, height=800)
MeanSteps<-aggregate(steps~interval+Day, completedData,mean)
ggplot(MeanSteps, aes(interval,steps)) + 
  facet_wrap(~Day,ncol=1) + 
  geom_line(color = "steelblue") +
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 
dev.off()
