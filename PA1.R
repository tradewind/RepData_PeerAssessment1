###
require(dplyr)
require(lattice)
require(ggplot2)
require(knitr)

###
unzip("activity.zip")
rawData <- read.csv("activity.csv")

###
totalStepsPerDay <- aggregate(x = rawData$steps, 
							  by = list(date = rawData$date), 
							  FUN = sum)

names(totalStepsPerDay) = c("date", "totalSteps")

meanSteps <- mean(totalStepsPerDay$totalSteps, na.rm = TRUE)
medianSteps <- median(totalStepsPerDay$totalSteps, na.rm = TRUE)

qplot(totalSteps, data = totalStepsPerDay, binwidth = 2500, 
	  xlab = "Number of Steps", main = "Histogram of Total Steps per Day")


###
avgStepsPerInt <- aggregate(x = rawData$steps, 
							by = list(interval = rawData$interval), 
							FUN = mean, na.rm = TRUE)

names(avgStepsPerInt) <- c("interval", "avgSteps")

xyplot(avgSteps ~ interval, data = avgStepsPerInt, type = "l", 
	   xlab = "Interval", ylab = "Number of Steps", main = "Average Steps per Interval")

maxInterval <- avgStepsPerInt[avgStepsPerInt$avgSteps == max(avgStepsPerInt$avgSteps), 1]


###
numNA <- sum(is.na(rawData$steps))

newData <- merge(rawData, avgStepsPerInt)

newData <- mutate(newData, steps = ifelse(is.na(steps), avgSteps, steps))

newTotalStepsPerDay <- aggregate(x = newData$steps, 
								 by = list(date = newData$date), 
								 FUN = sum)

names(newTotalStepsPerDay) = c("date", "totalSteps")

qplot(totalSteps, data = newTotalStepsPerDay, binwidth = 2500,
	  xlab = "Number of Steps", main = "Histogram of Total Steps per Day")

newMeanSteps <- mean(newTotalStepsPerDay$totalSteps, na.rm = TRUE)
newMedianSteps <- median(newTotalStepsPerDay$totalSteps, na.rm = TRUE)

comp <- matrix(c(meanSteps, medianSteps, newMeanSteps, newMedianSteps), ncol = 2)
colnames(comp) <- c('Original', 'New')
rownames(comp) <- c('Mean', 'Median')
comp.table <- as.table(comp)
comp.table


###
locale <- Sys.getlocale('LC_TIME')
Sys.setlocale('LC_TIME', 'C')

newData <- mutate(newData, weekday = 
				  	ifelse(weekdays(as.Date(date)) %in% c("Sunday", "Saturday"), FALSE, TRUE))

weekdayData <- newData[newData$weekday, ]
weekendData <- newData[!newData$weekday, ]

weekdayAvgSteps <- aggregate(x = weekdayData$steps, 
							 by = list(interval = weekdayData$interval), 
							 FUN = mean, na.rm = TRUE)

weekendAvgSteps <- aggregate(x = weekendData$steps, 
							 by = list(interval = weekendData$interval), 
							 FUN = mean, na.rm = TRUE)

weekendAvgSteps$weekday = "weekday"
weekdayAvgSteps$weekday = "weekend"

newAvgStepsPerInt <- rbind(weekdayAvgSteps, weekendAvgSteps)

names(newAvgStepsPerInt) = c("interval", "avgSteps", "weekday")

xyplot(avgSteps ~ interval | weekday, data = newAvgStepsPerInt, type = "l", layout = c(1, 2),
	   xlab = "Interval", ylab = "Number of Steps", main = "Average Steps per Interval")

Sys.setlocale('LC_TIME', locale)
