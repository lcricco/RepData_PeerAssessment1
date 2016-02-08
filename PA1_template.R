#Loading and preprocessing the data
setwd()
filepath<-file.path(getwd(),"activity.zip")
dataset<-read.csv(unz(filepath,"activity.csv"),sep=",")
#dataset<-na.omit(dataset) /* OPTIONAL */
dataset$date<as.Date(dataset$date,"%Y-%m-%d")

#What is mean total number of steps taken per day?

totalNum<-sum(is.na(dataset$steps))
mean_totalNum<-mean(dataset$steps,na.rm=TRUE)
median_totalNum<-median(dataset$steps,na.rm=TRUE)

=============================================
#load data
filepath<-file.path(getwd(), "activity.zip")
dataset<-read.csv(unz(filepath, "activity.csv"), sep=",")

#Process/transform the data (if necessary) into a format suitable for your analysis
dataset$date<-as.Date(dataset$date)

library(ggplot2)

#What is mean total number of steps taken per day?
#Make a histogram of the total number of steps taken each day
stepsByDay<-tapply(dataset$step, dataset$date,sum, na.rm=TRUE)
qplot(stepsByDay, xlab="Steps per day", main="Histogram of the total number steps taken per day", ylim=c(0,20), binwidth=2500)

#Calculate and report the mean and median total number of steps taken per day
meanSteps<-mean(stepsByDay)
medianSteps<-median(stepsByDay)

#What is the average daily activity pattern?
#Make a time series plot
averages<-aggregate(list(meanSteps=dataset$steps), list(interval=dataset$interval), mean, na.rm=TRUE)
#tapply(dataset$step, dataset$interval, mean, na.rm=TRUE)
ggplot(averages, aes(interval, meanSteps))+geom_line(color="steelblue")+labs(title="Time series plot", x="5-minute interval", y="Average number of steps taken")

#Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
maxSteps<-which.max(averages$meanSteps)
maxInterval<-averages[maxSteps,1]

#Imputing missing values
#Calculate and report the total number of missing values in the dataset
missingValue<-sum(is.na(dataset$step))

#Devise a strategy for filling in all of the missing values in the dataset.
#Create a new dataset that is equal to the original dataset but with the missing data filled in
newDataset<-dataset
newDataset$steps<-impute(newDataset$steps, mean)

#Make a histogram of the total number of steps taken each day
numStepsByDay<-tapply(newDataset$steps, newDataset$date, sum)
qplot(numStepByDay, xlab="Total steps per day", main="Total number steps taken per day", binwidth=800)
qplot(numStepsByDay, xlab="Steps per day", main="Histogram of the total number steps taken per day", ylim=c(0,20), binwidth=1500)

#Calculate and report the mean and median total number of steps taken per day
newMeanSteps<-mean(numStepsByDay)
newMedianSteps<-median(numStepsByDay)

#Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
newDataset$day<-weekdays(as.POSIXct(newDataset$date))
newDataset <- cbind(newDataset, daytype=ifelse(dataset$day == "saturday" | newDataset$day == "sunday", "weekend", "weekday"))

#Make a panel plot containing a time series plot
newAverages<-aggregate(steps ~ interval + datetype, newDataset, mean)
ggplot(newAverages, aes(interval, steps)) + geom_line() + facet_grid(daytpe ~ .) + xlab("5-minute interval") +  ylab("avarage number of steps")










