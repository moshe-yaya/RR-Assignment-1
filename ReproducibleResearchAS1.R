library(plyr)
library(ggplot2)
library(knitr)
url<-'http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip'
temp <- tempfile()
download.file(url,temp)
#method="curl
activity <- unz(temp, "activity.csv")
data <- read.csv(activity)
unlink(temp)

data1 <-  data[!is.na(data$steps),]


# -- What is  total number of steps taken per day?------------------------------------
#1
totalSteps <- aggregate( data1$steps , by = list(data1$date), FUN = "sum", simplify = TRUE)

#ploting the bar chart by ggplot2 sistem
g<- ggplot(totalSteps, aes(totalSteps$Group.1, totalSteps$x))
g + geom_bar(stat="identity",fill="green", colour="darkgreen") +
labs(title = "  Total number of steps per day") +
labs(x = "Days", y = " Steps ") +
theme(axis.text.x = element_text(angle = 45, hjust = 1))      

#2
meanSteps<-mean(totalSteps$x)
medianSteps<-median(totalSteps$x)


# -- What is the average daily activity pattern?-------------------------------------
averageInterval <- aggregate( data1$steps , by = list(data1$interval), FUN = "mean", simplify = TRUE)

g<- ggplot(averageInterval, aes(averageInterval$Group.1, averageInterval$x))
g + geom_bar(stat="identity",fill="red", colour="green") +
     labs(title = "  Average number of steps per 5 mineute interval") +
     labs(x = "interval", y = " Steps ") 
    # theme(axis.text.x = element_text(angle = 45, hjust = 1))    

maximumSteps <- max(averageInterval$x)
maxInterval<-averageInterval[averageInterval$x == maximumSteps, ]

colnames(maxInterval) <- c('Inerval','AverageSteps')


#--Imputing missing values  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

TotalNA<-sum(is.na(data$steps))



colnames(averageInterval) <- c('interval','steps')

for (i in 1:nrow(data)) {
     if (is.na(data$steps[i])) {
          data$steps[i]<- averageInterval[2][averageInterval$interval ==   data$interval[i],]
          
     }
}
          

totalStepsPlosNA <- aggregate( data$steps , by = list(data$date), FUN = "sum", simplify = TRUE)

#ploting the bar chart by ggplot2 sistem
g<- ggplot(totalStepsPlosNA, aes(totalStepsPlosNA$Group.1, totalStepsPlosNA$x))
g + geom_bar(stat="identity",fill="red", colour="darkred") +
     labs(title = "  Total number of steps per day") +
     labs(x = "Days", y = " Steps ") +
     theme(axis.text.x = element_text(angle = 45, hjust = 1))      


#2
meanStepsPlasNA<-mean(totalStepsPlosNA$x)
medianStepsPlasNA<-median(totalStepsPlosNA$x)




#Are there differences in activity patterns between weekdays and weekends?

Sys.setlocale("LC_TIME", "English")

weekday<-rep('weekday',nrow(data))
data<-cbind(data,weekday)

data$weekday<-as.character(data$weekday)

for (i in 1:nrow(data)) {
     if (weekdays(as.Date(data$date[i]))=='Saturday' | weekdays(as.Date(data$date[i]))=='Sunday'){
          data$weekday[i] <-'weekend'
     }
}
          
data$weekday<-as.factor(data$weekday)



meanSteps <- aggregate(  data$steps , by = list(interval=data$interval,weekdays=data$weekday), FUN = "mean", simplify = TRUE)



g<- ggplot(meanSteps, aes(interval, meanSteps$x))
g + 
labs(title = "  Mean number of steps per interval group by Weekdays") +
labs(x = "intervals", y = " Steps ") +
geom_point(stat="identity",fill="red", colour="darkred")+
facet_grid( weekdays ~ . ) +
geom_line(stat = "identity")











































