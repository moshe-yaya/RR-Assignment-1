Reproducible Research: Peer Assessment 1
========================================================

## Loading and preprocessing the data


```r
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
```
subset NA row

```r
data1 <-  data[!is.na(data$steps),]
```

## What is mean total number of steps taken per day?


```r
totalSteps <- aggregate( data1$steps , by = list(data1$date), FUN = "sum", simplify = TRUE)

#ploting the bar chart by ggplot2 sistem
g<- ggplot(totalSteps, aes(totalSteps$Group.1, totalSteps$x))
g + geom_bar(stat="identity",fill="green", colour="darkgreen") +
labs(title = "  Total number of steps per day") +
labs(x = "Days", y = " Steps ") +
theme(axis.text.x = element_text(angle = 45, hjust = 1))   
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

```r
meanSteps<-mean(totalSteps$x)
medianSteps<-median(totalSteps$x)

message('the average total steps ', meanSteps,'\n','the median total steps ',medianSteps)
```

```
## the average total steps 10766.1886792453
## the median total steps 10765
```

## What is the average daily activity pattern?

```r
averageInterval <- aggregate( data1$steps , by = list(data1$interval), FUN = "mean", simplify = TRUE)

g<- ggplot(averageInterval, aes(averageInterval$Group.1, averageInterval$x))
g + geom_bar(stat="identity",fill="red", colour="green") +
     labs(title = "  Average number of steps per 5 mineute interval") +
     labs(x = "interval", y = " Steps ") 
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

```r
    # theme(axis.text.x = element_text(angle = 45, hjust = 1))    

maximumSteps <- max(averageInterval$x)
maxInterval<-averageInterval[averageInterval$x == maximumSteps, ]
colnames(maxInterval) <- c('Inerval','AverageSteps')
print(maxInterval)
```

```
##     Inerval AverageSteps
## 104     835        206.2
```


## Imputing missing values

the number of NA val

```r
TotalNA<-sum(is.na(data$steps))
TotalNA
```

```
## [1] 2304
```


## Are there differences in activity patterns between weekdays and weekends?


```r
TotalNA<-sum(is.na(data$steps))



colnames(averageInterval) <- c('interval','steps')

for (i in 1:nrow(data)) {
     if (is.na(data$steps[i])) {
          data$steps[i]<- averageInterval[2][averageInterval$interval ==   data$interval[i],]
          
     }
}
          

totalStepsPlosNA <- aggregate( data$steps , by = list(data$date), FUN = "sum", simplify = TRUE)

meanStepsPlasNA<-mean(totalStepsPlosNA$x)
medianStepsPlasNA<-median(totalStepsPlosNA$x)

#ploting the bar chart by ggplot2 sistem
g<- ggplot(totalStepsPlosNA, aes(totalStepsPlosNA$Group.1, totalStepsPlosNA$x))
g + geom_bar(stat="identity",fill="red", colour="darkred") +
     labs(title = "  Total number of steps per day") +
     labs(x = "Days", y = " Steps ") +
     theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 

```r
message('the average total steps ', meanStepsPlasNA,'\n','the median total steps ',medianStepsPlasNA)
```

```
## the average total steps 10766.1886792453
## the median total steps 10766.1886792453
```



#Are there differences in activity patterns between weekdays and weekends?


```r
Sys.setlocale("LC_TIME", "English")
```

```
## [1] "English_United States.1252"
```

```r
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
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7.png) 












































