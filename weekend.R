library(dplyr)
library(gridExtra)

## add 'day' column for day of the week
actfill <- mutate(actfill, day = weekdays(as.Date(actfill$date)))

##add week column to data frame
w <- vector()
for (i in 1:dim(actfill)[1]) {
        if (actfill[i,4] == "Sunday" | actfill[i,4] == "Saturday") {
                w <- c(w, "weekend")
        }
        else {w <- c(w, "weekday")}
}
actfill <- mutate(actfill, week = as.factor(w))

wendavg<-as.numeric(with(subset(actfill, week == "weekend"), 
                         lapply(split(steps, interval), mean)))
wdayavg<-as.numeric(with(subset(actfill, week == "weekday"), 
                         lapply(split(steps, interval), mean)))

wend<-data.frame(rep(1,288))
wend$steps<-wendavg
wend$interval<-unique(actfill$interval)
wend$week<-as.factor("weekend")
wend<-wend[,2:4]
wday<-data.frame(rep(1,288))
wday$steps<-wdayavg
wday$interval<-unique(actfill$interval)
wday$week<-as.factor("weekday")
wday<-wday[,2:4]
week<-rbind(wday,wend)

xyplot(steps ~ interval | week, week, type = "l", layout = c(1,2))
