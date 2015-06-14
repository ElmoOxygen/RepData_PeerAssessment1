##means of steps per interval over all days, make timeplot
steps <- sapply(split(act$steps, act$interval), mean, na.rm = TRUE)
steps<-data.frame(unique(act$interval), steps)
colnames(steps) <- c("interval", "steps")
ggplot(steps, aes(x = interval, y = steps), "l", xlab = "interval")+geom_line()

##max interval
stepsmax <- max(steps$steps)
steps$interval[grep(stepsmax, steps$steps)]