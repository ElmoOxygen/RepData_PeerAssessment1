##total NAs in activityraw
sum(is.na(act$steps))

##fill NAs with mean associated with integer
actfill <- act
for (i in 1:(dim(actfill)[1]) - 1){
        if (is.na(actfill[i+1,1])){
                actfill[i+1,1] <- as.numeric(steps[(i%%288)+1,2])}}

##calculate total-steps-by-day
sumsfill <- as.numeric(sapply(split(actfill$steps, actfill$date), sum))

##histogram
histogram(sumsfill[!is.na(sumsfill)], breaks = 12, main = NULL,xlab = "Steps per Day")

##mean & median of total-steps-by-day
mean(sumsfill, na.rm = TRUE)
median(sumsfill, na.rm = TRUE)