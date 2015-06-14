library(lattice)
##calculate total-steps-by-day
sums <- as.numeric(sapply(split(act$steps, act$date), sum))

##histogram
histogram(sums[!is.na(sums)], breaks = 12, main = "Total Steps by Day",xlab = "Steps per Day")

##mean & median of total-steps-by-day
mean(sums, na.rm = TRUE)
median(sums, na.rm = TRUE)