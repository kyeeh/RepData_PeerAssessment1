data <- read.csv("data/activity.csv", na.strings = 'NA',colClasses = c("numeric", "Date", "numeric"))
sbd <- aggregate(steps ~ date, data = data, FUN=sum,na.rm=TRUE)
sbi <- aggregate(steps ~ interval, data = data, FUN=mean,na.rm=TRUE)
data_filled <- adply(data, 1, function(x) if (is.na(x$steps)) {
  x$steps = round(sbi[sbi$interval == x$interval, 2])
  x
} else {
  x
})
sbd_filled <- aggregate(steps ~ date + interval, data = data_filled, FUN=sum)
data_weekend <- subset(sbd_filled, weekdays(as.Date(date)) %in% c("Saturday", "Sunday"))
data_weekday <- subset(sbd_filled, !weekdays(as.Date(date)) %in% c("Saturday", "Sunday"))
data_weekend <- aggregate(steps ~ interval, data_weekend, mean)
data_weekday <- aggregate(steps ~ interval, data_weekday, mean)