barriom <- subset(tempMonth, INDICATIVO == "2017Y")
windows(12, 8)
par(mfrow=c(3,4))
myMon <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
for(i in 1:12){
  plot(Tmean_avg ~ year, data = subset(barriom, month == i), main = myMon[i],
       ylab = 'T (C)', xlab = ' ', type = 'l', ylim = c(-5, 30))
  lines(Tmax_avg ~ year, data = subset(barriom, month == i), lty = 2)
  lines(Tmin_avg ~ year, data = subset(barriom, month == i), lty = 2)
}
