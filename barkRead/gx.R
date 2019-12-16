gx <- read.csv('barkData/gx20191009.csv')
gx$DateTime <- lubridate::ymd_hms(as.character(paste0('2019-10-09 ', gx$HHMMSS)))
gx$outOfWater <- lubridate::ymd_hms(as.character(paste0('2019-10-09 ', gx$intitialTime)))
gx$timeMin <- as.numeric(gx$DateTime - gx$outOfWater)
gx <- doBy::orderBy(~ Obs + DateTime, gx)
gx$timeInterval <- as.numeric(gx$DateTime - dplyr::lag(gx$DateTime))
gx$dCond <- (gx$Cond - dplyr::lag(gx$Cond, 1))/(gx$timeInterval*60)
gx$dPhoto <- (gx$Photo - dplyr::lag(gx$Photo, 1))/(gx$timeInterval*60)
gx[which(gx$timeInterval >= 15 | gx$timeInterval < 0), c('dCond', 'dPhoto')] <- NA
windows(12,8)
par(mfrow=c(2,2), mar= c(5, 6, 0.5, 0.5))
plot(gx$Photo ~ gx$timeMin, pch = 19, col = as.factor(gx$Obs), cex.lab = 1.8,
     ylab = expression(italic(A)[net]~(mu*mol~m^-2~s^-1)), xlab = ' ')
legend('topright', legend = c('Control', 'Bandage'), pch = 19,
       col = c('black', 'red'), bty = 'n', cex = 1.8)
plot(gx$Cond*1000 ~ gx$timeMin, pch = 19, col = as.factor(gx$Obs), cex.lab = 1.8,
     ylab = expression(italic(g)[s]~(mmol~m^-2~s^-1)), xlab = ' ')
plot(gx$dPhoto ~ gx$timeMin, pch = 19, col = as.factor(gx$Obs), cex.lab = 1.8,
     ylab = expression(d(italic(A)[net])~(mu*mol~m^-2~s^-2)), xlab = ' ')
plot(gx$dCond*1000 ~ gx$timeMin, pch = 19, col = as.factor(gx$Obs), cex.lab = 1.8,
     ylab = expression(d(italic(g)[s])~(mmol~m^-2~s^-2)), xlab = ' ')
summary(lm(dPhoto ~ Obs * round, data = gx))
summary(lm(dCond ~ Obs * round, data = gx))

ctrl <- subset(gx, Obs == 'branch1')
bnd <- subset(gx, Obs == 'branch2')
windows(12,8)
par(mfrow=c(1,2))
plot(gx$Photo~gx$timeMin, pch=19, col=as.numeric(gx$Obs))
plot(gx$Cond~gx$timeMin, pch=19, col=as.numeric(gx$Obs))
plot(ctrl$Cond ~ ctrl$timeMin, ylim = c(0, 0.12), xlim= c(0, 75))
points(bnd$Cond ~ bnd$timeMin, pch = 19)
View(bnd)
