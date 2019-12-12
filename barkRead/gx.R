gx <- read.csv('barkData/gx20191009.csv')
gx$DateTime <- lubridate::ymd_hms(as.character(paste0('2019-10-09 ', gx$HHMMSS)))
gx$outOfWater <- lubridate::ymd_hms(as.character(paste0('2019-10-09 ', gx$intitialTime)))
gx$timeMin <- as.numeric(gx$DateTime - gx$outOfWater)
gx <- doBy::orderBy(~ Obs + DateTime, gx)
gx$timeInterval <- as.numeric(gx$DateTime - dplyr::lag(gx$DateTime))
gx$dCond <- (gx$Cond - dplyr::lag(gx$Cond, 1))/gx$timeInterval
gx$dPhoto <- (gx$Photo - dplyr::lag(gx$Photo, 1))/gx$timeInterval
gx[which(gx$timeInterval >= 15 | gx$timeInterval < 0), c('dCond', 'dPhoto')] <- NA

ctrl <- subset(gx, Obs == 'branch1')
bnd <- subset(gx, Obs == 'branch2')
windows(12,8)
par(mfrow=c(1,2))
plot(gx$Photo~gx$timeMin, pch=19, col=as.numeric(gx$Obs))
plot(gx$Cond~gx$timeMin, pch=19, col=as.numeric(gx$Obs))
plot(ctrl$Cond ~ ctrl$timeMin, ylim = c(0, 0.12), xlim= c(0, 75))
points(bnd$Cond ~ bnd$timeMin, pch = 19)
View(bnd)
