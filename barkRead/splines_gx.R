library(lubridate)
library(data.table)
library(ggplot2)

dfS <- read.table('barkData/Sept_complete.csv', sep = ';', header = TRUE)
dfS$DT <- as.POSIXct(dfS$DT, format="%Y-%m-%d %H:%M:%S")
wiS <- read.csv("barkData/Sept_xylem_wi.csv")
dfS <- dplyr::left_join(dfS, wiS[, 1:5], by = 'MpNo')
dfS$time <- yday(dfS$DT) + (hour(dfS$DT)+ minute(dfS$DT)/60)/24
dfS$DOY <- yday(dfS$DT)
str(dfS)

theDays <- unique(dfS$DOY)
MpNo <- unique(dfS$MpNo)

test <- list()
for (j in 1:length(MpNo)){                                                    
  empty <- list()
  for (i in 1:length(theDays)){                                 
   cuv <- subset(dfS, DOY == theDays[i] & MpNo == MpNo[j])
   spl <- spline(x = cuv$time, y = cuv$TrA,
                 xout = seq(from = min(cuv$time), to = max(cuv$time), by = 1/(24*60)))
   spl$MpNo <- rep(cuv[1, 'MpNo'], times = length(spl$x))
   spl$DOY <- rep(cuv[1, 'DOY'], times = length(spl$x))
   spl <- as.data.frame(spl)
   names(spl) <- c('time', 'TrPred', 'MpNo', 'DOY')
   empty[[i]] <- spl
  }
  test[[j]] <- do.call(rbind, empty)
}

trPred <- do.call(rbind, test)

trAll <- dplyr::left_join(trPred[, c('time', 'MpNo', 'TrPred')], dfS[, c('DT', 'MpNo', 'TrA', 'time')],
                          by = c('time', 'MpNo'))


