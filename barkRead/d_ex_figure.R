library(lubridate)
library(dplyr)
s.err <- function(x){
  se <- sd(x)/sqrt(length(x))
  return(se)
}
dfS <- read.table('barkData/Sept_complete.csv', sep = ';', header = TRUE)
# recalculate transpiration (in mmol m-2 s-1) following Zsofia's email on 1-March-2021
dfS$TrA_old <- dfS$TrA
dfS$TrA <- 1000*((dfS$FlowOut/dfS$Area)*((dfS$H2Oout_G - dfS$H2Oin_G)/(dfS$ATP - dfS$H2Oin_G)))
dfS$DT <- as.POSIXct(dfS$DT, format="%Y-%m-%d %H:%M:%S")
dfS$day <- day(dfS$DT)
dfS$mon <- lubridate::month(dfS$DT, label = T)
dfS$datePretty <- paste0(dfS$day, '-', dfS$mon)
dfS$Date <- as.Date(dfS$DT)
dfS$timeDec <- hour(dfS$DT)+ (minute(dfS$DT)/60)
dfS$midday <- ifelse(dfS$timeDec >= 10 & dfS$timeDec <= 16 & dfS$PAR >= 300, 'yes', 'no')
dfSMD <- subset(dfS, midday == 'yes')

sumSep <- summarise(group_by(subset(dfS, midday == 'yes'), Date, MpNo),
                    E_mean = mean(TrA), E_se = s.err(TrA),
                    d_ex_mean = mean(dDH_ex), d_ex_se = s.err(dDH_ex))

cuv <- unique(dfSMD$MpNo)
myDates <- unique(dfSMD$Date)
cuvL <- list()
for(i in 1:length(cuv)){
  cuvL[[i]] <- subset(dfSMD, MpNo == cuv[i])
}

#version1
windows(10,10)
par(mfrow=c(1,1), las=1, cex=1.25, mar=c(6, 6, 2, 2))
plot(subset(dfSMD, MpNo == cuv[1])[,'dDH_ex']~ subset(dfSMD, MpNo == cuv[1])[,'TrA'],
     pch=15, col = 'white',  
     ylim = c(-5, 110), xlim=c(0, 2),
     ylab =  "d-excess (\u2030)",  xlab=expression(italic(E)~(mmol~m^-2~s^-1)), cex.lab=1.3)
myChar <- c(21:24)
myPal <- c('grey', 'blue', 'darkgoldenrod1', 'cyan', 'deeppink', 'black', 'darkorchid1')
for(i in 1:length(cuv)){
  for(j in 1:length(myDates)){
    points(subset(cuvL[[i]], Date == myDates[j])$dDH_ex ~
             subset(cuvL[[i]], Date == myDates[j])$TrA, pch = myChar[i], col = 'black', bg = myPal[j])
  }
}
plotrix::ablineclip(lm(dDH_ex ~ TrA, data = dfSMD),
                    x1 = min(dfSMD$TrA), x2 = max(dfSMD$TrA), lty = 2)
plotrix::ablineclip(lm(dDH_ex ~ TrA, data = subset(dfSMD, day >= 5)),
                    x1 = min(dfSMD$TrA), x2 = max(dfSMD$TrA), lwd = 2)
legend('topleft', pch = rep(19, 7), col = myPal, legend = unique(dfSMD$datePretty), bty ='n')
legend('bottomright', pch = myChar, col = 'black', bg = 'grey', legend = paste0('Cuv.', cuv), bty = 'n')


#version2 (the good one)
windows(10,10)
par(mfrow=c(1,1), las=1, cex=1.25, mar=c(6, 6, 2, 2))
plot(subset(dfSMD, MpNo == cuv[1])[,'dDH_ex']~ subset(dfSMD, MpNo == cuv[1])[,'TrA'],
     pch=15, col = 'white',
     ylim = c(-5, 110), xlim=c(0, 2),
     ylab = expression(paste(italic(d), "-excess (\u2030)")), xlab=expression(italic(E)[leaf]~(mmol~m^-2~s^-1)), cex.lab=1.3)
myChar <- c(21:24)
myPal <- c('grey', 'blue', 'darkgoldenrod1', 'cyan', 'deeppink', 'black', 'darkorchid1')
for(i in 1:length(cuv)){
  for(j in 1:length(myDates)){
    points(subset(cuvL[[i]], Date == myDates[j])$dDH_ex ~
             subset(cuvL[[i]], Date == myDates[j])$TrA, pch = myChar[i], cex = 0.9,
           col = scales::alpha(myPal[j], 0.3), bg = scales::alpha(myPal[j], 0.3))
  }
}
plotrix::ablineclip(lm(dDH_ex ~ TrA, data = dfSMD),
                    x1 = min(dfSMD$TrA), x2 = max(dfSMD$TrA), lty = 2)
plotrix::ablineclip(lm(dDH_ex ~ TrA, data = subset(dfSMD, day >= 5)),
                    x1 = min(dfSMD$TrA), x2 = max(dfSMD$TrA), lwd = 2)
legend('topleft', pch = rep(19, 7), col = myPal, legend = unique(dfSMD$datePretty), bty ='n')
legend('bottomright', pch = myChar, col = 'black', bg = 'grey', legend = paste0('Cuv. ', c('A', 'B', 'C', 'D')), bty = 'n')

sumSep <- as.data.frame(sumSep)
sumSepL <- list()
for(i in 1:length(cuv)){
  sumSepL[[i]] <- subset(sumSep, MpNo == cuv[i])
}
for(i in 1:length(cuv)){
  for(j in 1:length(myDates)){
    Hmisc::errbar(x=subset(sumSepL[[i]], Date == myDates[j])[,'E_mean'],
                  y=subset(sumSepL[[i]], Date == myDates[j])[,'d_ex_mean'],
                  yplus=subset(sumSepL[[i]], Date == myDates[j])[,'d_ex_mean']+
                    subset(sumSepL[[i]], Date == myDates[j])[,'d_ex_se'],
                  yminus=subset(sumSepL[[i]], Date == myDates[j])[,'d_ex_mean']-
                    subset(sumSepL[[i]], Date == myDates[j])[,'d_ex_se'],
                  pch=myChar[i], col='black', bg = myPal[j], errbar.col = 'black', add=T, cex=1.6)
    arrows(x0 = subset(sumSepL[[i]], Date == myDates[j])[,'E_mean'],
           y0 = subset(sumSepL[[i]], Date == myDates[j])[,'d_ex_mean'],
           x1 = subset(sumSepL[[i]], Date == myDates[j])[,'E_mean']+
             subset(sumSepL[[i]], Date == myDates[j])[,'E_se'],
           length = 0.03, angle = 90, code = 2)
    arrows(x0 = subset(sumSepL[[i]], Date == myDates[j])[,'E_mean'],
           y0 = subset(sumSepL[[i]], Date == myDates[j])[,'d_ex_mean'],
           x1 = subset(sumSepL[[i]], Date == myDates[j])[,'E_mean']-
             subset(sumSepL[[i]], Date == myDates[j])[,'E_se'],
           length = 0.03, angle = 90, code = 2)
  }
}

#version3
windows(10,10)
par(mfrow=c(1,1), las=1, cex=1.25, mar=c(6, 6, 2, 2))
plot(subset(dfSMD, MpNo == cuv[1])[,'dDH_ex']~ subset(dfSMD, MpNo == cuv[1])[,'TrA'],
     pch=15, col = 'white',  
     ylim = c(-5, 110), xlim=c(0, 2),
     ylab =  "d-excess (\u2030)",  xlab=expression(italic(E)~(mmol~m^-2~s^-1)), cex.lab=1.3)
myChar <- c(21:24)
myPal <- c('grey', 'blue', 'darkgoldenrod1', 'cyan', 'deeppink', 'black', 'darkorchid1')
plotrix::ablineclip(lm(dDH_ex ~ TrA, data = dfSMD),
                    x1 = min(dfSMD$TrA), x2 = max(dfSMD$TrA), lty = 2)
plotrix::ablineclip(lm(dDH_ex ~ TrA, data = subset(dfSMD, day >= 5)),
                    x1 = min(dfSMD$TrA), x2 = max(dfSMD$TrA), lwd = 2)
legend('topleft', pch = rep(19, 7), col = myPal, legend = unique(dfSMD$datePretty), bty ='n')
legend('bottomright', pch = myChar, col = 'black', bg = 'grey', legend = paste0('Cuv.', cuv), bty = 'n')

sumSep <- as.data.frame(sumSep)
sumSepL <- list()
for(i in 1:length(cuv)){
  sumSepL[[i]] <- subset(sumSep, MpNo == cuv[i])
}
for(i in 1:length(cuv)){
  for(j in 1:length(myDates)){
    Hmisc::errbar(x=subset(sumSepL[[i]], Date == myDates[j])[,'E_mean'],
                  y=subset(sumSepL[[i]], Date == myDates[j])[,'d_ex_mean'],
                  yplus=subset(sumSepL[[i]], Date == myDates[j])[,'d_ex_mean']+
                    subset(sumSepL[[i]], Date == myDates[j])[,'d_ex_se'],
                  yminus=subset(sumSepL[[i]], Date == myDates[j])[,'d_ex_mean']-
                    subset(sumSepL[[i]], Date == myDates[j])[,'d_ex_se'],
                  pch=myChar[i], col='black', bg = myPal[j], errbar.col = 'black', add=T, cex=1.6)
    arrows(x0 = subset(sumSepL[[i]], Date == myDates[j])[,'E_mean'],
           y0 = subset(sumSepL[[i]], Date == myDates[j])[,'d_ex_mean'],
           x1 = subset(sumSepL[[i]], Date == myDates[j])[,'E_mean']+
             subset(sumSepL[[i]], Date == myDates[j])[,'E_se'],
           length = 0.03, angle = 90, code = 2)
    arrows(x0 = subset(sumSepL[[i]], Date == myDates[j])[,'E_mean'],
           y0 = subset(sumSepL[[i]], Date == myDates[j])[,'d_ex_mean'],
           x1 = subset(sumSepL[[i]], Date == myDates[j])[,'E_mean']-
             subset(sumSepL[[i]], Date == myDates[j])[,'E_se'],
           length = 0.03, angle = 90, code = 2)
  }
}
