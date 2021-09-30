library(lubridate)

dfS <- read.table('barkData/Sept_complete.csv', sep = ';', header = TRUE)
dfS$DT <- as.POSIXct(dfS$DT, format="%Y-%m-%d %H:%M:%S")
wiS <- read.csv("barkData/Sept_xylem_wi.csv")
dfS <- dplyr::left_join(dfS, wiS, by = 'MpNo')
dfS$band_surface_m2 <- dfS$diam_cm * pi * dfS$length_cm/10000
# estimated needle area per branch given a certain branch diameter
# equation according to Cermak et al. 1998 Ann For Sci
# y = 0.003*x^2 - 0.008*x
# y is needle area in m2 and x is branch sectional area in m2 (mm2???)
dfS$cross_section_area_mm2 <- pi*(dfS$diam_cm*10*0.5)^2
dfS$needle_area_m2 <- 0.003*dfS$cross_section_area_mm2^2 - 0.08*dfS$cross_section_area_mm2
# calculate branch transpiration rate
dfS$E_branch <- dfS$TrA * dfS$needle_area_m2
dfS$time <- yday(dfS$DT) + (hour(dfS$DT)+ minute(dfS$DT)/60)/24
dfS$timeDec <- hour(dfS$DT)+ (minute(dfS$DT)/60)
dfS$DOY <- yday(dfS$DT)
# calculate deltas (d18O and d2H) of transpired water (d_E), according to:
dfS$d18O_E <- (dfS$FlowOut*dfS$H2Oout_G*dfS$d18O_out*0.001 - dfS$FlowIn*dfS$H2Oin_G*dfS$d18O_in*0.001)*1000/
  (dfS$FlowOut*dfS$H2Oout_G - dfS$FlowIn*dfS$H2Oin_G)
dfS$ss <- ifelse(dfS$d18O_E <= dfS$d18_up_lim & dfS$PAR >= 100 &
                   dfS$timeDec >= 10 & dfS$timeDec < 18 &
                   dfS$DOY >= 247, 'yes', 'no')

dfS$d2H_E <- (dfS$FlowOut*dfS$H2Oout_G*dfS$dDH_out*0.001 - dfS$FlowIn*dfS$H2Oin_G*dfS$dDH_in*0.001)*1000/
  (dfS$FlowOut*dfS$H2Oout_G - dfS$FlowIn*dfS$H2Oin_G)

myMpNo <- unique(dfS$MpNo)

dfSwhole <- dfS
dfS <- subset(dfS, DOY != 252)

windows(12,8)
par(mfrow=c(3, 4), mar = c(0, 5, 4, 0))
plot(subset(dfS, MpNo == myMpNo[1])$d18O_in ~ subset(dfS, MpNo == myMpNo[1])$DT,
     pch =19, col = 'blue', ylim = c(-25, 30), main = 'Cuvette 1', axes = F,
     ylab = expression(paste(delta^{18}, "O (\u2030)")), xlab = '', cex.lab = 1.6)
points(subset(dfS, MpNo == myMpNo[1])$d18O_out ~ subset(dfS, MpNo == myMpNo[1])$DT, pch =19, col = 'red')
points(subset(dfS, MpNo == myMpNo[1])$d18O_E ~ subset(dfS, MpNo == myMpNo[1])$DT, pch =19, col = 'black')
abline(subset(dfS, MpNo == myMpNo[1])$d18O_b[1], 0, lty = 2)
abline(subset(dfS, MpNo == myMpNo[1])$d18O_a[1], 0, lty = 3)
abline(subset(dfS, MpNo == myMpNo[1])$d18_up_lim[1], 0)
axis(side = 2, at = seq(-20, 30, 10), labels = seq(-20, 30, 10))
axis(side = 1, at = seq(min(subset(dfS, MpNo == myMpNo[1])$DT), max(subset(dfS, MpNo == myMpNo[1])$DT), 'day'),
     labels = F)
box()
legend('topleft', expression(bold((a))), bty = 'n', cex = 1.2, pt.cex = 1)

par(mar = c(0, 5*2/3, 4, 5/3))
plot(subset(dfS, MpNo == myMpNo[2])$d18O_in ~ subset(dfS, MpNo == myMpNo[2])$DT,
     pch =19, col = 'blue', ylim = c(-25, 30), axes = F,
     ylab = '', xlab = '', main = 'Cuvette 2')
points(subset(dfS, MpNo == myMpNo[2])$d18O_out ~ subset(dfS, MpNo == myMpNo[2])$DT, pch =19, col = 'red')
points(subset(dfS, MpNo == myMpNo[2])$d18O_E ~ subset(dfS, MpNo == myMpNo[2])$DT, pch =19, col = 'black')
abline(subset(dfS, MpNo == myMpNo[2])$d18O_b[1], 0, lty = 2)
abline(subset(dfS, MpNo == myMpNo[2])$d18O_a[1], 0, lty = 3)
abline(subset(dfS, MpNo == myMpNo[2])$d18_up_lim[1], 0)
axis(side = 2, at = seq(-20, 30, 10), labels = seq(-20, 30, 10))
axis(side = 1, at = seq(min(subset(dfS, MpNo == myMpNo[2])$DT), max(subset(dfS, MpNo == myMpNo[2])$DT), 'day'),
     labels = F)
box()
legend('topleft', expression(bold((b))), bty = 'n', cex = 1.2, pt.cex = 1)

par(mar = c(0, 5/3, 4, 5*2/3))
plot(subset(dfS, MpNo == myMpNo[3])$d18O_in ~ subset(dfS, MpNo == myMpNo[3])$DT,
     pch =19, col = 'blue', ylim = c(-25, 30), axes = F,
     ylab = '', xlab = '', main = 'Cuvette 7')
points(subset(dfS, MpNo == myMpNo[3])$d18O_out ~ subset(dfS, MpNo == myMpNo[3])$DT, pch =19, col = 'red')
points(subset(dfS, MpNo == myMpNo[3])$d18O_E ~ subset(dfS, MpNo == myMpNo[3])$DT, pch =19, col = 'black')
abline(subset(dfS, MpNo == myMpNo[3])$d18O_b[1], 0, lty = 2)
abline(subset(dfS, MpNo == myMpNo[3])$d18O_a[1], 0, lty = 3)
abline(subset(dfS, MpNo == myMpNo[3])$d18_up_lim[1], 0)
axis(side = 2, at = seq(-20, 30, 10), seq(-20, 30, 10))
axis(side = 1, at = seq(min(subset(dfS, MpNo == myMpNo[3])$DT), max(subset(dfS, MpNo == myMpNo[3])$DT), 'day'),
     labels = F)
box()
legend('topleft', expression(bold((c))), bty = 'n', cex = 1.2, pt.cex = 1)

legend('topright', legend = c('Xyl. Up', 'Xyl. Down'), bty = 'n', lty = c(2, 3),
       cex = 1.2, pt.cex = 1)

par(mar = c(0, 0, 4, 5))
plot(subset(dfS, MpNo == myMpNo[4])$d18O_in ~ subset(dfS, MpNo == myMpNo[4])$DT,
     pch =19, col = 'blue', ylim = c(-25, 30), axes = F,
     ylab = '', xlab = '', main = 'Cuvette 8')
points(subset(dfS, MpNo == myMpNo[4])$d18O_out ~ subset(dfS, MpNo == myMpNo[4])$DT, pch =19, col = 'red')
points(subset(dfS, MpNo == myMpNo[4])$d18O_E ~ subset(dfS, MpNo == myMpNo[4])$DT, pch =19, col = 'black')
abline(subset(dfS, MpNo == myMpNo[4])$d18O_b[1], 0, lty = 2)
abline(subset(dfS, MpNo == myMpNo[4])$d18O_a[1], 0, lty = 3)
abline(subset(dfS, MpNo == myMpNo[4])$d18_up_lim[1], 0)
axis(side = 2, at = seq(-20, 30, 10), labels = seq(-20, 30, 10))
axis(side = 1, at = seq(min(subset(dfS, MpNo == myMpNo[4])$DT), max(subset(dfS, MpNo == myMpNo[4])$DT), 'day'),
     labels = F)
box()
legend('topleft', expression(bold((d))), bty = 'n', cex = 1.2, pt.cex = 1)

legend('topright', pch = rep(19, 3), legend = c(expression(delta['in']), expression(delta[out]),
                              expression(delta[italic(E)])), 
       col = c('blue', 'red', 'black'), bty = 'n', cex = 1.3, pt.cex = 1)

par(mar = c(2, 5, 2, 0))
plot(subset(dfS, MpNo == myMpNo[1])$dDH_in ~ subset(dfS, MpNo == myMpNo[1])$DT,
     pch =19, col = 'blue', ylim = c(-170, 750), axes = F,
     ylab = expression(paste(delta^{2}, "H (\u2030)")), xlab = '', cex.lab = 1.6)
points(subset(dfS, MpNo == myMpNo[1])$dDH_out ~ subset(dfS, MpNo == myMpNo[1])$DT, pch =19, col = 'red')
points(subset(dfS, MpNo == myMpNo[1])$d2H_E ~ subset(dfS, MpNo == myMpNo[1])$DT, pch =19, col = 'black')
abline(subset(dfS, MpNo == myMpNo[1])$d2H_b[1], 0, lty = 2)
abline(subset(dfS, MpNo == myMpNo[1])$d2H_a[1], 0, lty = 3)
axis(side = 2, at = seq(-200, 600, 200), labels = seq(-200, 600, 200))
axis(side = 1, at = seq(min(subset(dfS, MpNo == myMpNo[1])$DT), max(subset(dfS, MpNo == myMpNo[1])$DT), 'day'),
     labels = F)
box()
legend('topleft', expression(bold((e))), bty = 'n', cex = 1.2, pt.cex = 1)

par(mar = c(2, 5*2/3, 2, 5/3))
plot(subset(dfS, MpNo == myMpNo[2])$dDH_in ~ subset(dfS, MpNo == myMpNo[2])$DT,
     pch =19, col = 'blue', ylim = c(-170, 750), axes = F,
     ylab = '', xlab = '')
points(subset(dfS, MpNo == myMpNo[2])$dDH_out ~ subset(dfS, MpNo == myMpNo[2])$DT, pch =19, col = 'red')
points(subset(dfS, MpNo == myMpNo[2])$d2H_E ~ subset(dfS, MpNo == myMpNo[2])$DT, pch =19, col = 'black')
abline(subset(dfS, MpNo == myMpNo[2])$d2H_b[1], 0, lty = 2)
abline(subset(dfS, MpNo == myMpNo[2])$d2H_a[1], 0, lty = 3)
axis(side = 2, at = seq(-200, 600, 200), labels = seq(-200, 600, 200))
axis(side = 1, at = seq(min(subset(dfS, MpNo == myMpNo[1])$DT), max(subset(dfS, MpNo == myMpNo[1])$DT), 'day'),
     labels = F)
box()
legend('topleft', expression(bold((f))), bty = 'n', cex = 1.2, pt.cex = 1)

par(mar = c(2, 5/3, 2, 5*2/3))
plot(subset(dfS, MpNo == myMpNo[3])$dDH_in ~ subset(dfS, MpNo == myMpNo[3])$DT,
     pch =19, col = 'blue', ylim = c(-170, 750), axes = F,
     ylab = '', xlab = '')
points(subset(dfS, MpNo == myMpNo[3])$dDH_out ~ subset(dfS, MpNo == myMpNo[3])$DT, pch =19, col = 'red')
points(subset(dfS, MpNo == myMpNo[3])$d2H_E ~ subset(dfS, MpNo == myMpNo[3])$DT, pch =19, col = 'black')
abline(subset(dfS, MpNo == myMpNo[3])$d2H_b[1], 0, lty = 2)
abline(subset(dfS, MpNo == myMpNo[3])$d2H_a[1], 0, lty = 3)
axis(side = 2, at = seq(-200, 600, 200), labels = seq(-200, 600, 200))
axis(side = 1, at = seq(min(subset(dfS, MpNo == myMpNo[1])$DT), max(subset(dfS, MpNo == myMpNo[1])$DT), 'day'),
     labels = F)
box()
legend('topleft', expression(bold((g))), bty = 'n', cex = 1.2, pt.cex = 1)

par(mar = c(2, 0, 2, 5))
plot(subset(dfS, MpNo == myMpNo[4])$dDH_in ~ subset(dfS, MpNo == myMpNo[4])$DT,
     pch =19, col = 'blue', ylim = c(-170, 750), axes = F,
     ylab = '', xlab = '')
points(subset(dfS, MpNo == myMpNo[4])$dDH_out ~ subset(dfS, MpNo == myMpNo[4])$DT, pch =19, col = 'red')
points(subset(dfS, MpNo == myMpNo[4])$d2H_E ~ subset(dfS, MpNo == myMpNo[4])$DT, pch =19, col = 'black')
abline(subset(dfS, MpNo == myMpNo[4])$d2H_b[1], 0, lty = 2)
abline(subset(dfS, MpNo == myMpNo[4])$d2H_a[1], 0, lty = 3)
axis(side = 2, at = seq(-200, 600, 200), labels = seq(-200, 600, 200))
axis(side = 1, at = seq(min(subset(dfS, MpNo == myMpNo[1])$DT), max(subset(dfS, MpNo == myMpNo[1])$DT), 'day'),
     labels = F)
box()
legend('topleft', expression(bold((h))), bty = 'n', cex = 1.2, pt.cex = 1)

par(mar = c(4, 5, 0, 0))
plot(subset(dfS, MpNo == myMpNo[1])$TrA ~ subset(dfS, MpNo == myMpNo[1])$DT,
     pch = 19, cex = 1.25, col = 'darkgreen', ylim = c(0, 2.25), cex.lab = 1.6,
     ylab = expression(italic(E)~(mmol~m^-2~s^-1)), xlab = '')
lines(subset(dfS, MpNo == myMpNo[1])$TrA ~ subset(dfS, MpNo == myMpNo[1])$DT, col ='darkgreen')
legend('topleft', expression(bold((i))), bty = 'n', cex = 1.2, pt.cex = 1)

par(mar = c(4, 5*2/3, 0, 5/3))
plot(subset(dfS, MpNo == myMpNo[2])$TrA ~ subset(dfS, MpNo == myMpNo[2])$DT,
     pch = 19, cex = 1.25, col = 'darkgreen', ylim = c(0, 2.25), 
     ylab = '', xlab = '')
lines(subset(dfS, MpNo == myMpNo[2])$TrA ~ subset(dfS, MpNo == myMpNo[2])$DT, col ='darkgreen')
legend('topleft', expression(bold((j))), bty = 'n', cex = 1.2, pt.cex = 1)

par(mar = c(4, 5/3, 0, 5*2/3))
plot(subset(dfS, MpNo == myMpNo[3])$TrA ~ subset(dfS, MpNo == myMpNo[3])$DT,
     pch = 19, cex = 1.25, col = 'darkgreen', ylim = c(0, 2.25), 
     ylab = '', xlab = '')
lines(subset(dfS, MpNo == myMpNo[3])$TrA ~ subset(dfS, MpNo == myMpNo[3])$DT, col ='darkgreen')
legend('topleft', expression(bold((k))), bty = 'n', cex = 1.2, pt.cex = 1)

par(mar = c(4, 0, 0, 5))
plot(subset(dfS, MpNo == myMpNo[4])$TrA ~ subset(dfS, MpNo == myMpNo[4])$DT,
     pch = 19, cex = 1.25, col = 'darkgreen', ylim = c(0, 2.25), 
     ylab = '', xlab = '')
lines(subset(dfS, MpNo == myMpNo[4])$TrA ~ subset(dfS, MpNo == myMpNo[4])$DT, col ='darkgreen')
legend('topleft', expression(bold((l))), bty = 'n', cex = 1.2, pt.cex = 1)

