library(lubridate)
library(data.table)
library(ggplot2)

dfJ <- read.table('barkData/July24_complete.csv', sep = ';', header = TRUE)
dfJ$DT <- as.POSIXct(dfJ$DT, format="%Y-%m-%d %H:%M:%S")
wiJ <- read.csv("barkData/July24_xylem_wi.csv")
dfJ <- dplyr::left_join(dfJ, wiJ, by = 'MpNo')
dfJ$band_surface_m2 <- dfJ$diam_cm * pi * dfJ$length_cm/10000
# estimated needle area per branch given a certain branch diameter
# equation according to Cermak et al. 1998 Ann For Sci
# y = 0.003*x^2 - 0.008*x
# y is needle area in m2 and x is branch sectional area in m2 (mm2???)
dfJ$cross_section_area_mm2 <- pi*(dfJ$diam_cm*10*0.5)^2
dfJ$needle_area_m2 <- 0.003*dfJ$cross_section_area_mm2^2 - 0.08*dfJ$cross_section_area_mm2
# calculate branch transpiration rate
dfJ$E_branch <- dfJ$TrA * dfJ$needle_area_m2
dfJ$time <- yday(dfJ$DT) + (hour(dfJ$DT)+ minute(dfJ$DT)/60)/24
dfJ$DOY <- yday(dfJ$DT)
# calculate deltas (d18O and d2H) of transpired water (d_E), according to:
dfJ$d18O_E <- (dfJ$FlowOut*dfJ$H2Oout_G*dfJ$d18O_out*0.001 - dfJ$FlowIn*dfJ$H2Oin_G*dfJ$d18O_in*0.001)*1000/
  (dfJ$FlowOut*dfJ$H2Oout_G - dfJ$FlowIn*dfJ$H2Oin_G)
dfJ$ss <- ifelse(dfJ$d18O_E >= dfJ$d18_lw_lim & dfJ$d18O_E <= dfJ$d18_up_lim, 'yes', 'no')

dfJ$d2H_E <- (dfJ$FlowOut*dfJ$H2Oout_G*dfJ$dDH_out*0.001 - dfJ$FlowIn*dfJ$H2Oin_G*dfJ$dDH_in*0.001)*1000/
  (dfJ$FlowOut*dfJ$H2Oout_G - dfJ$FlowIn*dfJ$H2Oin_G)

myMpNo <- unique(dfJ$MpNo)

windows(12, 12)
par(mfrow=c(3, 2), mar = c(0, 5, 4, 0), cex = 1.1)
plot(subset(dfJ, MpNo == myMpNo[1])$d18O_in ~ subset(dfJ, MpNo == myMpNo[1])$DT,
     pch =19, col = 'blue', ylim = c(-22, 20), main = 'Cuvette 2', axes = F,
     ylab = expression(paste(delta^{18}, "O (\u2030)")), xlab = '', cex.lab = 1.6)
points(subset(dfJ, MpNo == myMpNo[1])$d18O_out ~ subset(dfJ, MpNo == myMpNo[1])$DT, pch =19, col = 'red')
points(subset(dfJ, MpNo == myMpNo[1])$d18O_E ~ subset(dfJ, MpNo == myMpNo[1])$DT, pch =19, col = 'black')
points(subset(dfJ, MpNo == myMpNo[1] & ss == 'yes')$d18O_E ~ subset(dfJ, MpNo == myMpNo[1] & ss == 'yes')$DT,
       pch =19, col = 'green')
abline(subset(dfJ, MpNo == myMpNo[1])$d18O_b[1], 0, lty = 2)
abline(subset(dfJ, MpNo == myMpNo[1])$d18O_a[1], 0, lty = 3)
abline(subset(dfJ, MpNo == myMpNo[1])$d18_up_lim[1], 0)
abline(subset(dfJ, MpNo == myMpNo[1])$d18_lw_lim[1], 0)
axis(side = 2, at = seq(-20, 20, 10), labels = seq(-20, 20, 10))
axis(side = 1, at = seq(min(subset(dfJ, MpNo == myMpNo[1])$DT), max(subset(dfJ, MpNo == myMpNo[1])$DT), 'hour'),
     labels = F)
box()
legend('topleft', expression(bold((a))), bty = 'n', cex = 1.2, pt.cex = 1)

legend('topright', legend = c('Xyl. Up', 'Xyl. Down'), bty = 'n', lty = c(2, 3),
       cex = 1.2, pt.cex = 1)

par(mar = c(0, 2, 4, 3))
plot(subset(dfJ, MpNo == myMpNo[2])$d18O_in ~ subset(dfJ, MpNo == myMpNo[2])$DT,
     pch =19, col = 'blue', ylim = c(-25, 30), axes = F,
     ylab = '', xlab = '', main = 'Cuvette 8')
points(subset(dfJ, MpNo == myMpNo[2])$d18O_out ~ subset(dfJ, MpNo == myMpNo[2])$DT, pch =19, col = 'red')
points(subset(dfJ, MpNo == myMpNo[2])$d18O_E ~ subset(dfJ, MpNo == myMpNo[2])$DT, pch =19, col = 'black')
points(subset(dfJ, MpNo == myMpNo[2] & ss == 'yes')$d18O_E ~ subset(dfJ, MpNo == myMpNo[4] & ss == 'yes')$DT,
       pch =19, col = 'green')
abline(subset(dfJ, MpNo == myMpNo[2])$d18O_b[1], 0, lty = 2)
abline(subset(dfJ, MpNo == myMpNo[2])$d18O_a[1], 0, lty = 3)
abline(subset(dfJ, MpNo == myMpNo[2])$d18_up_lim[1], 0)
abline(subset(dfJ, MpNo == myMpNo[2])$d18_lw_lim[1], 0)
axis(side = 2, at = seq(-20, 30, 10), labels = seq(-20, 30, 10))
axis(side = 1, at = seq(min(subset(dfJ, MpNo == myMpNo[2])$DT), max(subset(dfJ, MpNo == myMpNo[2])$DT), 'hour'),
     labels = F)
box()
legend('topleft', expression(bold((b))), bty = 'n', cex = 1.2, pt.cex = 1)

legend('topright', pch = rep(19, 3), legend = c(expression(delta['in']), expression(delta[out]),
                                                expression(delta[italic(E)])), 
       col = c('blue', 'red', 'black'), bty = 'n', cex = 1.3, pt.cex = 1)

par(mar = c(2, 5, 2, 0))
plot(subset(dfJ, MpNo == myMpNo[1])$dDH_in ~ subset(dfJ, MpNo == myMpNo[1])$DT,
     pch =19, col = 'blue', ylim = c(-150, 1250), axes = F,
     ylab = expression(paste(delta^{2}, "H (\u2030)")), xlab = '', cex.lab = 1.6)
points(subset(dfJ, MpNo == myMpNo[1])$dDH_out ~ subset(dfJ, MpNo == myMpNo[1])$DT, pch =19, col = 'red')
points(subset(dfJ, MpNo == myMpNo[1])$d2H_E ~ subset(dfJ, MpNo == myMpNo[1])$DT, pch =19, col = 'black')
points(subset(dfJ, MpNo == myMpNo[1] & ss == 'yes')$d2H_E ~ subset(dfJ, MpNo == myMpNo[1] & ss == 'yes')$DT,
       pch =19, col = 'green')
abline(subset(dfJ, MpNo == myMpNo[1])$d2H_b[1], 0, lty = 2)
#abline(subset(dfJ, MpNo == myMpNo[1])$d2H_a[1], 0, lty = 3)
axis(side = 2, at = seq(-100, 1200, 200), labels = seq(-100, 1200, 200))
axis(side = 1, at = seq(min(subset(dfJ, MpNo == myMpNo[1])$DT), max(subset(dfJ, MpNo == myMpNo[1])$DT), 'hour'),
     labels = F)
box()
legend('topleft', expression(bold((c))), bty = 'n', cex = 1.2, pt.cex = 1)

par(mar = c(2, 2, 2, 3))
plot(subset(dfJ, MpNo == myMpNo[2])$dDH_in ~ subset(dfJ, MpNo == myMpNo[2])$DT,
     pch =19, col = 'blue', ylim = c(-150, 1250), axes = F,
     ylab = '', xlab = '')
points(subset(dfJ, MpNo == myMpNo[2])$dDH_out ~ subset(dfJ, MpNo == myMpNo[2])$DT, pch =19, col = 'red')
points(subset(dfJ, MpNo == myMpNo[2])$d2H_E ~ subset(dfJ, MpNo == myMpNo[2])$DT, pch =19, col = 'black')
points(subset(dfJ, MpNo == myMpNo[2] & ss == 'yes')$d2H_E ~ subset(dfJ, MpNo == myMpNo[2] & ss == 'yes')$DT,
       pch =19, col = 'green')
abline(subset(dfJ, MpNo == myMpNo[2])$d2H_b[1], 0, lty = 2)
abline(subset(dfJ, MpNo == myMpNo[2])$d2H_a[1], 0, lty = 3)
axis(side = 2, at = seq(-100, 1200, 200), labels = seq(-100, 1200, 200))
axis(side = 1, at = seq(min(subset(dfJ, MpNo == myMpNo[1])$DT), max(subset(dfJ, MpNo == myMpNo[1])$DT), 'hour'),
     labels = F)
box()
legend('topleft', expression(bold((d))), bty = 'n', cex = 1.2, pt.cex = 1)


par(mar = c(4, 5, 0, 0))
plot(subset(dfJ, MpNo == myMpNo[1])$TrA ~ subset(dfJ, MpNo == myMpNo[1])$DT,
     pch = 19, col = 'darkgreen', ylim = c(0, 2), cex.lab = 1.6,
     ylab = expression(italic(E)~(mmol~m^-2~s^-1)), xlab = '')
lines(subset(dfJ, MpNo == myMpNo[1])$TrA ~ subset(dfJ, MpNo == myMpNo[1])$DT, col ='darkgreen')
legend('topleft', expression(bold((e))), bty = 'n', cex = 1.2, pt.cex = 1)

par(mar = c(4, 2, 0, 3))
plot(subset(dfJ, MpNo == myMpNo[2])$TrA ~ subset(dfJ, MpNo == myMpNo[2])$DT,
     pch = 19, col = 'darkgreen', ylim = c(0, 2), 
     ylab = '', xlab = '')
lines(subset(dfJ, MpNo == myMpNo[2])$TrA ~ subset(dfJ, MpNo == myMpNo[2])$DT, col ='darkgreen')
legend('topleft', expression(bold((f))), bty = 'n', cex = 1.2, pt.cex = 1)

