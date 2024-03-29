library(lubridate)
library(data.table)

dfJ <- read.table('barkData/July24_complete.csv', sep = ';', header = TRUE)
# recalculate transpiration (in mmol m-2 s-1) following Zsofia's email on 1-March-2021
dfJ$TrA_old <- dfJ$TrA
dfJ$TrA <- 1000*((dfJ$FlowOut/dfJ$Area)*((dfJ$H2Oout_G - dfJ$H2Oin_G)/(dfJ$ATP - dfJ$H2Oin_G)))
dfJ$DT <- as.POSIXct(dfJ$DT, format="%Y-%m-%d %H:%M:%S")
dfJ$DOY <- yday(dfJ$DT)
dfJ$timeDec <- hour(dfJ$DT) + (minute(dfJ$DT)/60)
wiJ <- read.csv("barkData/July24_xylem_wi.csv")
dfJ <- dplyr::left_join(dfJ, wiJ, by = 'MpNo')
dfJ$time <- yday(dfJ$DT) + (hour(dfJ$DT)+ minute(dfJ$DT)/60)/24
# calculate deltas (d18O and d2H) of transpired water (d_E), according to:
dfJ$d18O_E <- (dfJ$FlowOut*dfJ$H2Oout_G*dfJ$d18O_out*0.001 - dfJ$FlowIn*dfJ$H2Oin_G*dfJ$d18O_in*0.001)*1000/
        (dfJ$FlowOut*dfJ$H2Oout_G - dfJ$FlowIn*dfJ$H2Oin_G)
dfJ$ss <- ifelse(dfJ$d18O_E <= dfJ$d18_up_lim, 'yes', 'no')
dfJ$d2H_E <- (dfJ$FlowOut*dfJ$H2Oout_G*dfJ$dDH_out*0.001 - dfJ$FlowIn*dfJ$H2Oin_G*dfJ$dDH_in*0.001)*1000/
        (dfJ$FlowOut*dfJ$H2Oout_G - dfJ$FlowIn*dfJ$H2Oin_G)
dfJ$dDH_ex <- dfJ$dDH_out - 8*dfJ$d18O_out
dfJ$dDH_ex_a <- dfJ$dDH_in - 8*dfJ$d18O_in

windows(12, 14)
par(mfrow=c(3, 2), mar = c(0, 5, 4, 0), cex = 1.1)
plot(subset(dfJ, MpNo == 2)$d18O_in ~ subset(dfJ, MpNo == 2)$DT,
     ylim = c(-22, 30), xlim = c(min(dfJ$DT), max(dfJ$DT)),
     main = 'Cuvette B', axes = F, pch =19, col = 'blue',
     ylab = expression(paste(delta^{18}, "O (\u2030)")), xlab = '', cex.lab = 1.6)
points(subset(dfJ, MpNo == 2)$d18O_out ~ subset(dfJ, MpNo == 2)$DT, pch =19, col = 'red')
points(subset(dfJ, MpNo == 2 & ss == 'yes')$d18O_E ~ subset(dfJ, MpNo == 2 & ss == 'yes')$DT,
       pch =19, col = 'black')
points(subset(dfJ, MpNo == 2 & ss == 'no')$d18O_E ~ subset(dfJ, MpNo == 2 & ss == 'no')$DT,
       pch =1, col = 'black')
abline(subset(dfJ, MpNo == 2)$d18O_b[1], 0, lty = 2)
# no data available for the exact segment, use the tree average
abline(subset(dfJ, MpNo == 2)$d18O_a_tree[1], 0, lty = 3)
abline(subset(dfJ, MpNo == 2)$d18_up_lim[1], 0)
axis(side = 2, at = seq(-20, 30, 10), labels = seq(-20, 30, 10))
axis(side = 1, at = seq(min(dfJ$DT), max(dfJ$DT), 'hour'), labels = F)
box()
legend('topleft', expression(bold((a))), bty = 'n', cex = 1.2, pt.cex = 1)
legend('topright', legend = c('Xyl. Up', 'Xyl. Down'), bty = 'n', lty = c(2, 3),
       cex = 1.2, pt.cex = 1)

par(mar = c(0, 2, 4, 3))
plot(subset(dfJ, MpNo == 7)$d18O_in ~ subset(dfJ, MpNo == 7)$DT,
     ylim = c(-22, 30), xlim = c(min(dfJ$DT), max(dfJ$DT)),
     pch =19, col = 'blue', axes = F, ylab = '', xlab = '', main = 'Cuvette C')
points(subset(dfJ, MpNo == 7)$d18O_out ~ subset(dfJ, MpNo == 7)$DT, pch =19, col = 'red')
points(subset(dfJ, MpNo == 7 & ss == 'yes')$d18O_E ~ subset(dfJ, MpNo == 7 & ss == 'yes')$DT,
       pch =19, col = 'black')
points(subset(dfJ, MpNo == 7 & ss == 'no')$d18O_E ~ subset(dfJ, MpNo == 7 & ss == 'no')$DT,
       pch =1, col = 'black')
abline(subset(dfJ, MpNo == 7)$d18O_b[1], 0, lty = 2)
abline(subset(dfJ, MpNo == 7)$d18O_a[1], 0, lty = 3)
abline(subset(dfJ, MpNo == 7)$d18_up_lim[1], 0)
axis(side = 2, at = seq(-20, 30, 10), labels = seq(-20, 30, 10))
axis(side = 1, at = seq(min(dfJ$DT), max(dfJ$DT), 'hour'), labels = F)
box()
legend('topleft', expression(bold((b))), bty = 'n', cex = 1.2, pt.cex = 1)
legend('topright', pch = c(19, 19, 1), legend = c(expression(delta['in']), expression(delta[out]),
                                                expression(delta[italic(E)])), 
       col = c('blue', 'red', 'black'), bty = 'n', cex = 1.3, pt.cex = 1)

par(mar = c(2, 5, 2, 0))
plot(subset(dfJ, MpNo == 2)$dDH_in ~ subset(dfJ, MpNo == 2)$DT,
     ylim = c(-150, 1250), xlim = c(min(dfJ$DT), max(dfJ$DT)),
     pch =19, col = 'blue', axes = F,
     ylab = expression(paste(delta^{2}, "H (\u2030)")), xlab = '', cex.lab = 1.6)
points(subset(dfJ, MpNo == 2)$dDH_out ~ subset(dfJ, MpNo == 2)$DT, pch =19, col = 'red')
points(subset(dfJ, MpNo == 2 & ss == 'yes')$d2H_E ~ subset(dfJ, MpNo == 2 & ss == 'yes')$DT,
       pch =19, col = 'black')
points(subset(dfJ, MpNo == 2 & ss == 'no')$d2H_E ~ subset(dfJ, MpNo == 2 & ss == 'no')$DT,
       pch =1, col = 'black')
abline(subset(dfJ, MpNo == 2)$d2H_b[1], 0, lty = 2)
# no data available for the exact segment, use the tree average
abline(subset(dfJ, MpNo == 2)$d2H_a_tree[1], 0, lty = 3)
axis(side = 2, at = seq(-100, 1200, 200), labels = seq(-100, 1200, 200))
axis(side = 1, at = seq(min(subset(dfJ, MpNo == 2)$DT), max(subset(dfJ, MpNo == 2)$DT), 'hour'),
     labels = F)
box()
legend('topleft', expression(bold((c))), bty = 'n', cex = 1.2, pt.cex = 1)

par(mar = c(2, 2, 2, 3))
plot(subset(dfJ, MpNo == 7)$dDH_in ~ subset(dfJ, MpNo == 7)$DT,
     ylim = c(-150, 1250), xlim = c(min(dfJ$DT), max(dfJ$DT)),
     pch =19, col = 'blue', axes = F, ylab = '', xlab = '')
points(subset(dfJ, MpNo == 7)$dDH_out ~ subset(dfJ, MpNo == 7)$DT, pch =19, col = 'red')
points(subset(dfJ, MpNo == 7 & ss == 'yes')$d2H_E ~ subset(dfJ, MpNo == 7 & ss == 'yes')$DT,
       pch =19, col = 'black')
points(subset(dfJ, MpNo == 7 & ss == 'no')$d2H_E ~ subset(dfJ, MpNo == 7 & ss == 'no')$DT,
       pch =1, col = 'black')
abline(subset(dfJ, MpNo == 7)$d2H_b[1], 0, lty = 2)
abline(subset(dfJ, MpNo == 7)$d2H_a[1], 0, lty = 3)
axis(side = 2, at = seq(-100, 1200, 200), labels = seq(-100, 1200, 200))
axis(side = 1, at = seq(min(subset(dfJ, MpNo == 7)$DT), max(subset(dfJ, MpNo == 7)$DT), 'hour'),
     labels = F)
box()
legend('topleft', expression(bold((d))), bty = 'n', cex = 1.2, pt.cex = 1)

par(mar = c(4, 5, 0, 0))
plot(subset(dfJ, MpNo == 2)$TrA ~ subset(dfJ, MpNo == 2)$DT,
     ylim = c(0, 2), xlim = c(min(dfJ$DT), max(dfJ$DT)),
     pch = 19, col = 'darkgreen', cex.lab = 1.4,
     ylab = expression(italic(E)[leaf]~(mmol~m^-2~s^-1)), xlab = '')
lines(subset(dfJ, MpNo == 2)$TrA ~ subset(dfJ, MpNo == 2)$DT, col ='darkgreen')
legend('topleft', expression(bold((e))), bty = 'n', cex = 1.2, pt.cex = 1)

par(mar = c(4, 2, 0, 3))
plot(subset(dfJ, MpNo == 7)$TrA ~ subset(dfJ, MpNo == 7)$DT,
     ylim = c(0, 2), xlim = c(min(dfJ$DT), max(dfJ$DT)),
     pch = 19, col = 'darkgreen', ylab = '', xlab = '')
lines(subset(dfJ, MpNo == 7)$TrA ~ subset(dfJ, MpNo == 7)$DT, col ='darkgreen')
legend('topleft', expression(bold((f))), bty = 'n', cex = 1.2, pt.cex = 1)
