windows(12,8)
par(mfrow = c(1,3), mar = c(4, 6, 4, 0.5))
plot(tree$swEx_gapF ~ tree$Nday, pch = 17, col = 'green',
     ylim = c(min(tree$swEx_gapF, na.rm=T), max(tree$swEx_gapF, na.rm=T)),
     ylab=expression(Soil~water~excess~('\211')), xlab = ' ', cex.lab = 1.8)
points(tree$avg_SWexcess ~ tree$Nday, pch = 19, col = 'black')
lines(tree$mv_avg_swEx_2 ~ tree$Nday, lwd = 2, col = 'blue')
lines(tree$mv_avg_swEx ~ tree$Nday, lwd = 2, col = 'darkgrey')
legend('topleft', legend = c('Data', 'Gap-f', 'Mv Avg Gap-f', 'Mv Avg'), pch=c(19, 17, NA, NA),
   lty=c(NA, NA, 1, 1), col=c('black','green','blue','darkgrey'), lwd = 2, bty = 'n', cex = 1.8)

plot(tree$d18O_gapF ~ tree$Nday, pch = 17, col = 'green',
     ylim = c(min(tree$d18O_gapF, na.rm=T), max(tree$d18O_gapF, na.rm=T)),
     ylab=expression(delta^18*O~('\211')), xlab = ' ', cex.lab = 1.8, main = 'Tree 6')
points(tree$liq_O18 ~ tree$Nday, pch = 19, col = 'black')
lines(tree$mv_avg_d18O_2 ~ tree$Nday, lwd = 2, col = 'blue')
lines(tree$mv_avg_d18O ~ tree$Nday, lwd = 2, col = 'darkgrey')

plot(tree$d2H_gapF ~ tree$Nday, pch = 17, col = 'green',
     ylim = c(min(tree$d2H_gapF, na.rm=T), max(tree$d2H_gapF, na.rm=T)),
     ylab=expression(delta^2*H~('\211')), xlab = ' ', cex.lab = 1.8)
points(tree$liq_2H ~ tree$Nday, pch = 19, col = 'black')
lines(tree$mv_avg_d2H_2 ~ tree$Nday, lwd = 2, col = 'blue')
lines(tree$mv_avg_d2H ~ tree$Nday, lwd = 2, col = 'darkgrey')
