library(lubridate)
library(data.table)
library(ggplot2)

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
dfS$DOY <- yday(dfS$DT)
# calculate deltas (d18O and d2H) of transpired water (d_E), according to:
dfS$d18O_E <- (dfS$FlowOut*dfS$H2Oout_G*dfS$d18O_out*0.001 - dfS$FlowIn*dfS$H2Oin_G*dfS$d18O_in*0.001)*1000/
  (dfS$FlowOut*dfS$H2Oout_G - dfS$FlowIn*dfS$H2Oin_G)
dfS$ss <- ifelse(dfS$d18O_E >= dfS$d18_lw_lim & dfS$d18O_E <= dfS$d18_up_lim, 'yes', 'no')

dfS$d2H_E <- (dfS$FlowOut*dfS$H2Oout_G*dfS$dDH_out*0.001 - dfS$FlowIn*dfS$H2Oin_G*dfS$dDH_in*0.001)*1000/
  (dfS$FlowOut*dfS$H2Oout_G - dfS$FlowIn*dfS$H2Oin_G)
# calculation of molar fraction of 2H
# 155.76 is H2/H for VSMOW
dfS$D_ppm <- (dfS$d2H_E*0.001+1)*155.76
# calculate uptake rate with 2Rtracer = 16125 ppm
dfS$Ubr <- dfS$D_ppm * dfS$E_branch *1000/ 16125

ggplot(dfS, aes(x = DT, y = Ubr)) +
  geom_point() +
  geom_point(aes(x = DT, y = Ubr), color = 'black') +
  labs(x = 'DateTime', y = 'Ubranch (umol s-1)') +
  theme_bw() +
  theme(axis.text=element_text(size=8, colour = "black"),
        axis.title.y = element_text(colour = "black"),
        panel.border = element_rect(colour = "black",size=1),
        strip.background = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        text = element_text(size = 10)) +
  facet_wrap(~MpNo)

myMpNo <- unique(dfS$MpNo)

windows(12,8)
par(mfrow=c(2,2), mar = c(4, 5, 1, 1))
for (i in 1:length(myMpNo)){
  plot(subset(dfS, MpNo == myMpNo[i])$d18O_in ~ subset(dfS, MpNo == myMpNo[i])$DT,
       pch =19, col = 'blue', ylim = c(-25, 30),
       ylab = expression(paste(delta^{18}, "O (\u2030)")), xlab = '', cex.lab = 1.25,
       main = paste0('Cuv ', subset(dfS, MpNo == myMpNo[i])$MpNo[1]))
  points(subset(dfS, MpNo == myMpNo[i])$d18O_out ~ subset(dfS, MpNo == myMpNo[i])$DT, pch =19, col = 'red')
  points(subset(dfS, MpNo == myMpNo[i])$d18O_E ~ subset(dfS, MpNo == myMpNo[i])$DT, pch =19, col = 'black')
  points(subset(dfS, MpNo == myMpNo[i] & ss == 'yes')$d18O_E ~ subset(dfS, MpNo == myMpNo[i] & ss == 'yes')$DT,
         pch =19, col = 'green')
  abline(subset(dfS, MpNo == myMpNo[i])$d18O_b[1], 0, lty = 2)
  abline(subset(dfS, MpNo == myMpNo[i])$d18O_a[1], 0, lty = 3)
  abline(subset(dfS, MpNo == myMpNo[i])$d18_up_lim[1], 0)
  abline(subset(dfS, MpNo == myMpNo[i])$d18_lw_lim[1], 0)
}
legend('topleft', legend = c('in', 'out', 'E', 'SS', 'xyl bef', 'xyl aft'),
       pch = c(19, 19, 19, 19, NA, NA), lty=c(NA, NA, NA, NA, 2, 3),
       col = c('blue', 'red', 'black', 'green', 'black', 'black'), bty = 'n')

windows(12,8)
par(mfrow=c(2,2), mar = c(4, 5, 1, 1))
for (i in 1:length(myMpNo)){
  plot(subset(dfS, MpNo == myMpNo[i])$dDH_in ~ subset(dfS, MpNo == myMpNo[i])$DT,
       pch =19, col = 'blue', ylim = c(-200, 600),
       ylab = expression(paste(delta^{2}, "H (\u2030)")), xlab = '', cex.lab = 1.25,
       main = paste0('Cuv ', subset(dfS, MpNo == myMpNo[i])$MpNo[1]))
  points(subset(dfS, MpNo == myMpNo[i])$dDH_out ~ subset(dfS, MpNo == myMpNo[i])$DT, pch =19, col = 'red')
  points(subset(dfS, MpNo == myMpNo[i])$d2H_E ~ subset(dfS, MpNo == myMpNo[i])$DT, pch =19, col = 'black')
  abline(subset(dfS, MpNo == myMpNo[i])$d2H_b[1], 0, lty = 2)
  abline(subset(dfS, MpNo == myMpNo[i])$d2H_a[1], 0, lty = 3)
}
legend('topleft', legend = c('in', 'out', 'E', 'xyl bef', 'xyl aft'),
       pch = c(19, 19, 19, NA, NA), lty=c(NA, NA, NA, 2, 3),
       col = c('blue', 'red', 'black','black', 'black'), bty = 'n')
