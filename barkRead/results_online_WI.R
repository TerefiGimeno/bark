library(lubridate)
library(data.table)
library(dplyr)
library(ggplot2)
s.err <- function(x){
  se <- sd(x)/sqrt(length(x))
  return(se)
}

dfJ <- read.table('barkData/July24_complete.csv', sep = ';', header = TRUE)
dfJ$DT <- as.POSIXct(dfJ$DT, format="%Y-%m-%d %H:%M:%S")
wiJ <- read.csv("barkData/July24_xylem_wi.csv")
dfJ <- dplyr::left_join(dfJ, wiJ, by = 'MpNo')
# calculate branch transpiration rate
dfJ$E_branch <- dfJ$TrA * dfJ$needle_area_m2
dfJ$time <- yday(dfJ$DT) + (hour(dfJ$DT)+ minute(dfJ$DT)/60)/24
dfJ$DOY <- yday(dfJ$DT)
dfJ$timeDec <- hour(dfJ$DT) + (minute(dfJ$DT)/60)
# calculate deltas (d18O and d2H) of transpired water (d_E), according to:
dfJ$d18O_E <- (dfJ$FlowOut*dfJ$H2Oout_G*dfJ$d18O_out*0.001 - dfJ$FlowIn*dfJ$H2Oin_G*dfJ$d18O_in*0.001)*1000/
  (dfJ$FlowOut*dfJ$H2Oout_G - dfJ$FlowIn*dfJ$H2Oin_G)
dfJ$ss <- ifelse(dfJ$d18O_E >= dfJ$d18_lw_lim & dfJ$d18O_E <= dfJ$d18_up_lim, 'yes', 'no')
# no measurements under steady state in July
dfJ$d2H_E <- (dfJ$FlowOut*dfJ$H2Oout_G*dfJ$dDH_out*0.001 - dfJ$FlowIn*dfJ$H2Oin_G*dfJ$dDH_in*0.001)*1000/
  (dfJ$FlowOut*dfJ$H2Oout_G - dfJ$FlowIn*dfJ$H2Oin_G)
sumJul <- summarise(group_by(dfJ, DOY, MpNo),
                    E_mean = mean(TrA), E_se = s.err(TrA),
                    d_ex = mean(dDH_ex), d_ex_se = s.err(dDH_ex),
                    d_ex_a = mean(dDH_ex_a), d_ex_a_se = s.err(dDH_ex_a),
                    n = length(dDH_ex))
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
dfS$timeDec <- hour(dfS$DT)+ (minute(dfS$DT)/60)
# calculate deltas (d18O and d2H) of transpired water (d_E), according to:
dfS$d18O_E <- (dfS$FlowOut*dfS$H2Oout_G*dfS$d18O_out*0.001 - dfS$FlowIn*dfS$H2Oin_G*dfS$d18O_in*0.001)*1000/
  (dfS$FlowOut*dfS$H2Oout_G - dfS$FlowIn*dfS$H2Oin_G)
dfS$ss <- ifelse(dfS$d18O_E >= dfS$d18_lw_lim & dfS$d18O_E <= dfS$d18_up_lim, 'yes', 'no')

dfS$d2H_E <- (dfS$FlowOut*dfS$H2Oout_G*dfS$dDH_out*0.001 - dfS$FlowIn*dfS$H2Oin_G*dfS$dDH_in*0.001)*1000/
  (dfS$FlowOut*dfS$H2Oout_G - dfS$FlowIn*dfS$H2Oin_G)
dfS$midday <- ifelse(dfS$timeDec >= 10 & dfS$timeDec <= 16 & dfS$PAR >= 300, 'yes', 'no')

sumSep <- summarise(group_by(subset(dfS, midday == 'yes'), DOY, MpNo),
                    E_mean = mean(TrA), E_se = s.err(TrA),
                    n = length(dDH_ex))
sumSep2 <- summarise(group_by(subset(sumSep, n >= 3), MpNo), E = mean(E_mean),
                     E_se = s.err(E_mean))
summary(lme(dDH_ex ~ TrA, random = ~1|MpNo, na.action = na.exclude, data = subset(dfS, midday == 'yes')))
summary(lme(dH2O ~ TrA, random = ~1|MpNo, na.action = na.exclude, data = dfS_sub))