###previous####

library(dplyr)
library(lubridate)
library(ggplot2)
calcSatVap <- function(temp){
  e <- 0.61365 * exp(17.502 * temp/(240.97 + temp))
  return(e)
}

####read July 2018 data####

dfJ <- read.table('barkData/July24_complete.csv', sep = ';', header = TRUE)
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

####read September 2018 data####

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
dfS$Date <- as.Date(dfS$DT)
dfS$DOY <- yday(dfS$DT)
dfS$fDOY <- as.factor(dfS$DOY)
dfS$timeDec <- hour(dfS$DT)+ (minute(dfS$DT)/60)
dfS$midday <- ifelse(dfS$timeDec >= 10 & dfS$timeDec <= 16 & dfS$PAR >= 300, 'yes', 'no')
# calculate deltas (d18O and d2H) of transpired water (d_E), according to:
dfS$d18O_E <- (dfS$FlowOut*dfS$H2Oout_G*dfS$d18O_out*0.001 - dfS$FlowIn*dfS$H2Oin_G*dfS$d18O_in*0.001)*1000/
  (dfS$FlowOut*dfS$H2Oout_G - dfS$FlowIn*dfS$H2Oin_G)
dfS$ss <- ifelse(dfS$d18O_E <= dfS$d18_up_lim & dfS$PAR >= 100 &
                   dfS$timeDec >= 10 & dfS$timeDec < 18 &
                   dfS$DOY >= 247, 'yes', 'no')
dfS$d2H_E <- (dfS$FlowOut*dfS$H2Oout_G*dfS$dDH_out*0.001 - dfS$FlowIn*dfS$H2Oin_G*dfS$dDH_in*0.001)*1000/
  (dfS$FlowOut*dfS$H2Oout_G - dfS$FlowIn*dfS$H2Oin_G)
# 2H/H molar fraction of VSMOW in micromol/mol:
RVSMOW_2H <- 155.76
# calculate 2H/H from deltas in micromol/mol:
dfS$R_2H_E <- (dfS$d2H_E*0.001 +1)*RVSMOW_2H
dfS$R_2H_b <- (dfS$d2H_b*0.001 +1)*RVSMOW_2H
dfS$R_2H_a <- (dfS$d2H_a*0.001 +1)*RVSMOW_2H
# 2H/H molar fraction of water injected to the bandage in micromol/mol:
Rtracer_2H <- 16124.41
# uptake of tracer through the bark in micromol/s:
# dfS$Ubark <- dfS$R_2H_E * dfS$E_branch *1000/Rtracer_2H
dfS$Ubark_old <- dfS$E_branch*(dfS$R_2H_E - dfS$d2H_b)/(Rtracer_2H - dfS$d2H_b)
dfS$Ubark <- dfS$E_branch*(dfS$R_2H_E - dfS$R_2H_b)/(Rtracer_2H - dfS$R_2H_b)
dfS$Ubark_alt <- dfS$E_branch*(dfS$R_2H_a - dfS$R_2H_b)/(Rtracer_2H - dfS$R_2H_b)
# calculate saturated vapor deficit for a given temperature in mol mol-1
# first calculate e_sat in kPa and convert to mbar (multiply by 10),
# then convert to mol mol-1 dividing by atmospheric pressure in mbar
dfS$e_sat <- calcSatVap(dfS$Tref)*10/dfS$ATP
# bark conductance to H2O: 1mmol m-2 s-1
# based on measurements for Co2 on Pinus monticola 
# Cernusak et al. 2001 Oecologia
gbark <- 1
# bark transpiration under the bandage in mmol s-1
dfS$Ebark <- gbark*dfS$e_sat*dfS$band_surface_m2
# isotopic vapour-phase diffusion flow through the bark into the xylem in nmol/s
dfS$Ubark_gas <- Rtracer_2H*1e-06*gbark*0.001*dfS$e_sat*dfS$band_surface_m2*1e09

dfS_summ <- dfS %>%
  subset(ss == 'yes' & DOY != 252) %>%
  group_by(MpNo, Date) %>%
  summarise(Ubark_avg = mean(Ubark, na.rm = T), Ubark_se = s.err.na(Ubark),
            Ubark_old_avg = mean(Ubark_old, na.rm = T), Ubark_old_se = s.err.na(Ubark_old),
            Ubark_alt_avg = mean(Ubark_alt, na.rm = T), Ubark_alt_se = s.err.na(Ubark_alt),
            Ubark_N = lengthWithoutNA(Ubark),
            Ubark_gas_avg = mean(Ubark_gas, na.rm = T),
            Ubark_gas_se = s.err.na(Ubark_gas),
            E_avg = mean(TrA, na.rm = T), E_se = s.err.na(TrA))
myNames <- data.frame(row.names = c(1:4))
myNames$MpNo <- c(1, 2, 7, 8)
myNames$Cuv. <- ifelse(myNames$MpNo == 1, 'Cuv. A', 'Cuv. D')
myNames[which(myNames$MpNo == 2), 'Cuv.'] <- 'Cuv. B'
myNames[which(myNames$MpNo == 7), 'Cuv.'] <- 'Cuv. C'
dfS_summ <- left_join(dfS_summ, myNames, by = 'MpNo')

#### calculate daily mean values of Ubark-gas####

kk <- doBy::summaryBy(Ubark_gas_avg + Ubark_avg ~ Date, FUN = c(mean, s.err), data = dfS_summ)
round(mean(kk$Ubark_avg.mean*1000), 2)
round(s.err(kk$Ubark_avg.mean*1000), 2)
round(mean(kk$Ubark_gas_avg.mean), 2)
round(s.err(kk$Ubark_gas_avg.mean), 2)
round(max(kk$Ubark_gas_avg.mean), 2)
round(kk[which.max(kk$Ubark_gas_avg.mean), 'Ubark_gas_avg.s.err'], 2)

summary(aov(Ubark ~ MpNo_m * fDOY, data = dfS))

####graph Ubark over time ####

windows(12, 8)
ggplot(dfS_summ, aes(x=Date, y=Ubark_avg, shape = Cuv.)) + 
  geom_errorbar(aes(ymin=Ubark_avg - Ubark_se, ymax=Ubark_avg + Ubark_se), width=.1) +
  geom_line()+
  scale_shape_manual(values = c(19, 15, 18, 17)) +
  geom_point(aes(colour = E_avg), size = 5) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(col=expression(italic(E)[leaf]~(mol~m^-2~s^-1)), shape=" ") +
  scale_x_date(date_breaks = "days", date_labels = "%d-%b")+
  labs(title = ' ', x='', y = expression(italic(U)[bark]~(mmol~s^-1)), size = rel(2))+
  theme(axis.text = element_text(size = rel(1.75))) +
  theme(axis.title.y = element_text(size = rel(2))) +
  scale_fill_manual(name = " ", values = c(rep('white', 5))) +
  theme(legend.key = element_blank(), legend.position = c(0.9, 0.75))+
  theme(legend.title = element_text(size = rel(1.3))) +
  theme(legend.text=element_text(size=rel(1.15)))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(colour = "black"))+
  theme(panel.border = element_rect(colour = "black", fill=NA))
