library(lubridate)
library(data.table)
library(ggplot2)

setwd('barkData/')

## ==================
## July
## ==================

dfJ <- read.table('July24_complete.csv', sep = ';', header = TRUE)
dfJ$DT <- as.POSIXct(dfJ$DT, format="%Y-%m-%d %H:%M:%S")
str(dfJ)
dfJ$DT <- as.POSIXct(dfJ$DT, format="%Y-%m-%d %H:%M")

### ==================
## Variables
## I use H2Oin_G and H2Oout_G in the calculations, becasue that has been 
## calibrated. I'm not sure about Picarro H2O
## But I can give you H2O reference values for the Picarro as well, if you don't agree with this :)
### ==================
#"MpNo" - #measurement point, i.e. cuvette number  
#"DT" - #Date and time 
#"CO2in_G" - # reference CO2 measured by gas-exchange system (GUS)
#"CO2out_G" - # cuvette CO2 measured by GUS
#"H20in_G" - # reference H2O vapour measured by GUS (mmol)
#"H2Oout_G" - # cuvette H2O vapour measured by GUS (mmol)
#"ATP" - # atmospheric pressure by GUS  (mbar)
#"Tref" - # reference temperature by GUS
#"Tcuv" - # cuvette temperature by GUS 
#"FlowIn" - # flow of air into the cuvette (umol s-1)
#"FlowOut" - # flow of air out of the cuvette (umol s-1)
#"PAR" - # irradiance by GUS (umol m-2 s-1)
#"H2O_P" - # cuvette H2O vapour by Picarro
#"mH2O_P" - # cuvette H2O by Picarro, in mmol
#"dDH_out" - # delta deuterium in the cuvette
#"d18O_out" - # d18O in the cuvette
#"dDH_in" - # reference delta deuterium
#"d18O_in" - #reference d18O
#"ShootID" - # shoot ID
#"Area" - #needle area in (mm2) 

## hours and day - for filtering and plotting
dfJ$hour <- hour(dfJ$DT)
dfJ$day <- day(dfJ$DT)
dfJ$month <- month(dfJ$DT)

## ==================
## Calculations 
## ==================
dfJ <- data.table(dfJ)

## Transpiration (mol H2O m-2 s-1)
dfJ <- data.table(dfJ[,TrA := (1000*((FlowOut/Area)*((H2Oout_G-H2Oin_G)/(ATP-H2Oin_G))))])
ggplot(dfJ, aes(x = DT, y = TrA, color = as.factor(MpNo))) +
  geom_point()

## deuterium excess
dfJ <- data.table(dfJ[,dDH_ex := (dDH_out-(8*d18O_out))]) #cuvette
dfJ <- data.table(dfJ[,dDH_ex_a := (dDH_in-(8*d18O_in))]) #air

ggplot(dfJ, aes(x = DT, y = dDH_ex, color = as.factor(MpNo_m))) +
  geom_point()

## enrichment of deuterium in the cuvette relative to ambient air
dfJ <- data.table(dfJ[,dH2O := (((H2Oout_G*dDH_ex)-(H2Oin_G*dDH_ex_a))/(H2Oout_G-H2Oin_G))])

ggplot(dfJ, aes(x = DT, y = dH2O, color = as.factor(MpNo))) +
  geom_point()

## ==================
## September
## ==================

dfS <- read.table('Sept_complete.csv', sep = ';', header = TRUE)
dfS$DT <- as.POSIXct(dfS$DT, format="%Y-%m-%d %H:%M:%S")
str(dfS)

## hours and day - for filtering and plotting
dfS$hour <- hour(dfS$DT)
dfS$day <- day(dfS$DT)
dfS$month <- month(dfS$DT)

### reduced dfS to appr. same time-frame as July 24
dfS_sub <- subset(dfS, dfS$hour %in% c('12', '13', '14', '15'))

## ==================
## Calculations 
## ==================
dfS <- data.table(dfS)

## Transpiration (mol H2O m-2 s-1)
dfS <- data.table(dfS[,TrA := (1000*((FlowOut/Area)*((H2Oout_G-H2Oin_G)/(ATP-H2Oin_G))))])

ggplot(dfS, aes(x = DT, y = TrA, color = as.factor(MpNo_m))) +
  geom_point()

## deuterium excess
dfS <- data.table(dfS[,dDH_ex := (dDH_out-(8*d18O_out))]) #cuvette
dfS <- data.table(dfS[,dDH_ex_a := (dDH_in-(8*d18O_in))]) #air

ggplot(dfS, aes(x = DT, y = dDH_ex, color = as.factor(MpNo_m))) +
  geom_point()

## enrichment of deuterium in the cuvette relative to ambient air
dfS <- data.table(dfS[,dH2O := (((H2Oout_G*dDH_ex)-(H2Oin_G*dDH_ex_a))/(H2Oout_G-H2Oin_G))])

ggplot(dfS, aes(x = DT, y = dH2O, color = as.factor(MpNo_m))) +
  geom_point()

ggplot(dfS_sub, aes(x = DT, y = dH2O, color = as.factor(MpNo))) +
  geom_point()

### =======================
### Figures
### =======================
ggplot(dfS_sub, aes(x = DT, y = dDH_in)) +
  geom_point() +
  geom_point(aes(x = DT, y = dDH_out), color = 'red') +
  labs(x = 'DateTime (appr. same hours as July24)', y = 'Delta Deuterium') +
  geom_label(label = 'red:cuvette; black:ambient air',aes(x = DT[255], y = 45)) +
  theme_bw() +
  theme(axis.text=element_text(size=8, colour = "black"),
        axis.title.y = element_text(colour = "black"),
        panel.border = element_rect(colour = "black",size=1),
        strip.background = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        text = element_text(size = 10)) +
  facet_wrap(~MpNo)
ggplot(dfS_sub, aes(x = DT, y = d18O_in)) +
  geom_point() +
  geom_point(aes(x = DT, y = d18O_out), color = 'red') +
  labs(x = 'DateTime (appr. same hours as July24)', y = 'Delta 18O') +
  geom_label(label = 'red:cuvette; black:ambient air',aes(x = DT[255], y = -10)) +
  theme_bw() +
  theme(axis.text=element_text(size=8, colour = "black"),
        axis.title.y = element_text(colour = "black"),
        panel.border = element_rect(colour = "black",size=1),
        strip.background = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        text = element_text(size = 10)) +
  facet_wrap(~MpNo)
ggplot(dfJ, aes(x = DT, y = dDH_in)) +
  geom_point() +
  geom_point(aes(x = DT, y = dDH_out), color = 'red') +
  labs(x = 'DateTime', y = 'Delta Deuterium') +
  geom_label(label = 'red:cuvette; black:ambient air',aes(x = DT[3], y = 320)) +
  theme_bw() +
  theme(axis.text=element_text(size=8, colour = "black"),
        axis.title.y = element_text(colour = "black"),
        panel.border = element_rect(colour = "black",size=1),
        strip.background = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        text = element_text(size = 10)) +
  facet_wrap(~MpNo)
ggplot(dfJ, aes(x = DT, y = d18O_in)) +
  geom_point() +
  geom_point(aes(x = DT, y = d18O_out), color = 'red') +
  labs(x = 'DateTime', y = 'Delta 18O') +
  geom_label(label = 'red:cuvette; black:ambient air',aes(x = DT[3], y = -10)) +
  theme_bw() +
  theme(axis.text=element_text(size=8, colour = "black"),
        axis.title.y = element_text(colour = "black"),
        panel.border = element_rect(colour = "black",size=1),
        strip.background = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        text = element_text(size = 10)) +
  facet_wrap(~MpNo)
ggplot(dfS_sub, aes(x = TrA, y = dDH_out)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(x = 'Transpiration (appr. same hours as July24)', y = 'delta Deuterium') +
  theme_bw() +
  theme(axis.text=element_text(size=8, colour = "black"),
        axis.title.y = element_text(colour = "black"),
        panel.border = element_rect(colour = "black",size=1),
        strip.background = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        text = element_text(size = 10)) +
  facet_wrap(~MpNo)

ggplot(dfS_sub, aes(x = TrA, y = dH2O)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(x = 'Transpiration (appr. same hours as July24)', y = 'd-ex rel') +
  theme_bw() +
  theme(axis.text=element_text(size=8, colour = "black"),
        axis.title.y = element_text(colour = "black"),
        panel.border = element_rect(colour = "black",size=1),
        strip.background = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        text = element_text(size = 10)) +
  facet_wrap(~MpNo)

ggplot(dfS_sub, aes(x = TrA, y = dDH_out)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(x = 'Transpiration (appr. same hours as July24)', y = 'd-ex out') +
  theme_bw() +
  theme(axis.text=element_text(size=8, colour = "black"),
        axis.title.y = element_text(colour = "black"),
        panel.border = element_rect(colour = "black",size=1),
        strip.background = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        text = element_text(size = 10)) +
  facet_wrap(~MpNo)

library(nlme)
summary(lme(dDH_ex ~ TrA, random = ~1|MpNo, na.action = na.exclude, data = dfS_sub))
summary(lme(dH2O ~ TrA, random = ~1|MpNo, na.action = na.exclude, data = dfS_sub))

summary(lme(dDH_ex ~ TrA, random = ~1|MpNo, na.action = na.exclude, data = dfJ))
