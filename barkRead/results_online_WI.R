library(lubridate)
library(data.table)
library(dplyr)
library(nlme)
library(ggplot2)
s.err <- function(x){
  se <- sd(x)/sqrt(length(x))
  return(se)
}

source('barkRead/read_calcs_online_WI.R')
sumJul <- summarise(group_by(dfJ, DOY, MpNo),
                    E_mean = mean(TrA), E_se = s.err(TrA),
                    d_ex = mean(dDH_ex), d_ex_se = s.err(dDH_ex),
                    d_ex_a = mean(dDH_ex_a), d_ex_a_se = s.err(dDH_ex_a),
                    n = length(dDH_ex))

summary(lme(dDH_ex ~ TrA, random = ~1|MpNo, na.action = na.exclude, data = dfJ))
summary(lme(dDH_ex ~ PAR, random = ~1|MpNo, na.action = na.exclude, data = dfJ))
summary(lme(dDH_ex ~ Tcuv, random = ~1|MpNo, na.action = na.exclude, data = dfJ))

sumSep <- summarise(group_by(subset(dfS, midday == 'yes'), DOY, MpNo),
                    E_mean = mean(TrA), E_se = s.err(TrA),
                    d_ex_mean = mean(dDH_ex), d_ex_se = s.err(dDH_ex),
                    n = length(dDH_ex))
sumSep2 <- summarise(group_by(subset(sumSep, n >= 3), MpNo), E = mean(E_mean),
                     E_se = s.err(E_mean))
summary(lme(dDH_ex ~ TrA*as.factor(DOY), random = ~1|MpNo_m, na.action = na.exclude, data = subset(dfS, midday == 'yes')))
summary(lme(dDH_ex ~ TrA*DOY, random = ~1|MpNo_m, na.action = na.exclude,
            data = subset(dfS, midday == 'yes' & DOY >= 247 & DOY < 252)))
anova(lme(dDH_ex ~ TrA*as.factor(DOY), random = ~1|MpNo_m, na.action = na.exclude, data = subset(dfS, midday == 'yes')))
model <- lme4::lmer(dDH_ex ~ TrA*as.factor(DOY) + (1|MpNo_m), na.action = na.exclude, data = subset(dfS, midday == 'yes'))
performance::r2_nakagawa(model)
summary(lme(dDH_ex ~ Tref*as.factor(DOY), random = ~1|MpNo_m, na.action = na.exclude, data = subset(dfS, midday == 'yes')))
summary(lme(dDH_ex ~ PAR*as.factor(DOY), random = ~1|MpNo_m, na.action = na.exclude, data = subset(dfS, midday == 'yes')))


summary(lm(Ubark ~ MpNo_m+as.factor(DOY), data = dfS))
anova(lm(Ubark ~ MpNo_m+as.factor(DOY), data = dfS))
boxplot(dfS$Ubark ~ dfS$MpNo_m*dfS$DOY)
