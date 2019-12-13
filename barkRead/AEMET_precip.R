lengthNA <- function(x){
  return(length(x) - length(which(is.na(x))))
}
s.err <- function(x){
  return(sd(x, na.rm = T)/sqrt(lengthNA(x)))
}
rainWide <- read.csv('barkData/AEMET/pcp.csv')
library(dplyr)
library(tidyr)
rain <- rainWide %>% pivot_longer(cols = starts_with('P'),
                                  names_to = 'name_day', values_to = 'P_10mm')
rain$day <-  stringi::stri_replace_all_fixed(rain$name_day, "P", "0")
write.csv(rain, file='temp.csv', row.names = F)
rain <- read.csv('temp.csv')
rain <- subset(rain, year >= 1949)
stations <- read.csv('barkData/AEMET/estacionesT.csv')
rain <- left_join(rain, stations[,c('INDICATIVO', 'NOMBRE')], by = 'INDICATIVO')
rain[which(rain$P_10mm <= -3), 'P_10mm'] <- 0
rainMonth <- summarise(group_by(rain, NOMBRE, year, month),
                       Pmonth = sum(P_10mm*0.1, na.rm = T), Pndays = lengthNA(P_10mm))
rainMonth[which(rainMonth$Pndays <= 24), 'Pmonth'] <- NA
rainYear <- summarise(group_by(rain, NOMBRE, year),
                      Pyear = sum(P_10mm*0.1, na.rm = T), Pndays = lengthNA(P_10mm))
rainYear[which(rainYear$Pndays <= 293), 'Pyear'] <- NA
rainMonthSumm <- summarise(group_by(rainMonth, NOMBRE, month),
                           Pmean = mean(Pmonth, na.rm = T), 
                           Pse = s.err(Pmonth),
                           Pnyears = lengthNA(Pmonth))
rainYearSumm <- summarise(group_by(rainYear, NOMBRE),
                          Pmean = mean(Pyear, na.rm = T), 
                          Pse = s.err(Pyear),
                          Pnyears = lengthNA(Pyear))
write.csv(rainMonthSumm, file='barkOutput/rainMonthSummAEMET.csv', row.names = F)
write.csv(rainYearSumm, file='barkOutput/rainYearSummAEMET.csv', row.names = F)
