lengthNA <- function(x){
  return(length(x) - length(which(is.na(x))))
}
s.err <- function(x){
  return(sd(x, na.rm = T)/sqrt(lengthNA(x)))
}
temp <- read.csv('barkData/AEMET/t.csv')
tempMonth <- summarise(group_by(temp, INDICATIVO, year, month),
                       Tmean_avg = mean(TMED*0.1, na.rm = T), Tmean_se = s.err(TMED*0.1),
                       Tmin_avg = mean(TMIN*0.1, na.rm = T), Tmin_se = s.err(TMIN*0.1),
                       Tmax_avg = mean(TMAX*0.1, na.rm = T), Tmax_se = s.err(TMAX*0.1),
                       Tmean_n = lengthNA(TMED), Tmin_n = lengthNA(TMIN), Tmax_n = lengthNA(TMAX))
tempMonth[which(tempMonth$Tmean_n <= 9), c('Tmean_avg', 'Tmean_se')] <- NA
tempMonth[which(tempMonth$Tmin_n <= 9), c('Tmin_avg', 'Tmin_se')] <- NA
tempMonth[which(tempMonth$Tmax_n <= 9), c('Tmax_avg', 'Tmax_se')] <- NA
tempMonthSumm <- summarise(group_by(tempMonth, INDICATIVO, month),
                           Tmean_avg_month = mean(Tmean_avg, na.rm = T),
                           Tmean_se_month = s.err(Tmean_avg),
                           Tmin_avg_month = mean(Tmin_avg, na.rm = T),
                           Tmin_se_month = s.err(Tmin_avg),
                           Tmax_avg_month = mean(Tmax_avg, na.rm = T),
                           Tmax_se_month = s.err(Tmax_avg), Tnmonths = lengthNA(Tmean_avg))
write.csv(tempMonthSumm, file = 'barkOutput/tempMonthAEMET.csv', row.names = F)
tempYear <- summarise(group_by(temp, INDICATIVO, year),
                      Tmean_avg = mean(TMED*0.1, na.rm = T), Tmean_se = s.err(TMED*0.1),
                      Tmin_avg = mean(TMIN*0.1, na.rm = T), Tmin_se = s.err(TMIN*0.1),
                      Tmax_avg = mean(TMAX*0.1, na.rm = T), Tmax_se = s.err(TMAX*0.1),
                      Tmean_n = lengthNA(TMED), Tmin_n = lengthNA(TMIN), Tmax_n = lengthNA(TMAX))
tempYearSumm <- summarise(group_by(tempYear, INDICATIVO),
                          Tmean_avg_year = mean(Tmean_avg, na.rm = T),
                          Tmean_se_year = s.err(Tmean_avg),
                          Tmin_avg_year = mean(Tmin_avg, na.rm = T),
                          Tmin_se_year = s.err(Tmin_avg),
                          Tmax_avg_year = mean(Tmax_avg, na.rm = T),
                          Tmax_se_year = s.err(Tmax_avg), Tnmonths = lengthNA(Tmean_avg))
write.csv(tempYearSumm, file = 'barkOutput/tempYearAEMET.csv', row.names = F)
