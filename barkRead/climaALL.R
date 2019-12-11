lengthNA <- function(x){
  return(length(x) - length(which(is.na(x))))
}
s.err <- function(x){
  return(sd(x, na.rm = T)/sqrt(lengthNA(x)))
}
stationName <- 'Amurrio'
Precip <- read.csv(paste0('barkData/meteo_Orduna/Pday_', stationName, '.csv'))
Tmean <- read.csv(paste0('barkData/meteo_Orduna/TmeanDay_', stationName, '.csv'))
Tmin <- read.csv(paste0('barkData/meteo_Orduna/TminDay_', stationName, '.csv'))
Tmax <- read.csv(paste0('barkData/meteo_Orduna/TmaxDay_', stationName, '.csv'))
clim <- merge(merge(merge(Precip[, c('YYYYMMDD', 'Pday_mm')],
                          Tmean[, c('YYYYMMDD', 'TmeanDay_C')], by='YYYYMMDD', all=T),
                        Tmin[, c('YYYYMMDD', 'TminDay_C')], by='YYYYMMDD', all=T),
              Tmax[, c('YYYYMMDD', 'TmaxDay_C')], by='YYYYMMDD', all=T)
clim$Date <- lubridate::ymd(as.character(clim$YYYYMMDD))
clim$Year <- lubridate::year(clim$Date)
clim$Month <- lubridate::month(clim$Date, label = T)
clim$Month_Y <- paste0(clim$Year, '-', clim$Month)
climMonth <- dplyr::summarise(dplyr::group_by(clim, Month_Y),
                              Pmonth=sum(Pday_mm, na.rm = T), Pndays=lengthNA(Pday_mm),
                              TmeanMonth=mean(TmeanDay_C, na.rm = T), Tndays=lengthNA(TmeanDay_C),
                              TminMonth=mean(TminDay_C, na.rm = T), TmaxMonth=mean(TmaxDay_C, na.rm = T))
climMonth$Pmonth <- ifelse(climMonth$Pndays <= 24, NA, climMonth$Pmonth)
climMonth[which(climMonth$Tndays <= 24), c('TmeanMonth', 'TminMonth', 'TmaxMonth')] <- NA
climMonth$Month <- substr(climMonth$Month_Y, 6, 8)
climMonth$Year <- substr(climMonth$Month_Y, 1, 4)
climMonthSumm <- dplyr::summarise(dplyr::group_by(climMonth, Month),
                                  Pavg_month = mean(Pmonth, na.rm = T), Pse_month = s.err(Pmonth),
                                  Pnmonths = lengthNA(Pmonth),
                                  Tmean_avg_month = mean(TmeanMonth, na.rm = T),
                                  Tmean_se_month = s.err(TmeanMonth),
                                  Tmin_avg_month = mean(TminMonth, na.rm = T),
                                  Tmin_se_month = s.err(TminMonth),
                                  Tmax_avg_month = mean(TmaxMonth, na.rm = T),
                                  Tmax_se_month = s.err(TmaxMonth),
                                  Tnmonths = lengthNA(TmeanMonth))
climYear <- dplyr::summarise(dplyr::group_by(clim, Year),
                                          Pyear=sum(Pday_mm, na.rm = T), Pndays=lengthNA(Pday_mm),
                                          TmeanYear=mean(TmeanDay_C, na.rm = T),
                                          TminYear=mean(TminDay_C, na.rm = T),
                                          TmaxYear=mean(TmaxDay_C, na.rm = T),
                                          Tndays=lengthNA(TmeanDay_C))
climYear[which(climYear$Pndays <= 293), 'Pyear'] <- NA 
climYear[which(climYear$Tndays <= 293), c('TmeanYear', 'TminYear', 'TmaxYear')] <- NA
climYearSumm <- dplyr::summarise(dplyr::group_by(climYear),
                                  Pavg_year = mean(Pyear, na.rm = T), Pse_year = s.err(Pyear),
                                  Pnyears = lengthNA(Pyear),
                                  Tmean_avg_year = mean(TmeanYear, na.rm = T),
                                  Tmean_se_year = s.err(TmeanYear),
                                  Tmin_avg_year = mean(TminYear, na.rm = T),
                                  Tmin_se_year = s.err(TminYear),
                                  Tmax_avg_year = mean(TmaxYear, na.rm = T),
                                  Tmax_se_year = s.err(TmaxYear),
                                  Tnmonths = lengthNA(TmeanYear))

climYearSumm
climMonthSumm
