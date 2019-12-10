lengthNA <- function(x){
  return(length(x) - length(which(is.na(x))))
}
stationName <- 'Orduna'
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
                              TminMonth=mean(TminDay_C, na.rm = T), TmaxMonth=mean(TmaxDay_C, na.rm = T),)
climMonth <- as.data.frame(climMonth)
climMonth$Pmonth <- ifelse(climMonth$Pndays <= 24, NA, climMonth$Pmonth)
climMonth[which(climMonth$Tndays <= 24), c('TmeanMonth', 'TminMonth', 'TmaxMonth')] <- NA