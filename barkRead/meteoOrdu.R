fileNames <- as.list(paste0('barkData/meteo_orduna/detailed/', list.files('barkData/meteo_orduna/detailed/')))
allRaw <- dplyr::bind_rows(lapply(fileNames, data.table::fread))
names(allRaw) <- c(as.character(read.csv('barkData/meteo_Orduna/descripcion_datos.csv')[,2]))
valuesL <- strsplit(allRaw$MEDICION, ',')
allRaw$Value1 <- c(unlist(lapply(valuesL, function(L) L[1])))
allRaw$Value2 <- c(unlist(lapply(valuesL, function(L) L[2])))
write.csv(allRaw, file = 'barkData/meteo_Orduna/temp.csv', row.names = F)
clean <- read.csv('barkData/meteo_Orduna/temp.csv')
clean[which(is.na(clean$Value2)), 'Value2'] <- 0
clean$Value2 <- ifelse(clean$Value2 <= 9, clean$Value2 * 10, clean$Value2)
clean$value <- clean$Value1 + clean$Value2*0.01
clean$DateTime <- paste0(clean$FECHA_MEDICION, ' ', clean$HORA_MEDICION)
clean$DateTime <- lubridate::dmy_hm(as.character(clean$DateTime))
clean <- doBy::orderBy(~DateTime, clean)
codes <- read.csv('barkData/meteo_Orduna/codigo_meteoro.csv')[, c('CODIGO_METEORO', 'short')]
clean <- dplyr::left_join(clean, codes, by = 'CODIGO_METEORO')[,c('DateTime', 'short', 'value')]
library(dplyr)
library(tidyr)
meteo <- clean
meteo <- clean %>% spread(short, value)
meteo$Date <- as.Date(meteo$DateTime)
meteo$VPD <- 0.61365 * exp(17.502 * meteo$AirT_avg/(240.97 + meteo$AirT_avg)) * (1 -(meteo$RH_avg/100))
meteoDay <- summarise(group_by(meteo, Date), Pday = sum(P_tot),
                      Tmean = mean(AirT_avg), Tmin = min(AirT_avg),
                      Tmax = max(AirT_avg),
                      vpd = mean(VPD), vpdMax = max(VPD))