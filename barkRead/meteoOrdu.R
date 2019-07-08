fileNames <- as.list(paste0('barkData/meteo_orduna/detailed/', list.files('barkData/meteo_orduna/detailed/')))
allRaw <- dplyr::bind_rows(lapply(fileNames, data.table::fread))
names(allRaw) <- c(as.character(read.csv('barkData/meteo_Orduna/descripcion_datos.csv')[,2]))
allRaw$Value1 <- strsplit(allRaw$MEDICION, ',')[[1]][1]
allRaw$Value2 <- strsplit(allRaw$MEDICION, ',')[[1]][2]
write.csv(allRaw, file = 'barkData/meteo_Orduna/temp.csv', row.names = F)
clean <- read.csv('barkData/meteo_Orduna/temp.csv')
clean$value <- clean$Value1 + clean$Value2*0.01
clean$DateTime <- paste0(clean$FECHA_MEDICION, ' ', clean$HORA_MEDICION)
clean$DateTime <- lubridate::dmy_hm(as.character(clean$DateTime))
clean <- doBy::orderBy(~DateTime, clean)
codes <- read.csv('barkData/meteo_Orduna/codigo_meteoro.csv')[, c('CODIGO_METEORO', 'short')]
clean <- merge(clean, codes, by = 'CODIGO_METEORO', all.x = T, all.y = F)[,c('DateTime', 'short', 'value')]
library(dplyr)
library(tidyr)
meteo <- clean
meteo <- clean %>% spread(short, value)

stocks <- data.frame(
  time = as.Date('2009-01-01') + 0:9,
  X = rnorm(10, 0, 1),
  Y = rnorm(10, 0, 2),
  Z = rnorm(10, 0, 4)
)
stocksm <- stocks %>% gather(stock, price, -time)
