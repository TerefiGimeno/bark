library(lubridate)
library(dplyr)
dfS <- read.table('barkData/Sept_complete.csv', sep = ';', header = TRUE)
dfS$DT <- as.POSIXct(dfS$DT, format="%Y-%m-%d %H:%M:%S")
PAR <- dfS[, c('DT', 'PAR', 'Tref')]
PAR <- doBy::orderBy(~DT, PAR)
PAR$int <- difftime(PAR$DT, lag(PAR$DT), units = 'secs')
PAR$int <- ifelse(PAR$int > 10000, NA, PAR$int)
PAR$PARint <- PAR$PAR * PAR$int/1000000
PAR$Date <- as.Date(PAR$DT)
PARday <- summarise(group_by(PAR, Date), PARday = sum(PARint, na.rm = T),
                    Tmean = mean(Tref), Tmax = max(Tref), Tmin = min(Tref))
write.csv(PARday, file = 'barkOutput/climateRosinedal.csv', row.names = F)
rm(dfS, PAR, PARday)