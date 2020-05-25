cuv1 <- read.csv('barkData/zsofia_dt.csv')
cuv1$DateTime <- lubridate::ymd_hms(cuv1$DateTime)
cuv1 <- doBy::orderBy(~DateTime, cuv1)
cuv1$dD_a_t <- cuv1$dD_a/8
cuv1$dD_c_t <- cuv1$dD_c/8

windows(8, 12)
par(mfrow= c(3, 1))
plot(cuv1$E ~ cuv1$DateTime, type = 'l')
plot(cuv1$d18O_c ~ cuv1$DateTime, type = 'l', col = 'blue')
lines(cuv1$d18O_a ~ cuv1$DateTime, col = 'black')
plot(cuv1$dD_c*0.125 ~ cuv1$DateTime, type = 'l', col = 'blue')
lines(cuv1$dD_a*0.125 ~ cuv1$DateTime, col = 'black')
plot(cuv1$Dex_c ~ cuv1$DateTime, type = 'l', col = 'blue')
lines(cuv1$Dex_a ~ cuv1$DateTime)
