windows(12,8)
plot(subset(dfS, ss =='yes' & midday =='yes')$Ubark_gas*0.001 ~ subset(dfS, ss =='yes' & midday =='yes')$DT,
     pch = 19, col ='blue', ylim =c(0, 0.0014), ylab = 'Ubark gas (mumol/s)', xlab = '')
points(subset(dfS, ss =='yes' & midday =='no')$Ubark_gas*0.001 ~ subset(dfS, ss =='yes' & midday =='no')$DT,
     pch = 1, col ='blue')
points(subset(dfS, ss =='no' & midday =='yes')$Ubark_gas*0.001 ~ subset(dfS, ss =='no' & midday =='yes')$DT,
       pch = 19, col ='red')
points(subset(dfS, ss =='no' & midday =='no')$Ubark_gas*0.001 ~ subset(dfS, ss =='no' & midday =='no')$DT,
       pch = 1, col ='red')

windows(20, 12)
par(mfrow=c(3,1))
plot(subset(dfS, ss =='yes' & midday =='yes')$Ubark_old ~ subset(dfS, ss =='yes' & midday =='yes')$DT,
     pch = 19, col ='blue', ylim =c(-0.02, 0.05), ylab = 'Ubark_old (mumol/s)', xlab = '')
points(subset(dfS, ss =='yes' & midday =='no')$Ubark_old ~ subset(dfS, ss =='yes' & midday =='no')$DT,
     pch = 1, col ='blue')
points(subset(dfS, ss =='no' & midday =='yes')$Ubark_old ~ subset(dfS, ss =='no' & midday =='yes')$DT,
       pch = 19, col ='red')
points(subset(dfS, ss =='no' & midday =='no')$Ubark_old ~ subset(dfS, ss =='no' & midday =='no')$DT,
       pch = 1, col ='red')

plot(subset(dfS, ss =='yes' & midday =='yes')$Ubark ~ subset(dfS, ss =='yes' & midday =='yes')$DT,
     pch = 19, col ='blue', ylim =c(-0.002, 0.004), ylab = 'Ubark (mumol/s)', xlab = '')
points(subset(dfS, ss =='yes' & midday =='no')$Ubark ~ subset(dfS, ss =='yes' & midday =='no')$DT,
       pch = 1, col ='blue')
points(subset(dfS, ss =='no' & midday =='yes')$Ubark ~ subset(dfS, ss =='no' & midday =='yes')$DT,
       pch = 19, col ='red')
points(subset(dfS, ss =='no' & midday =='no')$Ubark ~ subset(dfS, ss =='no' & midday =='no')$DT,
       pch = 1, col ='red')
abline(0, 0)

plot(subset(dfS, ss =='yes' & midday =='yes')$Ubark_alt ~ subset(dfS, ss =='yes' & midday =='yes')$DT,
     pch = 19, col ='blue', ylim =c(0, 0.02), ylab = 'Ubark (mumol/s)', xlab = '')
points(subset(dfS, ss =='yes' & midday =='no')$Ubark_alt ~ subset(dfS, ss =='yes' & midday =='no')$DT,
       pch = 1, col ='blue')
points(subset(dfS, ss =='no' & midday =='yes')$Ubark_alt ~ subset(dfS, ss =='no' & midday =='yes')$DT,
       pch = 19, col ='red')
points(subset(dfS, ss =='no' & midday =='no')$Ubark_alt ~ subset(dfS, ss =='no' & midday =='no')$DT,
       pch = 1, col ='red')
plot(dfS$R_2H_E ~ dfS$DT, pch =19, ylim =c(0, 220))
points(dfS$R_2H_a ~ dfS$DT, pch = 19, col = 'red')
points(dfS$R_2H_b ~ dfS$DT, pch = 19, col = 'blue')

windows(12, 8)
par(mfrow=c(1,2))
plot(dfS$d18O_E ~ dfS$DT, pch =19, ylim = c(-30, 15))
points(dfS$d18O_a ~ dfS$DT, pch = 19, col = 'red')

plot(dfS$d2H_E ~ dfS$DT, pch =19, ylim = c(-250, 220))
points(dfS$d2H_b ~ dfS$DT, pch = 19, col = 'blue')
points(dfS$d2H_a ~ dfS$DT, pch = 19, col = 'red')
