windows(12,8)
plot(subset(dfS, ss =='yes' & midday =='yes')$Ubark_gas ~ subset(dfS, ss =='yes' & midday =='yes')$DT,
     pch = 19, col ='blue', ylim =c(0, 1.4), ylab = 'Ubark gas (nmol/s)', xlab = '')
points(subset(dfS, ss =='yes' & midday =='no')$Ubark_gas ~ subset(dfS, ss =='yes' & midday =='no')$DT,
     pch = 1, col ='blue')
points(subset(dfS, ss =='no' & midday =='yes')$Ubark_gas ~ subset(dfS, ss =='no' & midday =='yes')$DT,
       pch = 19, col ='red')
points(subset(dfS, ss =='no' & midday =='no')$Ubark_gas ~ subset(dfS, ss =='no' & midday =='no')$DT,
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