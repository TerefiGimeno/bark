library(lubridate)
tree <- read.csv('tree6clean.csv')
tree$Date <- dmy(as.character(tree$Date))
tree$Nday <- as.numeric(tree$Date - as.Date('2018-07-10'))
mySeries <- c(1:max(tree$Nday))
splswtree <- spline(x = tree$Nday, y = tree$avg_SWexcess, xout = mySeries)
spld2Htree <- spline(x = tree$Nday, y = tree$liq_2H, xout = mySeries)
spld18Otree <- spline(x = tree$Nday, y = tree$liq_O18, xout = mySeries)
predtree <- data.frame(row.names = 1:length(splswtree$x))
predtree$Nday <- splswtree$x
predtree$SWexPred <- splswtree$y
predtree$d2Hpred <- spld2Htree$y
predtree$d18Opred <- spld18Otree$y
tree <- merge(tree, predtree, by = 'Nday', all = T)
tree$binomial <- ifelse(is.na(tree$avg_SWexcess), 0, 1)
tree$flag1 <- dplyr::lead(tree$binomial, 1)
tree$flag2 <- dplyr::lead(tree$binomial, 2)
tree$flag3 <- dplyr::lead(tree$binomial, 3)
tree$flag4 <- dplyr::lag(tree$binomial, 1)
tree$flag5 <- dplyr::lag(tree$binomial, 2)
tree$flag6 <- dplyr::lag(tree$binomial, 3)
tree$flagBefore <- rowSums(tree[, c(paste0('flag', 4:6))], na.rm = T)
tree$flagAfter <- rowSums(tree[, c(paste0('flag', 1:3))], na.rm = T)
tree$flagSandwich <- rowSums(tree[, c('flag1','flag4')], na.rm = T)
tree$flag <- rowSums(tree[, c(paste0('flag', 1:6))], na.rm = T)
tree$FLAG <- ifelse(tree$flagAfter == 0 | tree$flagBefore == 0 | tree$flagSandwich == 0, 0, 1)
tree[which(tree$flag <= 3 & tree$FLAG == 0), c('SWexPred', 'd2Hpred', 'd18Opred')] <- NA
tree$swEx_gapF <- ifelse(is.na(tree$avg_SWexcess), tree$SWexPred, tree$avg_SWexcess)
tree$d18O_gapF <- ifelse(is.na(tree$liq_O18), tree$d18Opred, tree$liq_O18)
tree$d2H_gapF <- ifelse(is.na(tree$liq_2H), tree$d2Hpred, tree$liq_2H)
tree$swEx_lag1 <- dplyr::lag(tree$swEx_gapF, 1)
tree$swEx_lag2 <- dplyr::lag(tree$swEx_gapF, 2)
tree$swEx_lead1 <- dplyr::lead(tree$swEx_gapF, 1)
tree$swEx_lead2 <- dplyr::lead(tree$swEx_gapF, 2)
tree$mv_avg_swEx <- rowMeans(tree[,c("swEx_gapF", 'swEx_lag1', 'swEx_lag2',
                                       'swEx_lead1', 'swEx_lead2')])
tree$mv_avg_swEx_2 <- rowMeans(tree[,c("swEx_gapF", 'swEx_lag1', 'swEx_lag2',
                                       'swEx_lead1', 'swEx_lead2')], na.rm = T)
tree[is.na(tree$swEx_gapF), 'mv_avg_swEx_2'] <- NA
tree$d18O_lag1 <- dplyr::lag(tree$d18O_gapF, 1)
tree$d18O_lag2 <- dplyr::lag(tree$d18O_gapF, 2)
tree$d18O_lead1 <- dplyr::lead(tree$d18O_gapF, 1)
tree$d18O_lead2 <- dplyr::lead(tree$d18O_gapF, 2)
tree$mv_avg_d18O <- rowMeans(tree[,c("d18O_gapF", 'd18O_lag1', 'd18O_lag2',
                                       'd18O_lead1', 'd18O_lead2')])
tree$mv_avg_d18O_2 <- rowMeans(tree[,c("d18O_gapF", 'd18O_lag1', 'd18O_lag2',
                                       'd18O_lead1', 'd18O_lead2')], na.rm = T)
tree[is.na(tree$d18O_gapF), 'mv_avg_d18O_2'] <- NA
tree$d2H_lag1 <- dplyr::lag(tree$d2H_gapF, 1)
tree$d2H_lag2 <- dplyr::lag(tree$d2H_gapF, 2)
tree$d2H_lead1 <- dplyr::lead(tree$d2H_gapF, 1)
tree$d2H_lead2 <- dplyr::lead(tree$d2H_gapF, 2)
tree$mv_avg_d2H <- rowMeans(tree[,c("d2H_gapF", 'd2H_lag1', 'd2H_lag2',
                                       'd2H_lead1', 'd2H_lead2')])
tree$mv_avg_d2H_2 <- rowMeans(tree[,c("d2H_gapF", 'd2H_lag1', 'd2H_lag2',
                                      'd2H_lead1', 'd2H_lead2')], na.rm=T)
tree[is.na(tree$d2H_gapF), 'mv_avg_d2H_2'] <- NA
