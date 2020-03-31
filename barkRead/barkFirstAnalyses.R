source('barkRead/basicFunTEG.R')
bark <- read.csv('barkData/field_labelling.csv')
barkID <- as.data.frame(dplyr::summarise(dplyr::group_by(bark, Species, Campaing, Tree,
                                                         id, Tissue, Segment2),
                                         d2H = mean(d2H, na.rm = T), d18O = mean(d18O, na.rm = T),
                                         rwc = mean(RWC, na.rm = T)))
xylemCont <- subset(barkID, Tissue == 'xylem' &
                      Segment2 == 'control')[, c('Campaing', 'Tree', 'Species', 'd2H', 'd18O')]
names(xylemCont)[(ncol(xylemCont)-1):ncol(xylemCont)] <- c('d2H_cont', 'd18O_cont')
xylemBef <- subset(barkID, Tissue == 'xylem' &
                     Segment2 == 'before')[,c('Campaing', 'Tree', 'id', 'd2H', 'd18O')]
names(xylemBef)[(ncol(xylemBef)-1):ncol(xylemBef)] <- c('d2H_bef', 'd18O_bef')
xylemAft <- subset(barkID, Tissue == 'xylem' &
                     Segment2 == 'after')[,c('Campaing', 'Tree', 'id', 'd2H', 'd18O')]
names(xylemAft)[(ncol(xylemAft)-1):ncol(xylemAft)] <- c('d2H_aft', 'd18O_aft')
xylem <- merge(xylemBef, xylemAft, by = c('Campaing', 'Tree', 'id'), all = T)
xylem <- merge(xylem, xylemCont, by = c('Campaing', 'Tree'), all = T)
# xylem$d2H_bef_alt <- ifelse(is.na(xylem$d2H_bef), xylem$d2H_cont, xylem$d2H_bef)
# xylem$d18O_bef_alt <- ifelse(is.na(xylem$d18O_bef), xylem$d18O_cont, xylem$d18O_bef)
xylem$dif_d2H_cont.bef <- xylem$d2H_cont - xylem$d2H_bef
xylem$dif_d18O_cont.bef <- xylem$d18O_cont - xylem$d18O_bef
# xylem$dif_d2H_aft.bef <- xylem$d2H_aft - xylem$d2H_bef_alt
# xylem$dif_d18O_aft.bef <- xylem$d18O_aft - xylem$d18O_bef_alt
xylem$dif_d2H_aft.bef <- xylem$d2H_aft - xylem$d2H_bef
xylem$dif_d18O_aft.bef <- xylem$d18O_aft - xylem$d18O_bef


summer18 <- subset(xylem, Campaing == 'Summer2018')
t.test(summer18$dif_d18O_aft.bef, alternative = 'two.sided', mu = 0)
t.test(summer18$dif_d2H_aft.bef, alternative = 'two.sided', mu = 0)

autumn18 <- subset(xylem, Campaing == 'Autumn2018')
t.test(autumn18$dif_d18O_cont.bef, alternative = 'two.sided', mu = 0)
t.test(autumn18$dif_d2H_cont.bef, alternative = 'two.sided', mu = 0)
t.test(autumn18$dif_d18O_aft.bef, alternative = 'two.sided', mu = 0)
t.test(autumn18$dif_d2H_aft.bef, alternative = 'two.sided', mu = 0)

winter19 <- subset(xylem, Campaing == 'Winter2019')
t.test(winter19$dif_d18O_cont.bef, alternative = 'two.sided', mu = 0)
t.test(winter19$dif_d2H_cont.bef, alternative = 'two.sided', mu = 0)
t.test(winter19$dif_d18O_aft.bef, alternative = 'two.sided', mu = 0)
t.test(winter19$dif_d2H_aft.bef, alternative = 'two.sided', mu = 0)

summer19_Ps <- subset(xylem, Campaing == 'Summer2019' & Species == 'Pinus sylvestris')
t.test(summer19_Ps$dif_d18O_cont.bef, alternative = 'two.sided', mu = 0)
t.test(summer19_Ps$dif_d2H_cont.bef, alternative = 'two.sided', mu = 0)
t.test(summer19_Ps$dif_d18O_aft.bef, alternative = 'two.sided', mu = 0)
t.test(summer19_Ps$dif_d2H_aft.bef, alternative = 'two.sided', mu = 0)

summer19_Fs <- subset(xylem, Campaing == 'Summer2019' & Species == 'Fagus sylvatica')
t.test(summer19_Fs$dif_d18O_cont.bef, alternative = 'two.sided', mu = 0)
t.test(summer19_Fs$dif_d2H_cont.bef, alternative = 'two.sided', mu = 0)
t.test(summer19_Fs$dif_d18O_aft.bef, alternative = 'two.sided', mu = 0)
t.test(summer19_Fs$dif_d2H_aft.bef, alternative = 'two.sided', mu = 0)

xylemTreeSumm <- dplyr::summarise(dplyr::group_by(xylem, Campaing, Tree),
                             d2Hcont = mean(d2H_cont), d18Ocont = mean(d18O_cont),
                             d2Hbef = mean(d2H_bef, na.rm = T), d18Obef = mean(d18O_bef, na.rm = T),
                             d2Haft = mean(d2H_aft, na.rm = T), d18Oaft = mean(d18O_aft, na.rm = T))
xylemTreeSumm$Species <- rep('Ps', nrow(xylemTreeSumm))
xylemTreeSumm[which(xylemTreeSumm$Tree == 7 | xylemTreeSumm$Tree == 8 |
                      xylemTreeSumm$Tree == 9 | xylemTreeSumm$Tree == 10 |
                      xylemTreeSumm$Tree == 11), 'Species'] <- 'Fs'
xylemSumm <- dplyr::summarise(dplyr::group_by(xylemTreeSumm, Campaing, Species),
                              d2H_cont = mean(d2Hcont, na.rm = T), d2H_cont_se = s.err.na(d2Hcont),
                              d18O_cont = mean(d18Ocont, na.rm = T), d18O_cont_se = s.err.na(d18Ocont),
                              d2H_bef = mean(d2Hbef, na.rm = T), d2H_bef_se = s.err.na(d2Hbef),
                              d18O_bef = mean(d18Obef, na.rm = T), d18O_bef_se = s.err.na(d18Obef),
                              d2H_aft = mean(d2Haft, na.rm = T), d2H_aft_se = s.err.na(d2Haft), 
                              d18O_aft = mean(d18Oaft, na.rm = T), d18O_aft_se = s.err.na(d18Oaft))
write.csv(xylemSumm, file='barkOutput/xylemSum.csv', row.names = F)

windows(8, 9)
plot(summer18$d2H_bef ~ summer18$d18O_bef, pch = 19, col = 'darkgreen',
     xlim = c(-14, 1), ylim = c(-110, 575), ylab = 'd2H (permil)',
     xlab = 'd18O (permil)', main = 'Xylem', cex.lab = 1.3)
points(autumn18$d2H_bef ~ autumn18$d18O_bef, pch = 19, col = 'green')
points(autumn18$d2H_cont ~ autumn18$d18O_cont, pch = 15, col = 'green')
points(winter19$d2H_bef ~ winter19$d18O_bef, pch = 19, col = 'red')
points(winter19$d2H_cont ~ winter19$d18O_cont, pch = 15, col = 'red')
points(summer19_Ps$d2H_bef ~ summer19_Ps$d18O_bef, pch = 19, col = 'orange')
points(summer19_Ps$d2H_cont ~ summer19_Ps$d18O_cont, pch = 15, col = 'orange')
points(summer19_Fs$d2H_bef ~ summer19_Fs$d18O_bef, pch = 19, col = 'blue')
points(summer19_Fs$d2H_cont ~ summer19_Fs$d18O_cont, pch = 15, col = 'blue')
points(summer18$d2H_aft ~ summer18$d18O_aft, pch = 17, col = 'darkgreen')
points(autumn18$d2H_aft ~ autumn18$d18O_aft, pch = 17, col = 'green')
points(winter19$d2H_aft ~ winter19$d18O_aft, pch = 17, col = 'red')
points(summer19_Ps$d2H_aft ~ summer19_Ps$d18O_aft, pch = 17, col = 'orange')
points(summer19_Fs$d2H_aft ~ summer19_Fs$d18O_aft, pch = 17, col = 'blue')

legend('bottomright', legend = c('Sw Su Ps Bef', 'Sw Au Ps Bef', 'Sw Au Ps Ctr',
                                 'Sp Wi Ps Bef', 'Sp Wi Ps Ctr',
                                 'Sp Su Ps Bef', 'Sp Su Ps Ctr',
                                 'Sp Su Fs Bef', 'Sp Su Fs Ctr'),
       pch = c(19, 19, 15, 19, 15, 19, 15, 19, 15), bty = 'n',
       col = c('darkgreen', 'green', 'green', 'red', 'red', 'orange', 'orange',
               'blue', 'blue'))
abline(a = 10, b = 8)

# add global meteoric water (d2H = 10 + 8*d18O)