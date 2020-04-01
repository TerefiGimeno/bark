source('barkRead/basicFunTEG.R')
bark <- read.csv('barkData/field_labelling.csv')
barkID <- as.data.frame(dplyr::summarise(dplyr::group_by(bark, Species, Campaing, Tree,
                                                         id, Tissue, Segment2),
                                         d2H = mean(d2H, na.rm = T), d18O = mean(d18O, na.rm = T),
                                         rwc = mean(RWC, na.rm = T)))
xylemCont <- subset(barkID, Tissue == 'xylem' &
                      Segment2 == 'control')[, c('Campaing', 'Tree', 'Species', 'rwc','d2H', 'd18O')]
names(xylemCont)[(ncol(xylemCont)-2):ncol(xylemCont)] <- c('rwc_cont', 'd2H_cont', 'd18O_cont')
xylemBef <- subset(barkID, Tissue == 'xylem' &
                     Segment2 == 'before')[,c('Campaing', 'Tree', 'id', 'rwc','d2H', 'd18O')]
names(xylemBef)[(ncol(xylemBef)-2):ncol(xylemBef)] <- c('rwc_bef', 'd2H_bef', 'd18O_bef')
xylemAft <- subset(barkID, Tissue == 'xylem' &
                     Segment2 == 'after')[,c('Campaing', 'Tree', 'id', 'rwc', 'd2H', 'd18O')]
names(xylemAft)[(ncol(xylemAft)-2):ncol(xylemAft)] <- c('rwc_aft', 'd2H_aft', 'd18O_aft')
xylem <- merge(xylemBef, xylemAft, by = c('Campaing', 'Tree', 'id'), all = F)
xylem <- merge(xylem, xylemCont, by = c('Campaing', 'Tree'), all = T)
# xylem$d2H_bef_alt <- ifelse(is.na(xylem$d2H_bef), xylem$d2H_cont, xylem$d2H_bef)
# xylem$d18O_bef_alt <- ifelse(is.na(xylem$d18O_bef), xylem$d18O_cont, xylem$d18O_bef)
xylem$dif_d2H_cont.bef <- xylem$d2H_cont - xylem$d2H_bef
xylem$dif_d18O_cont.bef <- xylem$d18O_cont - xylem$d18O_bef
# xylem$dif_d2H_aft.bef <- xylem$d2H_aft - xylem$d2H_bef_alt
# xylem$dif_d18O_aft.bef <- xylem$d18O_aft - xylem$d18O_bef_alt
xylem$dif_d2H_aft.bef <- xylem$d2H_aft - xylem$d2H_bef
xylem$dif_d18O_aft.bef <- xylem$d18O_aft - xylem$d18O_bef
xylem$dif_rwc_aft.bef <- xylem$rwc_aft - xylem$rwc_bef


summer18 <- subset(xylem, Campaing == 'Summer2018')
t.test(summer18$dif_d18O_aft.bef, alternative = 'two.sided', mu = 0)
t.test(summer18$dif_d2H_aft.bef, alternative = 'two.sided', mu = 0)
t.test(summer18$dif_rwc_aft.bef, alternative = 'two.sided', mu = 0)

autumn18 <- subset(xylem, Campaing == 'Autumn2018')
t.test(autumn18$dif_d18O_cont.bef, alternative = 'two.sided', mu = 0)
t.test(autumn18$dif_d2H_cont.bef, alternative = 'two.sided', mu = 0)
t.test(autumn18$dif_d18O_aft.bef, alternative = 'two.sided', mu = 0)
t.test(autumn18$dif_d2H_aft.bef, alternative = 'two.sided', mu = 0)
t.test(autumn18$dif_rwc_aft.bef, alternative = 'two.sided', mu = 0)

winter19 <- subset(xylem, Campaing == 'Winter2019')
t.test(winter19$dif_d18O_cont.bef, alternative = 'two.sided', mu = 0)
t.test(winter19$dif_d2H_cont.bef, alternative = 'two.sided', mu = 0)
t.test(winter19$dif_d18O_aft.bef, alternative = 'two.sided', mu = 0)
t.test(winter19$dif_d2H_aft.bef, alternative = 'two.sided', mu = 0)
t.test(winter19$dif_rwc_aft.bef, alternative = 'two.sided', mu = 0)

summer19_Ps <- subset(xylem, Campaing == 'Summer2019' & Species == 'Pinus sylvestris')
t.test(summer19_Ps$dif_d18O_cont.bef, alternative = 'two.sided', mu = 0)
t.test(summer19_Ps$dif_d2H_cont.bef, alternative = 'two.sided', mu = 0)
t.test(summer19_Ps$dif_d18O_aft.bef, alternative = 'two.sided', mu = 0)
t.test(summer19_Ps$dif_d2H_aft.bef, alternative = 'two.sided', mu = 0)
t.test(summer19_Ps$dif_rwc_aft.bef, alternative = 'two.sided', mu = 0)

summer19_Fs <- subset(xylem, Campaing == 'Summer2019' & Species == 'Fagus sylvatica')
t.test(summer19_Fs$dif_d18O_cont.bef, alternative = 'two.sided', mu = 0)
t.test(summer19_Fs$dif_d2H_cont.bef, alternative = 'two.sided', mu = 0)
t.test(summer19_Fs$dif_d18O_aft.bef, alternative = 'two.sided', mu = 0)
t.test(summer19_Fs$dif_d2H_aft.bef, alternative = 'two.sided', mu = 0)
