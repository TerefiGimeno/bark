source('barkRead/basicFunTEG.R')
bark <- read.csv('barkData/field_labelling.csv')
barkID <- as.data.frame(dplyr::summarise(dplyr::group_by(bark, Species, Campaign, Tree,
                                                         id, Tissue, Segment2),
                                         d2H = mean(d2H, na.rm = T), d18O = mean(d18O, na.rm = T),
                                         rwc = mean(RWC, na.rm = T)))
xylemCont <- subset(barkID, Tissue == 'xylem' &
                      Segment2 == 'control')[, c('Campaign', 'Tree', 'Species', 'rwc','d2H', 'd18O')]
names(xylemCont)[(ncol(xylemCont)-2):ncol(xylemCont)] <- c('rwc_cont', 'd2H_cont', 'd18O_cont')
xylemBef <- subset(barkID, Tissue == 'xylem' &
                     Segment2 == 'before')[,c('Campaign', 'Tree', 'Species', 'id', 'rwc','d2H', 'd18O')]
names(xylemBef)[(ncol(xylemBef)-2):ncol(xylemBef)] <- c('rwc_bef', 'd2H_bef', 'd18O_bef')
xylemAft <- subset(barkID, Tissue == 'xylem' &
                     Segment2 == 'after')[,c('Campaign', 'Tree', 'Species', 'id', 'rwc', 'd2H', 'd18O')]
names(xylemAft)[(ncol(xylemAft)-2):ncol(xylemAft)] <- c('rwc_aft', 'd2H_aft', 'd18O_aft')
xylem <- merge(xylemBef, xylemAft, by = c('Campaign', 'Tree', 'Species', 'id'), all = F)
xylem <- merge(xylem, xylemCont, by = c('Campaign', 'Tree', 'Species'), all = T)
# xylem$d2H_bef_alt <- ifelse(is.na(xylem$d2H_bef), xylem$d2H_cont, xylem$d2H_bef)
# xylem$d18O_bef_alt <- ifelse(is.na(xylem$d18O_bef), xylem$d18O_cont, xylem$d18O_bef)
# xylem$dif_d2H_cont.bef <- xylem$d2H_cont - xylem$d2H_bef
# xylem$dif_d18O_cont.bef <- xylem$d18O_cont - xylem$d18O_bef
# xylem$dif_d2H_aft.bef <- xylem$d2H_aft - xylem$d2H_bef_alt
# xylem$dif_d18O_aft.bef <- xylem$d18O_aft - xylem$d18O_bef_alt
xylem$dif_d2H_aft.bef <- xylem$d2H_aft - xylem$d2H_bef
xylem$dif_d18O_aft.bef <- xylem$d18O_aft - xylem$d18O_bef
# xylem$dif_rwc_aft.bef <- xylem$rwc_aft - xylem$rwc_bef
xylem$Site <- ifelse(xylem$Campaign == 'Autumn2018' | xylem$Campaign == 'Summer2018',
                     'Sweden', 'Spain')

summer18 <- subset(xylem, Campaign == 'Summer2018')
t.test(summer18$dif_d18O_aft.bef, alternative = 'two.sided', mu = 0)
t.test(summer18$dif_d2H_aft.bef, alternative = 'two.sided', mu = 0)
# t.test(summer18$dif_rwc_aft.bef, alternative = 'two.sided', mu = 0)

autumn18 <- subset(xylem, Campaign == 'Autumn2018')
# t.test(autumn18$dif_d18O_cont.bef, alternative = 'two.sided', mu = 0)
# t.test(autumn18$dif_d2H_cont.bef, alternative = 'two.sided', mu = 0)
t.test(autumn18$dif_d18O_aft.bef, alternative = 'two.sided', mu = 0)
t.test(autumn18$dif_d2H_aft.bef, alternative = 'two.sided', mu = 0)
# t.test(autumn18$dif_rwc_aft.bef, alternative = 'two.sided', mu = 0)

winter19 <- subset(xylem, Campaign == 'Winter2019')
# t.test(winter19$dif_d18O_cont.bef, alternative = 'two.sided', mu = 0)
# t.test(winter19$dif_d2H_cont.bef, alternative = 'two.sided', mu = 0)
t.test(winter19$dif_d18O_aft.bef, alternative = 'two.sided', mu = 0)
t.test(winter19$dif_d2H_aft.bef, alternative = 'two.sided', mu = 0)
# t.test(winter19$dif_rwc_aft.bef, alternative = 'two.sided', mu = 0)

summer19_Ps <- subset(xylem, Campaign == 'Summer2019' & Species == 'Pinus sylvestris')
# t.test(summer19_Ps$dif_d18O_cont.bef, alternative = 'two.sided', mu = 0)
# t.test(summer19_Ps$dif_d2H_cont.bef, alternative = 'two.sided', mu = 0)
t.test(summer19_Ps$dif_d18O_aft.bef, alternative = 'two.sided', mu = 0)
t.test(summer19_Ps$dif_d2H_aft.bef, alternative = 'two.sided', mu = 0)
# t.test(summer19_Ps$dif_rwc_aft.bef, alternative = 'two.sided', mu = 0)

summer19_Fs <- subset(xylem, Campaign == 'Summer2019' & Species == 'Fagus sylvatica')
# t.test(summer19_Fs$dif_d18O_cont.bef, alternative = 'two.sided', mu = 0)
# t.test(summer19_Fs$dif_d2H_cont.bef, alternative = 'two.sided', mu = 0)
t.test(summer19_Fs$dif_d18O_aft.bef, alternative = 'two.sided', mu = 0)
t.test(summer19_Fs$dif_d2H_aft.bef, alternative = 'two.sided', mu = 0)

xylemSumm <- dplyr::summarise(dplyr::group_by(xylem, Campaign, Species),
                              d2Hctr = mean(d2H_cont, na.rm = T), d2Hctr_se = s.err.na(d2H_cont),
                              d18Octr = mean(d18O_cont, na.rm = T), d18Octr_se = s.err.na(d18O_cont),
                              d2Hbef = mean(d2H_bef, na.rm = T), d2Hbef_se = s.err.na(d2H_bef),
                              d18Obef = mean(d18O_bef, na.rm = T), d18Obef_se = s.err.na(d18O_bef),
                              d2Haft = mean(d2H_aft, na.rm = T), d2Haft_se = s.err.na(d2H_aft),
                              d18Oaft = mean(d18O_aft, na.rm = T), d18Octr_se = s.err.na(d18O_aft))
xylemSumm <- doBy::summaryBy(d2H_cont + d18O_cont + d2H_bef + d18O_bef + d2H_aft + d18O_aft ~
                               Campaign + Species, FUN = c(mean.na, s.err.na), data = xylem)
