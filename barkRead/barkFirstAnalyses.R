source('barkRead/basicFunTEG.R')
bark <- read.csv('barkData/field_labelling.csv')
barkID <- as.data.frame(dplyr::summarise(dplyr::group_by(bark, Campaing, id, Tissue, Segment2),
                                         d2H = mean(d2H, na.rm = T), d18O = mean(d18O, na.rm = T)))
barkID <- dplyr::left_join(barkID, bark[, -c(14, 15)], by = c('Campaing', 'id', 'Tissue', 'Segment2'))
msControl <- subset(barkID, Segment2 == 'control' & Tissue == 'xylem')
names(msControl)[(ncol(msControl)-1):ncol(msControl)] <- paste0(names(msControl)[(ncol(msControl)-1):ncol(msControl)], '_ctr')
msControl$Tree <- substring(as.character(msControl$id), 1, 1)
msBefore <- subset(barkID, Segment2 == 'before' & Tissue == 'xylem')
names(msBefore)[(ncol(msBefore)-1):ncol(msBefore)] <- paste0(names(msBefore)[(ncol(msBefore)-1):ncol(msBefore)], '_bef')
msBefore$Tree <- substring(as.character(msBefore$id), 1, 1)
msBefore <- merge(msControl[, c('Tree','d2H_ctr', 'd18O_ctr')], msBefore, by='Tree', all=T)
msAfter <- subset(barkID, Segment2 == 'after' & Tissue == 'xylem')
names(msAfter)[(ncol(msAfter)-1):ncol(msAfter)] <- paste0(names(msAfter)[(ncol(msAfter)-1):ncol(msAfter)], '_aft')
ms <- merge(msBefore, msAfter[,c('id', 'd2H_aft', 'd18O_aft')], by = 'id', all=T)
ms$diff_ctrl_d2H <- ms$d2H_bef - ms$d2H_ctr
ms$diff_ctrl_d18O <- ms$d18O_bef - ms$d18O_ctr
ms$diff_d2H <- ms$d2H_aft - ms$d2H_bef
ms$diff_d18O <- ms$d18O_aft - ms$d18O_bef

t.test(ms$diff_ctrl_d2H, alternative = 'two.sided', mu = 0)
t.test(ms$diff_ctrl_d18O, alternative = 'two.sided', mu = 0)

t.test(ms$diff_d18O, alternative = 'two.sided', mu = 0)
t.test(ms$diff_d2H, alternative = 'greater', mu = 0)

msSumm <- as.data.frame(dplyr::summarise(dplyr::group_by(ms, Tree),
                                         d18O_befAvg=mean(d18O_bef), d18O_befSE=s.err.na(d18O_bef),
                                         d2H_befAvg=mean(d2H_bef), d2H_befSE=s.err.na(d2H_bef),
                                         d18O_aftAvg=mean(d18O_aft), d18O_aftSE=s.err.na(d18O_aft),
                                         d2H_aftAvg=mean(d2H_aft), d2H_aftSE=s.err.na(d2H_aft)))
