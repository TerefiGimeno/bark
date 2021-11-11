source('barkRead/basicFunTEG.R')
bark <- read.csv('barkData/field_labelling.csv')
barkID <- as.data.frame(dplyr::summarise(dplyr::group_by(bark, Species, Campaign, Tree,
                                                         id, Tissue, Segment2),
                                         d2H = mean(d2H, na.rm = T), d18O = mean(d18O, na.rm = T),
                                         rwc = mean(RWC, na.rm = T)))
barkID$Site <- as.factor(ifelse(barkID$Campaign == 'Summer-18' | barkID$Campaign == 'Autumn-18',
                      'Sweden', 'Spain'))
barkID$Segment2 <- as.factor(barkID$Segment2)
barkID$Segment2 <- factor(barkID$Segment2, levels(barkID$Segment2)[c(3,2,1)])

mySumm <- as.data.frame(dplyr::summarise(dplyr::group_by(barkID, Tissue, Species),
                                         RWC = mean(rwc, na.rm = T), RWC.se = s.err.na(rwc)))

barkSumm <- as.data.frame(dplyr::summarise(dplyr::group_by(barkID, Species, Campaign, Tissue, Segment2),
                                         d2H_mean = mean(d2H, na.rm = T), d2H_se = s.err.na(d2H),
                                         d18O_mean = mean(d18O, na.rm = T), d18O_se = s.err.na(d18O),
                                         RWC = mean(rwc, na.rm = T), RWC.se = s.err.na(rwc),
                                         N_is = lengthWithoutNA(d18O)))
write.csv(barkSumm, file = 'barkOutput/barkSumm.csv', row.names = F)

barkSumm2 <- as.data.frame(dplyr::summarise(dplyr::group_by(subset(barkID, Tissue == 'xylem'),
                                                           Species, Campaign),
                                           d18O_mean = mean(d18O, na.rm = T), d18O_sd = sd(d18O, na.rm =T)))

library(nlme)
lmmL <- list()
lmmRes <- data.frame(row.names = 1:18)
lmmRes$isotope <- c(rep('d2H', 9), rep('d18O', 9))
lmmRes$site <- rep(c(rep('Sweden', 3), rep('Spain', 6)), 2)
lmmRes$species <- rep(c(rep('Ps', 6), rep('Fs', 3)), 2)
lmmRes$tissue <- rep(c('xyl', 'bark', 'leaf'), 6)
lmmRes[, 5:10] <- c(rep(9999, 18))
names(lmmRes)[5:10] <- c(paste0('F', c('_camp', '_seg', '_camp:seg')),
                         paste0('P', c('_camp', '_seg', '_camp:seg')))

lmmL[[1]] <- lm(d2H ~ Campaign * Segment2,
                data = subset(barkID, Tissue == 'xylem' & Site == 'Sweden'))
lmmL[[2]] <- lm(d2H ~ Campaign * Segment2,
                data = subset(barkID, Tissue == 'bark' & Site == 'Sweden'))
lmmL[[3]] <- lm(d2H ~ Campaign * Segment2,
                data = subset(barkID, Tissue == 'leaf' & Site == 'Sweden'))
lmmL[[4]] <- lme(d2H ~ Campaign + Segment2 + Campaign:Segment2, random = ~1|Tree,
                 na.action = na.exclude, data = subset(barkID, Tissue == 'xylem' & Site == 'Spain'
                                                       & Species == 'Pinus sylvestris'))
lmmL[[5]] <- lm(d2H ~ Campaign * Segment2,
                data = subset(barkID, Tissue == 'bark' & Site == 'Spain'
                              & Species == 'Pinus sylvestris'))
lmmL[[6]] <- lm(d2H ~ Campaign * Segment2,
                data = subset(barkID, Tissue == 'leaf' & Site == 'Spain'
                              & Species == 'Pinus sylvestris'))
lmmL[[7]] <- lme(d2H ~ Segment2, random = ~1|Tree, na.action = na.exclude,
                 data = subset(barkID, Tissue == 'xylem' & Species == 'Fagus sylvatica'))
lmmL[[8]] <- lm(d2H ~ Segment2, data = subset(barkID, Tissue == 'bark'
                                              & Species == 'Fagus sylvatica'))
lmmL[[9]] <- lm(d2H ~ Segment2, data = subset(barkID, Tissue == 'leaf'
                                              & Species == 'Fagus sylvatica'))
lmmL[[10]] <- lm(d18O ~ Campaign * Segment2,
                 data = subset(barkID, Tissue == 'xylem' & Site == 'Sweden'))
lmmL[[11]] <- lm(d18O ~ Campaign * Segment2,
                 data = subset(barkID, Tissue == 'bark' & Site == 'Sweden'))
lmmL[[12]] <- lm(d18O ~ Campaign * Segment2,
                 data = subset(barkID, Tissue == 'leaf' & Site == 'Sweden'))
lmmL[[13]] <- lme(d18O ~ Campaign + Segment2 + Campaign:Segment2, random = ~1|Tree,
                  na.action = na.exclude, data = subset(barkID, Tissue == 'xylem' & Site == 'Spain'
                                                        & Species == 'Pinus sylvestris'))
lmmL[[14]] <- lm(d18O ~ Campaign * Segment2,
                 data = subset(barkID, Tissue == 'bark' & Site == 'Spain'
                               & Species == 'Pinus sylvestris'))
lmmL[[15]] <- lm(d18O ~ Campaign * Segment2,
                 data = subset(barkID, Tissue == 'leaf' & Site == 'Spain'
                               & Species == 'Pinus sylvestris'))
lmmL[[16]] <- lme(d18O ~ Segment2, random = ~1|Tree, na.action = na.exclude,
                  data = subset(barkID, Tissue == 'xylem' & Species == 'Fagus sylvatica'))
lmmL[[17]] <- lm(d18O ~ Segment2, data = subset(barkID, Tissue == 'bark'
                                                & Species == 'Fagus sylvatica'))
lmmL[[18]] <- lm(d18O ~ Segment2, data = subset(barkID, Tissue == 'leaf'
                                                & Species == 'Fagus sylvatica'))

lmmAOV <- lapply(lmmL, anova)
for (i in 1:3){
  lmmRes[i, c('F_camp', 'F_seg', 'F_camp:seg')] <- lmmAOV[[i]][1:3, 4]
  lmmRes[i, c('P_camp', 'P_seg', 'P_camp:seg')] <- lmmAOV[[i]][1:3, 5]
}
lmmRes[4, c('F_camp', 'F_seg', 'F_camp:seg')] <- lmmAOV[[4]][2:4, 3]
lmmRes[4, c('P_camp', 'P_seg', 'P_camp:seg')] <- lmmAOV[[4]][2:4, 4]
for (i in 5:6){
  lmmRes[i, c('F_camp', 'F_seg', 'F_camp:seg')] <- lmmAOV[[i]][1:3, 4]
  lmmRes[i, c('P_camp', 'P_seg', 'P_camp:seg')] <- lmmAOV[[i]][1:3, 5]
}
lmmRes[7, 'F_seg'] <- lmmAOV[[7]][2, 3]
lmmRes[7, 'P_seg'] <- lmmAOV[[7]][2, 4]
for (i in 8:9){
  lmmRes[i, 'F_seg'] <- lmmAOV[[i]][1, 4]
  lmmRes[i, 'P_seg'] <- lmmAOV[[i]][1, 5]
}
for (i in 10:12){
  lmmRes[i, c('F_camp', 'F_seg', 'F_camp:seg')] <- lmmAOV[[i]][1:3, 4]
  lmmRes[i, c('P_camp', 'P_seg', 'P_camp:seg')] <- lmmAOV[[i]][1:3, 5]
}
lmmRes[13, c('F_camp', 'F_seg', 'F_camp:seg')] <- lmmAOV[[13]][2:4, 3]
lmmRes[13, c('P_camp', 'P_seg', 'P_camp:seg')] <- lmmAOV[[13]][2:4, 4]
for (i in 14:15){
  lmmRes[i, c('F_camp', 'F_seg', 'F_camp:seg')] <- lmmAOV[[i]][1:3, 4]
  lmmRes[i, c('P_camp', 'P_seg', 'P_camp:seg')] <- lmmAOV[[i]][1:3, 5]
}
lmmRes[16, 'F_seg'] <- lmmAOV[[16]][2, 3]
lmmRes[16, 'P_seg'] <- lmmAOV[[16]][2, 4]
for (i in 17:18){
  lmmRes[i, 'F_seg'] <- lmmAOV[[i]][1, 4]
  lmmRes[i, 'P_seg'] <- lmmAOV[[i]][1, 5]
}
write.csv(lmmRes, file = 'barkOutput/lmmRes.csv', row.names = F)
