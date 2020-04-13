source('barkRead/basicFunTEG.R')
source('barkRead/delta_conversion.R')
bark <- read.csv('barkData/field_labelling.csv')
bark$nday <- bark$CollectionDate - bark$DateBandageApplied
segments <- as.data.frame(dplyr::summarise(dplyr::group_by(bark, Species, Campaign, Tree, id),
                                           nday_bandage = mean(nday), diam = mean(Diam_cm, na.rm = T),
                                           len = mean(Length_cm, na.rm = T)))
segments$surface_exposed_cm2 <- 2 * pi * (segments$diam/2) * segments$len
segments$xylem_vol_sampled <- pi * (segments$diam/2)^2 * 10
segmentsTrees <- as.data.frame(dplyr::summarise(dplyr::group_by(segments, Species, Campaign, Tree),
                                                diam_tree = mean(diam, na.rm = T),
                                                len_tree = mean(len, na.rm = T)))
segmentsTrees$surface_TREE <- 2 * pi * (segmentsTrees$diam_tree/2) * segmentsTrees$len_tree
segmentsTrees$vol_TREE <- pi * (segmentsTrees$diam_tree/2)^2 * 10
segmentsSumm <- as.data.frame(dplyr::summarise(dplyr::group_by(segments, Species, Campaign),
                                               Diam = mean(diam, na.rm = T), d_se = s.err.na(diam),
                                               Len = mean(len, na.rm = T), l_se = s.err.na(len)))
segmentsSumm$surface_CAMP <- 2 * pi * (segmentsSumm$Diam/2) * segmentsSumm$Len
segmentsSumm$vol_CAMP <- pi * (segmentsSumm$Diam/2)^2 * 10
segments <- merge(segments, segmentsTrees, by = c('Species', 'Campaign', 'Tree'),
                  all.x = T, all.y = F)
segments$surface <- ifelse(is.na(segments$diam), segments$surface_TREE, segments$surface_exposed_cm2)
segments$xylVol <- ifelse(is.na(segments$diam), segments$vol_TREE, segments$xylem_vol_sampled)
segments[which(segments$Campaign == 'Autumn2018'), 'surface'] <-
  segmentsSumm[which(segmentsSumm$Campaign == 'Summer2018'), 'surface_CAMP']
segments[which(segments$Campaign == 'Autumn2018'), 'xylVol'] <-
  segmentsSumm[which(segmentsSumm$Campaign == 'Summer2018'), 'vol_CAMP']

barkID <- as.data.frame(dplyr::summarise(dplyr::group_by(bark, Species, Campaign, Tree,
                                                         id, Tissue, Segment2),
                                         d2H = mean(d2H, na.rm = T), d18O = mean(d18O, na.rm = T),
                                         rwc = mean(RWC, na.rm = T)))
xylemBef <- subset(barkID, Tissue == 'xylem' &
                     Segment2 == 'before')[,c('Campaign', 'Tree', 'Species', 'id', 'rwc','d2H', 'd18O')]
names(xylemBef)[(ncol(xylemBef)-2):ncol(xylemBef)] <- c('rwc_bef', 'd2H_bef', 'd18O_bef')
xylemAft <- subset(barkID, Tissue == 'xylem' &
                     Segment2 == 'after')[,c('Campaign', 'Tree', 'Species', 'id', 'rwc', 'd2H', 'd18O')]
names(xylemAft)[(ncol(xylemAft)-2):ncol(xylemAft)] <- c('rwc_aft', 'd2H_aft', 'd18O_aft')
xylem <- merge(xylemBef, xylemAft, by = c('Campaign', 'Tree', 'Species', 'id'), all = F)
xylem <- merge(xylem, segments[, c('Species', 'Campaign', 'Tree', 'id',
                                   'nday_bandage', 'surface', 'xylVol')],
               by = c('Species', 'Campaign', 'Tree', 'id'), all.x = T, all.y = F)
xylem$Site <- ifelse(xylem$Campaign == 'Autumn2018' | xylem$Campaign == 'Summer2018',
                     'Sweden', 'Spain')
xylem$ppm_2H_label <- ifelse(xylem$Site == 'Sweden', ppm_2H_label_Sweden, ppm_2H_label_Spain)
xylem$ppm_2H_before <- calc_ratio_ppm(xylem$d2H_bef, VSMOW_2R)
xylem$ppm_2H_after <- calc_ratio_ppm(xylem$d2H_aft, VSMOW_2R)
xylem$vol_contrib_uL <- (xylem$rwc_aft * xylem$xylVol) * 1000 *
  (xylem$ppm_2H_after - xylem$ppm_2H_before)/(xylem$ppm_2H_label - xylem$ppm_2H_before)
xylem$vol_contrib_mmol <- xylem$vol_contrib_uL/18
xylem$inf_rate_uL <- xylem$vol_contrib_uL/(xylem$surface * 0.0001 * xylem$nday_bandage)
xylem$inf_rate_mmol <- xylem$vol_contrib_mmol/(xylem$surface * 0.0001 * xylem$nday_bandage)
xylem$siteCamp <- as.factor(paste0(xylem$Site, '-', xylem$Campaign))

summary(lm(log(inf_rate) ~ Species, data = subset(xylem, Campaign == 'Summer2019')))
summary(lm(log(inf_rate) ~ Site, data = subset(xylem, Species == 'Pinus sylvestris')))
# summary(lm(log(inf_rate) ~ Site,
#           data = subset(xylem, Campaign == 'Summer2018' | Campaign == 'Summer2019')))
summary(aov(log(inf_rate) ~ Campaign,
           data = subset(xylem, Site == 'Spain' & Species == 'Pinus sylvestris')))
summary(aov(log(inf_rate) ~ Campaign, data = subset(xylem, Site == 'Sweden')))
anova(lm(log(inf_rate) ~ siteCamp, data = subset(xylem, Species == 'Pinus sylvestris')))
TukeyHSD(aov(log(inf_rate) ~ siteCamp, data = subset(xylem, Species == 'Pinus sylvestris')))
dplyr::summarise(dplyr::group_by(subset(xylem, Species == 'Pinus sylvestris'),
                                 siteCamp), var = mean(log(inf_rate), na.rm =T))
dplyr::summarise(dplyr::group_by(subset(xylem, Species == 'Pinus sylvestris'),
                                 siteCamp), var = median(log(inf_rate), na.rm =T))
dplyr::summarise(dplyr::group_by(subset(xylem, Species == 'Pinus sylvestris'),
                                 siteCamp), var = median(inf_rate, na.rm =T))
dplyr::summarise(dplyr::group_by(subset(xylem, Species == 'Pinus sylvestris'),
                                 siteCamp), var = mean(inf_rate, na.rm =T))

