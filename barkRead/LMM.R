source('barkRead/basicFunTEG.R')
bark <- read.csv('barkData/field_labelling.csv')
barkID <- as.data.frame(dplyr::summarise(dplyr::group_by(bark, Species, Campaign, Tree,
                                                         id, Tissue, Segment2),
                                         d2H = mean(d2H, na.rm = T), d18O = mean(d18O, na.rm = T),
                                         rwc = mean(RWC, na.rm = T)))
barkID$Site <- as.factor(ifelse(barkID$Campaign == 'Summer2018' | barkID$Campaign == 'Autumn2018',
                      'Sweden', 'Spain'))
barkID$Segment2 <- relevel(barkID$Segment2, ref = 'control')

library(nlme)
d2HlmeSpainFSxy <- lme(d2H ~ Segment2, random = ~1|Tree, na.action = na.exclude,
                     data = subset(barkID, Tissue == 'xylem' & Species == 'Fagus sylvatica'))
d18OlmeSpainFSxy <- lme(d18O ~ Segment2, random = ~1|Tree, na.action = na.exclude,
                     data = subset(barkID, Tissue == 'xylem' & Species == 'Fagus sylvatica'))
d2HlmeSpainPSxy <- lme(d2H ~ Campaign + Segment2 + Campaign:Segment2, random = ~1|Tree,
                     na.action = na.exclude, data = subset(barkID, Tissue == 'xylem' & Site == 'Spain'
                                                           & Species == 'Pinus sylvestris'))
d18OlmeSpainPSxy <- lme(d18O ~ Campaign + Segment2 + Campaign:Segment2, random = ~1|Tree,
                     na.action = na.exclude, data = subset(barkID, Tissue == 'xylem'& Site == 'Spain'
                                                           & Species == 'Pinus sylvestris'))
d2HlmSwPSxy <- lm(d2H ~ Campaign * Segment2,
                data = subset(barkID, Tissue == 'xylem' & Site == 'Sweden'))
d18OlmSwPSxy <- lm(d18O ~ Campaign * Segment2,
                data = subset(barkID, Tissue == 'xylem' & Site == 'Sweden'))
d2HlmSpainPSleaf <- lm(d2H ~ Campaign * Segment2,
                        data = subset(barkID, Tissue == 'leaf' & Site == 'Spain'
                                      & Species == 'Pinus sylvestris'))
d18OlmSpainPSleaf <- lm(d18O ~ Campaign * Segment2,
                        data = subset(barkID, Tissue == 'leaf' & Site == 'Spain'
                                      & Species == 'Pinus sylvestris'))
d2HlmSpainPSbark <- lm(d2H ~ Campaign * Segment2,
                       data = subset(barkID, Tissue == 'bark' & Site == 'Spain'
                                     & Species == 'Pinus sylvestris'))
d18OlmSpainPSbark <- lm(d18O ~ Campaign * Segment2,
                        data = subset(barkID, Tissue == 'bark' & Site == 'Spain'
                                      & Species == 'Pinus sylvestris'))
d2HlmSpainFSleaf <- lm(d2H ~ Segment2, data = subset(barkID, Tissue == 'leaf'
                                     & Species == 'Fagus sylvatica'))
d18OlmSpainFSleaf <- lm(d18O ~ Segment2, data = subset(barkID, Tissue == 'leaf'
                                                     & Species == 'Fagus sylvatica'))
d2HlmSpainFSbark <- lm(d2H ~ Segment2, data = subset(barkID, Tissue == 'bark'
                                                     & Species == 'Fagus sylvatica'))
d18OlmSpainFSbark <- lm(d18O ~ Segment2, data = subset(barkID, Tissue == 'bark'
                                                      & Species == 'Fagus sylvatica'))
d2HlmSwPSleaf <- lm(d2H ~ Campaign * Segment2,
                  data = subset(barkID, Tissue == 'leaf' & Site == 'Sweden'))
d18OlmSwPSleaf <- lm(d18O ~ Campaign * Segment2,
                   data = subset(barkID, Tissue == 'leaf' & Site == 'Sweden'))
d2HlmSwPSbark <- lm(d2H ~ Campaign * Segment2,
                    data = subset(barkID, Tissue == 'bark' & Site == 'Sweden'))
d18OlmSwPSbark <- lm(d18O ~ Campaign * Segment2,
                     data = subset(barkID, Tissue == 'bark' & Site == 'Sweden'))


barkSumm <- as.data.frame(dplyr::summarise(dplyr::group_by(bark, Species, Campaign, Tissue, Segment2),
                                         d2H = mean(d2H, na.rm = T), d18O = mean(d18O, na.rm = T)))


