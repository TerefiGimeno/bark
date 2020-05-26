lmmL <- list()
lmmRes <- data.frame(row.names = 1:18)
lmmRes$isotope <- c(rep('d2H', 9), rep('d18O', 9))
lmmRes$site <- rep(c(rep('Sweden', 3), rep('Spain', 6)), 2)
lmmRes$species <- rep(c(rep('Ps', 6), rep('Fs', 3)), 2)
lmmRes$tissue <- rep(c('xyl', 'bark', 'leaf'), 6)
lmmRes[, 5:10] <- c(rep(9999, 18))
names(lmmRes)[5:10] <- c(paste0(c('F', 'P'), '_camp'), paste0(c('F', 'P'), '_seg'),
                         paste0(c('F', 'P'), '_camp:seg'))

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

lmmSumm <- lapply(lmmL, summary)
lmmAOV <- lapply(lmmL, anova)
