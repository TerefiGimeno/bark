bark <- read.csv('barkData/field_labelling.csv')
barkID <- as.data.frame(dplyr::summarise(dplyr::group_by(bark, Species, Campaign, Tree,
                                                         id, Tissue, Segment2),
                                         d2H = mean(d2H, na.rm = T), d18O = mean(d18O, na.rm = T),
                                         rwc = mean(RWC, na.rm = T)))
cheat <- subset(barkID, Campaign == 'Summer-18' & Tissue == 'xylem')[1, ]
cheat[1, 'Segment2'] <- 'control'
cheat[1, c('d2H', 'd18O')] <- 0
barkID <- rbind(barkID, cheat)

library(dplyr)
library(ggplot2)

barkSumm <- summarise(group_by(subset(barkID, Tissue == 'xylem'), Species, Campaign, Segment2),
                                           d2H_mean = mean(d2H, na.rm = T), d2H_se = s.err.na(d2H),
                                           d18O_mean = mean(d18O, na.rm = T), d18O_se = s.err.na(d18O),
                                           N_is = lengthWithoutNA(d18O))
barkSumm$Site <- as.factor(ifelse(barkSumm$Campaign == 'Summer-18' | barkSumm$Campaign == 'Autumn-18',
                                  'Sweden', 'Spain'))
barkSumm$Segment2 <- as.factor(barkSumm$Segment2)
barkSumm$Segment2 <- factor(barkSumm$Segment2, levels(barkSumm$Segment2)[c(3,2,1)])
barkSumm$Campaign <- as.factor(barkSumm$Campaign)
barkSumm$Campaign <- factor(barkSumm$Campaign, levels(barkSumm$Campaign)[c(2, 1, 4 , 3)])

windows(12, 8)
ggplot(subset(barkSumm, Site == 'Sweden'), aes(x= Campaign, y=d2H_mean, fill = Segment2)) +
  geom_bar(position=position_dodge(), stat="identity", colour = 'black') +
  scale_fill_manual(values = c("darkgrey", "blue", "red"),
                    labels=c("Control", "Up-flow", "Down-flow")) +
  geom_errorbar( aes(ymin= d2H_mean - d2H_se, ymax= d2H_mean + d2H_se),
                 width=0.4, colour="black", position=position_dodge(.9)) +
  coord_cartesian(ylim = c(-100, 190), xlim = c(1.1, 1.9)) +
  scale_y_continuous(breaks=seq(-100, 190, 50)) +
  labs(x = '', y = expression(delta^{2}*H~("\u2030"))) +
  guides(fill=guide_legend(title=NULL)) +
  theme_bw() +
  theme(legend.position=c(0.1, 0.85), legend.text = element_text(size = 12),
        axis.text = element_text(size=11), axis.title=element_text(size=15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"))



ggplot(subset(barkSumm, Site == 'Sweden'), aes(x= Campaign, y=d18O_mean, fill = Segment2)) +
  geom_bar(position=position_dodge(), stat="identity", colour = 'black') +
  scale_fill_manual(values = c("darkgrey", "blue", "red")) +
  geom_errorbar( aes(ymin= d18O_mean - d18O_se, ymax= d18O_mean + d18O_se),
                 width=0.4, colour="black", position=position_dodge(.9)) +
  coord_cartesian(ylim = c(-15, -0.75)) +
  scale_y_continuous(breaks=seq(-15, 0, 5)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"))


