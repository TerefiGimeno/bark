data <- data.frame(
  name=letters[1:6],
  value=sample(seq(4,15),6),
  group=c('one','one','two','two','three', 'three'),
  sd=c(1,0.2,3,2,4,3)
)

bark <- read.csv('barkData/field_labelling.csv')
barkID <- as.data.frame(dplyr::summarise(dplyr::group_by(bark, Species, Campaign, Tree,
                                                         id, Tissue, Segment2),
                                         d2H = mean(d2H, na.rm = T), d18O = mean(d18O, na.rm = T),
                                         rwc = mean(RWC, na.rm = T)))
names(barkID) <- c("Species", "Campaign", "Tree", "id", "Tissue", "Segment", "d2H", "d18O", "rwc" )
barkID$Site <- as.factor(ifelse(barkID$Campaign == 'Summer-18' | barkID$Campaign == 'Autumn-18',
                                'SE', 'ES'))
barkID$camp <- ifelse(barkID$Campaign == 'Autumn-18', 'Aut-18', 'Win-19')
barkID[which(barkID$Campaign == 'Summer-18'), 'camp'] <- 'Sum-18'
barkID[which(barkID$Campaign == 'Summer-19'), 'camp'] <- 'Sum-19'
barkID$camp <- paste0(barkID$Site, '-', barkID$camp)

barkSumm <- as.data.frame(dplyr::summarise(dplyr::group_by(barkID, Species, camp, Tissue, Segment),
                                           d2H_mean = mean(d2H, na.rm = T), d2H_se = s.err.na(d2H),
                                           d18O_mean = mean(d18O, na.rm = T), d18O_se = s.err.na(d18O),
                                           N_is = lengthWithoutNA(d18O)))

cheat <- data.frame(row.names = 1:5)
cheat$Species <- rep('Pinus sylvestris', 5)
cheat$camp <- c(rep('SE-Sum-18', 3), 'SE-Aut-18', 'SP-Win-18')
cheat$Tissue <- c('xylem','bark','leaf','bark','leaf')
cheat$Segment <- rep('control', 5)
cheat[1:5, c("d2H_mean", "d2H_se", "d18O_mean", "d18O_se", "N_is")] <- 0
barkSumm <- rbind(barkSumm, cheat)
barkSumm$camp <- factor(barkSumm$camp, levels(as.factor(barkSumm$camp))[c(4, 3, 2, 1)])
barkSumm$Segment <- factor(barkSumm$Segment, levels(as.factor(barkSumm$Segment))[c(3, 2, 1)])

windows(12.8)
ggplot(subset(barkSumm, Species == 'Pinus sylvestris' & Tissue == 'xylem'), aes(x=camp, y=d18O_mean, fill=Segment)) + 
  geom_errorbar(aes(x=camp, ymin = d18O_mean - d18O_se, ymax= d18O_mean + d18O_se), width=.2,
                position=position_dodge(.9))+
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  labs(x='') +
  ylab(expression(delta^18*O~('\u2030')))+
  theme(axis.text.y = element_text(size = rel(1.25))) +
  theme(axis.title.y = element_text(size = rel(2))) +
  scale_fill_manual(values=c('blue','green', 'red'))+
  theme(legend.position = c(0.85, 0.9))+
  theme_classic()

