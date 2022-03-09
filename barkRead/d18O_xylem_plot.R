s.err.na <- function(x){sd(x, na.rm = TRUE)/sqrt(lengthWithoutNA(x))}
lengthWithoutNA <- function(x){
  l <- length(which(!is.na(x)))
  return(l)
}
library(ggplot2)
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
barkID[which(barkID$Segment == 'control'), 'Segment'] <- 'Control'
barkID[which(barkID$Segment == 'after'), 'Segment'] <- 'Down-flow'
barkID[which(barkID$Segment == 'before'), 'Segment'] <- 'Up-flow'

barkSumm <- as.data.frame(dplyr::summarise(dplyr::group_by(barkID, Species, camp, Tissue, Segment),
                                           d2H_mean = mean(d2H, na.rm = T), d2H_se = s.err.na(d2H),
                                           d18O_mean = mean(d18O, na.rm = T), d18O_se = s.err.na(d18O),
                                           N_is = lengthWithoutNA(d18O)))

cheat <- data.frame(row.names = 1:5)
cheat$Species <- rep('Pinus sylvestris', 5)
cheat$camp <- c(rep('SE-Sum-18', 3), 'SE-Aut-18', 'ES-Win-19')
cheat$Tissue <- c('xylem','bark','leaf','bark','leaf')
cheat$Segment <- rep('Control', 5)
cheat[1:5, c("d2H_mean", "d2H_se", "d18O_mean", "d18O_se", "N_is")] <- 0
barkSumm <- rbind(barkSumm, cheat)
barkSumm$camp <- factor(barkSumm$camp, levels(as.factor(barkSumm$camp))[c(4, 3, 2, 1)])
barkSumm$Segment <- factor(barkSumm$Segment, levels(as.factor(barkSumm$Segment))[c(1, 3, 2)])

myPlots <- list()
myPlots[[1]] <- 
  ggplot(subset(barkSumm, Species == 'Pinus sylvestris' & Tissue == 'xylem'), aes(x=camp, y=d18O_mean, fill=Segment)) + 
  geom_errorbar(aes(x=camp, ymin = d18O_mean - d18O_se, ymax= d18O_mean + d18O_se), width=.2,
                position=position_dodge(.9))+
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  ylim(-13, 1.999)+
  labs(title = expression(bold('  (a)')~italic('Pinus sylvestris')), x='', y = expression(delta^18*O~('\u2030')))+
  theme(plot.title = element_text(margin = margin(t = 10, b = -20)))+
  theme(axis.text = element_text(size = rel(1.3))) +
  theme(axis.title.y = element_text(size = rel(1.85))) +
  scale_fill_manual(name = '', values=c('#0496FF', '#FFBC42','#D81159'))+
  theme(legend.position = c(0.9, 0.2))+
  theme(legend.text=element_text(size=rel(1.15)))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(colour = "black"))+
  theme(panel.border = element_rect(colour = "black", fill=NA))

myPlots[[2]] <- 
  ggplot(subset(barkSumm, Species == 'Fagus sylvatica' & Tissue == 'xylem'), aes(x=camp, y=d18O_mean, fill=Segment)) + 
  geom_errorbar(aes(x=camp, ymin = d18O_mean - d18O_se, ymax= d18O_mean + d18O_se), width=.2,
                position=position_dodge(.9))+
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  scale_y_continuous(position = "right", limits = c(-13, 1.999))+
  labs(title = expression(bold('  (b)')~italic('Fagus sylvatica')), x='', y = '')+
  theme(plot.title = element_text(margin = margin(t = 10, b = -20)))+
  theme(axis.text = element_text(size = rel(1.3))) +
  scale_fill_manual(name = '', values=c('#0496FF', '#FFBC42','#D81159'))+
  theme(legend.position = 'none')+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(colour = "black"))+
  theme(panel.border = element_rect(colour = "black", fill=NA))

myPlots[[3]] <- 
  ggplot(subset(barkSumm, Species == 'Pinus sylvestris' & Tissue == 'xylem'), aes(x=camp, y=d2H_mean, fill=Segment)) + 
  geom_errorbar(aes(x=camp, ymin = d2H_mean - d2H_se, ymax= d2H_mean + d2H_se), width=.2,
                position=position_dodge(.9))+
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  ggsignif::geom_signif(y_position=c(125), xmin=c(1.3),
                        xmax=c(1.3), annotation=c("*"),
                        tip_length=0, vjust = 0.4, textsize = 8, color="black") +
  ggsignif::geom_signif(y_position=c(188), xmin=c(2.3),
                        xmax=c(2.3), annotation=c("*"),
                        tip_length=0, vjust = 0.4, textsize = 8, color="black") +
  ggsignif::geom_signif(y_position=c(75), xmin=c(3.3),
                        xmax=c(3.3), annotation=c("*"),
                        tip_length=0, vjust = 0.4, textsize = 8, color="black") +
  ggsignif::geom_signif(y_position=c(185), xmin=c(4.3),
                        xmax=c(4.3), annotation=c("*"),
                        tip_length=0, vjust = 0.4, textsize = 8, color="black") +
  ylim(-105, 200)+
  labs(title = expression(bold('  (c)')~italic('Pinus sylvestris')), x='', y = expression(delta^2*H~('\u2030')))+
  theme(plot.title = element_text(margin = margin(t = 10, b = -20)))+
  theme(axis.text = element_text(size = rel(1.3))) +
  theme(axis.title.y = element_text(size = rel(1.85))) +
  scale_fill_manual(name = '', values=c('#0496FF', '#FFBC42','#D81159'))+
  theme(legend.position = 'none')+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(colour = "black"))+
  theme(panel.border = element_rect(colour = "black", fill=NA))

myPlots[[4]] <- 
  ggplot(subset(barkSumm, Species == 'Fagus sylvatica' & Tissue == 'xylem'), aes(x=camp, y=d2H_mean, fill=Segment)) + 
  geom_errorbar(aes(x=camp, ymin = d2H_mean - d2H_se, ymax= d2H_mean + d2H_se), width=.2,
                position=position_dodge(.9))+
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  ggsignif::geom_signif(y_position=c(125), xmin=c(1.3),
                        xmax=c(1.3), annotation=c("*"),
                        tip_length=0, vjust = 0.8, textsize = 8, color="black") +
  labs(title = expression(bold(' (d)')~italic('Fagus sylvatica')), x='', y = '')+
  scale_y_continuous(position = "right", limits = c(-105, 200))+
  theme(plot.title = element_text(margin = margin(t = 10, b = -20)))+
  theme(axis.text = element_text(size = rel(1.3))) +
  scale_fill_manual(name = '', values=c('#0496FF', '#FFBC42','#D81159'))+
  theme(legend.position = 'none')+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(panel.background = element_blank())+
  theme(axis.line = element_line(colour = "black"))+
  theme(panel.border = element_rect(colour = "black", fill=NA))

grobL <- list()
grobL[[1]] <- ggplotGrob(myPlots[[1]])
grobL[[2]] <- ggplotGrob(myPlots[[2]])
grobL[[3]] <- ggplotGrob(myPlots[[3]])
grobL[[4]] <- ggplotGrob(myPlots[[4]])
maxWidth = grid::unit.pmax(grobL[[1]]$widths[2:5], grobL[[3]]$widths[2:5])
grobL[[1]]$widths[2:5] <- as.list(maxWidth)
grobL[[3]]$widths[2:5] <- as.list(maxWidth)
maxWidth = grid::unit.pmax(grobL[[2]]$widths, grobL[[4]]$widths)
grobL[[2]]$widths <- as.list(maxWidth)
grobL[[4]]$widths <- as.list(maxWidth)

windows(12,8)
gridExtra::grid.arrange(
  grobs = grobL,
  layout_matrix = rbind(c(1, 1, 1, 1, 1, 1, 1, 2, 2),
                        c(3, 3, 3, 3, 3, 3, 3, 4, 4))
)

