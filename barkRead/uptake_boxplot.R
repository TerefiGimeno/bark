source('barkRead/analyses_uptake_rate.R')
library(multcomp)
# figure out multicomparision
### specify all pair-wise comparisons among levels of variable "tension"
tuk <- glht(inf_rate_siteCamp, linfct = mcp(siteCamp = "Tukey"))
### extract information
tuk.cld <- cld(tuk)
### use sufficiently large upper margin
old.par <- par(mai=c(1,1,1.25,1), no.readonly = TRUE)
### plot
plot(tuk.cld) 

xylem$crap <- ifelse(xylem$siteCamp == 'Sweden-Summer2018', 'a', 'd')
xylem[which(xylem$siteCamp == 'Sweden-Autumn2018'), 'crap'] <- 'b'
xylem[which(xylem$siteCamp == 'Spain-Winter2019'), 'crap'] <- 'c'

myPal <- c('darkolivegreen', 'darkolivegreen', 'green3', 'green3')

windows(12, 8)
nf <- layout(matrix(c(rep(1, 2), 2), 1, 3, byrow = TRUE), c(2, 1), TRUE)
layout.show(nf)

par(mar = c(4, 6, 2, 0), cex = 1.3)
boxplot(inf_rate_mmol ~ crap, data = subset(xylem, Species == 'Pinus sylvestris'),
        col = myPal, outline = FALSE, xaxt='n',  ylim = c(0, 65), cex.lab = 1.3,
        ylab = expression('Bark absorption rate'~(mmol~m^-2~day^-1)),
        xlab = ' ', xlim = c(0.5, 4.5))
axis(1, at=c(1, 2, 3, 4), labels=c('Summer-18', 'Autumn-18', 'Winter-19', 'Summer-19'))
legend(0.2, 68, expression(bold('(a)'~~italic(P.~sylvestris))), bty = 'n')
legend('bottomright', legend = c('Sweden', 'Spain'), pch = 15, bty = 'n',
       col = c('darkolivegreen', 'green3'))
legend(0.735, 17, 'b', bty = 'n')
legend(1.735, 7, 'c', bty = 'n')
legend(2.735, 25, 'b', bty = 'n')
legend(3.71, 66, 'a A', bty = 'n')

par(mar = c(4, 0, 2, 2))
boxplot(inf_rate_mmol ~ siteCamp, data = subset(xylem, Species == 'Fagus sylvatica'),
        col = 'gold1', outline = FALSE, xaxt = 'n', ylim = c(0, 65), cex.lab = 1.3,
        ylab = ' ', xlab = ' ', axes = F, xlim = c(0.2, 1.8))
box()
axis(1, at=c(1), labels=c('Summer-19'))
legend(-0.05, 68, legend = expression(bold('(b)'~~italic(F.~sylvatica))), bty = 'n')
legend(0.69, 30, 'B', bty = 'n')

# same thing but with uL

par(mar = c(6, 6, 2, 0), cex = 1.3)
boxplot(inf_rate ~ crap, data = subset(xylem, Species == 'Pinus sylvestris'),
        col = myPal, outline = FALSE, xaxt='n',  ylim = c(0, 1150), cex.lab = 1.3,
        ylab = expression('Bark absorption rate'~(mu*L~m^-2~day^-1)),
        xlab = ' ', xlim = c(0.5, 4.5))
axis(1, at=c(1, 2, 3, 4), labels=c('Summer-18', 'Autumn-18', 'Winter-19', 'Summer-19'))
legend(0.2, 1200, expression(bold('(a)'~~italic(P.~sylvestris))), bty = 'n')
legend('bottomright', legend = c('Sweden', 'Spain'), pch = 15, bty = 'n',
       col = c('darkolivegreen', 'green3'))
legend(0.735, 300, 'b', bty = 'n')
legend(1.735, 125, 'c', bty = 'n')
legend(2.735, 450, 'b', bty = 'n')
legend(3.71, 1175, 'a A', bty = 'n')

par(mar = c(6, 0, 2, 2))
boxplot(inf_rate ~ siteCamp, data = subset(xylem, Species == 'Fagus sylvatica'),
        col = 'gold1', outline = FALSE, xaxt = 'n', ylim = c(0, 1150), cex.lab = 1.3,
        ylab = ' ', xlab = ' ', axes = F, xlim = c(0.2, 1.8))
box()
axis(1, at=c(1), labels=c('Summer-19'))
legend(-0.05, 1200, legend = expression(bold('(b)'~~italic(F.~sylvatica))), bty = 'n')
legend(0.69, 550, 'B', bty = 'n')


# same thing but with log scale

windows(12, 8)
nf <- layout(matrix(c(rep(1, 2), 2), 1, 3, byrow = TRUE), c(2, 1), TRUE)
layout.show(nf)

par(mar = c(6, 6, 2, 0), cex = 1.3)
boxplot(log(inf_rate) ~ crap, data = subset(xylem, Species == 'Pinus sylvestris'),
        col = myPal, outline = FALSE, xaxt='n',  ylim = c(2.5, 8), cex.lab = 1.3,
        ylab = expression('Ln (Bark absorption rate'~(mu*L~m^-2~day^-1)~')'),
        xlab = ' ', xlim = c(0.5, 4.5))
axis(1, at=c(1, 2, 3, 4), labels=c('Summer-18', 'Autumn-18', 'Winter-19', 'Summer-19'))
legend('topleft', legend = expression(bold('(a)'~~italic(P.~sylvestris))), bty = 'n')
legend('bottomright', legend = c('Sweden', 'Spain'), pch = 15, bty = 'n',
       col = c('darkolivegreen', 'green3'))

par(mar = c(6, 0, 2, 2))
boxplot(log(inf_rate) ~ siteCamp, data = subset(xylem, Species == 'Fagus sylvatica'),
        col = 'gold1', outline = FALSE, xaxt = 'n', ylim = c(2.5, 8), cex.lab = 1.3,
        ylab = ' ', xlab = ' ', axes = F, xlim = c(0.2, 1.8))
box()
axis(1, at=c(1), labels=c('Summer-19'))
legend('topleft', legend = expression(bold('(b)'~~italic(F.~sylvatica))), bty = 'n')
