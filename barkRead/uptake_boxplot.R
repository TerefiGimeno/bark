source('barkRead/analyses_uptake_rate.R')
mymodel <- lm(log(inf_rate) ~ siteCamp, data = subset(xylem, Species == 'Pinus sylvestris'))
# order: Spain-Su19 (a) > Spain-Wi19 (c) > Sw-Su18 (bc) > Sw-Au18 (ab) 
levels(xylem$siteCamp) <- c("Sweden-Summer2018", "Sweden-Autumn2018",
                            "Spain-Winter2019", "Spain-Summer2019")

myPal <- c('darkolivegreen', 'darkolivegreen', 'green3', 'green3')

windows(12, 8)
nf <- layout(matrix(c(rep(1, 2), 2), 1, 3, byrow = TRUE), c(2, 1), TRUE)
layout.show(nf)

par(mar = c(6, 6, 2, 0), cex = 1.3)
boxplot(inf_rate ~ siteCamp, data = subset(xylem, Species == 'Pinus sylvestris'),
        col = myPal, outline = FALSE, xaxt = 'n', ylim = c(0, 1150), cex.lab = 1.3,
        ylab = expression('Bark Absorption rate'~(mu*mol~m^-2~day^-1)),
        xlab = ' ', xlim = c(0.5, 4.5))
axis(1, at=c(1, 2, 3, 4), labels=c('Summer-18', 'Autumn-18', 'Winter-19', 'Summer-19'))
legend('topleft', legend = expression(bold('(a)'~~italic(P.~sylvestris))), bty = 'n')
legend('topright', legend = c('Sweden', 'Spain'), pch = 15, bty = 'n',
       col = c('darkolivegreen', 'green3'))

par(mar = c(6, 0, 2, 2))
boxplot(inf_rate ~ siteCamp, data = subset(xylem, Species == 'Fagus sylvatica'),
        col = 'gold1', outline = FALSE, xaxt = 'n', ylim = c(0, 1150), cex.lab = 1.3,
        ylab = ' ', xlab = ' ', axes = F, xlim = c(0.2, 1.8))
box()
axis(1, at=c(1), labels=c('Summer-19'))
legend('topleft', legend = expression(bold('(b)'~~italic(F.~sylvatica))), bty = 'n')