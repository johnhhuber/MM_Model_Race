# set working directory
setwd('~/Documents/MM_Model_Race/code/')

# clear existing workspace
rm(list = ls())

# install necessary packages
if(!require(shape)){install.packages('shape'); library(shape)}
if(!require(seqinr)){install.packages('seqinr'); library(seqinr)}

# load necessary data
load('../output/fig_2.RData')

# specify palette
palette <- c('#4E4E4E','#CB5A50', '#4386D4')
col.axis.labs <- '#7A7B7B'

# generate plot
pdf(file = '../output/figs/fig_2.pdf', width = 7.08661, height = 7.08661 * 2/3)
layout(mat = matrix(c(1,1,2,3,3,4), nrow = 2, byrow = T))
par(mar = c(3.6,5.3,1.3,1.1))
plot(NA, NA, xlim = c(0,100), ylim = c(0, 0.04),
     xlab = '', ylab = '', xaxs = 'i', yaxs = 'i', axes = F)
polygon(x = c(ages, rev(ages)),
        y = c(haz_mgus_age['2.5%',], rev(haz_mgus_age['97.5%',])),
        col = col2alpha(palette[1], alpha = 0.25), border = NA)
polygon(x = c(ages, rev(ages)),
        y = c(haz_mgus_age_sex['2.5%',], rev(haz_mgus_age_sex['97.5%',])),
        col = col2alpha(palette[2], alpha = 0.25), border = NA)
polygon(x = c(ages, rev(ages)),
        y = c(haz_mgus_age_race['2.5%',], rev(haz_mgus_age_race['97.5%',])),
        col = col2alpha(palette[3], alpha = 0.25), border = NA)
lines(ages, haz_mgus_age['50%',], lwd = 2, col = palette[1], lty = 1)
lines(ages, haz_mgus_age_sex['50%',], lwd = 2, col = palette[2], lty = 1)
lines(ages, haz_mgus_age_race['50%',], lwd = 2, col = palette[3], lty = 1)
axis(side = 1, col.axis = col.axis.labs, cex.axis = 1.1)
axis(side = 2, las = 1, col.axis = col.axis.labs, cex.axis = 1.1)
mtext(side = 2, line = 3.6, expression('MGUS Development (' * 'yr'^{-1} * ')'), cex = 0.95)
mtext(side = 3, line = 0, adj = 0, 'a', font = 2)
legend('topleft', lwd = c(2,2,2,2,NA), lty = c(1,1,1,1,NA),
       pch = c(NA,NA,NA,NA,15), pt.cex = 2, col = c(palette,col.axis.labs,col2alpha(col.axis.labs, alpha = 0.25)), c('Age', 'Age and Female Gender', 'Age and Black Race', 'Median', '95% CI'), bty = 'n')

plot(NA, NA, xlim = c(0.5,1.5), ylim = c(0,3), axes = F, xaxs = 'i', yaxs = 'i', xlab = '', ylab = '')
abline(h = 1, lwd = 2, lty = 2, col = col.axis.labs)
segments(x0 = 0.75, y0 = haz_mgus_sex[1], y1 = haz_mgus_sex[3], lwd = 2, col = palette[2])
points(x = 0.75, y = haz_mgus_sex[2], pch = 21, bg = palette[2], col = 'white', cex = 2.5)
segments(x0 = 1.25, y0 = haz_mgus_race[1], y1 = haz_mgus_race[3], lwd = 2, col = palette[3])
points(x = 1.25, y = haz_mgus_race[2], pch = 21, bg = palette[3], col = 'white', cex = 2.5)
axis(side = 1, at = c(0.25,0.75,1.25,2), labels = c(NA, 'Female Gender', 'Black Race', NA), col.axis = col.axis.labs, cex.axis = 1)
axis(side = 2, las = 1, col.axis = col.axis.labs, cex.axis = 1.1)
mtext(side = 2, line = 2.6, 'MGUS Multiplier')
mtext(side = 3, adj = 0, 'b', font = 2)
legend('topleft', pch = c(16,NA), lwd = c(NA,2), col = col.axis.labs, c('Median', '95% CI'), bty = 'n', pt.cex = 2)


plot(NA, NA, xlim = c(0,100), ylim = c(0, 0.02),
     xlab = '', ylab = '', xaxs = 'i', yaxs = 'i', axes = F)
polygon(x = c(ages, rev(ages)),
        y = c(haz_mm_age['2.5%',], rev(haz_mm_age['97.5%',])),
        col = col2alpha(palette[1], alpha = 0.25), border = NA)
polygon(x = c(ages, rev(ages)),
        y = c(haz_mm_age_sex['2.5%',], rev(haz_mm_age_sex['97.5%',])),
        col = col2alpha(palette[2], alpha = 0.25), border = NA)
polygon(x = c(ages, rev(ages)),
        y = c(haz_mm_age_race['2.5%',], rev(haz_mm_age_race['97.5%',])),
        col = col2alpha(palette[3], alpha = 0.25), border = NA)
lines(ages, haz_mm_age['50%',], lwd = 2, col = palette[1], lty = 1)
lines(ages, haz_mm_age_sex['50%',], lwd = 2, col = palette[2], lty = 1)
lines(ages, haz_mm_age_race['50%',], lwd = 2, col = palette[3], lty = 1)
axis(side = 1, col.axis = col.axis.labs, cex.axis = 1.1)
axis(side = 2, las = 1, col.axis = col.axis.labs, cex.axis = 1.1)
mtext(side = 1, line = 2.3, 'Age (yr)')
mtext(side = 2, line = 3.6, expression('MM Progression (' * 'yr'^{-1} * ')'), cex = 0.95)
mtext(side = 3, line = 0, adj = 0, 'c', font = 2)

plot(NA, NA, xlim = c(0.5,1.5), ylim = c(0,2), axes = F, xaxs = 'i', yaxs = 'i', xlab = '', ylab = '')
abline(h = 1, lwd = 2, lty = 2, col = col.axis.labs)
segments(x0 = 0.75, y0 = haz_mm_sex[1], y1 = haz_mm_sex[3], lwd = 2, col = palette[2])
points(x = 0.75, y = haz_mm_sex[2], pch = 21, bg = palette[2], col = 'white', cex = 2.5)
segments(x0 = 1.25, y0 = haz_mm_race[1], y1 = haz_mm_race[3], lwd = 2, col = palette[3])
points(x = 1.25, y = haz_mm_race[2], pch = 21, bg = palette[3], col = 'white', cex = 2.5)
axis(side = 1, at = c(0.25,0.75,1.25,2), labels = c(NA, 'Female Gender', 'Black Race', NA), col.axis = col.axis.labs, cex.axis = 1)
axis(side = 2, las = 1, col.axis = col.axis.labs, cex.axis = 1.1)
mtext(side = 1, line = 2.3, 'Covariate')
mtext(side = 2, line = 2.6, 'MM Multiplier')
mtext(side = 3, adj = 0, 'd', font = 2)

dev.off()