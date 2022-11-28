# set working directory
setwd('~/Documents/MM_Model_Race/code/')

# clear existing workspace
rm(list = ls())

# install necessary packages
if(!require(shape)){install.packages('shape'); library(shape)}
if(!require(seqinr)){install.packages('seqinr'); library(seqinr)}

# load necessary data
load('../output/fig_3.RData')

# specify palette
palette <- c('#4E4E4E','#CB5A50', '#4386D4')
col.axis.labs <- '#7A7B7B'

# generate plot
tiff(filename = '../output/figs/fig_3.tif', width = 6.5, height = 6.5 * 2/3, units = 'in', res = 500)
layout(mat = matrix(c(1,1,2,3,3,4), nrow = 2, byrow = T))
par(mar = c(3.6,5.1,1.3,1.1))
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
lines(ages, haz_mgus_age['50%',], lwd = 1.5, col = palette[1], lty = 1)
lines(ages, haz_mgus_age_sex['50%',], lwd = 1.5, col = palette[2], lty = 1)
lines(ages, haz_mgus_age_race['50%',], lwd = 1.5, col = palette[3], lty = 1)
axis(side = 1, col.axis = col.axis.labs)
axis(side = 2, las = 1, col.axis = col.axis.labs)
mtext(side = 2, line = 3.3, expression('MGUS Acquisition (' * 'yr'^{-1} * ')'))
mtext(side = 3, line = 0, adj = 0, 'A', font = 2)
legend('topleft', lwd = 1.5, col = palette, c('Age', 'Age and Female Sex', 'Age and Black Race'), bty = 'n')

plot(NA, NA, xlim = c(0.5,1.5), ylim = c(0,3), axes = F, xaxs = 'i', yaxs = 'i', xlab = '', ylab = '')
abline(h = 1, lwd = 1.5, lty = 2, col = col.axis.labs)
segments(x0 = 0.75, y0 = haz_mgus_sex[1], y1 = haz_mgus_sex[3], lwd = 1.5, col = palette[2])
points(x = 0.75, y = haz_mgus_sex[2], pch = 21, bg = palette[2], col = 'white', cex = 2)
segments(x0 = 1.25, y0 = haz_mgus_race[1], y1 = haz_mgus_race[3], lwd = 1.5, col = palette[3])
points(x = 1.25, y = haz_mgus_race[2], pch = 21, bg = palette[3], col = 'white', cex = 2)
axis(side = 1, at = c(0.25,0.75,1.25,2), labels = c(NA, 'Female Sex', 'Black Race', NA), col.axis = col.axis.labs)
axis(side = 2, las = 1, col.axis = col.axis.labs)
mtext(side = 2, line = 2.3, 'MGUS Multiplier')
mtext(side = 3, adj = 0, 'B', font = 2)
# text(x = 0.55, y = 1.1, 'Higher', col = col.axis.labs, srt = 90, pos = 4, offset = 0)
# Arrows(x0 = 0.55, x1 = 0.55, y0 = 1.65, y1 = 1.85, col = col.axis.labs, arr.length = 0.1)
# text(x = 0.55, y = 0.9, 'Lower', col = col.axis.labs, srt = 90, pos = 2, offset = 0)
# Arrows(x0 = 0.55, x1 = 0.55, y0 = 0.35, y1 = 0.15, col = col.axis.labs, arr.length = 0.1)

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
lines(ages, haz_mm_age['50%',], lwd = 1.5, col = palette[1], lty = 1)
lines(ages, haz_mm_age_sex['50%',], lwd = 1.5, col = palette[2], lty = 1)
lines(ages, haz_mm_age_race['50%',], lwd = 1.5, col = palette[3], lty = 1)
axis(side = 1, col.axis = col.axis.labs)
axis(side = 2, las = 1, col.axis = col.axis.labs)
mtext(side = 1, line = 2.3, 'Age (yr)')
mtext(side = 2, line = 3.3, expression('MM Acquisition (' * 'yr'^{-1} * ')'))
mtext(side = 3, line = 0, adj = 0, 'C', font = 2)

plot(NA, NA, xlim = c(0.5,1.5), ylim = c(0,2), axes = F, xaxs = 'i', yaxs = 'i', xlab = '', ylab = '')
abline(h = 1, lwd = 1.5, lty = 2, col = palette[1])
segments(x0 = 0.75, y0 = haz_mm_sex[1], y1 = haz_mm_sex[3], lwd = 1.5, col = palette[2])
points(x = 0.75, y = haz_mm_sex[2], pch = 21, bg = palette[2], col = 'white', cex = 2)
segments(x0 = 1.25, y0 = haz_mm_race[1], y1 = haz_mm_race[3], lwd = 1.5, col = palette[3])
points(x = 1.25, y = haz_mm_race[2], pch = 21, bg = palette[3], col = 'white', cex = 2)
axis(side = 1, at = c(0.25,0.75,1.25,2), labels = c(NA, 'Female Sex', 'Black Race', NA), col.axis = col.axis.labs)
axis(side = 2, las = 1, col.axis = col.axis.labs)
mtext(side = 1, line = 2.3, 'Covariate')
mtext(side = 2, line = 2.3, 'MM Multiplier')
mtext(side = 3, adj = 0, 'D', font = 2)

dev.off()