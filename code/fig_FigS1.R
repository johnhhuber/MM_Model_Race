# set working directory
setwd('~/Documents/MM_Model_Race/code/')

# clear existing workspace
rm(list = ls())

# install necessary packages
if(!require(openxlsx)){install.packages('openxlsx'); library(openxlsx)}
if(!require(rcartocolor)){install.packages('rcartocolor'); library(rcartocolor)}

# load necessary data
load('../output/fig_S1.RData')
dat <- read.xlsx('../data/SEER_MM_Survival.xlsx')

# subset data to different race-sex pairings
dat_nhw_male <- subset(dat, Race == 'Non_Hispanic_White' & Sex == 'Male')
dat_nhw_female <- subset(dat, Race == 'Non_Hispanic_White' & Sex == 'Female')
dat_nhb_male <- subset(dat, Race == 'Non_Hispanic_White' & Sex == 'Male')
dat_nhb_female <- subset(dat, Race == 'Non_Hispanic_White' & Sex == 'Female')

# specify yrs 
yrs <- unique(dat$Years_Since_Diagnosis)

# specify color palette
palette <- carto_pal(n = 12, name = 'Prism')[c(2,3,7,6)]

# specify plotting parameters
lwd = 1.5
cex = 1.25
text_x_pos = max(yrs)-0.75
text_y_pos = 0.95

# generate plot 
jpeg(filename = '../output/figs/fig_S1.jpg', width = 6.5, height = 5, units = 'in', res = 500)
layout(mat = matrix(1:4, nrow = 2, byrow = T))
par(mar = c(3.3,3.3,1.1,0.3))
plot(NA, NA, xlim = c(min(yrs)-0.5, max(yrs)+0.5), ylim = c(0,1.02), axes = F, xlab = '', ylab = '', xaxs = 'i', yaxs = 'i')
lines(yrs, exp(-yrs * mm_mu_nhw_male), lwd = lwd, col = palette[1])
points(dat_nhw_male$Years_Since_Diagnosis, dat_nhw_male$Survival, pch = 21, bg = '#222222', col = 'white', cex = cex)
box()
axis(side = 1, at = seq(from = -1, to = 10, by = 1), labels = seq(from = -1, to = 10, by = 1))
axis(side = 2, las = 1, at = seq(from = 0, to = 1, by = 0.2), labels = seq(from = 0, to = 100, by = 20))
mtext(side = 2, line = 2.3, 'Survival (%)')
mtext(side = 3, line = 0, 'Non-Hispanic White Male', font = 2, col = palette[1])
legend('bottomleft', lwd = c(2,NA), pch = c(NA,16), col = '#222222', legend = c('Fit', 'SEER 2000-2018'), bty = 'n')
text(x = text_x_pos, y = text_y_pos, bquote('R'^2 * ': ' * .(mm_r2_nhw_male)))

plot(NA, NA, xlim = c(min(yrs)-0.5, max(yrs)+0.5), ylim = c(0,1.02), axes = F, xlab = '', ylab = '', xaxs = 'i', yaxs = 'i')
lines(yrs, exp(-yrs * mm_mu_nhb_male), lwd = lwd, col = palette[3])
points(dat_nhb_male$Years_Since_Diagnosis, dat_nhb_male$Survival, pch = 21, bg = '#222222', col = 'white', cex = cex)
box()
axis(side = 1, at = seq(from = -1, to = 10, by = 1), labels = seq(from = -1, to = 10, by = 1))
axis(side = 2, las = 1, at = seq(from = 0, to = 1, by = 0.2), labels = seq(from = 0, to = 100, by = 20))
mtext(side = 3, line = 0, 'Non-Hispanic Black Male', font = 2, col = palette[3])
text(x = text_x_pos, y = text_y_pos, bquote('R'^2 * ': ' * .(mm_r2_nhb_male)))

plot(NA, NA, xlim = c(min(yrs)-0.5, max(yrs)+0.5), ylim = c(0,1.02), axes = F, xlab = '', ylab = '', xaxs = 'i', yaxs = 'i')
lines(yrs, exp(-yrs * mm_mu_nhw_female), lwd = lwd, col = palette[2])
points(dat_nhw_female$Years_Since_Diagnosis, dat_nhw_female$Survival, pch = 21, bg = '#222222', col = 'white', cex = cex)
box()
axis(side = 1, at = seq(from = -1, to = 10, by = 1), labels = seq(from = -1, to = 10, by = 1))
axis(side = 2, las = 1, at = seq(from = 0, to = 1, by = 0.2), labels = seq(from = 0, to = 100, by = 20))
mtext(side = 1, line = 2.3, 'Years since Diagnosis')
mtext(side = 2, line = 2.3, 'Survival (%)')
mtext(side = 3, line = 0, 'Non-Hispanic White Female', font = 2, col = palette[2])
text(x = text_x_pos, y = text_y_pos, bquote('R'^2 * ': ' * .(mm_r2_nhw_female)))

plot(NA, NA, xlim = c(min(yrs)-0.5, max(yrs)+0.5), ylim = c(0,1.02), axes = F, xlab = '', ylab = '', xaxs = 'i', yaxs = 'i')
lines(yrs, exp(-yrs * mm_mu_nhb_female), lwd = lwd, col = palette[4])
points(dat_nhb_female$Years_Since_Diagnosis, dat_nhb_female$Survival, pch = 21, bg = '#222222', col = 'white', cex = cex)
box()
axis(side = 1, at = seq(from = -1, to = 10, by = 1), labels = seq(from = -1, to = 10, by = 1))
axis(side = 2, las = 1, at = seq(from = 0, to = 1, by = 0.2), labels = seq(from = 0, to = 100, by = 20))
mtext(side = 1, line = 2.3, 'Years since Diagnosis')
mtext(side = 3, line = 0, 'Non-Hispanic Black Female', font = 2, col = palette[4])
text(x = text_x_pos, y = text_y_pos, bquote('R'^2 * ': ' * .(mm_r2_nhb_female)))

dev.off()