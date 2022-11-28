# set working directory
setwd('~/Documents/MM_Model_Race/code/')

# clear existing workspace
rm(list = ls())

# install necessary packages
if(!require(seqinr)){install.packages('seqinr'); library(seqinr)}

# load necessary data
load('../output/fig_4.RData')

# select ages 
ages_select <- seq(from = 50, to = 90, by = 10)

# specify palette
palette <- rcartocolor::carto_pal(n = 12, name = 'Prism')[c(2,3,7,6)]
col.axis.labs <- '#7A7B7B'
offset <- seq(from = -2, to = 2, length.out = 4)
cex = 1.5

# generate plot 
tiff(filename = '../output/figs/fig_4.tif', width = 6.5, height = 5, units = 'in', res = 500)
par(mar = c(3.1,3.3,0.3,0.3))
plot(NA, NA, xlim = c(min(ages)-5, 90+5), ylim = c(0,20),
     xaxs = 'i', yaxs = 'i', axes = F, xlab = '', ylab = '')
segments(x0 = ages_select + offset[1], y0 = mat_dur_mgus_male_nhw_CI[1,match(ages_select,ages)],
         y1 = mat_dur_mgus_male_nhw_CI[3,match(ages_select,ages)], lwd = 1.5, col = palette[1])
points(x = ages_select + offset[1], y = mat_dur_mgus_male_nhw_CI[2,match(ages_select,ages)],
       pch = 21, bg = palette[1], col = 'white', cex = cex)

segments(x0 = ages_select + offset[2], y0 = mat_dur_mgus_female_nhw_CI[1,match(ages_select,ages)],
         y1 = mat_dur_mgus_female_nhw_CI[3,match(ages_select,ages)], lwd = 1.5, col = palette[2])
points(x = ages_select + offset[2], y = mat_dur_mgus_female_nhw_CI[2,match(ages_select,ages)],
       pch = 21, bg = palette[2], col = 'white', cex = cex)

segments(x0 = ages_select + offset[3], y0 = mat_dur_mgus_male_nhb_CI[1,match(ages_select,ages)],
         y1 = mat_dur_mgus_male_nhb_CI[3,match(ages_select,ages)], lwd = 1.5, col = palette[3])
points(x = ages_select + offset[3], y = mat_dur_mgus_male_nhb_CI[2,match(ages_select,ages)],
       pch = 21, bg = palette[3], col = 'white', cex = cex)

segments(x0 = ages_select + offset[4], y0 = mat_dur_mgus_female_nhb_CI[1,match(ages_select,ages)],
         y1 = mat_dur_mgus_female_nhb_CI[3,match(ages_select,ages)], lwd = 1.5, col = palette[4])
points(x = ages_select + offset[4], y = mat_dur_mgus_female_nhb_CI[2,match(ages_select,ages)],
       pch = 21, bg = palette[4], col = 'white', cex = cex)

legend('topright', pch = 16, pt.cex = 1.5, col = palette, 
       legend = c('Non-Hispanic White Males',
                  'Non-Hispanic White Females',
                  'Non-Hispanic Black Males',
                  'Non-Hispanic Black Females'),
       bty = 'n')

axis(side = 1, at = seq(from = 0, to = 100, by = 10), labels = seq(from = 0, to = 100, by = 10), col.axis = col.axis.labs)
axis(side = 2, las = 1, col.axis = col.axis.labs)
mtext(side = 1, line = 2.0, 'Age of MGUS onset (yr)')
mtext(side = 2, line = 2.3, 'Preclinical Dwell Time (yr)')

dev.off()