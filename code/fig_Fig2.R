# set working directory
setwd('~/Documents/MM_Model_Race/code/')

# clear existing workspace
rm(list = ls())

# install necessary packages
if(!require(seqinr)){install.packages('seqinr'); library(seqinr)}

# load necessary data
load('../output/fig_2.RData')

# specify palette
palette <- rcartocolor::carto_pal(n = 12, name = 'Prism')[c(2,3,7,6)]
col.axis.labs <- '#7A7B7B'

# generate plot
tiff(filename = '../output/figs/fig_2.tif', width = 12, height = 6, units = 'in', res = 500)
layout(mat = matrix(1:8, nrow = 2, ncol = 4, byrow = T))
par(mar = c(4.7,4.0,0.4,0.4))

plot(NA, NA, xlim = c(min(data_list$mgus_prev_male_nhw$age_lower)-0.5,max(data_list$mgus_prev_male_nhw$age_lower)+0.5), ylim = c(0,0.35),
     xaxs = 'i', yaxs = 'i', axes = F, xlab = '', ylab = '')
polygon(x = c(data_list$mgus_prev_male_nhw$age_lower, rev(data_list$mgus_prev_male_nhw$age_lower)), y = c(preds$y_male_nhw_ppi['2.5%',] / data_list$mgus_prev_male_nhw$n, rev(preds$y_male_nhw_ppi['97.5%',] / data_list$mgus_prev_male_nhw$n)),
        col = col2alpha(palette[1],alpha = 0.25), border = NA)
polygon(x = c(data_list$mgus_prev_male_nhw$age_lower, rev(data_list$mgus_prev_male_nhw$age_lower)), y = c(preds$y_male_nhw_ppi['25%',] / data_list$mgus_prev_male_nhw$n, rev(preds$y_male_nhw_ppi['75%',] / data_list$mgus_prev_male_nhw$n)),
        col = col2alpha(palette[1],alpha = 0.5), border = NA)
lines(x = data_list$mgus_prev_male_nhw$age_lower, y = preds$y_male_nhw_ppi['50%',] / data_list$mgus_prev_male_nhw$n, col = palette[1], lwd = 2)
segments(x0 = data_list$mgus_prev_male_nhw$age_lower, y0 = p_male_nhw_lower, y1 = p_male_nhw_upper, lwd = 1.5, col = '#222222')
points(data_list$mgus_prev_male_nhw$age_lower, data_list$mgus_prev_male_nhw$y / data_list$mgus_prev_male_nhw$n, pch = 21, cex = 1.5, bg = '#222222', col = 'white')
axis(side = 1, at = seq(from = 45, to = 85, by = 5), labels = c(NA,'50-54', '55-59', '60-64', '65-69', '70-74', '75-79', '80-84', '85+'), las = 2, col.axis = col.axis.labs)
axis(side = 2, las = 1, at = seq(from = 0, to = 0.35, by = 0.05), labels = seq(from = 0, to = 35, by = 5), col.axis = col.axis.labs)
mtext(side = 2, line = 2.6, 'MGUS Prevalence (%)', col = '#222222')
mtext(side = 3, line = -1, 'Non-Hispanic White Male', font = 2, col = palette[1])

legend(x = min(data_list$mgus_prev_male_nhw$age_lower)-0.5,
       y = 0.325, pch = c(16,15,15,NA), lwd = c(NA,NA,NA,2),
       col = c('#222222', col2alpha(palette[1],0.25),col2alpha(palette[1],0.5),palette[1]),
       pt.cex = 1.5, cex = 1, c('NHANES 1999-2004', '50% CI', '95% CI', 'Median'), bty = 'n')

plot(NA, NA, xlim = c(min(data_list$mgus_prev_female_nhw$age_lower)-0.5,max(data_list$mgus_prev_female_nhw$age_lower)+0.5), ylim = c(0,0.35),
     xaxs = 'i', yaxs = 'i', axes = F, xlab = '', ylab = '')
polygon(x = c(data_list$mgus_prev_female_nhw$age_lower, rev(data_list$mgus_prev_female_nhw$age_lower)), y = c(preds$y_female_nhw_ppi['2.5%',] / data_list$mgus_prev_female_nhw$n, rev(preds$y_female_nhw_ppi['97.5%',] / data_list$mgus_prev_female_nhw$n)),
        col = col2alpha(palette[2],alpha = 0.25), border = NA)
polygon(x = c(data_list$mgus_prev_female_nhw$age_lower, rev(data_list$mgus_prev_female_nhw$age_lower)), y = c(preds$y_female_nhw_ppi['25%',] / data_list$mgus_prev_female_nhw$n, rev(preds$y_female_nhw_ppi['75%',] / data_list$mgus_prev_female_nhw$n)),
        col = col2alpha(palette[2],alpha = 0.5), border = NA)
lines(x = data_list$mgus_prev_female_nhw$age_lower, y = preds$y_female_nhw_ppi['50%',] / data_list$mgus_prev_female_nhw$n, col = palette[2], lwd = 2)
segments(x0 = data_list$mgus_prev_female_nhw$age_lower, y0 = p_female_nhw_lower, y1 = p_female_nhw_upper, lwd = 1.5, col = '#222222')
points(data_list$mgus_prev_female_nhw$age_lower, data_list$mgus_prev_female_nhw$y / data_list$mgus_prev_female_nhw$n, pch = 21, cex = 1.5, bg = '#222222', col = 'white')
axis(side = 1, at = seq(from = 45, to = 85, by = 5), labels = c(NA,'50-54', '55-59', '60-64', '65-69', '70-74', '75-79', '80-84', '85+'), las = 2, col.axis = col.axis.labs)
axis(side = 2, las = 1, at = seq(from = 0, to = 0.35, by = 0.05), labels = seq(from = 0, to = 35, by = 5), col.axis = col.axis.labs)
mtext(side = 3, line = -1, 'Non-Hispanic White Female', font = 2, col = palette[2])

plot(NA, NA, xlim = c(min(data_list$mgus_prev_male_nhb$age_lower)-0.5,max(data_list$mgus_prev_male_nhb$age_lower)+0.5), ylim = c(0,0.35),
     xaxs = 'i', yaxs = 'i', axes = F, xlab = '', ylab = '')
polygon(x = c(data_list$mgus_prev_male_nhb$age_lower, rev(data_list$mgus_prev_male_nhb$age_lower)), y = c(preds$y_male_nhb_ppi['2.5%',] / data_list$mgus_prev_male_nhb$n, rev(preds$y_male_nhb_ppi['97.5%',] / data_list$mgus_prev_male_nhb$n)),
        col = col2alpha(palette[3],alpha = 0.25), border = NA)
polygon(x = c(data_list$mgus_prev_male_nhb$age_lower, rev(data_list$mgus_prev_male_nhb$age_lower)), y = c(preds$y_male_nhb_ppi['25%',] / data_list$mgus_prev_male_nhb$n, rev(preds$y_male_nhb_ppi['75%',] / data_list$mgus_prev_male_nhb$n)),
        col = col2alpha(palette[3],alpha = 0.5), border = NA)
lines(x = data_list$mgus_prev_male_nhb$age_lower, y = preds$y_male_nhb_ppi['50%',] / data_list$mgus_prev_male_nhb$n, col = palette[3], lwd = 2)
segments(x0 = data_list$mgus_prev_male_nhb$age_lower, y0 = p_male_nhb_lower, y1 = p_male_nhb_upper, lwd = 1.5, col = '#222222')
points(data_list$mgus_prev_male_nhb$age_lower, data_list$mgus_prev_male_nhb$y / data_list$mgus_prev_male_nhb$n, pch = 21, cex = 1.5, bg = '#222222', col = 'white')
axis(side = 1, at = seq(from = 45, to = 85, by = 5), labels = c(NA,'50-54', '55-59', '60-64', '65-69', '70-74', '75-79', '80-84', '85+'), las = 2, col.axis = col.axis.labs)
axis(side = 2, las = 1, at = seq(from = 0, to = 0.35, by = 0.05), labels = seq(from = 0, to = 35, by = 5), col.axis = col.axis.labs)
mtext(side = 3, line = -1, 'Non-Hispanic Black Male', font = 2, col = palette[3])

plot(NA, NA, xlim = c(min(data_list$mgus_prev_female_nhb$age_lower)-0.5,max(data_list$mgus_prev_female_nhb$age_lower)+0.5), ylim = c(0,0.35),
     xaxs = 'i', yaxs = 'i', axes = F, xlab = '', ylab = '')
polygon(x = c(data_list$mgus_prev_female_nhb$age_lower, rev(data_list$mgus_prev_female_nhb$age_lower)), y = c(preds$y_female_nhb_ppi['2.5%',] / data_list$mgus_prev_female_nhb$n, rev(preds$y_female_nhb_ppi['97.5%',] / data_list$mgus_prev_female_nhb$n)),
        col = col2alpha(palette[4],alpha = 0.25), border = NA)
polygon(x = c(data_list$mgus_prev_female_nhb$age_lower, rev(data_list$mgus_prev_female_nhb$age_lower)), y = c(preds$y_female_nhb_ppi['25%',] / data_list$mgus_prev_female_nhb$n, rev(preds$y_female_nhb_ppi['75%',] / data_list$mgus_prev_female_nhb$n)),
        col = col2alpha(palette[4],alpha = 0.5), border = NA)
lines(x = data_list$mgus_prev_female_nhb$age_lower, y = preds$y_female_nhb_ppi['50%',] / data_list$mgus_prev_female_nhb$n, col = palette[4], lwd = 2)
segments(x0 = data_list$mgus_prev_female_nhb$age_lower, y0 = p_female_nhb_lower, y1 = p_female_nhb_upper, lwd = 1.5, col = '#222222')
points(data_list$mgus_prev_female_nhb$age_lower, data_list$mgus_prev_female_nhb$y / data_list$mgus_prev_female_nhb$n, pch = 21, cex = 1.5, bg = '#222222', col = 'white')
axis(side = 1, at = seq(from = 45, to = 85, by = 5), labels = c(NA,'50-54', '55-59', '60-64', '65-69', '70-74', '75-79', '80-84', '85+'), las = 2, col.axis = col.axis.labs)
axis(side = 2, las = 1, at = seq(from = 0, to = 0.35, by = 0.05), labels = seq(from = 0, to = 35, by = 5), col.axis = col.axis.labs)
mtext(side = 3, line = -1, 'Non-Hispanic Black Female', font = 2, col = palette[4])


plot(NA, NA, xlim = c(min(data_list$mm_incid_male_nhw$age_lower)-1, max(data_list$mm_incid_male_nhw$age_lower)+1), ylim = c(0,200),
     xaxs = 'i', yaxs = 'i', axes = F, xlab = '', ylab = '')
polygon(x = c(data_list$mm_incid_male_nhw$age_lower, rev(data_list$mm_incid_male_nhw$age_lower)), y = c(preds$x_male_nhw_ppi['2.5%',] * 1e5, rev(preds$x_male_nhw_ppi['97.5%',] * 1e5)),
        col = col2alpha(palette[1],alpha = 0.25), border = NA)
polygon(x = c(data_list$mm_incid_male_nhw$age_lower, rev(data_list$mm_incid_male_nhw$age_lower)), y = c(preds$x_male_nhw_ppi['25%',] * 1e5, rev(preds$x_male_nhw_ppi['75%',] * 1e5)),
        col = col2alpha(palette[1], alpha = 0.5), border = NA)
lines(x = data_list$mm_incid_male_nhw$age_lower, y = preds$x_male_nhw_ppi['50%',] * 1e5, col = palette[1], lwd = 2)
points(data_list$mm_incid_male_nhw$age_lower, data_list$mm_incid_male_nhw$x * 1e5, pch = 21, cex = 1.5, bg = 'black', col = 'white')
axis(side = 1, at = seq(from = 0, to = 85, by = 5), labels = c('0', '5-9', '10-14', '15-19', '20-24', '25-29', '30-34', '35-39', '40-44',
                                                               '45-49', '50-54', '55-59', '60-64', '65-69', '70-74', '75-79', '80-84', '85+'),
     las = 2, col.axis = col.axis.labs)
axis(side = 2, las = 1, col.axis = col.axis.labs)
mtext(side = 1, line = 3.5, 'Age (yr)', col = '#222222')
mtext(side = 2, line = 2.6, 'MM Incidence (per 100,000)', col = '#222222')

legend('topleft', pch = c(16,15,15,NA), lwd = c(NA,NA,NA,2),
       col = c('#222222', col2alpha(palette[1],0.25),col2alpha(palette[1],0.5),palette[1]),
       pt.cex = 1.5, cex = 1, c('SEER 2010', '50% PPI', '95% PPI', 'Median'), bty = 'n')

plot(NA, NA, xlim = c(min(data_list$mm_incid_female_nhw$age_lower)-1, max(data_list$mm_incid_female_nhw$age_lower)+1), ylim = c(0,200),
     xaxs = 'i', yaxs = 'i', axes = F, xlab = '', ylab = '')
polygon(x = c(data_list$mm_incid_female_nhw$age_lower, rev(data_list$mm_incid_female_nhw$age_lower)), y = c(preds$x_female_nhw_ppi['2.5%',] * 1e5, rev(preds$x_female_nhw_ppi['97.5%',] * 1e5)),
        col = col2alpha(palette[2],alpha = 0.25), border = NA)
polygon(x = c(data_list$mm_incid_female_nhw$age_lower, rev(data_list$mm_incid_female_nhw$age_lower)), y = c(preds$x_female_nhw_ppi['25%',] * 1e5, rev(preds$x_female_nhw_ppi['75%',] * 1e5)),
        col = col2alpha(palette[2], alpha = 0.5), border = NA)
lines(x = data_list$mm_incid_female_nhw$age_lower, y = preds$x_female_nhw_ppi['50%',] * 1e5, col = palette[2], lwd = 2)
points(data_list$mm_incid_female_nhw$age_lower, data_list$mm_incid_female_nhw$x * 1e5, pch = 21, cex = 1.5, bg = 'black', col = 'white')
axis(side = 1, at = seq(from = 0, to = 85, by = 5), labels = c('0', '5-9', '10-14', '15-19', '20-24', '25-29', '30-34', '35-39', '40-44',
                                                               '45-49', '50-54', '55-59', '60-64', '65-69', '70-74', '75-79', '80-84', '85+'),
     las = 2, col.axis = col.axis.labs)
axis(side = 2, las = 1, col.axis = col.axis.labs)
mtext(side = 1, line = 3.5, 'Age (yr)', col = '#222222')

plot(NA, NA, xlim = c(min(data_list$mm_incid_male_nhb$age_lower)-1, max(data_list$mm_incid_male_nhb$age_lower)+1), ylim = c(0,200),
     xaxs = 'i', yaxs = 'i', axes = F, xlab = '', ylab = '')
polygon(x = c(data_list$mm_incid_male_nhb$age_lower, rev(data_list$mm_incid_male_nhb$age_lower)), y = c(preds$x_male_nhb_ppi['2.5%',] * 1e5, rev(preds$x_male_nhb_ppi['97.5%',] * 1e5)),
        col = col2alpha(palette[3],alpha = 0.25), border = NA)
polygon(x = c(data_list$mm_incid_male_nhb$age_lower, rev(data_list$mm_incid_male_nhb$age_lower)), y = c(preds$x_male_nhb_ppi['25%',] * 1e5, rev(preds$x_male_nhb_ppi['75%',] * 1e5)),
        col = col2alpha(palette[3], alpha = 0.5), border = NA)
lines(x = data_list$mm_incid_male_nhb$age_lower, y = preds$x_male_nhb_ppi['50%',] * 1e5, col = palette[3], lwd = 2)
points(data_list$mm_incid_male_nhb$age_lower, data_list$mm_incid_male_nhb$x * 1e5, pch = 21, cex = 1.5, bg = 'black', col = 'white')
axis(side = 1, at = seq(from = 0, to = 85, by = 5), labels = c('0', '5-9', '10-14', '15-19', '20-24', '25-29', '30-34', '35-39', '40-44',
                                                               '45-49', '50-54', '55-59', '60-64', '65-69', '70-74', '75-79', '80-84', '85+'),
     las = 2, col.axis = col.axis.labs)
axis(side = 2, las = 1, col.axis = col.axis.labs)
mtext(side = 1, line = 3.5, 'Age (yr)', col = '#222222')

plot(NA, NA, xlim = c(min(data_list$mm_incid_female_nhb$age_lower)-1, max(data_list$mm_incid_female_nhb$age_lower)+1), ylim = c(0,200),
     xaxs = 'i', yaxs = 'i', axes = F, xlab = '', ylab = '')
polygon(x = c(data_list$mm_incid_female_nhb$age_lower, rev(data_list$mm_incid_female_nhb$age_lower)), y = c(preds$x_female_nhb_ppi['2.5%',] * 1e5, rev(preds$x_female_nhb_ppi['97.5%',] * 1e5)),
        col = col2alpha(palette[4],alpha = 0.25), border = NA)
polygon(x = c(data_list$mm_incid_female_nhb$age_lower, rev(data_list$mm_incid_female_nhb$age_lower)), y = c(preds$x_female_nhb_ppi['25%',] * 1e5, rev(preds$x_female_nhb_ppi['75%',] * 1e5)),
        col = col2alpha(palette[4], alpha = 0.5), border = NA)
lines(x = data_list$mm_incid_female_nhb$age_lower, y = preds$x_female_nhb_ppi['50%',] * 1e5, col = palette[4], lwd = 2)
points(data_list$mm_incid_female_nhb$age_lower, data_list$mm_incid_female_nhb$x * 1e5, pch = 21, cex = 1.5, bg = 'black', col = 'white')
axis(side = 1, at = seq(from = 0, to = 85, by = 5), labels = c('0', '5-9', '10-14', '15-19', '20-24', '25-29', '30-34', '35-39', '40-44',
                                                               '45-49', '50-54', '55-59', '60-64', '65-69', '70-74', '75-79', '80-84', '85+'),
     las = 2, col.axis = col.axis.labs)
axis(side = 2, las = 1, col.axis = col.axis.labs)
mtext(side = 1, line = 3.5, 'Age (yr)', col = '#222222')
dev.off()
