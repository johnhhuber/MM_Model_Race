# set working directory
setwd('~/Documents/MM_Model_Race/code/')

# clear existing workspace
rm(list = ls())

# install necessary packages
if(!require(seqinr)){install.packages('seqinr'); library(seqinr)}

# load output 
load('../output/fig_S5.RData')

# generate ylims 
xmin = pmin(apply(post_main,2,min), apply(post_sens,2,min))
xmin = ifelse(xmin < 0, 1.01, 0.99) * xmin
xmax = pmin(apply(post_main,2,max), apply(post_sens,2,max))
xmax = ifelse(xmax > 0, 1.01, 0.99) * xmax

# vector of parameter names
params.names <- c(expression(gamma['MGUS']),
                  expression(beta['MGUS,a']),
                  expression(beta['MGUS,s']),
                  expression(beta['MGUS,r']),
                  expression(gamma['MM']),
                  expression(beta['MM,a']),
                  expression(beta['MM,'*'a'^2]),
                  expression(beta['MM,s']),
                  expression(beta['MM,r']),
                  expression(tau^2))

# generate plot 
jpeg(filename = '../output/figs/fig_S5.jpg', width = 10, height = 4, units = 'in', res = 500)
layout(mat = matrix(1:10, nrow = 2, ncol = 5, byrow = T))
par(mar = c(3.3,4.1,1.6,1.1))
for(ii in 1:ncol(post_main))
{
  x <- hist(post_main[,ii], xlim = c(xmin[ii], xmax[ii]), col = col2alpha('#222222', alpha = 0.25), freq = F,
            border = 'white', axes = F, xlab = '', ylab = '', main = '', xaxs = 'i')
  hist(post_sens[,ii], xlim = c(xmin[ii], xmax[ii]), col = col2alpha('salmon', alpha = 0.25), freq = F, add = T,
       border = 'white')
  segments(x0 = post_main_ci['2.5%',ii], x1 = post_main_ci['97.5%',ii], y0 = 0, lwd = 1.5, col = '#222222')
  segments(x0 = post_sens_ci['2.5%',ii], x1 = post_sens_ci['97.5%',ii], y0 = 0.1 * 1.05 * max(x$density), lwd = 1.5, col = 'salmon')
  points(x = post_main_ci['50%',ii], y = 0, pch = 16, col = '#222222', cex = 1.5)
  points(x = post_sens_ci['50%',ii], y = 0.1 * 1.05 * max(x$density), pch = 16, col = 'salmon', cex = 1.5)
  axis(side = 1)
  axis(side = 2, las = 1)
  box()
  mtext(side = 3, line = 0, params.names[ii])
  if(ii > 5){mtext(side = 1, line = 2.3, 'Value')} 
  if(ii == 1 || ii == 6){mtext(side = 2, line = 2.9, 'Density')}
}
legend('topright', pch = c(15), col = c('#222222','salmon'), pt.cex = 1.25, legend = c('Default', 'No MGUS Mortality'),
       bty = 'n', cex = 0.8)
dev.off()
