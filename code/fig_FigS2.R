# set working directory
setwd('~/Documents/MM_Model_Race/code/')

# clear existing workspace
rm(list = ls())

# install necessary packages
if(!require(seqinr)){install.packages('seqinr'); library(seqinr)}

# load output 
load('../output/fig_S2.RData')

# generate ylims 
xmin <- apply(sapply(1:length(params), function(x){apply(params[[x]],2, min)}), 1, min)
xmax <- apply(sapply(1:length(params), function(x){apply(params[[x]],2, max)}), 1, max)

xmin = ifelse(xmin < 0, 1.25, 0.75) * xmin
xmax = ifelse(xmax > 0, 1.25, 0.75) * xmax

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
jpeg(filename = '../output/figs/fig_S2.jpg', width = 10, height = 4, units = 'in', res = 500)
layout(mat = matrix(1:10, nrow = 2, ncol = 5, byrow = T))
par(mar = c(3.3,4.1,1.6,1.1))

for(ii in 1:ncol(params[[1]]))
{
  plot(NA, NA, xlim = c(xmin[ii], xmax[ii]), ylim = c(xmin[ii], xmax[ii]), axes = F,
       xlab = '', ylab = '', xaxs = 'i', yaxs = 'i')
  lines(-100:100,-100:100, lwd = 1.5, col = 'gray', lty = 2)
  box()
  axis(side = 1)
  axis(side = 2, las = 1)
  for(jj in 1:length(posts))
  {
    segments(x0 = params[[jj]][,ii], y0 = posts[[jj]]['2.5%',ii], y1 = posts[[jj]]['97.5%',ii], lwd = 1.5)
    points(x = params[[jj]][,ii], y = posts[[jj]]['50%',ii], pch = 21, bg = 'black', col = 'white', cex = 1.5)
  }
  mtext(side = 3, line = 0, params.names[ii])
  if(ii > 5){mtext(side = 1, line = 2.3, 'True Value')} 
  if(ii == 1 || ii == 6){mtext(side = 2, line = 2.9, 'Estimated Value')}
}
dev.off()

