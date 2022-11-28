# set working directory
setwd('~/Documents/MM_Model_Race/code/')

# clear existing workspace
rm(list = ls())

# install necessary packages
if(!require(nord)){install.packages('nord'); library(nord)}

# list the necessary files
post.files <- list.files(path = '../output/', pattern = 'posterior_.*bz2', full.names = T)

# load the posteriors
posts <- lapply(post.files, read.csv)

# specify burn in and thin factor
burn.in = 5e5
num.samples = nrow(posts[[1]])
thin.factor = 50

indices <- seq(from = burn.in + 1, to = num.samples, by = thin.factor)

# generate palette
palette <- nord(palette = 'aurora', n = 5)

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
jpeg(filename = '../output/figs/fig_S3.jpg', width = 10, height = 4, units = 'in', res = 500)
par(mar = c(3.3,4.1,1.6,1.1))
layout(mat = matrix(1:10, nrow = 2, ncol = 5, byrow = T))
for(ii in 1:ncol(posts[[1]]))
{
  plot(posts[[1]][indices,ii], type = 'n', axes = 'F', xlab = '', ylab = '', xaxs = 'i', yaxs = 'i')
  for(jj in 1:length(posts))
  {
    lines(posts[[jj]][indices,ii], col = palette[jj])
  }
  box()
  axis(side = 1)
  axis(side = 2, las = 1)
  mtext(side = 3, line = 0, params.names[ii])
  if(ii > 5){mtext(side = 1, line = 2.3, 'Sample')} 
  if(ii == 1 || ii == 6){mtext(side = 2, line = 2.9, 'Value')}
}
dev.off()