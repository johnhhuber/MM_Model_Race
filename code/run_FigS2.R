# set working directory
setwd('~/Documents/MM_Model_Race/code/')

# clear existing workspace
rm(list = ls())

# load necessary functions
source('functions_convergence.R')

# specify path in 
path.in = '../output/sim_sweep/'

# specify number of simulations 
n_sims <- 410

# loop through the simulation files and add to list if converged
posts <- list()
params <- list()
counter <- 1 

for(ii in 1:n_sims)
{
  print(ii)
  is_converged <- (get_convergence_stats(path.in = path.in, file.pattern = paste('posterior_', ii, '_.*bz2', sep = ''), burn.in = 0, thin.factor = 1)$mpsrf < 1.1)
  if(is_converged)
  {
    posts_pooled <- lapply(list.files(path = path.in, pattern = paste('posterior_', ii, '_.*bz2', sep = ''), full.names = T), read.csv)
    posts[[counter]] <- apply(do.call(rbind, posts_pooled), 2, quantile, probs = c(0.025,0.50,0.975))
    params[[counter]] <- read.csv(file = paste(path.in, 'params_', ii, '.csv', sep = ''))
    counter <- counter + 1
  }
}

# compute the coverage probabilities
coverage_probs <- rep(0, ncol(posts[[1]]))
for(ii in 1:length(posts))
{
  coverage_probs <- coverage_probs + 1 * sapply(1:ncol(posts[[ii]]), function(jj){params[[ii]][jj] >= posts[[ii]]['2.5%',jj] & params[[ii]][jj] <= posts[[ii]]['97.5%',jj]})
}
coverage_probs <- coverage_probs / length(posts)

# save to file 
save(params, posts, file = '../output/fig_S2.RData')
