# set working directory
setwd('~/Documents/MM_Model_Race/code/')

# clear existing workspace
rm(list = ls())

# load necessary functions
source('functions_convergence.R')

# get convergence estimates and pool for no mgus mortality model
get_convergence_stats(path.in = '../output/sens_mgus_mortality/',
                      file.pattern = 'posterior_.*bz2',
                      burn.in = 5e5,
                      thin.factor = 50)

pool_chain(path.in = '../output/sens_mgus_mortality/',
           file.pattern = 'posterior_.*bz2',
           burn.in = 5e5,
           thin.factor = 50,
           file.out = '../output/sens_mgus_mortality/posterior_pooled.csv')

# load the posteriors 
post_main <- read.csv('../output/posterior_pooled.csv')
post_sens <- read.csv('../output/sens_mgus_mortality/posterior_pooled.csv')

# compute the quantiles for each parameter 
post_main_ci <- apply(post_main, 2, quantile, probs = c(0.025, 0.50, 0.975))
post_sens_ci <- apply(post_sens, 2, quantile, probs = c(0.025, 0.50, 0.975))

# save output to RData structure
save(post_main,
     post_sens,
     post_main_ci,
     post_sens_ci,
     file = '../output/fig_S4.RData')
