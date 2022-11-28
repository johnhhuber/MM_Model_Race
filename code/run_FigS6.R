# set working directory
setwd('~/Documents/MM_Model_Race/code/')

# clear existing workspace
rm(list = ls())

# load necessary functions
source('functions_convergence.R')
source('functions_likelihood.R')
source('functions_validate_inference.R')

# load the data
load('../output/data_organized_2004.RData')

# load the posterior distribution and specify the burn in period 
burn.in = 1:5e5
thin.factor = 50

get_convergence_stats(path.in = '../output/mcmc_2004/',
                      file.pattern = 'posterior_.*bz2',
                      burn.in = max(burn.in),
                      thin.factor = thin.factor)

pool_chain(path.in = '../output/mcmc_2004/',
           file.pattern = 'posterior_.*bz2',
           burn.in = max(burn.in),
           thin.factor = thin.factor,
           file.out = '../output/mcmc_2004/posterior_pooled.csv')
post <- read.csv('../output/mcmc_2004/posterior_pooled.csv')
