# set working directory
setwd('~/Documents/MM_Model_Race/code/')

# clear existing workspace
rm(list = ls())

# set seed for reproducibility
set.seed(1)

# load existing workspace
source('functions_convergence.R')
source('functions_likelihood.R')

# load necessary data
load('../output/data_organized.RData')

# get convergence for alternative model
get_convergence_stats(path.in = '../output/mcmc_alt/',
                      file.pattern = 'posterior_.*bz2',
                      burn.in = 5e5,
                      thin.factor = 50)

# pool estimates from alternative model
pool_chain(path.in = '../output/mcmc_alt/',
           file.pattern = 'posterior_.*bz2',
           burn.in = 5e5,
           thin.factor = 50,
           file.out = '../output/mcmc_alt/posterior_pooled.csv')

# load the posteriors 
post_main <- read.csv('../output/posterior_pooled.csv')
post_alt <- read.csv('../output/mcmc_alt/posterior_pooled.csv')

# calculate DIC
num.samps <- 5000
DIC(post = post_main, likelihood = ll_cmp, num.samps = num.samps)
DIC(post = post_alt, likelihood = ll_alt_cmp, num.samps = num.samps)

# load the posterior distribution and specify the burn in period 
get_convergence_stats(path.in = '../output/mcmc_no_age/',
                      file.pattern = 'posterior_.*bz2',
                      burn.in = 5e5,
                      thin.factor = 50)

pool_chain(path.in = '../output/mcmc_no_age/',
           file.pattern = 'posterior_.*bz2',
           burn.in = 5e5,
           thin.factor = 50,
           file.out = '../output/mcmc_no_age/posterior_pooled.csv')

# load the posteriors 
post_main <- read.csv('../output/posterior_pooled.csv')
post_alt <- read.csv('../output/mcmc_no_age/posterior_pooled.csv')

# calculate DIC
num.samps <- 5000
DIC(post = post_alt, likelihood = ll_age_cmp, num.samps = num.samps)
