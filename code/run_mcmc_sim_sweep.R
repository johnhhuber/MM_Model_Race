# set working directory
setwd('~/Documents/MM_Model_Race/code/')

# clear existing workspace
rm(list = ls())

# set seed 
set.seed(1)

# specify index
index = 6

# load necessary functions
source('functions_likelihood.R')

# load necessary data
load('../output/data_organized.RData')

# load necessary files
path.in = '../output/sim_sweep/'
mgus_prev_male_nhw <- read.csv(paste(path.in, 'mgus_prev_male_nhw_', index, '.csv', sep = ''))
mgus_prev_male_nhb <- read.csv(paste(path.in, 'mgus_prev_male_nhb_', index, '.csv', sep = ''))
mgus_prev_female_nhw <- read.csv(paste(path.in, 'mgus_prev_female_nhw_', index, '.csv', sep = ''))
mgus_prev_female_nhb <- read.csv(paste(path.in, 'mgus_prev_female_nhb_', index, '.csv', sep = ''))

mm_incid_male_nhw <- read.csv(paste(path.in, 'i_mm_male_nhw_', index, '.csv', sep = ''))
mm_incid_male_nhb <- read.csv(paste(path.in, 'i_mm_male_nhb_', index, '.csv', sep = ''))
mm_incid_female_nhw <- read.csv(paste(path.in, 'i_mm_female_nhw_', index, '.csv', sep = ''))
mm_incid_female_nhb <- read.csv(paste(path.in, 'i_mm_female_nhb_', index, '.csv', sep = ''))

# update data list 
data_list$mgus_prev_male_nhw <- mgus_prev_male_nhw
data_list$mgus_prev_male_nhb <- mgus_prev_male_nhb
data_list$mgus_prev_female_nhw <- mgus_prev_female_nhw
data_list$mgus_prev_female_nhb <- mgus_prev_female_nhb

data_list$mm_incid_male_nhw <- mm_incid_male_nhw
data_list$mm_incid_male_nhb <- mm_incid_male_nhb
data_list$mm_incid_female_nhw <- mm_incid_female_nhw
data_list$mm_incid_female_nhb <- mm_incid_female_nhb

# bayesian setup

# bayesian setup
params_lower <- c(-20, # gamma_mgus 
                  0, # beta_mgus_age
                  -15, # beta_mgus_sex
                  -15, # beta_mgus_race
                  -20, # gamma_mm
                  -15, # beta_mm_age
                  -15, # beta_mm_age_quad
                  -15, # beta_mm_sex
                  -15, # beta_mm_race
                  0)

params_upper <- c(0, # gamma_mgus 
                  1, # beta_mgus_age
                  5, # beta_mgus_sex
                  5, # beta_mgus_race
                  0, # gamma_mm
                  1, # beta_mm_age
                  1, # beta_mm_age_quad
                  5, # beta_mm_sex
                  5, # beta_mm_race
                  100)


bayesianSetup <- createBayesianSetup(ll_cmp, lower = params_lower, upper = params_upper)
settings = list(iterations = 1e6, message = T)

# run mcmc
out <- runMCMC(bayesianSetup, settings, sampler = "DEzs")
post <- out$Z  

# specify burn in and thin factor 
burn.in = 5e5
thin.factor = 50

# write to file 
write.csv(post[seq(from = burn.in + 1, to = nrow(post), by = thin.factor),],
          file = paste(path.in, 'posterior_', index, '.csv', sep = ''),
          row.names = F)
