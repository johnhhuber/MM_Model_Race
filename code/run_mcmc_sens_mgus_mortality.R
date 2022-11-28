# set working directory
setwd('~/Documents/MM_Model_Race/code/')

# clear existing workspace
rm(list = ls())

# load necessary functions
source('functions_likelihood.R')

# specify chain id 
chain_id = 5

# set seed for reproducibility
set.seed(chain_id)

# load the data list 
load('../output/data_organized.RData')

# set mgus mortality multiplier to 1
mgus_mortality_multiplier_female = 1
mgus_mortality_multiplier_male = 1

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

# write to file 
write.csv(post, file = paste('../output/sens_mgus_mortality/posterior_', chain_id, '.csv', sep = ''), row.names = F)
