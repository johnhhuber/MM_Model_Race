# set working directory
setwd('~/Documents/MM_Model_Race/code/')

# clear existing workspace
rm(list = ls())

# load necessary functions
source('functions_sim_sweep.R')

# load the data
load('../output/data_organized.RData')

# specify the relevant parameters
num_files = 2
path_out = '../output/sim_sweep/'

params_lower <- c(-20, # gamma_mgus 
                  0, # beta_mgus_age
                  -5, # beta_mgus_sex
                  -5, # beta_mgus_race
                  -5, # gamma_mm
                  -5, # beta_mm_age
                  -5, # beta_mm_age_quad
                  -5, # beta_mm_sex
                  -5, # beta_mm_race
                  0) # tau

params_upper <- c(0, # gamma_mgus 
                  0.5, # beta_mgus_age
                  1, # beta_mgus_sex
                  1, # beta_mgus_race
                  0, # gamma_mm
                  1, # beta_mm_age
                  1, # beta_mm_age_quad
                  1, # beta_mm_sex
                  1, # beta_mm_race
                  10) # tau

# generate sweep
gen_sim_sweep(n = num_files,
              path_out = path_out,
              params_lower = params_lower,
              params_upper = params_upper,
              ages = ages,
              pop_male_nhw = pop_male_nhw,
              pop_male_nhb = pop_male_nhb,
              pop_female_nhw = pop_female_nhw,
              pop_female_nhb = pop_female_nhb,
              mort_male_nhw = mort_male_nhw,
              mort_male_nhb = mort_male_nhb,
              mort_female_nhw = mort_female_nhw,
              mort_female_nhb = mort_female_nhb,
              mgus_mortality_multiplier_male = mgus_mortality_multiplier_male,
              mgus_mortality_multiplier_female = mgus_mortality_multiplier_female,
              mu_mm_male_nhw = mm_mu_nhw_male,
              mu_mm_male_nhb = mm_mu_nhb_male,
              mu_mm_female_nhw = mm_mu_nhw_female,
              mu_mm_female_nhb = mm_mu_nhb_female,
              data_list = data_list)
