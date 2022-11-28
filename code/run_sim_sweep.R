# set working directory
setwd('~/Documents/MM_Model_Race/code/')

# clear existing workspace
rm(list = ls())

# load necessary functions
source('functions_sim_sweep.R')

# load the data
load('../output/data_organized.RData')

# load the valid parameter sets 
params <- read.csv('../output/sim_sweep/sobol_sweep.csv')

# specify path out 
path_out = '../output/sim_sweep/'

# loop through and generate data
for(ii in 1:nrow(params))
{
  print(ii)
  sim_data_list <- gen_sim_data(params = as.numeric(params[ii,]),
               ages = ages,
               pop_male_nhw = pop_male_nhw,
               pop_female_nhw = pop_female_nhw,
               pop_male_nhb = pop_male_nhb,
               pop_female_nhb = pop_female_nhb,
               mort_male_nhw = mort_male_nhw,
               mort_female_nhw = mort_female_nhw,
               mort_male_nhb = mort_male_nhb,
               mort_female_nhb = mort_female_nhb,
               mgus_mortality_multiplier_male = mgus_mortality_multiplier_male,
               mgus_mortality_multiplier_female = mgus_mortality_multiplier_female,
               mu_mm_male_nhw = mm_mu_nhw_male,
               mu_mm_female_nhw = mm_mu_nhw_female,
               mu_mm_male_nhb = mm_mu_nhb_male,
               mu_mm_female_nhb = mm_mu_nhb_female,
               data_list = data_list)
  
  # write the data to file
  write.csv(sim_data_list$mgus_prev_male_nhw, file = paste(path_out, 'mgus_prev_male_nhw_', ii, '.csv', sep = ''), row.names = F)
  write.csv(sim_data_list$mgus_prev_female_nhw, file = paste(path_out, 'mgus_prev_female_nhw_', ii, '.csv', sep = ''), row.names = F)
  write.csv(sim_data_list$mgus_prev_male_nhb, file = paste(path_out, 'mgus_prev_male_nhb_', ii, '.csv', sep = ''), row.names = F)
  write.csv(sim_data_list$mgus_prev_female_nhb, file = paste(path_out, 'mgus_prev_female_nhb_', ii, '.csv', sep = ''), row.names = F)

  write.csv(sim_data_list$mm_incid_male_nhw, file = paste(path_out, 'i_mm_male_nhw_', ii, '.csv', sep = ''), row.names = F)
  write.csv(sim_data_list$mm_incid_female_nhw, file = paste(path_out, 'i_mm_female_nhw_', ii, '.csv', sep = ''), row.names = F)
  write.csv(sim_data_list$mm_incid_male_nhb, file = paste(path_out, 'i_mm_male_nhb_', ii, '.csv', sep = ''), row.names = F)
  write.csv(sim_data_list$mm_incid_female_nhb, file = paste(path_out, 'i_mm_female_nhb_', ii, '.csv', sep = ''), row.names = F)

  # write the params to file
  write.csv(params[ii,], file = paste(path_out, 'params_', ii, '.csv', sep = ''), row.names = F)
}
