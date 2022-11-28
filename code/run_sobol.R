# set working directory 
setwd('~/Documents/MM_Model_Race/code/')

# clear existing workspace
rm(list = ls())

# load necessary functions
source('functions_model.R')

# install necessary packages
if(!require(pomp)){install.packages('pomp'); library(pomp)}

# load necessary data
load('../output/data_organized.RData')

# specify ages for mgus data and mm data
ages_mgus <- min(data_list$mgus_prev_male_nhw$age_lower):max(data_list$mgus_prev_male_nhw$age_upper)
ages_mm <- min(data_list$mm_incid_male_nhw$age_lower):max(data_list$mm_incid_male_nhw$age_upper)

# specify upper and lower bounds for each parameter
params_lower <- c(gamma_mgus = -20, # gamma_mgus 
                  beta_mgus_age = 0, # beta_mgus_age
                  beta_mgus_sex = -2, # beta_mgus_sex
                  beta_mgus_race = -2, # beta_mgus_race
                  gamma_mm = -20, # gamma_mm
                  beta_mm_age = -2, # beta_mm_age
                  beta_mm_age_quad = -0.25, # beta_mm_age_quad
                  beta_mm_sex = -2, # beta_mm_sex
                  beta_mm_race = -2, # beta_mm_race
                  tau = 0)

params_upper <- c(gamma_mgus = 0, # gamma_mgus 
                  beta_mgus_age = 0.5, # beta_mgus_age
                  beta_mgus_sex = 2, # beta_mgus_sex
                  beta_mgus_race = 2, # beta_mgus_race
                  gamma_mm = 0, # gamma_mm
                  beta_mm_age = 1, # beta_mm_age
                  beta_mm_age_quad = 0.25, # beta_mm_age_quad
                  beta_mm_sex = 2, # beta_mm_sex
                  beta_mm_race = 2, # beta_mm_race
                  tau = 1)

# create sobol sweep 
samps <- sobol_design(lower = params_lower,
             upper = params_upper, 
             nseq = 1e6)

# loop through and check whether the parameter sets can generate non-zero MGUS prevalence and MM incidence 
is_valid <- rep(NA, nrow(samps))
i_threshold = 1e-5
p_threshold = 0.01

for(ii in 1:nrow(samps))
{
  print(ii)
  
  # simulate model
  out_male_nhw <- sim_model(ages = ages,
                            gamma_mgus = samps$gamma_mgus[ii],
                            beta_mgus_age = samps$beta_mgus_age[ii],
                            beta_mgus_age_quad = 0,
                            beta_mgus_sex = 0,
                            beta_mgus_race = 0,
                            gamma_mm = samps$gamma_mm[ii],
                            beta_mm_age = samps$beta_mm_age[ii],
                            beta_mm_age_quad = samps$beta_mm_age_quad[ii],
                            beta_mm_sex = 0,
                            beta_mm_race = 0,
                            mgus_mortality_multiplier = mgus_mortality_multiplier_male,
                            mort = mort_male_nhw,
                            mu_mm = mm_mu_nhw_male)
  
  p_mgus_male_nhw <- out_male_nhw$p_mgus
  i_mm_male_nhw <- out_male_nhw$i_mm
  
  is_valid[ii] = ifelse(all(!is.na(p_mgus_male_nhw)) & all(p_mgus_male_nhw >= 0) & any(p_mgus_male_nhw[ages_mgus+1] > p_threshold) & all(i_mm_male_nhw >= 0) & any(i_mm_male_nhw[ages_mm+1] > i_threshold), 1, 0)
  
  if(is_valid[ii])
  {
    out_male_nhb <- sim_model(ages = ages,
                              gamma_mgus = samps$gamma_mgus[ii],
                              beta_mgus_age = samps$beta_mgus_age[ii],
                              beta_mgus_age_quad = 0,
                              beta_mgus_sex = 0,
                              beta_mgus_race = samps$beta_mgus_race[ii],
                              gamma_mm = samps$gamma_mm[ii],
                              beta_mm_age = samps$beta_mm_age[ii],
                              beta_mm_age_quad = samps$beta_mm_age_quad[ii],
                              beta_mm_sex = 0,
                              beta_mm_race = samps$beta_mm_race[ii],
                              mgus_mortality_multiplier = mgus_mortality_multiplier_male,
                              mort = mort_male_nhb,
                              mu_mm = mm_mu_nhb_male)
    
    p_mgus_male_nhb <- out_male_nhb$p_mgus
    i_mm_male_nhb <- out_male_nhb$i_mm
    
    is_valid[ii] = ifelse(all(!is.na(p_mgus_male_nhb)) & all(p_mgus_male_nhb >= 0) & any(p_mgus_male_nhb[ages_mgus+1] > p_threshold) & all(i_mm_male_nhb >= 0) & any(i_mm_male_nhb[ages_mm+1] > i_threshold), 1, 0)
  }
  
  if(is_valid[ii])
  {
    out_female_nhw <- sim_model(ages = ages,
                                gamma_mgus = samps$gamma_mgus[ii],
                                beta_mgus_age = samps$beta_mgus_age[ii],
                                beta_mgus_age_quad = 0,
                                beta_mgus_sex = samps$beta_mgus_sex[ii],
                                beta_mgus_race = 0,
                                gamma_mm = samps$gamma_mm[ii],
                                beta_mm_age = samps$beta_mm_age[ii],
                                beta_mm_age_quad = samps$beta_mm_age_quad[ii],
                                beta_mm_sex = samps$beta_mm_sex[ii],
                                beta_mm_race = 0,
                                mgus_mortality_multiplier = mgus_mortality_multiplier_female,
                                mort = mort_female_nhw,
                                mu_mm = mm_mu_nhw_female)
    
    p_mgus_female_nhw <- out_female_nhw$p_mgus
    i_mm_female_nhw <- out_female_nhw$i_mm
    
    is_valid[ii] = ifelse(all(!is.na(p_mgus_female_nhw)) & all(p_mgus_female_nhw >= 0) & any(p_mgus_female_nhw[ages_mgus+1] > p_threshold) & all(i_mm_female_nhw >= 0) & any(i_mm_female_nhw[ages_mm+1] > i_threshold), 1, 0)
  }
  
  if(is_valid[ii])
  {
    out_female_nhb <- sim_model(ages = ages,
                                gamma_mgus = samps$gamma_mgus[ii],
                                beta_mgus_age = samps$beta_mgus_age[ii],
                                beta_mgus_age_quad = 0,
                                beta_mgus_sex = samps$beta_mgus_sex[ii],
                                beta_mgus_race = samps$beta_mgus_race[ii],
                                gamma_mm = samps$gamma_mm[ii],
                                beta_mm_age = samps$beta_mm_age[ii],
                                beta_mm_age_quad = samps$beta_mm_age_quad[ii],
                                beta_mm_sex = samps$beta_mm_sex[ii],
                                beta_mm_race = samps$beta_mm_race[ii],
                                mgus_mortality_multiplier = mgus_mortality_multiplier_female,
                                mort = mort_female_nhb,
                                mu_mm = mm_mu_nhb_female)
    
    p_mgus_female_nhb <- out_female_nhb$p_mgus
    i_mm_female_nhb <- out_female_nhb$i_mm
    
    is_valid[ii] = ifelse(all(!is.na(p_mgus_female_nhb)) & all(p_mgus_female_nhb >= 0) & any(p_mgus_female_nhb[ages_mgus+1] > p_threshold) & all(i_mm_female_nhb >= 0) & any(i_mm_female_nhb[ages_mm+1] > i_threshold), 1, 0)
  }
}

# subset to data sets that are valid 
samps <- samps[which(is_valid == 1),]

# write to file 
write.csv(samps, file = '../output/sim_sweep/sobol_sweep.csv', row.names = F)
