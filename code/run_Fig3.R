# set working directory
setwd('~/Documents/MM_Model_Race/code/')

# clear existing workspace
rm(list = ls())

# load necessary functions
source('functions_likelihood.R')
source('functions_validate_inference.R')

# load necessary data
load('../output/data_organized.RData')

# load the posterior distribution and specify the burn in period 
post <- read.csv('../output/posterior_pooled.csv')

# draw samples 
num.samps = 500
samps <- sample(1:nrow(post), size = num.samps, replace = T)

ages <- 50:99

# loop through and calculate the preclinical period and risk for each sex and race pairing 
mat_dur_mgus_male_nhw <- matrix(NA, nrow = num.samps, ncol = length(ages))
mat_dur_mgus_female_nhw <- matrix(NA, nrow = num.samps, ncol = length(ages))
mat_dur_mgus_male_nhb <- matrix(NA, nrow = num.samps, ncol = length(ages))
mat_dur_mgus_female_nhb <- matrix(NA, nrow = num.samps, ncol = length(ages))

mat_risk_mgus_male_nhw <- matrix(NA, nrow = num.samps, ncol = length(ages))
mat_risk_mgus_female_nhw <- matrix(NA, nrow = num.samps, ncol = length(ages))
mat_risk_mgus_male_nhb <- matrix(NA, nrow = num.samps, ncol = length(ages))
mat_risk_mgus_female_nhb <- matrix(NA, nrow = num.samps, ncol = length(ages))

for(i in 1:num.samps)
{
  print(i)
  for(a in 1:(length(ages)-1))
  {
    # simulate models
    sim_male_nhw <- sim_model(ages = ages[a]:max(ages),
                              gamma_mgus = post[samps[i],1],
                              beta_mgus_age = post[samps[i],2],
                              beta_mgus_age_quad = 0,
                              beta_mgus_sex = 0,
                              beta_mgus_race = 0,
                              gamma_mm = post[samps[i],5],
                              beta_mm_age = post[samps[i],6],
                              beta_mm_age_quad = post[samps[i],7],
                              beta_mm_sex = 0,
                              beta_mm_race = 0,
                              mgus_mortality_multiplier = mgus_mortality_multiplier_male,
                              mort = mort_male_nhw,
                              mu_mm = mm_mu_nhw_male,
                              xstart = c(H = 0, MGUS = 1, MM = 0, D_H = 0, D_MGUS = 0, D_MM = 0))
    
    sim_male_nhb <- sim_model(ages = ages[a]:max(ages),
                              gamma_mgus = post[samps[i],1],
                              beta_mgus_age = post[samps[i],2],
                              beta_mgus_age_quad = 0,
                              beta_mgus_sex = 0,
                              beta_mgus_race = post[samps[i],4],
                              gamma_mm = post[samps[i],5],
                              beta_mm_age = post[samps[i],6],
                              beta_mm_age_quad = post[samps[i],7],
                              beta_mm_sex = 0,
                              beta_mm_race = post[samps[i],9],
                              mgus_mortality_multiplier = mgus_mortality_multiplier_male,
                              mort = mort_male_nhb,
                              mu_mm = mm_mu_nhb_male,
                              xstart = c(H = 0, MGUS = 1, MM = 0, D_H = 0, D_MGUS = 0, D_MM = 0))
    
    sim_female_nhw <- sim_model(ages = ages[a]:max(ages),
                                gamma_mgus = post[samps[i],1],
                                beta_mgus_age = post[samps[i],2],
                                beta_mgus_age_quad = 0,
                                beta_mgus_sex = post[samps[i],3],
                                beta_mgus_race = 0,
                                gamma_mm = post[samps[i],5],
                                beta_mm_age = post[samps[i],6],
                                beta_mm_age_quad = post[samps[i],7],
                                beta_mm_sex = post[samps[i],8],
                                beta_mm_race = 0,
                                mgus_mortality_multiplier = mgus_mortality_multiplier_female,
                                mort = mort_female_nhw,
                                mu_mm = mm_mu_nhw_female,
                                xstart = c(H = 0, MGUS = 1, MM = 0, D_H = 0, D_MGUS = 0, D_MM = 0))
    
    sim_female_nhb <- sim_model(ages = ages[a]:max(ages),
                                gamma_mgus = post[samps[i],1],
                                beta_mgus_age = post[samps[i],2],
                                beta_mgus_age_quad = 0,
                                beta_mgus_sex = post[samps[i],3],
                                beta_mgus_race = post[samps[i],4],
                                gamma_mm = post[samps[i],5],
                                beta_mm_age = post[samps[i],6],
                                beta_mm_age_quad = post[samps[i],7],
                                beta_mm_sex = post[samps[i],8],
                                beta_mm_race = post[samps[i],9],
                                mgus_mortality_multiplier = mgus_mortality_multiplier_male,
                                mort = mort_female_nhb,
                                mu_mm = mm_mu_nhb_female,
                                xstart = c(H = 0, MGUS = 1, MM = 0, D_H = 0, D_MGUS = 0, D_MM = 0))
    
    # calculate the expected preclinical duration 
    mat_dur_mgus_male_nhw[i,a] = sum((ages[a]:max(ages) - ages[a]) * (sim_male_nhw$rate_mm)) / sum(sim_male_nhw$rate_mm)
    mat_dur_mgus_female_nhw[i,a] = sum((ages[a]:max(ages) - ages[a]) * (sim_female_nhw$rate_mm)) / sum(sim_female_nhw$rate_mm)
    mat_dur_mgus_male_nhb[i,a] = sum((ages[a]:max(ages) - ages[a]) * (sim_male_nhb$rate_mm)) / sum(sim_male_nhb$rate_mm)
    mat_dur_mgus_female_nhb[i,a] = sum((ages[a]:max(ages) - ages[a]) * (sim_female_nhb$rate_mm)) / sum(sim_female_nhb$rate_mm)
    
    mat_risk_mgus_male_nhw[i,a] = tail(sim_male_nhw$cum_prop_mm, n = 1)
    mat_risk_mgus_female_nhw[i,a] = tail(sim_female_nhw$cum_prop_mm, n = 1)
    mat_risk_mgus_male_nhb[i,a] = tail(sim_male_nhb$cum_prop_mm, n = 1)
    mat_risk_mgus_female_nhb[i,a] = tail(sim_female_nhb$cum_prop_mm, n = 1)
  }
}

# generate credible intervals 
mat_dur_mgus_male_nhw_CI <- apply(mat_dur_mgus_male_nhw, 2, quantile, probs = c(0.025,0.50,0.975), na.rm = T)
mat_dur_mgus_female_nhw_CI <- apply(mat_dur_mgus_female_nhw, 2, quantile, probs = c(0.025,0.50,0.975), na.rm = T)
mat_dur_mgus_male_nhb_CI <- apply(mat_dur_mgus_male_nhb, 2, quantile, probs = c(0.025,0.50,0.975), na.rm = T)
mat_dur_mgus_female_nhb_CI <- apply(mat_dur_mgus_female_nhb, 2, quantile, probs = c(0.025,0.50,0.975), na.rm = T)

mat_risk_mgus_male_nhw_CI <- apply(mat_risk_mgus_male_nhw, 2, quantile, probs = c(0.025,0.50,0.975), na.rm = T)
mat_risk_mgus_female_nhw_CI <- apply(mat_risk_mgus_female_nhw, 2, quantile, probs = c(0.025,0.50,0.975), na.rm = T)
mat_risk_mgus_male_nhb_CI <- apply(mat_risk_mgus_male_nhb, 2, quantile, probs = c(0.025,0.50,0.975), na.rm = T)
mat_risk_mgus_female_nhb_CI <- apply(mat_risk_mgus_female_nhb, 2, quantile, probs = c(0.025,0.50,0.975), na.rm = T)

# save output to file 
save(list = c('mat_dur_mgus_male_nhw_CI',
              'mat_dur_mgus_female_nhw_CI',
              'mat_dur_mgus_male_nhb_CI',
              'mat_dur_mgus_female_nhb_CI',
              'mat_risk_mgus_male_nhw_CI',
              'mat_risk_mgus_female_nhw_CI',
              'mat_risk_mgus_male_nhb_CI',
              'mat_risk_mgus_female_nhb_CI',
              'ages'),
     file = '../output/fig_3.RData')
