# load necessary functions
source('functions_likelihood.R')

# generate prediction for mgus cohort
pred_mgus_cohort <- function(post,
                             burn.in,
                             num.samps,
                             ages,
                             pop,
                             data_cohort)
{
  # draw samples 
  samps <- sample(max(burn.in):nrow(post), size = num.samps, replace = T)
  y_pred <- array(dim = c(num.samps, nrow(data_cohort), num.samps))
  
  for(i in 1:num.samps)
  {
    sim <- sim_model(ages = ages,
                     gamma_mgus = post[samps[i],1],
                     beta_mgus_age = post[samps[i],2],
                     gamma_mm = post[samps[i],3],
                     beta_mm_age = post[samps[i],4])
    for(j in 1:num.samps)
    {
      y_pred[i,,j] <- sim_data_mgus_prev(n = data_cohort$n, ages = ages, p_mgus = sim$p_mgus,
                                         age_lower = data_cohort$age_lower, age_upper = data_cohort$age_upper,
                                         pop = pop)$y
    }
  }
  
  # compute PPI 
  y_ppi <- apply(y_pred, 2, quantile, prob = c(0.025,0.25,0.50,0.75,0.975))
  
  # return output
  return(y_ppi)
}

# function to calculate preds for model
calc_preds <- function(post,
                       burn.in,
                       num.samps,
                       data_list,
                       ages,
                       pop_male_nhw,
                       pop_female_nhw,
                       pop_male_nhb,
                       pop_female_nhb,
                       mort_male_nhw,
                       mort_female_nhw,
                       mort_male_nhb,
                       mort_female_nhb,
                       mgus_mortality_multiplier_male,
                       mgus_mortality_multiplier_female,
                       mu_mm_male_nhw,
                       mu_mm_female_nhw,
                       mu_mm_male_nhb,
                       mu_mm_female_nhb)
{
  # draw samples 
  samps <- sample(max(burn.in):nrow(post), size = num.samps, replace = T)
  
  # create matrix to store quantities
  mat_i_mgus_male_nhw <- matrix(NA, nrow = num.samps, ncol = length(ages))
  mat_p_mgus_male_nhw <- matrix(NA, nrow = num.samps, ncol = length(ages))
  mat_i_mm_male_nhw <- matrix(NA, nrow = num.samps, ncol = length(ages))
  mat_p_mm_male_nhw <- matrix(NA, nrow = num.samps, ncol = length(ages))
  mat_lambda_mgus_male_nhw <- matrix(NA, nrow = num.samps, ncol = length(ages))
  mat_lambda_mm_male_nhw <- matrix(NA, nrow = num.samps, ncol = length(ages))
  mat_cum_prop_mgus_male_nhw <- matrix(NA, nrow = num.samps, ncol = length(ages))
  mat_cum_prop_mm_male_nhw <- matrix(NA, nrow = num.samps, ncol = length(ages))

  mat_i_mgus_male_nhb <- matrix(NA, nrow = num.samps, ncol = length(ages))
  mat_p_mgus_male_nhb <- matrix(NA, nrow = num.samps, ncol = length(ages))
  mat_i_mm_male_nhb <- matrix(NA, nrow = num.samps, ncol = length(ages))
  mat_p_mm_male_nhb <- matrix(NA, nrow = num.samps, ncol = length(ages))
  mat_lambda_mgus_male_nhb <- matrix(NA, nrow = num.samps, ncol = length(ages))
  mat_lambda_mm_male_nhb <- matrix(NA, nrow = num.samps, ncol = length(ages))
  mat_cum_prop_mgus_male_nhb <- matrix(NA, nrow = num.samps, ncol = length(ages))
  mat_cum_prop_mm_male_nhb <- matrix(NA, nrow = num.samps, ncol = length(ages))
  
  mat_i_mgus_female_nhw <- matrix(NA, nrow = num.samps, ncol = length(ages))
  mat_p_mgus_female_nhw <- matrix(NA, nrow = num.samps, ncol = length(ages))
  mat_i_mm_female_nhw <- matrix(NA, nrow = num.samps, ncol = length(ages))
  mat_p_mm_female_nhw <- matrix(NA, nrow = num.samps, ncol = length(ages))
  mat_lambda_mgus_female_nhw <- matrix(NA, nrow = num.samps, ncol = length(ages))
  mat_lambda_mm_female_nhw <- matrix(NA, nrow = num.samps, ncol = length(ages))
  mat_cum_prop_mgus_female_nhw <- matrix(NA, nrow = num.samps, ncol = length(ages))
  mat_cum_prop_mm_female_nhw <- matrix(NA, nrow = num.samps, ncol = length(ages))
  
  mat_i_mgus_female_nhb <- matrix(NA, nrow = num.samps, ncol = length(ages))
  mat_p_mgus_female_nhb <- matrix(NA, nrow = num.samps, ncol = length(ages))
  mat_i_mm_female_nhb <- matrix(NA, nrow = num.samps, ncol = length(ages))
  mat_p_mm_female_nhb <- matrix(NA, nrow = num.samps, ncol = length(ages))
  mat_lambda_mgus_female_nhb <- matrix(NA, nrow = num.samps, ncol = length(ages))
  mat_lambda_mm_female_nhb <- matrix(NA, nrow = num.samps, ncol = length(ages))
  mat_cum_prop_mgus_female_nhb <- matrix(NA, nrow = num.samps, ncol = length(ages))
  mat_cum_prop_mm_female_nhb <- matrix(NA, nrow = num.samps, ncol = length(ages))
  
  y_pred_male_nhw <- array(dim = c(num.samps, nrow(data_list$mgus_prev_male_nhw), num.samps))
  x_pred_male_nhw <- array(dim = c(num.samps, nrow(data_list$mm_incid_male_nhw), num.samps))
  
  y_pred_male_nhb <- array(dim = c(num.samps, nrow(data_list$mgus_prev_male_nhb), num.samps))
  x_pred_male_nhb <- array(dim = c(num.samps, nrow(data_list$mm_incid_male_nhb), num.samps))
  
  y_pred_female_nhw <- array(dim = c(num.samps, nrow(data_list$mgus_prev_female_nhw), num.samps))
  x_pred_female_nhw <- array(dim = c(num.samps, nrow(data_list$mm_incid_female_nhw), num.samps))
  
  y_pred_female_nhb <- array(dim = c(num.samps, nrow(data_list$mgus_prev_female_nhb), num.samps))
  x_pred_female_nhb <- array(dim = c(num.samps, nrow(data_list$mm_incid_female_nhb), num.samps))
  
  for(i in 1:num.samps)
  {
    print(i)
    sim_male_nhw <- sim_model(ages = ages,
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
                          mu_mm = mu_mm_male_nhw)
    
    sim_male_nhb <- sim_model(ages = ages,
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
                              mu_mm = mu_mm_male_nhb)
    
    sim_female_nhw <- sim_model(ages = ages,
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
                              mu_mm = mu_mm_female_nhw)
    
    sim_female_nhb <- sim_model(ages = ages,
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
                                mgus_mortality_multiplier = mgus_mortality_multiplier_female,
                                mort = mort_female_nhb,
                                mu_mm = mu_mm_female_nhb)
    
    
    mat_p_mgus_male_nhw[i,] <- sim_male_nhw$p_mgus
    mat_p_mm_male_nhw[i,] <- sim_male_nhw$p_mm
    mat_i_mgus_male_nhw[i,] <- sim_male_nhw$i_mgus
    mat_i_mm_male_nhw[i,] <- sim_male_nhw$i_mm
    mat_cum_prop_mgus_male_nhw[i,] <- sim_male_nhw$cum_prop_mgus
    mat_cum_prop_mm_male_nhw[i,] <- sim_male_nhw$cum_prop_mm
    mat_lambda_mgus_male_nhw[i,] <- exp(post[samps[i],1] + post[samps[i],2] * ages)
    mat_lambda_mm_male_nhw[i,] <- exp(post[samps[i],5] + post[samps[i],6] * ages + post[samps[i],7] * ages^2)
    
    mat_p_mgus_male_nhb[i,] <- sim_male_nhb$p_mgus
    mat_p_mm_male_nhb[i,] <- sim_male_nhb$p_mm
    mat_i_mgus_male_nhb[i,] <- sim_male_nhb$i_mgus
    mat_i_mm_male_nhb[i,] <- sim_male_nhb$i_mm
    mat_cum_prop_mgus_male_nhb[i,] <- sim_male_nhb$cum_prop_mgus
    mat_cum_prop_mm_male_nhb[i,] <- sim_male_nhb$cum_prop_mm
    mat_lambda_mgus_male_nhb[i,] <- exp(post[samps[i],1] + post[samps[i],2] * ages + post[samps[i],4])
    mat_lambda_mm_male_nhb[i,] <- exp(post[samps[i],5] + post[samps[i],6] * ages + post[samps[i],7] * ages^2 + post[samps[i],9])
    
    mat_p_mgus_female_nhw[i,] <- sim_female_nhw$p_mgus
    mat_p_mm_female_nhw[i,] <- sim_female_nhw$p_mm
    mat_i_mgus_female_nhw[i,] <- sim_female_nhw$i_mgus
    mat_i_mm_female_nhw[i,] <- sim_female_nhw$i_mm
    mat_cum_prop_mgus_female_nhw[i,] <- sim_female_nhw$cum_prop_mgus
    mat_cum_prop_mm_female_nhw[i,] <- sim_female_nhw$cum_prop_mm
    mat_lambda_mgus_female_nhw[i,] <- exp(post[samps[i],1] + post[samps[i],2] * ages + post[samps[i],3])
    mat_lambda_mm_female_nhw[i,] <- exp(post[samps[i],5] + post[samps[i],6] * ages + post[samps[i],7] * ages^2 + post[samps[i],8])
    
    mat_p_mgus_female_nhb[i,] <- sim_female_nhb$p_mgus
    mat_p_mm_female_nhb[i,] <- sim_female_nhb$p_mm
    mat_i_mgus_female_nhb[i,] <- sim_female_nhb$i_mgus
    mat_i_mm_female_nhb[i,] <- sim_female_nhb$i_mm
    mat_cum_prop_mgus_female_nhb[i,] <- sim_female_nhb$cum_prop_mgus
    mat_cum_prop_mm_female_nhb[i,] <- sim_female_nhb$cum_prop_mm
    mat_lambda_mgus_female_nhb[i,] <- exp(post[samps[i],1] + post[samps[i],2] * ages + post[samps[i],3] + post[samps[i],4])
    mat_lambda_mm_female_nhb[i,] <- exp(post[samps[i],5] + post[samps[i],6] * ages + post[samps[i],7] * ages^2 + post[samps[i],8] + post[samps[i],9])
    
    for(j in 1:num.samps)
    {
      y_pred_male_nhw[i,,j] <- sim_data_mgus_prev(n = data_list$mgus_prev_male_nhw$n, ages = ages, p_mgus = sim_male_nhw$p_mgus,
                                              age_lower = data_list$mgus_prev_male_nhw$age_lower, age_upper = data_list$mgus_prev_male_nhw$age_upper,
                                              pop = pop_male_nhw)$y
      y_pred_male_nhb[i,,j] <- sim_data_mgus_prev(n = data_list$mgus_prev_male_nhb$n, ages = ages, p_mgus = sim_male_nhb$p_mgus,
                                                  age_lower = data_list$mgus_prev_male_nhb$age_lower, age_upper = data_list$mgus_prev_male_nhb$age_upper,
                                                  pop = pop_male_nhb)$y
      
      x_pred_male_nhw[i,,j] <- sim_data_mm_incid(ages = ages, i_mm = sim_male_nhw$i_mm, tau = post[samps[i],10], pop = pop_male_nhw,
                                             age_lower = data_list$mm_incid_male_nhw$age_lower, age_upper = data_list$mm_incid_male_nhw$age_upper)$x
      x_pred_male_nhb[i,,j] <- sim_data_mm_incid(ages = ages, i_mm = sim_male_nhb$i_mm, tau = post[samps[i],10], pop = pop_male_nhb,
                                                 age_lower = data_list$mm_incid_male_nhb$age_lower, age_upper = data_list$mm_incid_male_nhb$age_upper)$x
      
      
      y_pred_female_nhw[i,,j] <- sim_data_mgus_prev(n = data_list$mgus_prev_female_nhw$n, ages = ages, p_mgus = sim_female_nhw$p_mgus,
                                                age_lower = data_list$mgus_prev_female_nhw$age_lower, age_upper = data_list$mgus_prev_female_nhw$age_upper,
                                                pop = pop_female_nhw)$y
      y_pred_female_nhb[i,,j] <- sim_data_mgus_prev(n = data_list$mgus_prev_female_nhb$n, ages = ages, p_mgus = sim_female_nhb$p_mgus,
                                                    age_lower = data_list$mgus_prev_female_nhb$age_lower, age_upper = data_list$mgus_prev_female_nhb$age_upper,
                                                    pop = pop_female_nhb)$y
      
      x_pred_female_nhw[i,,j] <- sim_data_mm_incid(ages = ages, i_mm = sim_female_nhw$i_mm, tau = post[samps[i],10], pop = pop_female_nhw,
                                               age_lower = data_list$mm_incid_female_nhw$age_lower, age_upper = data_list$mm_incid_female_nhw$age_upper)$x
      x_pred_female_nhb[i,,j] <- sim_data_mm_incid(ages = ages, i_mm = sim_female_nhb$i_mm, tau = post[samps[i],10], pop = pop_female_nhb,
                                                   age_lower = data_list$mm_incid_female_nhb$age_lower, age_upper = data_list$mm_incid_female_nhb$age_upper)$x
    }
  }
  
  # compute CI 
  p_mgus_male_nhw_CI <- apply(mat_p_mgus_male_nhw, 2, quantile, prob = c(0.025,0.25,0.50,0.75,0.975)) 
  i_mgus_male_nhw_CI <- apply(mat_i_mgus_male_nhw, 2, quantile, prob = c(0.025,0.25,0.50,0.75,0.975)) 
  p_mm_male_nhw_CI <- apply(mat_p_mm_male_nhw, 2, quantile, prob = c(0.025,0.25,0.50,0.75,0.975))
  i_mm_male_nhw_CI <- apply(mat_i_mm_male_nhw, 2, quantile, prob = c(0.025,0.25,0.50,0.75,0.975))
  cum_prop_mgus_male_nhw_CI <- apply(mat_cum_prop_mgus_male_nhw, 2, quantile, prob = c(0.025,0.25,0.50,0.75,0.975))
  cum_prop_mm_male_nhw_CI <- apply(mat_cum_prop_mm_male_nhw, 2, quantile, prob = c(0.025,0.25,0.50,0.75,0.975))
  lambda_mgus_male_nhw_CI <- apply(mat_lambda_mgus_male_nhw, 2, quantile, prob = c(0.025,0.25,0.50,0.75,0.975)) 
  lambda_mm_male_nhw_CI <- apply(mat_lambda_mm_male_nhw, 2, quantile, prob = c(0.025,0.25,0.50,0.75,0.975)) 
  
  p_mgus_male_nhb_CI <- apply(mat_p_mgus_male_nhb, 2, quantile, prob = c(0.025,0.25,0.50,0.75,0.975)) 
  i_mgus_male_nhb_CI <- apply(mat_i_mgus_male_nhb, 2, quantile, prob = c(0.025,0.25,0.50,0.75,0.975)) 
  p_mm_male_nhb_CI <- apply(mat_p_mm_male_nhb, 2, quantile, prob = c(0.025,0.25,0.50,0.75,0.975))
  i_mm_male_nhb_CI <- apply(mat_i_mm_male_nhb, 2, quantile, prob = c(0.025,0.25,0.50,0.75,0.975))
  cum_prop_mgus_male_nhb_CI <- apply(mat_cum_prop_mgus_male_nhb, 2, quantile, prob = c(0.025,0.25,0.50,0.75,0.975))
  cum_prop_mm_male_nhb_CI <- apply(mat_cum_prop_mm_male_nhb, 2, quantile, prob = c(0.025,0.25,0.50,0.75,0.975))
  lambda_mgus_male_nhb_CI <- apply(mat_lambda_mgus_male_nhb, 2, quantile, prob = c(0.025,0.25,0.50,0.75,0.975)) 
  lambda_mm_male_nhb_CI <- apply(mat_lambda_mm_male_nhb, 2, quantile, prob = c(0.025,0.25,0.50,0.75,0.975)) 
  
  p_mgus_female_nhw_CI <- apply(mat_p_mgus_female_nhw, 2, quantile, prob = c(0.025,0.25,0.50,0.75,0.975)) 
  i_mgus_female_nhw_CI <- apply(mat_i_mgus_female_nhw, 2, quantile, prob = c(0.025,0.25,0.50,0.75,0.975)) 
  p_mm_female_nhw_CI <- apply(mat_p_mm_female_nhw, 2, quantile, prob = c(0.025,0.25,0.50,0.75,0.975))
  i_mm_female_nhw_CI <- apply(mat_i_mm_female_nhw, 2, quantile, prob = c(0.025,0.25,0.50,0.75,0.975))
  cum_prop_mgus_female_nhw_CI <- apply(mat_cum_prop_mgus_female_nhw, 2, quantile, prob = c(0.025,0.25,0.50,0.75,0.975))
  cum_prop_mm_female_nhw_CI <- apply(mat_cum_prop_mm_female_nhw, 2, quantile, prob = c(0.025,0.25,0.50,0.75,0.975))
  lambda_mgus_female_nhw_CI <- apply(mat_lambda_mgus_female_nhw, 2, quantile, prob = c(0.025,0.25,0.50,0.75,0.975)) 
  lambda_mm_female_nhw_CI <- apply(mat_lambda_mm_female_nhw, 2, quantile, prob = c(0.025,0.25,0.50,0.75,0.975)) 
  
  p_mgus_female_nhb_CI <- apply(mat_p_mgus_female_nhb, 2, quantile, prob = c(0.025,0.25,0.50,0.75,0.975)) 
  i_mgus_female_nhb_CI <- apply(mat_i_mgus_female_nhb, 2, quantile, prob = c(0.025,0.25,0.50,0.75,0.975)) 
  p_mm_female_nhb_CI <- apply(mat_p_mm_female_nhb, 2, quantile, prob = c(0.025,0.25,0.50,0.75,0.975))
  i_mm_female_nhb_CI <- apply(mat_i_mm_female_nhb, 2, quantile, prob = c(0.025,0.25,0.50,0.75,0.975))
  cum_prop_mgus_female_nhb_CI <- apply(mat_cum_prop_mgus_female_nhb, 2, quantile, prob = c(0.025,0.25,0.50,0.75,0.975))
  cum_prop_mm_female_nhb_CI <- apply(mat_cum_prop_mm_female_nhb, 2, quantile, prob = c(0.025,0.25,0.50,0.75,0.975))
  lambda_mgus_female_nhb_CI <- apply(mat_lambda_mgus_female_nhb, 2, quantile, prob = c(0.025,0.25,0.50,0.75,0.975)) 
  lambda_mm_female_nhb_CI <- apply(mat_lambda_mm_female_nhb, 2, quantile, prob = c(0.025,0.25,0.50,0.75,0.975)) 
  
  # compute PPI 
  y_male_nhw_ppi <- apply(y_pred_male_nhw, 2, quantile, prob = c(0.025,0.25,0.50,0.75,0.975), na.rm = T)
  y_male_nhb_ppi <- apply(y_pred_male_nhb, 2, quantile, prob = c(0.025,0.25,0.50,0.75,0.975), na.rm = T)
  x_male_nhw_ppi <- apply(x_pred_male_nhw, 2, quantile, prob = c(0.025,0.25,0.50,0.75,0.975), na.rm = T)
  x_male_nhb_ppi <- apply(x_pred_male_nhb, 2, quantile, prob = c(0.025,0.25,0.50,0.75,0.975), na.rm = T)
  
  y_female_nhw_ppi <- apply(y_pred_female_nhw, 2, quantile, prob = c(0.025,0.25,0.50,0.75,0.975), na.rm = T)
  y_female_nhb_ppi <- apply(y_pred_female_nhb, 2, quantile, prob = c(0.025,0.25,0.50,0.75,0.975), na.rm = T)
  x_female_nhw_ppi <- apply(x_pred_female_nhw, 2, quantile, prob = c(0.025,0.25,0.50,0.75,0.975), na.rm = T)
  x_female_nhb_ppi <- apply(x_pred_female_nhb, 2, quantile, prob = c(0.025,0.25,0.50,0.75,0.975), na.rm = T)
  
  # return output
  return(list(p_mgus_male_nhw_CI = p_mgus_male_nhw_CI,
              i_mgus_male_nhw_CI = i_mgus_male_nhw_CI,
              p_mm_male_nhw_CI = p_mm_male_nhw_CI,
              i_mm_male_nhw_CI = i_mm_male_nhw_CI,
              cum_prop_mgus_male_nhw_CI = cum_prop_mgus_male_nhw_CI,
              cum_prop_mm_male_nhw_CI = cum_prop_mm_male_nhw_CI,
              lambda_mgus_male_nhw_CI = lambda_mgus_male_nhw_CI,
              lambda_mm_male_nhw_CI = lambda_mm_male_nhw_CI,
              y_male_nhw_ppi = y_male_nhw_ppi,
              x_male_nhw_ppi = x_male_nhw_ppi,
              p_mgus_male_nhb_CI = p_mgus_male_nhb_CI,
              i_mgus_male_nhb_CI = i_mgus_male_nhb_CI,
              p_mm_male_nhb_CI = p_mm_male_nhb_CI,
              i_mm_male_nhb_CI = i_mm_male_nhb_CI,
              cum_prop_mgus_male_nhb_CI = cum_prop_mgus_male_nhb_CI,
              cum_prop_mm_male_nhb_CI = cum_prop_mm_male_nhb_CI,
              lambda_mgus_male_nhb_CI = lambda_mgus_male_nhb_CI,
              lambda_mm_male_nhb_CI = lambda_mm_male_nhb_CI,
              y_male_nhb_ppi = y_male_nhb_ppi,
              x_male_nhb_ppi = x_male_nhb_ppi,
              p_mgus_female_nhw_CI = p_mgus_female_nhw_CI,
              i_mgus_female_nhw_CI = i_mgus_female_nhw_CI,
              p_mm_female_nhw_CI = p_mm_female_nhw_CI,
              i_mm_female_nhw_CI = i_mm_female_nhw_CI,
              cum_prop_mgus_female_nhw_CI = cum_prop_mgus_female_nhw_CI,
              cum_prop_mm_female_nhw_CI = cum_prop_mm_female_nhw_CI,
              lambda_mgus_female_nhw_CI = lambda_mgus_female_nhw_CI,
              lambda_mm_female_nhw_CI = lambda_mm_female_nhw_CI,
              y_female_nhw_ppi = y_female_nhw_ppi,
              x_female_nhw_ppi = x_female_nhw_ppi,
              p_mgus_female_nhb_CI = p_mgus_female_nhb_CI,
              i_mgus_female_nhb_CI = i_mgus_female_nhb_CI,
              p_mm_female_nhb_CI = p_mm_female_nhb_CI,
              i_mm_female_nhb_CI = i_mm_female_nhb_CI,
              cum_prop_mgus_female_nhb_CI = cum_prop_mgus_female_nhb_CI,
              cum_prop_mm_female_nhb_CI = cum_prop_mm_female_nhb_CI,
              lambda_mgus_female_nhb_CI = lambda_mgus_female_nhb_CI,
              lambda_mm_female_nhb_CI = lambda_mm_female_nhb_CI,
              y_female_nhb_ppi = y_female_nhb_ppi,
              x_female_nhb_ppi = x_female_nhb_ppi))
}

# generate prediction for mgus cohort
pred_mgus_cohort <- function(post,
                             burn.in,
                             num.samps,
                             data_list,
                             ages,
                             pop_male_nhw,
                             pop_female_nhw,
                             mort_male_nhw,
                             mort_female_nhw,
                             mgus_mortality_multiplier_male,
                             mgus_mortality_multiplier_female,
                             mu_mm_male_nhw,
                             mu_mm_female_nhw,
                             data_cohort,
                             is_male)
{
  # draw samples 
  samps <- sample(max(burn.in):nrow(post), size = num.samps, replace = T)
  
  # subset data cohort according to sex and make prediction matrix 
  if(is_male)
  {
    data_cohort <- subset(data_cohort, sex == 'Male')
  }else{
    data_cohort <- subset(data_cohort, sex == 'Female')
  }
  y_pred <- array(dim = c(num.samps, nrow(data_cohort), num.samps))
  
  for(i in 1:num.samps)
  {
    if(is_male)
    {
      sim <- sim_model(ages = ages,
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
                       mu_mm = mu_mm_male_nhw)
      
      for(j in 1:num.samps)
      {
        y_pred[i,,j] <- sim_data_mgus_prev(n = data_cohort$n, ages = ages, p_mgus = sim$p_mgus,
                                           age_lower = data_cohort$age_lower, age_upper = data_cohort$age_upper,
                                           pop = pop_male_nhw)$y
      }
    }else{
      sim <- sim_model(ages = ages,
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
                       mu_mm = mu_mm_female_nhw)
      
      for(j in 1:num.samps)
      {
        y_pred[i,,j] <- sim_data_mgus_prev(n = data_cohort$n, ages = ages, p_mgus = sim$p_mgus,
                                           age_lower = data_cohort$age_lower, age_upper = data_cohort$age_upper,
                                           pop = pop_female_nhw)$y
      }
    }
  }
  
  # compute PPI 
  y_ppi <- apply(y_pred, 2, quantile, prob = c(0.025,0.25,0.50,0.75,0.975))
  
  # return output
  return(y_ppi)
}