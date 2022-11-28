# load necessary functions
source('functions_sim_data.R')

# function to generate simulation sweep
gen_sim_sweep <- function(n,
                          path_out,
                          params_lower,
                          params_upper,
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
                          mu_mm_female_nhb,
                          data_list)
{
  # loop through and generate the data 
  for(i in 1:n)
  {
    repeat{
      print(i)
      # sample the parameters 
      params <- runif(n = length(params_lower), min = params_lower, max = params_upper)
      
      # simulate the data
      sim_data_list <- gen_sim_data(params = params,
                                    ages = ages,
                                    pop_male_nhw = pop_male_nhw,
                                    pop_female_nhw = pop_female_nhw,
                                    pop_male_nhb = pop_male_nhb,
                                    pop_female_nhb = pop_female_nhb,
                                    mort_male_nhw = mort_male_nhw,
                                    mort_male_nhb = mort_male_nhb,
                                    mort_female_nhw = mort_female_nhw,
                                    mort_female_nhb = mort_female_nhb,
                                    mgus_mortality_multiplier_male = mgus_mortality_multiplier_male,
                                    mgus_mortality_multiplier_female = mgus_mortality_multiplier_female,
                                    mu_mm_male_nhw = mu_mm_male_nhw,
                                    mu_mm_female_nhw = mu_mm_female_nhw,
                                    mu_mm_male_nhb = mu_mm_male_nhb,
                                    mu_mm_female_nhb = mu_mm_female_nhb,
                                    data_list = data_list)
      
      # if the data does not have NaN or non-negative, then simulated data is valid and you can exit the loop
      if(!anyNA(sim_data_list$mgus_prev_male_nhw) &
         !anyNA(sim_data_list$mgus_prev_female_nhw) & 
         !anyNA(sim_data_list$mgus_prev_male_nhb) &
         !anyNA(sim_data_list$mgus_prev_female_nhb) &
         !anyNA(sim_data_list$mm_incid_male_nhw) & any(sim_data_list$mm_incid_male_nhw$x > 0) & 
         !anyNA(sim_data_list$mm_incid_female_nhw) & any(sim_data_list$mm_incid_female_nhw$x > 0) & 
         !anyNA(sim_data_list$mm_incid_male_nhb) & any(sim_data_list$mm_incid_male_nhb$x > 0) & 
         !anyNA(sim_data_list$mm_incid_female_nhb) & any(sim_data_list$mm_incid_female_nhb$x > 0)){break}
    }
    
    # write the data to file
    write.csv(sim_data_list$mgus_prev_male_nhw, file = paste(path_out, 'mgus_prev_male_nhw_', i, '.csv', sep = ''), row.names = F)
    write.csv(sim_data_list$mgus_prev_female_nhw, file = paste(path_out, 'mgus_prev_female_nhw_', i, '.csv', sep = ''), row.names = F)
    write.csv(sim_data_list$mgus_prev_male_nhb, file = paste(path_out, 'mgus_prev_male_nhb_', i, '.csv', sep = ''), row.names = F)
    write.csv(sim_data_list$mgus_prev_female_nhb, file = paste(path_out, 'mgus_prev_female_nhb_', i, '.csv', sep = ''), row.names = F)
    
    write.csv(sim_data_list$mm_incid_male_nhw, file = paste(path_out, 'i_mm_male_nhw_', i, '.csv', sep = ''), row.names = F)
    write.csv(sim_data_list$mm_incid_female_nhw, file = paste(path_out, 'i_mm_female_nhw_', i, '.csv', sep = ''), row.names = F)
    write.csv(sim_data_list$mm_incid_male_nhb, file = paste(path_out, 'i_mm_male_nhb_', i, '.csv', sep = ''), row.names = F)
    write.csv(sim_data_list$mm_incid_female_nhb, file = paste(path_out, 'i_mm_female_nhb_', i, '.csv', sep = ''), row.names = F)
    
    # write the params to file
    write.csv(params, file = paste(path_out, 'params_', i, '.csv', sep = ''), row.names = F)
  }
}

#function to simulate the data of interest 
gen_sim_data <- function(params,
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
                         mu_mm_female_nhb,
                         data_list)
{
  # get the parameters 
  gamma_mgus = params[1]
  beta_mgus_age = params[2]
  beta_mgus_sex = params[3]
  beta_mgus_race = params[4]
  
  gamma_mm = params[5]
  beta_mm_age = params[6]
  beta_mm_age_quad = params[7]
  beta_mm_sex = params[8]
  beta_mm_race = params[9]
  tau = params[10]
  
  # simulate model
  out_male_nhw <- sim_model(ages = ages,
                            gamma_mgus = gamma_mgus,
                            beta_mgus_age = beta_mgus_age,
                            beta_mgus_age_quad = 0,
                            beta_mgus_sex = 0,
                            beta_mgus_race = 0,
                            gamma_mm = gamma_mm,
                            beta_mm_age = beta_mm_age,
                            beta_mm_age_quad = beta_mm_age_quad,
                            beta_mm_sex = 0,
                            beta_mm_race = 0,
                            mgus_mortality_multiplier = mgus_mortality_multiplier_male,
                            mort = mort_male_nhw,
                            mu_mm = mu_mm_male_nhw)
  
  out_male_nhb <- sim_model(ages = ages,
                            gamma_mgus = gamma_mgus,
                            beta_mgus_age = beta_mgus_age,
                            beta_mgus_age_quad = 0,
                            beta_mgus_sex = 0,
                            beta_mgus_race = beta_mgus_race,
                            gamma_mm = gamma_mm,
                            beta_mm_age = beta_mm_age,
                            beta_mm_age_quad = beta_mm_age_quad,
                            beta_mm_sex = 0,
                            beta_mm_race = beta_mm_race,
                            mgus_mortality_multiplier = mgus_mortality_multiplier_male,
                            mort = mort_male_nhb,
                            mu_mm = mu_mm_male_nhb)
  
  out_female_nhw <- sim_model(ages = ages,
                              gamma_mgus = gamma_mgus,
                              beta_mgus_age = beta_mgus_age,
                              beta_mgus_age_quad = 0,
                              beta_mgus_sex = beta_mgus_sex,
                              beta_mgus_race = 0,
                              gamma_mm = gamma_mm,
                              beta_mm_age = beta_mm_age,
                              beta_mm_age_quad = beta_mm_age_quad,
                              beta_mm_sex = beta_mm_sex,
                              beta_mm_race = 0,
                              mgus_mortality_multiplier = mgus_mortality_multiplier_female,
                              mort = mort_female_nhw,
                              mu_mm = mu_mm_female_nhw)
  
  out_female_nhb <- sim_model(ages = ages,
                              gamma_mgus = gamma_mgus,
                              beta_mgus_age = beta_mgus_age,
                              beta_mgus_age_quad = 0,
                              beta_mgus_sex = beta_mgus_sex,
                              beta_mgus_race = beta_mgus_race,
                              gamma_mm = gamma_mm,
                              beta_mm_age = beta_mm_age,
                              beta_mm_age_quad = beta_mm_age_quad,
                              beta_mm_sex = beta_mm_sex,
                              beta_mm_race = beta_mm_race,
                              mgus_mortality_multiplier = mgus_mortality_multiplier_female,
                              mort = mort_female_nhb,
                              mu_mm = mu_mm_female_nhb)
  
  # get the mgus prevalence and mm incidence
  p_mgus_male_nhw <- out_male_nhw$p_mgus
  p_mgus_female_nhw <- out_female_nhw$p_mgus
  p_mgus_male_nhb <- out_male_nhb$p_mgus
  p_mgus_female_nhb <- out_female_nhb$p_mgus
  
  i_mm_male_nhw <- out_male_nhw$i_mm
  i_mm_female_nhw <- out_female_nhw$i_mm
  i_mm_male_nhb <- out_male_nhb$i_mm
  i_mm_female_nhb <- out_female_nhb$i_mm
  
  # simulate the data 
  mgus_prev_male_nhw <- sim_data_mgus_prev(n = data_list$mgus_prev_male_nhw$n, 
                                           ages = ages, 
                                           p_mgus = p_mgus_male_nhw,
                                           age_lower = data_list$mgus_prev_male_nhw$age_lower, 
                                           age_upper = data_list$mgus_prev_male_nhw$age_upper,
                                           pop = pop_male_nhw)
  
  mgus_prev_female_nhw <- sim_data_mgus_prev(n = data_list$mgus_prev_female_nhw$n, 
                                           ages = ages, 
                                           p_mgus = p_mgus_female_nhw,
                                           age_lower = data_list$mgus_prev_female_nhw$age_lower, 
                                           age_upper = data_list$mgus_prev_female_nhw$age_upper,
                                           pop = pop_female_nhw)
  
  mgus_prev_male_nhb <- sim_data_mgus_prev(n = data_list$mgus_prev_male_nhb$n, 
                                           ages = ages, 
                                           p_mgus = p_mgus_male_nhb,
                                           age_lower = data_list$mgus_prev_male_nhb$age_lower, 
                                           age_upper = data_list$mgus_prev_male_nhb$age_upper,
                                           pop = pop_male_nhb)
  
  mgus_prev_female_nhb <- sim_data_mgus_prev(n = data_list$mgus_prev_female_nhb$n, 
                                             ages = ages, 
                                             p_mgus = p_mgus_female_nhb,
                                             age_lower = data_list$mgus_prev_female_nhb$age_lower, 
                                             age_upper = data_list$mgus_prev_female_nhb$age_upper,
                                             pop = pop_female_nhb)
  
  mm_incid_male_nhw <- sim_data_mm_incid(ages = ages, 
                                         i_mm = i_mm_male_nhw, 
                                         tau = tau, 
                                         pop = pop_male_nhw,
                                         age_lower = data_list$mm_incid_male_nhw$age_lower, 
                                         age_upper = data_list$mm_incid_male_nhw$age_upper)
  
  mm_incid_female_nhw <- sim_data_mm_incid(ages = ages, 
                                         i_mm = i_mm_female_nhw, 
                                         tau = tau, 
                                         pop = pop_female_nhw,
                                         age_lower = data_list$mm_incid_female_nhw$age_lower, 
                                         age_upper = data_list$mm_incid_female_nhw$age_upper)
  
  mm_incid_male_nhb <- sim_data_mm_incid(ages = ages, 
                                         i_mm = i_mm_male_nhb, 
                                         tau = tau, 
                                         pop = pop_male_nhb,
                                         age_lower = data_list$mm_incid_male_nhb$age_lower, 
                                         age_upper = data_list$mm_incid_male_nhb$age_upper)
  
  mm_incid_female_nhb <- sim_data_mm_incid(ages = ages, 
                                           i_mm = i_mm_female_nhb, 
                                           tau = tau, 
                                           pop = pop_female_nhw,
                                           age_lower = data_list$mm_incid_female_nhb$age_lower, 
                                           age_upper = data_list$mm_incid_female_nhb$age_upper)
  
  # return output
  return(list(mgus_prev_male_nhw = mgus_prev_male_nhw,
              mgus_prev_female_nhw = mgus_prev_female_nhw,
              mgus_prev_male_nhb = mgus_prev_male_nhb,
              mgus_prev_female_nhb = mgus_prev_female_nhb,
              mm_incid_male_nhw = mm_incid_male_nhw,
              mm_incid_female_nhw = mm_incid_female_nhw,
              mm_incid_male_nhb = mm_incid_male_nhb,
              mm_incid_female_nhb = mm_incid_female_nhb))
}