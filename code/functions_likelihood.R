# install necessary packages
if(!require(BayesianTools)){install.packages('BayesianTools'); library(BayesianTools)}
if(!require(compiler)){install.packages('compiler'); library(compiler)}

# load necessary functions
source('functions_sim_data.R')

# specify likelihood for mgus prevalence data
ll_mgus_prev <- function(df_mgus, ages, p_mgus, pop)
{
  # generate weighted prevalence 
  p_mgus_weighted <- rep(NA, nrow(df_mgus))
  if(any(df_mgus$age_upper == df_mgus$age_lower))
  {
    p_mgus_weighted[which(df_mgus$age_upper == df_mgus$age_lower)] = p_mgus[sapply(which(df_mgus$age_upper == df_mgus$age_lower), function(i){match(df_mgus$age_lower[i], ages)})]
  }
  
  for(ii in which(df_mgus$age_lower != df_mgus$age_upper))
  {
    p_mgus_weighted[ii] = sum(p_mgus[match(df_mgus$age_lower[ii], ages):match(df_mgus$age_upper[ii], ages)] * pop$N[match(df_mgus$age_lower[ii], pop$age):match(df_mgus$age_upper[ii], pop$age)]) /
      sum(pop$N[match(df_mgus$age_lower[ii], pop$age):match(df_mgus$age_upper[ii], pop$age)])
  }
  
  ll <- sum(dbinom(x = df_mgus$y, size = df_mgus$n, prob = p_mgus_weighted, log = T))
  return(ll)
}

#specify likelihood for mm incidence data
ll_mm_incid <- function(df_mm, i_mm, tau, ages, pop)
{
  # compute the weighted incidence
  i_mm_weighted <- rep(NA, length(df_mm$age_lower))

  for(ii in 1:length(df_mm$age_lower))
  {
    i_mm_weighted[ii] = sum(i_mm[match(df_mm$age_lower[ii], ages):match(df_mm$age_upper[ii], ages)] * pop$N[match(df_mm$age_lower[ii], pop$age):match(df_mm$age_upper[ii], pop$age)]) /
      sum(pop$N[match(df_mm$age_lower[ii], pop$age):match(df_mm$age_upper[ii], pop$age)])
  }
  meanlog <- log(i_mm_weighted)
  ll <- sum(dnorm(x = log(df_mm$x), mean = meanlog, sd = sqrt(tau), log = T))
  return(ll)
}

# ll_mm_incid <- function(df_mm, i_mm, tau, ages, pop)
# {
#   # compute the weighted incidence
#   i_mm_weighted <- rep(NA, length(df_mm$age_lower))
# 
#   for(ii in 1:length(df_mm$age_lower))
#   {
#     i_mm_weighted[ii] = sum(i_mm[match(df_mm$age_lower[ii], ages):match(df_mm$age_upper[ii], ages)] * pop$N[match(df_mm$age_lower[ii], pop$age):match(df_mm$age_upper[ii], pop$age)]) /
#       sum(pop$N[match(df_mm$age_lower[ii], pop$age):match(df_mm$age_upper[ii], pop$age)])
#   }
#   meanlog <- log(i_mm_weighted[-1]) - 0.5 * log(i_mm_weighted[-1]^2 + tau^2)
#   varlog <- log(1 + (tau^2)/(i_mm_weighted[-1]^2))
#   ll <- sum(dlnorm(x = df_mm$x[-1], meanlog = meanlog, sdlog = sqrt(varlog), log = T))
#   #ll <- sum(dnorm(x = log(df_mm$x[-1]), mean = meanlog, sd = sqrt(tau), log = T))
#   return(ll)
# }

# 
# ll_mm_incid <- function(df_mm, i_mm, tau, ages, pop)
# {
#   # compute the weighted incidence
#   i_mm_weighted <- rep(NA, length(df_mm$age_lower))
#   
#   for(ii in 1:length(df_mm$age_lower))
#   {
#     i_mm_weighted[ii] = sum(i_mm[match(df_mm$age_lower[ii], ages):match(df_mm$age_upper[ii], ages)] * pop$N[match(df_mm$age_lower[ii], pop$age):match(df_mm$age_upper[ii], pop$age)]) /
#       sum(pop$N[match(df_mm$age_lower[ii], pop$age):match(df_mm$age_upper[ii], pop$age)])
#   }
#   ll <- sum(dgamma(x = df_mm$x[-1], shape = (i_mm_weighted[-1]^2) / tau, rate = i_mm_weighted[-1] / tau, log = T))
#   return(ll)
# }

# specify ll function
ll <- function(params)
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
                            mu_mm = mm_mu_nhw_male)
  
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
                            mu_mm = mm_mu_nhb_male)

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
                            mu_mm = mm_mu_nhw_female)
  
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
                            mu_mm = mm_mu_nhb_female)
  
  # calculate the log-likelihood
  ll <- ll_mgus_prev(df_mgus = data_list[['mgus_prev_male_nhw']], ages = ages, p_mgus = out_male_nhw$p_mgus, pop = pop_male_nhw)
  ll <- ll + ll_mgus_prev(df_mgus = data_list[['mgus_prev_male_nhb']], ages = ages, p_mgus = out_male_nhb$p_mgus, pop = pop_male_nhb)
  ll <- ll + ll_mgus_prev(df_mgus = data_list[['mgus_prev_female_nhw']], ages = ages, p_mgus = out_female_nhw$p_mgus, pop = pop_female_nhw)
  ll <- ll + ll_mgus_prev(df_mgus = data_list[['mgus_prev_female_nhb']], ages = ages, p_mgus = out_female_nhb$p_mgus, pop = pop_female_nhb)
  
  ll <- ll + ll_mm_incid(df_mm = data_list[['mm_incid_male_nhw']], i_mm = out_male_nhw$i_mm, tau = tau, ages = ages, pop = pop_male_nhw)
  ll <- ll + ll_mm_incid(df_mm = data_list[['mm_incid_male_nhb']], i_mm = out_male_nhb$i_mm, tau = tau, ages = ages, pop = pop_male_nhb)
  ll <- ll + ll_mm_incid(df_mm = data_list[['mm_incid_female_nhw']], i_mm = out_female_nhw$i_mm, tau = tau, ages = ages, pop = pop_female_nhw)
  ll <- ll + ll_mm_incid(df_mm = data_list[['mm_incid_female_nhb']], i_mm = out_female_nhb$i_mm, tau = tau, ages = ages, pop = pop_female_nhb)
  
  # return log-likelihood
  if(is.infinite(ll) | is.na(ll)){ll <- -1e20}
  return(ll)
}

ll_cmp <- cmpfun(ll)

# specify ll function
ll_alt <- function(params)
{
  # get the parameters 
  gamma_mgus = params[1]
  beta_mgus_age = params[2]
  beta_mgus_sex = params[3]
  beta_mgus_race = params[4]
  
  gamma_mm = params[5]
  beta_mm_age = params[6]
  beta_mm_age_quad = 0
  beta_mm_sex = params[7]
  beta_mm_race = params[8]
  tau = params[9]
  
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
                            mu_mm = mm_mu_nhw_male)
  
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
                            mu_mm = mm_mu_nhb_male)
  
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
                              mu_mm = mm_mu_nhw_female)
  
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
                              mu_mm = mm_mu_nhb_female)
  
  # calculate the log-likelihood
  ll <- ll_mgus_prev(df_mgus = data_list[['mgus_prev_male_nhw']], ages = ages, p_mgus = out_male_nhw$p_mgus, pop = pop_male_nhw)
  ll <- ll + ll_mgus_prev(df_mgus = data_list[['mgus_prev_male_nhb']], ages = ages, p_mgus = out_male_nhb$p_mgus, pop = pop_male_nhb)
  ll <- ll + ll_mgus_prev(df_mgus = data_list[['mgus_prev_female_nhw']], ages = ages, p_mgus = out_female_nhw$p_mgus, pop = pop_female_nhw)
  ll <- ll + ll_mgus_prev(df_mgus = data_list[['mgus_prev_female_nhb']], ages = ages, p_mgus = out_female_nhb$p_mgus, pop = pop_female_nhb)
  
  ll <- ll + ll_mm_incid(df_mm = data_list[['mm_incid_male_nhw']], i_mm = out_male_nhw$i_mm, tau = tau, ages = ages, pop = pop_male_nhw)
  ll <- ll + ll_mm_incid(df_mm = data_list[['mm_incid_male_nhb']], i_mm = out_male_nhb$i_mm, tau = tau, ages = ages, pop = pop_male_nhb)
  ll <- ll + ll_mm_incid(df_mm = data_list[['mm_incid_female_nhw']], i_mm = out_female_nhw$i_mm, tau = tau, ages = ages, pop = pop_female_nhw)
  ll <- ll + ll_mm_incid(df_mm = data_list[['mm_incid_female_nhb']], i_mm = out_female_nhb$i_mm, tau = tau, ages = ages, pop = pop_female_nhb)
  
  # return log-likelihood
  if(is.infinite(ll) | is.na(ll)){ll <- -1e20}
  return(ll)
}

ll_alt_cmp <- cmpfun(ll_alt)

# specify ll function
ll_age <- function(params)
{
  # get the parameters 
  gamma_mgus = params[1]
  beta_mgus_age = 0
  beta_mgus_sex = params[2]
  beta_mgus_race = params[3]
  
  gamma_mm = params[4]
  beta_mm_age = 0
  beta_mm_age_quad = 0
  beta_mm_sex = params[5]
  beta_mm_race = params[6]
  tau = params[7]
  
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
                            mu_mm = mm_mu_nhw_male)
  
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
                            mu_mm = mm_mu_nhb_male)
  
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
                              mu_mm = mm_mu_nhw_female)
  
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
                              mu_mm = mm_mu_nhb_female)
  
  # calculate the log-likelihood
  ll <- ll_mgus_prev(df_mgus = data_list[['mgus_prev_male_nhw']], ages = ages, p_mgus = out_male_nhw$p_mgus, pop = pop_male_nhw)
  ll <- ll + ll_mgus_prev(df_mgus = data_list[['mgus_prev_male_nhb']], ages = ages, p_mgus = out_male_nhb$p_mgus, pop = pop_male_nhb)
  ll <- ll + ll_mgus_prev(df_mgus = data_list[['mgus_prev_female_nhw']], ages = ages, p_mgus = out_female_nhw$p_mgus, pop = pop_female_nhw)
  ll <- ll + ll_mgus_prev(df_mgus = data_list[['mgus_prev_female_nhb']], ages = ages, p_mgus = out_female_nhb$p_mgus, pop = pop_female_nhb)
  
  ll <- ll + ll_mm_incid(df_mm = data_list[['mm_incid_male_nhw']], i_mm = out_male_nhw$i_mm, tau = tau, ages = ages, pop = pop_male_nhw)
  ll <- ll + ll_mm_incid(df_mm = data_list[['mm_incid_male_nhb']], i_mm = out_male_nhb$i_mm, tau = tau, ages = ages, pop = pop_male_nhb)
  ll <- ll + ll_mm_incid(df_mm = data_list[['mm_incid_female_nhw']], i_mm = out_female_nhw$i_mm, tau = tau, ages = ages, pop = pop_female_nhw)
  ll <- ll + ll_mm_incid(df_mm = data_list[['mm_incid_female_nhb']], i_mm = out_female_nhb$i_mm, tau = tau, ages = ages, pop = pop_female_nhb)
  
  # return log-likelihood
  if(is.infinite(ll) | is.na(ll)){ll <- -1e20}
  return(ll)
}

ll_age_cmp <- cmpfun(ll_age)

