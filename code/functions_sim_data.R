# load necessary functions
source('functions_model.R')

# function to simulate the MGUS prevalence data
sim_data_mgus_prev <- function(n,
                               age_lower,
                               age_upper,
                               pop,
                               ages,
                               p_mgus)
{
  # weight p_mgus if necessary for upper age groups 
  p_mgus_weighted <- rep(NA, length(n))
  if(any(age_upper == age_lower))
  {
    p_mgus_weighted[which(age_upper == age_lower)] = p_mgus[sapply(which(age_upper == age_lower), function(i){match(age_lower[i], ages)})]
  }
  
  for(ii in which(age_lower != age_upper))
  {
    p_mgus_weighted[ii] = sum(p_mgus[match(age_lower[ii], ages):match(age_upper[ii], ages)] * pop$N[match(age_lower[ii], pop$age):match(age_upper[ii], pop$age)]) /
      sum(pop$N[match(age_lower[ii], pop$age):match(age_upper[ii], pop$age)])
  }
  
  # simulate number of positive samples y as a binomial distribution
  y <- rbinom(n = length(n), size = n, prob = p_mgus_weighted)
  
  # generate dataframe and return
  df <- data.frame(age_lower = age_lower,
                   age_upper = age_upper,
                   y = y,
                   n = n)
  return(df)
}

# function to simulate MM incidence data
sim_data_mm_incid <- function(ages,
                              pop,
                              age_lower,
                              age_upper,
                              i_mm,
                              tau)
{
  # compute the weighted incidence 
  i_mm_weighted <- rep(NA, length(age_lower))
  
  for(ii in 1:length(age_lower))
  {
    i_mm_weighted[ii] = sum(i_mm[match(age_lower[ii], ages):match(age_upper[ii], ages)] * pop$N[match(age_lower[ii], pop$age):match(age_upper[ii], pop$age)]) /
      sum(pop$N[match(age_lower[ii], pop$age):match(age_upper[ii], pop$age)])
  }
  
  # compute the mean on log scale for log-normal distribution
  # meanlog <- log(i_mm_weighted) - 0.5 * log(i_mm_weighted^2 + tau^2)
  # varlog <- log(1 + (tau^2)/(i_mm_weighted^2))
  # meanlog <- log(i_mm_weighted)
  
  # simulate incidence from log-normal distribution
  meanlog <- log(i_mm_weighted)
  x <- rnorm(n = length(meanlog), mean = meanlog, sd = sqrt(tau))
  x <- exp(x)
  #x <- rlnorm(n = length(meanlog), meanlog = meanlog, sdlog = sqrt(varlog))
  
  #x <- rgamma(n = length(i_mm_weighted), shape = (i_mm_weighted^2) / tau, rate = i_mm_weighted / tau)
  
  # generate dataframe and return
  df <- data.frame(age_lower = age_lower,
                   age_upper = age_upper,
                   x = x)
  return(df)
}
# generate simulated data
sim_data <- function(ages,
                     age_lower_mgus_prev,
                     age_upper_mgus_prev,
                     age_lower_mm_incid,
                     age_upper_mm_incid,
                     gamma_mgus,
                     beta_mgus_age,
                     gamma_mm,
                     beta_mm_age,
                     tau,
                     n_mgus,
                     pop,
                     mort)
{
  # simulate model
  out <- sim_model(ages = ages,
                   gamma_mgus = gamma_mgus,
                   beta_mgus_age = beta_mgus_age,
                   gamma_mm = gamma_mm,
                   beta_mm_age = beta_mm_age)
  
  # simulate the mgus prevalence data
  df_mgus <- sim_data_mgus_prev(n = n_mgus, age_lower = age_lower_mgus_prev, age_upper = age_upper_mgus_prev, pop = pop, ages = ages, p_mgus = out$p_mgus)
  
  # simulate the mm incidence data
  df_mm <- sim_data_mm_incid(ages = ages, pop = pop, age_lower = age_lower_mm_incid, age_upper = age_upper_mm_incid, i_mm = out$i_mm, tau = tau)
  
  # generate list and return
  data_list <- list(mgus_prev = df_mgus, mm_incid = df_mm)
  return(data_list)
}
