# set working directory
setwd('~/Documents/MM_Model_Race/code/')

# clear existing workspace
rm(list = ls())

# load necessary functions
source('functions_convergence.R')
source('functions_likelihood.R')
source('functions_validate_inference.R')

# load the data
load('../output/data_organized.RData')

# load the posterior distribution and specify the burn in period 
burn.in = 1:5e5
thin.factor = 50

get_convergence_stats(path.in = '../output/',
                      file.pattern = 'posterior_.*bz2',
                      burn.in = max(burn.in),
                      thin.factor = thin.factor)

pool_chain(path.in = '../output/',
           file.pattern = 'posterior_.*bz2',
           burn.in = max(burn.in),
           thin.factor = thin.factor,
           file.out = '../output/posterior_pooled.csv')

post <- read.csv('../output/posterior_pooled.csv')

# generate CI / PPI
preds <- calc_preds(post = post,
                    burn.in = 0:0,
                    num.samps = 500,
                    data_list = data_list,
                    ages = ages,
                    pop_male_nhw = pop_male_nhw,
                    pop_male_nhb = pop_male_nhb,
                    pop_female_nhw = pop_female_nhw,
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
                    mu_mm_female_nhb = mm_mu_nhb_female)

# calculate the 95% confidence intervals for the mgus prevalence data 
n_male_nhw <- data_list$mgus_prev_male_nhw$n
ns_male_nhw <- data_list$mgus_prev_male_nhw$y
nf_male_nhw <- n_male_nhw - ns_male_nhw
p_male_nhw_lower <- (ns_male_nhw / n_male_nhw) - (1.96 / (n_male_nhw * sqrt(n_male_nhw))) * sqrt(ns_male_nhw * nf_male_nhw)
p_male_nhw_upper <- (ns_male_nhw / n_male_nhw) + (1.96 / (n_male_nhw * sqrt(n_male_nhw))) * sqrt(ns_male_nhw * nf_male_nhw)

n_female_nhw <- data_list$mgus_prev_female_nhw$n
ns_female_nhw <- data_list$mgus_prev_female_nhw$y
nf_female_nhw <- n_female_nhw - ns_female_nhw
p_female_nhw_lower <- (ns_female_nhw / n_female_nhw) - (1.96 / (n_female_nhw * sqrt(n_female_nhw))) * sqrt(ns_female_nhw * nf_female_nhw)
p_female_nhw_upper <- (ns_female_nhw / n_female_nhw) + (1.96 / (n_female_nhw * sqrt(n_female_nhw))) * sqrt(ns_female_nhw * nf_female_nhw)

n_male_nhb <- data_list$mgus_prev_male_nhb$n
ns_male_nhb <- data_list$mgus_prev_male_nhb$y
nf_male_nhb <- n_male_nhb - ns_male_nhb
p_male_nhb_lower <- (ns_male_nhb / n_male_nhb) - (1.96 / (n_male_nhb * sqrt(n_male_nhb))) * sqrt(ns_male_nhb * nf_male_nhb)
p_male_nhb_upper <- (ns_male_nhb / n_male_nhb) + (1.96 / (n_male_nhb * sqrt(n_male_nhb))) * sqrt(ns_male_nhb * nf_male_nhb)

n_female_nhb <- data_list$mgus_prev_female_nhb$n
ns_female_nhb <- data_list$mgus_prev_female_nhb$y
nf_female_nhb <- n_female_nhb - ns_female_nhb
p_female_nhb_lower <- (ns_female_nhb / n_female_nhb) - (1.96 / (n_female_nhb * sqrt(n_female_nhb))) * sqrt(ns_female_nhb * nf_female_nhb)
p_female_nhb_upper <- (ns_female_nhb / n_female_nhb) + (1.96 / (n_female_nhb * sqrt(n_female_nhb))) * sqrt(ns_female_nhb * nf_female_nhb)


# save output to data frame 
save(list = c('preds',
              'data_list',
              'ages',
              'p_male_nhw_lower',
              'p_male_nhw_upper',
              'p_female_nhw_lower',
              'p_female_nhw_upper',
              'p_male_nhb_lower',
              'p_male_nhb_upper',
              'p_female_nhb_lower',
              'p_female_nhb_upper'),
     file = '../output/fig_1.RData')

# check coverage probs 
all((data_list$mgus_prev_male_nhw$y >= preds$y_male_nhw_ppi['2.5%',]) & (data_list$mgus_prev_male_nhw$y <= preds$y_male_nhw_ppi['97.5%',]))
all((data_list$mgus_prev_female_nhw$y >= preds$y_female_nhw_ppi['2.5%',]) & (data_list$mgus_prev_female_nhw$y <= preds$y_female_nhw_ppi['97.5%',]))
all((data_list$mgus_prev_male_nhb$y >= preds$y_male_nhb_ppi['2.5%',]) & (data_list$mgus_prev_male_nhb$y <= preds$y_male_nhb_ppi['97.5%',]))
all((data_list$mgus_prev_female_nhb$y >= preds$y_female_nhb_ppi['2.5%',]) & (data_list$mgus_prev_female_nhb$y <= preds$y_female_nhb_ppi['97.5%',]))

all((data_list$mm_incid_male_nhw$x >= preds$x_male_nhw_ppi['2.5%',]) & (data_list$mm_incid_male_nhw$x <= preds$x_male_nhw_ppi['97.5%',]))
all((data_list$mm_incid_female_nhw$x >= preds$x_female_nhw_ppi['2.5%',]) & (data_list$mm_incid_female_nhw$x <= preds$x_female_nhw_ppi['97.5%',]))
all((data_list$mm_incid_male_nhb$x >= preds$x_male_nhb_ppi['2.5%',]) & (data_list$mm_incid_male_nhb$x <= preds$x_male_nhb_ppi['97.5%',]))
all((data_list$mm_incid_female_nhb$x >= preds$x_female_nhb_ppi['2.5%',]) & (data_list$mm_incid_female_nhb$x <= preds$x_female_nhb_ppi['97.5%',]))

