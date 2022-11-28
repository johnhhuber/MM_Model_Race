# set working directory
setwd('~/Documents/MM_Model_Race/code/')

# clear existing workspace
rm(list = ls())

# load the posterior distribution and specify burn in period
post <- read.csv('../output/posterior_pooled.csv')

# specify ages 
ages <- 0:100

# calculate the hazard of each factor 
haz_mgus_age <- apply(t(sapply(1:nrow(post), function(x){exp(post[x,1] + post[x,2] * ages)})),2,quantile, probs = c(0.025,0.50,0.975))
haz_mgus_age_sex <- apply(t(sapply(1:nrow(post), function(x){exp(post[x,1] + post[x,2] * ages + post[x,3])})),2,quantile, probs = c(0.025,0.50,0.975))
haz_mgus_age_race <- apply(t(sapply(1:nrow(post), function(x){exp(post[x,1] + post[x,2] * ages + post[x,4])})),2,quantile, probs = c(0.025,0.50,0.975))
haz_mgus_sex <- quantile(exp(post[,3]), probs = c(0.025,0.50,0.975))
haz_mgus_race <- quantile(exp(post[,4]), probs = c(0.025,0.50,0.975))

haz_mm_age <- apply(t(sapply(1:nrow(post), function(x){exp(post[x,5] + post[x,6] * ages + post[x,7] * ages^2)})),2,quantile, probs = c(0.025,0.50,0.975))
haz_mm_age_sex <- apply(t(sapply(1:nrow(post), function(x){exp(post[x,5] + post[x,6] * ages + post[x,7] * ages^2 + post[x,8])})),2,quantile, probs = c(0.025,0.50,0.975))
haz_mm_age_race <- apply(t(sapply(1:nrow(post), function(x){exp(post[x,5] + post[x,6] * ages + post[x,7] * ages^2 + post[x,9])})),2,quantile, probs = c(0.025,0.50,0.975))
haz_mm_sex <- quantile(exp(post[,8]), probs = c(0.025,0.50,0.975))
haz_mm_race <- quantile(exp(post[,9]), probs = c(0.025,0.50,0.975))

haz_mm_age_max <- quantile(apply(t(sapply(1:nrow(post), function(x){exp(post[x,5] + post[x,6] * ages + post[x,7] * ages^2)})),1,which.max), probs = c(0.025,0.50,0.975)) - 1

# save output to data frame 
save(list = c('ages',
              'haz_mgus_age',
              'haz_mgus_age_sex',
              'haz_mgus_age_race',
              'haz_mgus_sex',
              'haz_mgus_race',
              'haz_mm_age',
              'haz_mm_age_max',
              'haz_mm_age_sex',
              'haz_mm_age_race',
              'haz_mm_sex',
              'haz_mm_race'),
     file = '../output/fig_3.RData')
