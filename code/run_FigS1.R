# set working directory
setwd('~/Documents/MM_Model_Race/code/')

# clear existing workspace
rm(list = ls())

# install necessary packages
if(!require(openxlsx)){install.packages('openxlsx'); library(openxlsx)}

# load the data 
dat <- read.xlsx('../data/SEER_MM_Survival.xlsx')
yrs <- unique(dat$Years_Since_Diagnosis)

# least squares function
least_squares <- function(par, dat)
{
  # get the years since diagnosis and survival
  yr <- dat$Years_Since_Diagnosis
  surv <- dat$Survival
  
  # generate predicted survival
  surv_pred <- exp(-yr * par[1])
  
  # compute least squares and return
  sum_ls <- sum((surv - surv_pred)^2)
  return(sum_ls)
}

## Non-Hispanic White Males

# subset data
dat_nhw_male <- subset(dat, Race == 'Non_Hispanic_White' & Sex == 'Male')

# fit to data 
fit_nhw_male <- optimize(f = least_squares, interval = c(0,2), dat = dat_nhw_male)
mm_mu_nhw_male <- fit_nhw_male$minimum
mm_ls_nhw_male <- fit_nhw_male$objective

pred_nhw_male <- exp(-yrs * mm_mu_nhw_male)
surv_nhw_male <- dat_nhw_male$Survival

mm_r2_nhw_male <- round(cor(pred_nhw_male, surv_nhw_male) ^ 2, digits = 2)

## Non-Hispanic White Female

# subset data
dat_nhw_female <- subset(dat, Race == 'Non_Hispanic_White' & Sex == 'Female')

# fit to data 
fit_nhw_female <- optimize(f = least_squares, interval = c(0,2), dat = dat_nhw_female)
mm_mu_nhw_female <- fit_nhw_female$minimum
mm_ls_nhw_female <- fit_nhw_female$objective

pred_nhw_female <- exp(-yrs * mm_mu_nhw_female)
surv_nhw_female <- dat_nhw_female$Survival

mm_r2_nhw_female <- round(cor(pred_nhw_female, surv_nhw_female) ^ 2, digits = 2)

## Non-Hispanic Black Males

# subset data
dat_nhb_male <- subset(dat, Race == 'Non_Hispanic_Black' & Sex == 'Male')

# fit to data 
fit_nhb_male <- optimize(f = least_squares, interval = c(0,2), dat = dat_nhb_male)
mm_mu_nhb_male <- fit_nhb_male$minimum
mm_ls_nhb_male <- fit_nhb_male$objective

pred_nhb_male <- exp(-yrs * mm_mu_nhb_male)
surv_nhb_male <- dat_nhb_male$Survival

mm_r2_nhb_male <- round(cor(pred_nhb_male, surv_nhb_male) ^ 2, digits = 2)

## Non-Hispanic Black Female

# subset data
dat_nhb_female <- subset(dat, Race == 'Non_Hispanic_Black' & Sex == 'Female')

# fit to data 
fit_nhb_female <- optimize(f = least_squares, interval = c(0,2), dat = dat_nhb_female)
mm_mu_nhb_female <- fit_nhb_female$minimum
mm_ls_nhb_female <- fit_nhb_female$objective

pred_nhb_female <- exp(-yrs * mm_mu_nhb_female)
surv_nhb_female <- dat_nhb_female$Survival

mm_r2_nhb_female <- round(cor(pred_nhb_female, surv_nhb_female) ^ 2, digits = 2)

# write data to file
save(mm_mu_nhw_male,
     mm_mu_nhw_female,
     mm_mu_nhb_male,
     mm_mu_nhb_female,
     mm_ls_nhw_male,
     mm_ls_nhw_female,
     mm_ls_nhb_male,
     mm_ls_nhb_female,
     mm_r2_nhw_male,
     mm_r2_nhw_female,
     mm_r2_nhb_male,
     mm_r2_nhb_female,
     file = '../output/fig_S1.RData')

