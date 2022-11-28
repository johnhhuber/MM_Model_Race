# set working directory
setwd('~/Documents/MM_Model_Race/code/')

# clear existing workspace
rm(list = ls())

# install necessary packages
if(!require(openxlsx)){install.packages('openxlsx'); library(openxlsx)}

# load data structures
load('../output/fig_S1.RData')

# load population data and subset
pop <- read.csv('../data/Population_Data_Disaggregated_2010.csv')

pop_male_nhw <- subset(pop, gender == 'Male' & race == 'Non_Hispanic_White')
pop_female_nhw <- subset(pop, gender == 'Female' & race == 'Non_Hispanic_White')
pop_male_nhb <- subset(pop, gender == 'Male' & race == 'Non_Hispanic_Black')
pop_female_nhb <- subset(pop, gender == 'Female' & race == 'Non_Hispanic_Black')

# load mortality data
mort <- read.xlsx('../data/CDC_Life_Tables.xlsx')
mort <- subset(mort, Age < 100 & Year == 2010)

mort_male_nhw <- subset(mort, Sex == 'Male' & Race == 'Non_Hispanic_White')
mort_female_nhw <- subset(mort, Sex == 'Female' & Race == 'Non_Hispanic_White')
mort_male_nhb <- subset(mort, Sex == 'Male' & Race == 'Non_Hispanic_Black')
mort_female_nhb <- subset(mort, Sex == 'Female' & Race == 'Non_Hispanic_Black')

mort_male_nhw <- data.frame(age = mort_male_nhw$Age, 
                            mu = -log(1 - mort_male_nhw$Death_Prob))
mort_female_nhw <- data.frame(age = mort_female_nhw$Age, 
                            mu = -log(1 - mort_female_nhw$Death_Prob))
mort_male_nhb <- data.frame(age = mort_male_nhb$Age, 
                            mu = -log(1 - mort_male_nhb$Death_Prob))
mort_female_nhb <- data.frame(age = mort_female_nhb$Age, 
                              mu = -log(1 - mort_female_nhb$Death_Prob))

# specify the mgus mortality multipliers
mgus_mortality_multiplier_male = 1.25
mgus_mortality_multiplier_female = 1.11

# specify ages
ages = 0:99

# load the epi data and construct data list 
mgus_prev <- read.csv('../data/MGUS_Data_Reformatted.csv')

mgus_prev_male_nhw <- subset(mgus_prev, sex == 'Male' & race == 'Non_Hispanic_White')
mgus_prev_female_nhw <- subset(mgus_prev, sex == 'Female' & race == 'Non_Hispanic_White')
mgus_prev_male_nhb <- subset(mgus_prev, sex == 'Male' & race == 'Non_Hispanic_Black')
mgus_prev_female_nhb <- subset(mgus_prev, sex == 'Female' & race == 'Non_Hispanic_Black')

mm_incid <- read.xlsx('../data/SEER_MM_Incidence.xlsx')
mm_incid <- subset(mm_incid, Year == 2010)
mm_incid <- data.frame(age_lower = mm_incid$Age_Lower,
                       age_upper = mm_incid$Age_Upper,
                       sex = mm_incid$Sex,
                       race = mm_incid$Race,
                       x = mm_incid$MM_Incidence)
mm_incid$x <- mm_incid$x / 1e5
mm_incid <- subset(mm_incid, age_lower >= 30)

mm_incid_male_nhw <- subset(mm_incid, sex == 'Male' & race == 'Non_Hispanic_White')
mm_incid_female_nhw <- subset(mm_incid, sex == 'Female' & race == 'Non_Hispanic_White')
mm_incid_male_nhb <- subset(mm_incid, sex == 'Male' & race == 'Non_Hispanic_Black')
mm_incid_female_nhb <- subset(mm_incid, sex == 'Female' & race == 'Non_Hispanic_Black')

data_list = list(mgus_prev_male_nhw = mgus_prev_male_nhw,
                 mgus_prev_female_nhw = mgus_prev_female_nhw,
                 mgus_prev_male_nhb = mgus_prev_male_nhb,
                 mgus_prev_female_nhb = mgus_prev_female_nhb,
                 mm_incid_male_nhw = mm_incid_male_nhw,
                 mm_incid_female_nhw = mm_incid_female_nhw,
                 mm_incid_male_nhb = mm_incid_male_nhb,
                 mm_incid_female_nhb = mm_incid_female_nhb)

# save data to RData 
save(pop_male_nhw,
     pop_male_nhb,
     pop_female_nhw,
     pop_female_nhb,
     mort_male_nhw,
     mort_male_nhb,
     mort_female_nhw,
     mort_female_nhb,
     mm_mu_nhw_male,
     mm_mu_nhb_male,
     mm_mu_nhw_female,
     mm_mu_nhb_female,
     mgus_mortality_multiplier_male,
     mgus_mortality_multiplier_female,
     ages,
     data_list,
     file = '../output/data_organized.RData')

# load population data and subset
pop <- read.csv('../data/Population_Data_Disaggregated_2004.csv')

pop_male_nhw <- subset(pop, gender == 'Male' & race == 'Non_Hispanic_White')
pop_female_nhw <- subset(pop, gender == 'Female' & race == 'Non_Hispanic_White')
pop_male_nhb <- subset(pop, gender == 'Male' & race == 'Non_Hispanic_Black')
pop_female_nhb <- subset(pop, gender == 'Female' & race == 'Non_Hispanic_Black')

# load mortality data
mort <- read.xlsx('../data/CDC_Life_Tables.xlsx')
mort <- subset(mort, Age < 100 & Year == 2004)

mort_male_nhw <- subset(mort, Sex == 'Male' & Race == 'Non_Hispanic_White')
mort_female_nhw <- subset(mort, Sex == 'Female' & Race == 'Non_Hispanic_White')
mort_male_nhb <- subset(mort, Sex == 'Male' & Race == 'Non_Hispanic_Black')
mort_female_nhb <- subset(mort, Sex == 'Female' & Race == 'Non_Hispanic_Black')

mort_male_nhw <- data.frame(age = mort_male_nhw$Age, 
                            mu = -log(1 - mort_male_nhw$Death_Prob))
mort_female_nhw <- data.frame(age = mort_female_nhw$Age, 
                              mu = -log(1 - mort_female_nhw$Death_Prob))
mort_male_nhb <- data.frame(age = mort_male_nhb$Age, 
                            mu = -log(1 - mort_male_nhb$Death_Prob))
mort_female_nhb <- data.frame(age = mort_female_nhb$Age, 
                              mu = -log(1 - mort_female_nhb$Death_Prob))

# specify the mgus mortality multipliers
mgus_mortality_multiplier_male = 1.25
mgus_mortality_multiplier_female = 1.11

# specify ages
ages = 0:99

# load the epi data and construct data list 
mgus_prev <- read.csv('../data/MGUS_Data_Reformatted.csv')

mgus_prev_male_nhw <- subset(mgus_prev, sex == 'Male' & race == 'Non_Hispanic_White')
mgus_prev_female_nhw <- subset(mgus_prev, sex == 'Female' & race == 'Non_Hispanic_White')
mgus_prev_male_nhb <- subset(mgus_prev, sex == 'Male' & race == 'Non_Hispanic_Black')
mgus_prev_female_nhb <- subset(mgus_prev, sex == 'Female' & race == 'Non_Hispanic_Black')

mm_incid <- read.xlsx('../data/SEER_MM_Incidence.xlsx')
mm_incid <- subset(mm_incid, Year == 2004)
mm_incid <- data.frame(age_lower = mm_incid$Age_Lower,
                       age_upper = mm_incid$Age_Upper,
                       sex = mm_incid$Sex,
                       race = mm_incid$Race,
                       x = mm_incid$MM_Incidence)
mm_incid$x <- mm_incid$x / 1e5
mm_incid <- subset(mm_incid, age_lower >= 30)

mm_incid_male_nhw <- subset(mm_incid, sex == 'Male' & race == 'Non_Hispanic_White')
mm_incid_female_nhw <- subset(mm_incid, sex == 'Female' & race == 'Non_Hispanic_White')
mm_incid_male_nhb <- subset(mm_incid, sex == 'Male' & race == 'Non_Hispanic_Black')
mm_incid_female_nhb <- subset(mm_incid, sex == 'Female' & race == 'Non_Hispanic_Black')

data_list = list(mgus_prev_male_nhw = mgus_prev_male_nhw,
                 mgus_prev_female_nhw = mgus_prev_female_nhw,
                 mgus_prev_male_nhb = mgus_prev_male_nhb,
                 mgus_prev_female_nhb = mgus_prev_female_nhb,
                 mm_incid_male_nhw = mm_incid_male_nhw,
                 mm_incid_female_nhw = mm_incid_female_nhw,
                 mm_incid_male_nhb = mm_incid_male_nhb,
                 mm_incid_female_nhb = mm_incid_female_nhb)

# save data to RData 
save(pop_male_nhw,
     pop_male_nhb,
     pop_female_nhw,
     pop_female_nhb,
     mort_male_nhw,
     mort_male_nhb,
     mort_female_nhw,
     mort_female_nhb,
     mm_mu_nhw_male,
     mm_mu_nhb_male,
     mm_mu_nhw_female,
     mm_mu_nhb_female,
     mgus_mortality_multiplier_male,
     mgus_mortality_multiplier_female,
     ages,
     data_list,
     file = '../output/data_organized_2004.RData')
