# set working directory
setwd('~/Documents/MM_Model_Race/code/')

# clear existing workspace
rm(list = ls())

# function to normalize vector
normalize <- function(x){x / sum(x)}

# load the bridged race population projections from 2010
pop_br <- read.table('../data/CDC_Wonder_Bridged_Race_2010.txt', sep = '\t', header = T)

# load the population projections from 2014. this will be used to disaggregate the 85+ years age category from the bridged race projections
pop_pp <- read.table('../data/CDC_Wonder_Population_Projection_2014.txt', sep = '\t', header = T)

# subset the data into the race/gender pairings
pop_br_male_nhw <- subset(pop_br, Gender == 'Male' & Race == 'White')
pop_pp_male_nhw <- subset(pop_pp, Gender == 'Male' & Race == 'White')

pop_br_female_nhw <- subset(pop_br, Gender == 'Female' & Race == 'White')
pop_pp_female_nhw <- subset(pop_pp, Gender == 'Female' & Race == 'White')

pop_br_male_nhb <- subset(pop_br, Gender == 'Male' & Race == 'Black or African American')
pop_pp_male_nhb <- subset(pop_pp, Gender == 'Male' & Race == 'Black or African American')

pop_br_female_nhb <- subset(pop_br, Gender == 'Female' & Race == 'Black or African American')
pop_pp_female_nhb <- subset(pop_pp, Gender == 'Female' & Race == 'Black or African American')

# create the data frame 
df_male_nhw <- data.frame(age = 0:99,
                          gender = 'Male',
                          race = 'Non_Hispanic_White',
                          N = NA)

df_female_nhw <- data.frame(age = 0:99,
                          gender = 'Female',
                          race = 'Non_Hispanic_White',
                          N = NA)

df_male_nhb <- data.frame(age = 0:99,
                          gender = 'Male',
                          race = 'Non_Hispanic_Black',
                          N = NA)

df_female_nhb <- data.frame(age = 0:99,
                            gender = 'Female',
                            race = 'Non_Hispanic_Black',
                            N = NA)

# calculate the age distribution for individuals 85-100 based upon the population projections from 2014
dist_male_nhw <- normalize(pop_pp_male_nhw$Projected.Populations[pop_pp_male_nhw$Age.Code %in% 85:100])
dist_female_nhw <- normalize(pop_pp_female_nhw$Projected.Populations[pop_pp_female_nhw$Age.Code %in% 85:100])
dist_male_nhb <- normalize(pop_pp_male_nhb$Projected.Populations[pop_pp_male_nhb$Age.Code %in% 85:100])
dist_female_nhb <- normalize(pop_pp_female_nhb$Projected.Populations[pop_pp_female_nhb$Age.Code %in% 85:100])

# fill in the ages 0:84 from the bridged race projections
df_male_nhw$N[df_male_nhw$age %in% 0:84] <- pop_br_male_nhw$Population[pop_br_male_nhw$Age.Code %in% 0:84]
df_female_nhw$N[df_female_nhw$age %in% 0:84] <- pop_br_female_nhw$Population[pop_br_female_nhw$Age.Code %in% 0:84]
df_male_nhb$N[df_male_nhb$age %in% 0:84] <- pop_br_male_nhb$Population[pop_br_male_nhb$Age.Code %in% 0:84]
df_female_nhb$N[df_female_nhb$age %in% 0:84] <- pop_br_female_nhb$Population[pop_br_female_nhb$Age.Code %in% 0:84]

# fill in the ages 85:99 by disaggregating the bridged race projections using the distribution calculated from the population projections
df_male_nhw$N[df_male_nhw$age %in% 85:99] <- round(pop_br_male_nhw$Population[pop_br_male_nhw$Age.Code == 85] * head(dist_male_nhw, n = -1))
df_female_nhw$N[df_female_nhw$age %in% 85:99] <- round(pop_br_female_nhw$Population[pop_br_female_nhw$Age.Code == 85] * head(dist_female_nhw, n = -1))
df_male_nhb$N[df_male_nhb$age %in% 85:99] <- round(pop_br_male_nhb$Population[pop_br_male_nhb$Age.Code == 85] * head(dist_male_nhb, n = -1))
df_female_nhb$N[df_female_nhb$age %in% 85:99] <- round(pop_br_female_nhb$Population[pop_br_female_nhb$Age.Code == 85] * head(dist_female_nhb, n = -1))

# combine the data frames
df <- rbind(df_male_nhw,
            df_female_nhw,
            df_male_nhb,
            df_female_nhb)

# write data frame to file
write.csv(df,
          file = '../data/Population_Data_Disaggregated_2010.csv',
          row.names = F)

# load the bridged race population projections from 2010
pop_br <- read.table('../data/CDC_Wonder_Bridged_Race_2004.txt', sep = '\t', header = T)

# load the population projections from 2014. this will be used to disaggregate the 85+ years age category from the bridged race projections
pop_pp <- read.table('../data/CDC_Wonder_Population_Projection_2014.txt', sep = '\t', header = T)

# subset the data into the race/gender pairings
pop_br_male_nhw <- subset(pop_br, Gender == 'Male' & Race == 'White')
pop_pp_male_nhw <- subset(pop_pp, Gender == 'Male' & Race == 'White')

pop_br_female_nhw <- subset(pop_br, Gender == 'Female' & Race == 'White')
pop_pp_female_nhw <- subset(pop_pp, Gender == 'Female' & Race == 'White')

pop_br_male_nhb <- subset(pop_br, Gender == 'Male' & Race == 'Black or African American')
pop_pp_male_nhb <- subset(pop_pp, Gender == 'Male' & Race == 'Black or African American')

pop_br_female_nhb <- subset(pop_br, Gender == 'Female' & Race == 'Black or African American')
pop_pp_female_nhb <- subset(pop_pp, Gender == 'Female' & Race == 'Black or African American')

# create the data frame 
df_male_nhw <- data.frame(age = 0:99,
                          gender = 'Male',
                          race = 'Non_Hispanic_White',
                          N = NA)

df_female_nhw <- data.frame(age = 0:99,
                            gender = 'Female',
                            race = 'Non_Hispanic_White',
                            N = NA)

df_male_nhb <- data.frame(age = 0:99,
                          gender = 'Male',
                          race = 'Non_Hispanic_Black',
                          N = NA)

df_female_nhb <- data.frame(age = 0:99,
                            gender = 'Female',
                            race = 'Non_Hispanic_Black',
                            N = NA)

# calculate the age distribution for individuals 85-100 based upon the population projections from 2014
dist_male_nhw <- normalize(pop_pp_male_nhw$Projected.Populations[pop_pp_male_nhw$Age.Code %in% 85:100])
dist_female_nhw <- normalize(pop_pp_female_nhw$Projected.Populations[pop_pp_female_nhw$Age.Code %in% 85:100])
dist_male_nhb <- normalize(pop_pp_male_nhb$Projected.Populations[pop_pp_male_nhb$Age.Code %in% 85:100])
dist_female_nhb <- normalize(pop_pp_female_nhb$Projected.Populations[pop_pp_female_nhb$Age.Code %in% 85:100])

# fill in the ages 0:84 from the bridged race projections
df_male_nhw$N[df_male_nhw$age %in% 0:84] <- pop_br_male_nhw$Population[pop_br_male_nhw$Age.Code %in% 0:84]
df_female_nhw$N[df_female_nhw$age %in% 0:84] <- pop_br_female_nhw$Population[pop_br_female_nhw$Age.Code %in% 0:84]
df_male_nhb$N[df_male_nhb$age %in% 0:84] <- pop_br_male_nhb$Population[pop_br_male_nhb$Age.Code %in% 0:84]
df_female_nhb$N[df_female_nhb$age %in% 0:84] <- pop_br_female_nhb$Population[pop_br_female_nhb$Age.Code %in% 0:84]

# fill in the ages 85:99 by disaggregating the bridged race projections using the distribution calculated from the population projections
df_male_nhw$N[df_male_nhw$age %in% 85:99] <- round(pop_br_male_nhw$Population[pop_br_male_nhw$Age.Code == 85] * head(dist_male_nhw, n = -1))
df_female_nhw$N[df_female_nhw$age %in% 85:99] <- round(pop_br_female_nhw$Population[pop_br_female_nhw$Age.Code == 85] * head(dist_female_nhw, n = -1))
df_male_nhb$N[df_male_nhb$age %in% 85:99] <- round(pop_br_male_nhb$Population[pop_br_male_nhb$Age.Code == 85] * head(dist_male_nhb, n = -1))
df_female_nhb$N[df_female_nhb$age %in% 85:99] <- round(pop_br_female_nhb$Population[pop_br_female_nhb$Age.Code == 85] * head(dist_female_nhb, n = -1))

# combine the data frames
df <- rbind(df_male_nhw,
            df_female_nhw,
            df_male_nhb,
            df_female_nhb)

# write data frame to file
write.csv(df,
          file = '../data/Population_Data_Disaggregated_2004.csv',
          row.names = F)