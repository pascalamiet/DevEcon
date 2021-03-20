# Loading .csv data
library(tidyverse)
setwd("~/Documents/00_Semester 5/Development Economics/Empirical Assignment")
ind_data <- read.csv('data_raw/Post Harvest Wave 2/Household/sect1_harvestw2.csv')

# identifier
ind_data <- rename(ind_data, id_hh = hhid)
ind_data <- within(ind_data, id_ind <- paste(id_hh,indiv,sep='-'))

# sex
ind_data <- rename(ind_data, ind_sex = s1q2) # 1=male; 2=female

# position
ind_data <- ind_data %>% 
  rename(ind_position = s1q3) %>% 
  mutate(ind_position = case_when(ind_position == 4 ~ 3,
                                  ind_position == 5 ~ 3,
                                  ind_position == 6 ~ 4,
                                  ind_position > 6 ~ 5,
                                  ind_position %in% c(1:3) ~ as.double(ind_position)))

count(ind_data,ind_position)

# age 
ind_data <- rename(ind_data, ind_age = s1q4)

# religion
ind_data <- ind_data %>% 
  rename(ind_religion = s1q17) %>% 
  mutate(ind_religion = case_when(ind_religion == 3 ~ 6,
                                  ind_religion == 4 ~ 8,
                                  ind_religion %in% c(1:2) ~ as.double(ind_religion)))

count(ind_data, ind_religion)

# marital status
ind_data <- ind_data %>%
  rename(ind_marital_status = s1q7) %>% 
  mutate(ind_marital_status = case_when(ind_marital_status %in% c(1:3) ~ 1,
                                        ind_marital_status %in% c(4:6) ~ 2,
                                        ind_marital_status == 7 ~ 3))

count(ind_data, ind_marital_status)

# years in location - not very helpful data!
ind_data <- mutate(ind_data, ind_years_in_location = NA) # question doesn't exist

# permanency (>4 months/year)
ind_data <- mutate(ind_data, ind_permanent = NA) # question doesn't exist

# hh size
ind_data <- ind_data %>% 
  group_by(id_hh) %>% 
  mutate(n_ind_household = n()) %>% 
  ungroup()

# children below 15y
ind_data <- ind_data %>% 
  group_by(id_hh) %>% 
  mutate(n_child_15 = sum(ind_age < 15, na.rm = T)) %>% 
  ungroup()

# children below 5y
ind_data <- ind_data %>% 
  group_by(id_hh) %>% 
  mutate(n_child_5 = sum(ind_age < 5, na.rm = T)) %>% 
  ungroup()

# clean data
ind_data <- select(ind_data, id_hh, id_ind, ind_sex, ind_position, ind_age,
                   ind_religion, ind_marital_status, ind_years_in_location,
                   ind_permanent, n_ind_household, n_child_15, n_child_5)
head(ind_data)
write.csv(ind_data,'data_clean/ind_characteristics.csv',row.names = F)
