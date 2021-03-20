# Loading .csv data
library(tidyverse)
setwd("~/Documents/00_Semester 5/Development Economics/Empirical Assignment")
edu_data <- read.csv('data_raw/Post Planting Wave 2/Household/sect2_plantingw2.csv')

# identifier
edu_data <- rename(edu_data, id_hh = hhid)
edu_data <- within(edu_data, id_ind <- paste(id_hh,indiv,sep='-'))

# literacy
edu_data <- edu_data %>% 
  mutate(ind_read = NA, ind_write = NA) %>% # no data on the separate questions
  rename(ind_literacy = s2q4)

# schooling
edu_data <- edu_data %>%
  rename(ind_any_schooling = s2q5) %>% # have you ever attended school?
  rename(ind_school = s2q10) %>% # currently at school?
  rename(ind_education = s2q9) %>% # highest completed degree
  mutate(ind_education = case_when(ind_education == 1 ~ 0,
                                   ind_education %in% c(2:3) ~ 1,
                                   ind_education == 5 ~ 2,
                                   ind_education %in% c(6:7) ~ 3,
                                   ind_education == 4 ~ 4,
                                   ind_education == 8 ~ 4,
                                   ind_education %in% c(9:10) ~ 5,
                                   ind_education %in% c(11:12) ~ 6)) %>%
  mutate(ind_education = case_when(ind_school == 1 ~ NA_real_,
                                   ind_school %in% c(2,NA) ~ ind_education)) %>% 
  mutate(ind_education = case_when(ind_any_schooling == 2 ~ 0,
                                   ind_any_schooling %in% c(1,NA) ~ ind_education)) %>%  
  mutate(ind_education_years = case_when(ind_education == 0 ~ 0,
                                         ind_education == 1 ~ 6,
                                         ind_education == 2 ~ 9,
                                         ind_education %in% c(3:4) ~ 12,
                                         ind_education == 5 ~ 15,
                                         ind_education == 6 ~ 17))

count(edu_data, ind_education)
count(edu_data, ind_education_years)

# select clean data
edu_data <- select(edu_data, id_hh, id_ind, ind_read, ind_write, ind_literacy, 
                   ind_any_schooling, ind_school, ind_education, 
                   ind_education_years)
head(edu_data)

# save data
write.csv(edu_data,'data_clean/ind_education.csv',row.names = F)

