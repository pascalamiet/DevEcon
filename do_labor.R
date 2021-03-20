# loading environment -----------------------------------------------------
library(tidyverse)
setwd("~/Documents/00_Semester 5/Development Economics/Empirical Assignment")
labor_data <- read.csv('data_raw/Post Harvest Wave 2/Household/sect3a_harvestw2.csv') # labor data for the past 7 days


# identifier --------------------------------------------------------------
labor_data <- rename(labor_data, id_hh = hhid) 
labor_data <- within(labor_data, id_ind <- paste(id_hh,indiv,sep='-'))


# labor market status -----------------------------------------------------
labor_data <- rename(labor_data, ind_work = s3aq4) # ind_work
ind_char <- read_csv('data_clean/ind_characteristics.csv') # get ind ages out of another data set
ind_char <- select(ind_char, id_ind, ind_age)
labor_data <- left_join(labor_data, ind_char, by='id_ind')
rm(ind_char) # remove characteristics data from environment
labor_data <- mutate(labor_data, ind_work = 
                         case_when(ind_age >= 15 ~ ind_work)) # everyone below 15yo gets an NA

labor_data <- rename(labor_data, ind_emp_search = s3aq5) # ind_emp_search only looks at past 7 days!
labor_data <- mutate(labor_data, ind_emp_search = 
                         case_when(ind_work %in% c(2,NA) ~ ind_emp_search)) # if ind_work == 1, then NA

labor_data <- mutate(labor_data, # no data available for these questions
                       ind_emp_search_mean = NA, 
                       ind_emp_search_duration = NA,
                       ind_emp_search_duration_unit = NA,
                       ind_start_se = NA)

labor_data <- labor_data %>% 
  rename(ind_nlf_reason = s3aq6) %>% 
  mutate(ind_nlf_reason = case_when(ind_nlf_reason %in% c(1,11) ~ 1,
                                    ind_nlf_reason == 2 ~ 2,
                                    ind_nlf_reason %in% c(4,5) ~ 3,
                                    ind_nlf_reason == 3 ~ 4,
                                    ind_nlf_reason %in% c(6:10,12) ~ 5)) %>% 
  mutate(ind_nlf_reason = 
           case_when(ind_work %in% c(2,NA) ~ ind_nlf_reason)) # if ind_work == 1, then NA

count(labor_data,ind_nlf_reason) # check if numbers are correct


# main job ----------------------------------------------------------------
labor_data <- labor_data %>% # ind_main_job_type
  mutate(ind_main_job_type = s3aq12a) %>% 
  mutate(ind_main_job_type = case_when(ind_main_job_type == 9 ~ 2,
                                       ind_main_job_type != 9 ~ 1)) %>% 
  mutate(ind_main_job_type = 
           case_when(s3aq12a1 %in% c('UNPAID','UNPAID APPRENTICE', # brute force, not elegant at all
                                     'UNPAID FAM WORKER','UNPAID FAMILY',
                                     'UNPAID FAMILY MEMBER',
                                     'UNPAID FAMILY WORKER',
                                     'UNPAID FAMINLY WORKE',
                                     'UNPAID FARM WORK',
                                     'UNPAID FARMILY WORKA',
                                     'UNPAID HH WORKER', 'UNPAID TRAINER') ~ 3,
                     ind_main_job_type == 2 ~ 2,
                     ind_main_job_type == 1 ~ 1)) %>% 
  mutate(ind_main_job_type = case_when(ind_work == 1 ~ ind_main_job_type)) # NA if ind_work == 2

labor_data <- labor_data %>% 
  mutate(ind_main_job_se_type = NA) %>% # no data found here
  mutate(ind_main_job_employer = case_when(s3aq12a %in% c(1:3) ~ 1, # what about self-employed people? they are NA in this setting!
                                           s3aq12a == 4 ~ 2,
                                           s3aq12a %in% c(5:8)  ~ 3,
                                           s3aq12a == 10 ~ 4)) %>% 
  mutate(ind_main_job_employer = case_when(ind_work == 1 ~ ind_main_job_employer))

labor_data <- labor_data %>% 
  rename(ind_main_job_hours = s3aq15) %>% # hours worked during the past week
  mutate(ind_main_job_hours = case_when(ind_work == 1 ~ ind_main_job_hours)) %>% # sets to NA for ppl that don't work
  mutate(ind_main_job_hours = case_when(ind_main_job_hours < 114 ~ ind_main_job_hours)) %>% # some ppl said they worked more hours than are in one week!
  rename(ind_main_job_wage = s3aq18a1) %>% 
  mutate(ind_main_job_wage = case_when(s3aq18a2 == 4 ~ as.integer(ind_main_job_wage/2), # adjust to different time frame
                                       s3aq18a2 == 6 ~ as.integer(ind_main_job_wage/3),
                                       s3aq18a2 == 7 ~ as.integer(ind_main_job_wage/6),
                                       s3aq18a2 == 8 ~ as.integer(ind_main_job_wage/12))) %>% 
  mutate(ind_main_job_wage = case_when(ind_work == 1 ~ ind_main_job_wage)) %>% 
  mutate(ind_main_job_wage_freq = s3aq18a2) %>% # I want to keep the old column as well
  mutate(ind_main_job_wage_freq = case_when(s3aq18a2 == 1 ~ 0,
                                            s3aq18a2 == 2 ~ 1,
                                            s3aq18a2 %in% c(3,4) ~ 2,
                                            s3aq18a2 %in% c(5:8) ~ 3)) %>% 
  mutate(ind_main_job_wage_freq = case_when(ind_work == 1 ~ 
                                              ind_main_job_wage_freq)) # if ind_work %in% (2,NA) then NA

labor_data <- labor_data %>% 
  mutate(ind_main_job_wage_hourly = 
           case_when(ind_main_job_wage_freq == 0 ~ as.integer(ind_main_job_wage),
                     ind_main_job_wage_freq == 1 ~ as.integer(ind_main_job_wage*5/ind_main_job_hours),
                     ind_main_job_wage_freq == 2 ~ as.integer(ind_main_job_wage/ind_main_job_hours),
                     ind_main_job_wage_freq == 3 ~ as.integer(ind_main_job_wage/(4.3*ind_main_job_hours)))) %>% 
  mutate(ind_main_job_wage_hourly = case_when(ind_work == 1 ~ ind_main_job_wage_hourly))


# store data set ----------------------------------------------------------
# select clean data
labor_data <- select(labor_data, id_hh, id_ind, ind_work, ind_emp_search, 
                     ind_emp_search_mean, ind_start_se, ind_nlf_reason, 
                     ind_main_job_type, ind_main_job_se_type, 
                     ind_main_job_employer, ind_main_job_hours, 
                     ind_main_job_wage, ind_main_job_wage_freq, 
                     ind_main_job_wage_hourly)
head(labor_data)

# save data
write.csv(labor_data,'data_clean/labor.csv',row.names = F)

