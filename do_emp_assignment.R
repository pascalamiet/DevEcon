# 0. Environment -----------------------------------------------------

# set up
library(tidyverse)
library(AER)
library(scales)
library(stargazer)
library(spatstat)

setwd("~/Documents/00_Semester 5/Development Economics/Empirical Assignment")

# import the data
hh <- read.csv('data_clean/hh.csv')
character <- read.csv('data_clean/ind_characteristics.csv')
edu <- read.csv('data_clean/ind_education.csv')
labor <- read.csv('data_clean/labor.csv')

# merge
edu$id_hh <- NULL
labor$id_hh <- NULL

data <- left_join(character, hh, by='id_hh')
data <- left_join(data, edu, by='id_ind')
data <- left_join(data, labor, by='id_ind')

data <- select(data,
               id_hh,id_ind,wgt,ea,rural,everything()) # rearrange the data

rm(character,edu,hh,labor)

# save data
write.csv(data,'data_clean/total_data.csv',row.names = F)


# 1. Descriptive Statistics -----------------------------------------------

# 1
mean_age <- data %>%
  summarize(mean_age = weighted.mean(ind_age, weight=wgt, na.rm = T)) # 23.99y

data1 <- select(data,ind_age)
data1 <- mutate(data1, ind_age = as.numeric(ind_age))
weighted.median(data1$ind_age,data$wgt) # 19y
rm(data1)

# plot
data %>% # maybe talk about comparison to switzerland
  ggplot(aes(x=ind_age, weight=wgt)) +
  geom_histogram(aes(y=..density..),
                 color="#e9ecef",
                 alpha=0.5,
                 binwidth = 2) +
  geom_vline(xintercept = as.double(mean_age),
             color = "black",
             linetype = 'dashed') +
  theme_bw() +
  theme(aspect.ratio = 1) +
  xlim(0,100) +
  labs(fill="", x='Age', y='Density')

rm(mean_age)

# 2
mean_edu <- data %>% 
  summarise(mean_edu=weighted.mean(ind_education_years,weight=wgt,na.rm=T)) # 4.65y
data_m <- data %>%
  filter(ind_sex == 1)
mean_edu_m <- data_m %>% # 5.6y
  summarise(mean_edu_m=weighted.mean(ind_education_years,weight=wgt,na.rm=T))
data_f <- data %>%
  filter(ind_sex == 2)
mean_edu_f <- data_f %>% # 3.85y
  summarise(mean_edu_f=weighted.mean(ind_education_years,weight=wgt,na.rm=T))
edu_gap <- as.double(mean_edu_m/mean_edu_f) # 1.45

edu_matrix <- matrix(NA,1,3)
edu_matrix[1,] <- round(c(as.double(mean_edu_m),as.double(mean_edu_f),edu_gap),2)
colnames(edu_matrix) <- c('Men','Women','Education Gap')
edu_matrix <- data.frame(edu_matrix)

variable_names <- list(
  "1" = "Men" ,
  "2" = "Women"
)

variable_labeller <- function(variable,value){
  return(variable_names[value])
}

# Plot
data %>% 
  filter(ind_sex %in% c(1:2)) %>% 
  ggplot(aes(x=ind_education_years, weight=wgt)) +
  geom_histogram(aes(y=..density..),
                 color="#e9ecef",
                 alpha=0.5,
                 binwidth = 2) +
  geom_vline(data=filter(data,ind_sex == 1),
             aes(xintercept = as.double(mean_edu_m)),
             color = "blue",
             linetype = 'dashed') +
  geom_vline(data=filter(data,ind_sex == 2),
             aes(xintercept = as.double(mean_edu_f)),
             color = "red",
             linetype = 'dashed') +
  theme_bw() +
  theme(aspect.ratio = 1) +
  xlim(-1,16) +
  labs(fill="", x='Years of education', y='Density') +
  facet_grid(. ~ ind_sex, labeller=variable_labeller)

rm(variable_labeller,variable_names)
rm(data_f,data_m,mean_edu,mean_edu_f,mean_edu_m,edu_gap)

count(filter(data,ind_sex==1),ind_education_years)
count(filter(data,ind_sex==2),ind_education_years) 

# 3
data <- data %>% 
  mutate(age_group = case_when(ind_age %in% c(15:25) ~ 1,
                               ind_age %in% c(25:35) ~ 2,
                               ind_age %in% c(35:45) ~ 3,
                               ind_age %in% c(45:55) ~ 4))

s1 <- data %>% # whole population
  filter(ind_literacy==1) %>% 
  summarize(s=sum(wgt,na.rm=T))
s2 <- data %>% 
  filter(ind_literacy %in% c(1:2)) %>% 
  summarize(s=sum(wgt,na.rm=T))
lit_rate <- as.double(s1)/as.double(s2) # 65.13%

data1 <- filter(data,age_group==1) # 15-25
s1 <- data1 %>% 
  filter(ind_literacy==1) %>% 
  summarize(s=sum(wgt,na.rm=T))
s2 <- data1 %>% 
  filter(ind_literacy %in% c(1:2)) %>% 
  summarize(s=sum(wgt,na.rm=T))
lit_rate1 <- as.double(s1)/as.double(s2) # 83.69%

data2 <- filter(data,age_group==2) # 25-35
s1 <- data2 %>% 
  filter(ind_literacy==1) %>% 
  summarize(s=sum(wgt,na.rm=T))
s2 <- data2 %>% 
  filter(ind_literacy %in% c(1:2)) %>% 
  summarize(s=sum(wgt,na.rm=T))
lit_rate2 <- as.double(s1)/as.double(s2) # 75.35%

data3 <- filter(data,age_group==3) # 35-45
s1 <- data3 %>% 
  filter(ind_literacy==1) %>% 
  summarize(s=sum(wgt,na.rm=T))
s2 <- data3 %>% 
  filter(ind_literacy %in% c(1:2)) %>% 
  summarize(s=sum(wgt,na.rm=T))
lit_rate3 <- as.double(s1)/as.double(s2) # 67.49%

data4 <- filter(data,age_group==4) # 45-55
s1 <- data4 %>% 
  filter(ind_literacy==1) %>% 
  summarize(s=sum(wgt,na.rm=T))
s2 <- data4 %>% 
  filter(ind_literacy %in% c(1:2)) %>% 
  summarize(s=sum(wgt,na.rm=T))
lit_rate4 <- as.double(s1)/as.double(s2) # 62.78%

rm(s1,s2)

lit_matrix <- matrix(NA,4,2) # create matrix for table
lit_matrix[,1] <- c('15-25','25-35','35-45','45-55')
lit_matrix[,2] <- c(round(100*lit_rate1,2),
                    round(100*lit_rate2,2),
                    round(100*lit_rate3,2),
                    round(100*lit_rate4,2))
colnames(lit_matrix) <- c('Age','Literacy Rate (%)')
lit_matrix <- data.frame(lit_matrix)

rm(lit_rate,lit_rate1,lit_rate2,lit_rate3,lit_rate4,edu_matrix)

# 4

data1 <- filter(data,age_group==1) # 15-25
s1 <- data1 %>% 
  filter(ind_education==0) %>% 
  summarize(s=sum(wgt,na.rm=T))
s2 <- data1 %>% 
  filter(ind_education %in% c(0:5)) %>% 
  summarize(s=sum(wgt,na.rm=T))
edu_rate10 <- as.double(s1)/as.double(s2) # 23.82%
s1 <- data1 %>% 
  filter(ind_education==1) %>% 
  summarize(s=sum(wgt,na.rm=T))
edu_rate11 <- as.double(s1)/as.double(s2) # 30.54%
s1 <- data1 %>% 
  filter(ind_education %in% c(2:4)) %>% 
  summarize(s=sum(wgt,na.rm=T))
edu_rate12 <- as.double(s1)/as.double(s2) # 45.64%
s1 <- data1 %>% 
  filter(ind_education==5) %>% 
  summarize(s=sum(wgt,na.rm=T))
edu_rate13 <- as.double(s1)/as.double(s2) # 0%

data2 <- filter(data,age_group==2) # 25-35
s1 <- data2 %>% 
  filter(ind_education==0) %>% 
  summarize(s=sum(wgt,na.rm=T))
s2 <- data2 %>% 
  filter(ind_education %in% c(0:5)) %>% 
  summarize(s=sum(wgt,na.rm=T))
edu_rate20 <- as.double(s1)/as.double(s2) # 26.73%
s1 <- data2 %>% 
  filter(ind_education==1) %>% 
  summarize(s=sum(wgt,na.rm=T))
edu_rate21 <- as.double(s1)/as.double(s2) # 22.27%
s1 <- data2 %>% 
  filter(ind_education %in% c(2:4)) %>% 
  summarize(s=sum(wgt,na.rm=T))
edu_rate22 <- as.double(s1)/as.double(s2) # 33.59%
s1 <- data2 %>% 
  filter(ind_education==5) %>% 
  summarize(s=sum(wgt,na.rm=T))
edu_rate23 <- as.double(s1)/as.double(s2) # 17.41%

data3 <- filter(data,age_group==3) # 35-45
s1 <- data3 %>% 
  filter(ind_education==0) %>% 
  summarize(s=sum(wgt,na.rm=T))
s2 <- data3 %>% 
  filter(ind_education %in% c(0:5)) %>% 
  summarize(s=sum(wgt,na.rm=T))
edu_rate30 <- as.double(s1)/as.double(s2) # 37.04%
s1 <- data3 %>% 
  filter(ind_education==1) %>% 
  summarize(s=sum(wgt,na.rm=T))
edu_rate31 <- as.double(s1)/as.double(s2) # 31.18%
s1 <- data3 %>% 
  filter(ind_education %in% c(2:4)) %>% 
  summarize(s=sum(wgt,na.rm=T))
edu_rate32 <- as.double(s1)/as.double(s2) # 25.87%
s1 <- data3 %>% 
  filter(ind_education==5) %>% 
  summarize(s=sum(wgt,na.rm=T))
edu_rate33 <- as.double(s1)/as.double(s2) # 5.91%

data4 <- filter(data,age_group==4) # 45-55
s1 <- data4 %>% 
  filter(ind_education==0) %>% 
  summarize(s=sum(wgt,na.rm=T))
s2 <- data4 %>% 
  filter(ind_education %in% c(0:5)) %>% 
  summarize(s=sum(wgt,na.rm=T))
edu_rate40 <- as.double(s1)/as.double(s2) # 49.04%
s1 <- data4 %>% 
  filter(ind_education==1) %>% 
  summarize(s=sum(wgt,na.rm=T))
edu_rate41 <- as.double(s1)/as.double(s2) # 15.57%
s1 <- data4 %>% 
  filter(ind_education %in% c(2:4)) %>% 
  summarize(s=sum(wgt,na.rm=T))
edu_rate42 <- as.double(s1)/as.double(s2) # 35.38%
s1 <- data4 %>% 
  filter(ind_education==5) %>% 
  summarize(s=sum(wgt,na.rm=T))
edu_rate43 <- as.double(s1)/as.double(s2) # 0%


edu_matrix <- matrix(NA,4,5) # create matrix for table
colnames(edu_matrix) <- c('Age','No Education (%)','Primary Education (%)',
                          'Secondary Education (%)','Tertiary Education (%)')
edu_matrix[,1] <- c('15-24','25-34','35-44','45-55')
edu_matrix[,2] <- round(100*c(edu_rate10,edu_rate20,edu_rate30,edu_rate40),2)
edu_matrix[,3] <- round(100*c(edu_rate11,edu_rate21,edu_rate31,edu_rate41),2)
edu_matrix[,4] <- round(100*c(edu_rate12,edu_rate22,edu_rate32,edu_rate42),2)
edu_matrix[,5] <- round(100*c(edu_rate13,edu_rate23,edu_rate33,edu_rate43),2)
edu_matrix <- data.frame(edu_matrix)

rm(edu_rate10,edu_rate11,edu_rate12,edu_rate13,edu_rate20,edu_rate21,edu_rate21,
   edu_rate22,edu_rate23,edu_rate31,edu_rate30,edu_rate32,edu_rate33,edu_rate40,
   edu_rate41,edu_rate42,edu_rate43,s1,s2,lit_matrix)


# 2.1 Labor Market Status -------------------------------------------------

# 1
p <- data %>% 
  filter(ind_age %in% c(15:55)) %>% 
  summarize(s=sum(wgt,na.rm=T))
q <- data %>% 
  summarize(s=sum(wgt,na.rm=T))
work_pop <- as.double(p)/as.double(q) # 47.56%

x <- matrix(NA,2,3)
colnames(x) <- c('Age','Percentage','lab.ypos')
x[,1] <- c('Working Age','Non-Working Age')
x[,2] <- c(round(100*work_pop,2),round(100*(1-work_pop),2))
x[,3] <- c(100-0.5*52.5,100*work_pop/2)
x <- data.frame(x)
x_table <- data.frame(x[,1:2])

rm(p,q,x,work_pop,edu_matrix)

# 2
data_working <- filter(data,ind_age %in% c(15:55))

p <- data_working %>% 
  filter(ind_work==1) %>% 
  summarize(s=sum(wgt,na.rm=T))
q <- data_working %>% 
  filter(ind_work %in% c(1:2)) %>% 
  summarize(s=sum(wgt,na.rm=T))
work_share <- as.double(p)/as.double(q) # 55.46%

p <- data_working %>% 
  filter(ind_main_job_type==1) %>% 
  summarize(s=sum(wgt,na.rm=T))
wagew_share <- as.double(p)/as.double(q) # 17.48%

p <- data_working %>% 
  filter(ind_main_job_type==2) %>% 
  summarize(s=sum(wgt,na.rm=T))
se_share <- as.double(p)/as.double(q) # 37.44%

p <- data_working %>% 
  filter(ind_main_job_type==3) %>% 
  summarize(s=sum(wgt,na.rm=T))
unpaid_share <- as.double(p)/as.double(q) # 0.28%

x <- matrix(NA,4,1)
colnames(x) <- '% of Working-Age Population'
rownames(x) <- c('Working','Wage Earner','Self-Employed','Unpaid')
x[,1] <- round(100*c(work_share,wagew_share,se_share,unpaid_share),2)

rm(x_table,p,q,work_share,se_share,wagew_share,unpaid_share,data_working)


# 2.2 Wage Measurement ----------------------------------------------------

# 1
rm(x) # remove variables we don't need anymore

mean_hours <- data %>%
  summarize(mean_hours = weighted.mean(ind_main_job_hours, weight=wgt, na.rm = T)) # 41.85h

weighted.median(data$ind_main_job_hours,data$wgt) # 42

data %>% # maybe talk about comparison to switzerland
  ggplot(aes(x=ind_main_job_hours, weight=wgt)) +
  geom_histogram(aes(y=..density..),
                 color="#e9ecef",
                 alpha=0.5,
                 binwidth = 3) +
  geom_vline(xintercept = as.double(mean_hours),
             color = "black",
             linetype = 'dashed') +
  theme_bw() +
  theme(aspect.ratio = 1) +
  xlim(-1,115) +
  labs(fill="", x='Hours Worked', y='Density')

rm(mean_hours)

# 2
library(scales)

mean_wage <- data %>%
  summarize(mean_wage = weighted.mean(ind_main_job_wage, weight=wgt, na.rm = T)) # 13770.28

mean_wage_h <- data %>%
  summarize(mean_wage_h = weighted.mean(ind_main_job_wage_hourly, weight=wgt, na.rm = T)) # 119.8

data %>%
  ggplot(aes(x=log(ind_main_job_wage), weight=wgt)) +
  geom_histogram(aes(y=..density..),
                 bins=30,
                 color="#e9ecef",
                 alpha=0.5) +
  geom_vline(xintercept = log(as.double(mean_wage)),
             color = "black",
             linetype = 'dashed') +
  theme_bw() +
  xlim(-1,15) +
  theme(aspect.ratio = 1) +
  labs(fill="", x='Wage', y='Density')

rm(mean_wage)

data %>% # maybe talk about comparison to switzerland
  ggplot(aes(x=log(ind_main_job_wage_hourly), weight=wgt)) +
  geom_histogram(aes(y=..density..),
                 bins=30,
                 color="#e9ecef",
                 alpha=0.5) +
  geom_vline(xintercept = log(as.double(mean_wage_h)),
             color = "black",
             linetype = 'dashed') +
  theme_bw() +
  xlim(-1,10) +
  theme(aspect.ratio = 1) +
  labs(fill="", x='Hourly Wage', y='Density')

rm(mean_wage_h)


# 2.3 Returns to Education ------------------------------------------------

# 1

# discuss the regression and write down the formula
data_reg <- select(data,ind_main_job_wage_hourly,ind_education_years,wgt)
data_reg <- mutate(data_reg,ind_education_years_squared=ind_education_years^2)

wage_reg <- lm(log(ind_main_job_wage_hourly) ~ ind_education_years + 
                 ind_education_years_squared, data=data_reg, weights=data_reg$wgt)

coef <- data.frame(wage_reg$coefficients) # table
colnames(coef) = c("Estimates")


data_reg2 <- data
data_reg2 <- data_reg2 %>% 
  mutate(ind_education_years_squared=ind_education_years^2) %>% 
  mutate(ind_sex = case_when(ind_sex == 1 ~ 1,
                             ind_sex == 2 ~ 0)) %>% 
  mutate(rural = case_when(rural == 1 ~ 0,
                           rural == 2 ~ 1)) %>% 
  mutate(dummy_se = case_when(ind_main_job_type == 2 ~ 1,
                              ind_main_job_type %in% c(1,3) ~ 0))

wage_reg2 <- lm(log(ind_main_job_wage_hourly) ~ ind_education_years + 
                  ind_education_years_squared + ind_sex + rural + ind_age +
                  dummy_se, data=data_reg2, weights=data_reg2$wgt)

coef2 <- data.frame(wage_reg2$coefficients) # table
colnames(coef2) = c("Estimates")
coef2

data %>% # plot
  ggplot(aes(x=ind_education_years,y=log(ind_main_job_wage_hourly), weight=wgt)) +
  geom_point(alpha=0.5) +
  geom_smooth(method='lm',color='tomato1', se=T) +
  theme_bw() +
  ylim(-1,10) +
  theme(aspect.ratio = 1) +
  labs(fill="", x='Years of education', y='Log of hourly wage')


# 3. Who is Self-Employed (BONUS) -----------------------------------------

# statistics
avg_edu_se <- data %>% 
  filter(ind_main_job_type == 2) %>% 
  summarize(m=weighted.mean(ind_education_years,weight=wgt,na.rm=T)) # 4.59y
avg_edu_ww <- data %>% 
  filter(ind_main_job_type == 1) %>% 
  summarize(m=weighted.mean(ind_education_years,weight=wgt,na.rm=T)) # 7.81y

avg_age_se <- data %>% 
  filter(ind_main_job_type==2) %>% 
  summarize(m=weighted.mean(ind_age,weight=wgt,na.rm=T)) # 43.95y
avg_age_ww <- data %>% 
  filter(ind_main_job_type==1) %>% 
  summarize(m=weighted.mean(ind_age,weight=wgt,na.rm=T)) # 35.73y

avg_hours_se <- data %>% 
  filter(ind_main_job_type==2) %>% 
  summarize(m=weighted.mean(ind_main_job_hours,weight=wgt,na.rm=T)) # 41.93h
avg_hours_ww <- data %>% 
  filter(ind_main_job_type==1) %>% 
  summarize(m=weighted.mean(ind_main_job_hours,weight=wgt,na.rm=T)) # 41.83h

stat_matrix <- matrix(NA,3,2)
stat_matrix[1,] <- round(c(as.double(avg_edu_se),as.double(avg_edu_ww)),2)
stat_matrix[2,] <- round(c(as.double(avg_age_se),as.double(avg_age_ww)),2)
stat_matrix[3,] <- round(c(as.double(avg_hours_se),as.double(avg_hours_ww)),2)
colnames(stat_matrix) <- c('Self-Employed','Wage Worker')
rownames(stat_matrix) <- c('Average Years of Schooling','Average Age',
                           'Average Hours Worked per Week')
stat_matrix <- data.frame(stat_matrix)

# share of population (gender,urban)
data_m <- filter(data,ind_sex==1)
data_f <- filter(data,ind_sex==2)

p  <- data_m %>% 
  filter(ind_main_job_type==2) %>% 
  filter(rural==1) %>% 
  summarize(s=sum(wgt,na.rm=T))
q <- data_m %>% 
  filter(ind_main_job_type %in% c(1:3)) %>% 
  summarize(s=sum(wgt,na.rm=T))
share_se_m_urb <- p/q # 20.87%
p  <- data_m %>% 
  filter(ind_main_job_type==2) %>% 
  filter(rural==2) %>% 
  summarize(s=sum(wgt,na.rm=T))
share_se_m_rur <- p/q # 47.49%

p  <- data_f %>% 
  filter(ind_main_job_type==2) %>% 
  filter(rural==1) %>% 
  summarize(s=sum(wgt,na.rm=T))
q <- data_f %>% 
  filter(ind_main_job_type %in% c(1:3)) %>% 
  summarize(s=sum(wgt,na.rm=T))
share_se_f_urb <- p/q # 27.89%
p  <- data_f %>% 
  filter(ind_main_job_type==2) %>% 
  filter(rural==2) %>% 
  summarize(s=sum(wgt,na.rm=T))
share_se_f_rur <- p/q # 47%

pop_matrix <- matrix(NA,3,3)
pop_matrix[1,] <- round(100*c(as.double(share_se_m_rur),as.double(share_se_f_rur),NA),2)
pop_matrix[2,] <- round(100*c(as.double(share_se_m_urb),as.double(share_se_f_urb),NA),2)
pop_matrix[3,] <- pop_matrix[1,]+pop_matrix[2,]
pop_matrix[,3] <- pop_matrix[,1]+pop_matrix[,2]
pop_matrix[3,3] <- '-'
colnames(pop_matrix) <- c('Men %','Women %','Total %')
rownames(pop_matrix) <- c('Rural %','Urban %','Total %')
pop_matrix

rm(avg_age_se,avg_age_ww,avg_edu_se,avg_edu_ww,avg_hours_se,avg_hours_ww,data_f,
   data_m,p,q,pop_matrix,share_se_f_rur,share_se_f_urb,share_se_m_rur,
   share_se_m_urb,stat_matrix)
