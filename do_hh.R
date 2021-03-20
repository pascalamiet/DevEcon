# Loading .csv data
library(tidyverse)
setwd("~/Documents/00_Semester 5/Development Economics/Empirical Assignment")

hhdata <- read.csv('data_raw/cons_agg_wave2_visit1.csv')
as_tibble(hhdata)

# rename/mutate columns
hhdata <- rename(hhdata, id_hh = hhid)
hhdata <- rename(hhdata, wgt = hhweight)
hhdata <- mutate(hhdata,
                 rural = dplyr::if_else(rururb == '0',2,1))

# clean data
hh_data <- select(hhdata, id_hh, wgt, ea, rural)
head(hh_data)
write.csv(hh_data,'data_clean/hh.csv',row.names = F)
