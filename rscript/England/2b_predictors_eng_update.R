#### Updated #####


## I am replacing predictors values generated using ARIMA/ ETS using LR model predictions. LR provide more reasonable predictions and prediction intervals


## generate future values of predictors - Eng 

library(tidyverse)
library(tsibble)
library(fpp3)


## load final datset

nurse <- read_rds("data/eng_workforce_master.rds")

pred <- read.csv("data/eng_predictors.csv")


## genarate unique ids

pred <- pred %>% mutate(ds = yearmonth(ds), id = paste0(ds, unique_id)) %>%
  
  select(-ds, -unique_id)

nurse <- nurse %>% mutate(id = paste0(yearmonth(date), org_code),
                
                MHS32_high = MHS32,
                MHS32_low = MHS32,
                
                MHS32_high_lag1 = MHS32_lag1,
                MHS32_high_lag2 = MHS32_lag2,
                MHS32_high_lag3 = MHS32_lag3,
                # MHS32_high_lag4 = MHS32_lag4,
                
                MHS32_low_lag1 = MHS32_lag1,
                MHS32_low_lag2 = MHS32_lag2,
                MHS32_low_lag3 = MHS32_lag3,
                # MHS32_low_lag4 = MHS32_lag4,
                
                MHS01_high = MHS01,
                MHS01_low = MHS01,
                
                MHS01_high_lag1 = MHS01_lag1,
                MHS01_high_lag2 = MHS01_lag2,
                MHS01_high_lag3 = MHS01_lag3,
                # MHS01_high_lag4 = MHS01_lag4,
                
                MHS01_low_lag1 = MHS01_lag1,
                MHS01_low_lag2 = MHS01_lag2,
                MHS01_low_lag3 = MHS01_lag3,
                # MHS01_low_lag4 = MHS01_lag4,
                
                MHS07_high = MHS07,
                MHS07_low = MHS07,
                
                MHS07_high_lag1 = MHS07_lag1,
                MHS07_high_lag2 = MHS07_lag2,
                MHS07_high_lag3 = MHS07_lag3,
                # MHS07_high_lag4 = MHS07_lag4,
                
                MHS07_low_lag1 = MHS07_lag1,
                MHS07_low_lag2 = MHS07_lag2,
                MHS07_low_lag3 = MHS07_lag3,
                # MHS07_low_lag4 = MHS07_lag4,
                
                MHS29_high = MHS29,
                MHS29_low = MHS29,
                
                MHS29_high_lag1 = MHS29_lag1,
                MHS29_high_lag2 = MHS29_lag2,
                MHS29_high_lag3 = MHS29_lag3,
                # MHS29_high_lag4 = MHS29_lag4,
                
                MHS29_low_lag1 = MHS29_lag1,
                MHS29_low_lag2 = MHS29_lag2,
                MHS29_low_lag3 = MHS29_lag3,
                # MHS29_low_lag4 = MHS29_lag4
                ) 



## update future values
  
nurse_master <- nurse %>% rows_update(pred, by = "id")


## update lags values based on the new values

########################### adding lag for future values #######################

## add year and month as time features


nurse_master <- nurse_master %>% select(-id, -hc_lag2, -hc_lag3, -hc_lag4)  ## ML framework has the capacility to create lags of the target value


# creating lagas

nurse_lag <- nurse_master %>% 
  
  group_by(org_code) %>%
  
  mutate(hc_leavers_lag1 = lag(hc_leavers, n = 1, default = 0),
         hc_leavers_lag2 = lag(hc_leavers, n = 2, default = 0),
         hc_leavers_lag3 = lag(hc_leavers, n = 3, default = 0),
         hc_leavers_lag4 = lag(hc_leavers, n = 4, default = 0),
         
         hc_joiners_lag1 = lag(hc_joiners, n = 1, default = 0),
         hc_joiners_lag2 = lag(hc_joiners, n = 2, default = 0),
         hc_joiners_lag3 = lag(hc_joiners, n = 3, default = 0),
         hc_joiners_lag4 = lag(hc_joiners, n = 4, default = 0),
         
         number_of_vacancies_lag1 = lag(number_of_vacancies, n = 1, default = 0),
         
         graduates_per_1000_inhabitants_lag1 = lag(garduates_per_1000_inhabitants, n = 1, default = 0),
         
         MHS32_lag1 = lag(MHS32, n = 1, default = 0),
         MHS32_lag2 = lag(MHS32, n = 2, default = 0),
         MHS32_lag3 = lag(MHS32, n = 3, default = 0),
         # MHS32_lag4 = lag(MHS32, n = 4, default = 0),
         
         MHS32_high_lag1 = lag(MHS32_high, n = 1, default = 0),
         MHS32_high_lag2 = lag(MHS32_high, n = 2, default = 0),
         MHS32_high_lag3 = lag(MHS32_high, n = 3, default = 0),
         # MHS32_high_lag4 = lag(MHS32_high, n = 4, default = 0),
         
         MHS32_low_lag1 = lag(MHS32_low, n = 1, default = 0),
         MHS32_low_lag2 = lag(MHS32_low, n = 2, default = 0),
         MHS32_low_lag3 = lag(MHS32_low, n = 3, default = 0),
         # MHS32_low_lag4 = lag(MHS32_low, n = 4, default = 0),
         
         MHS01_lag1 = lag(MHS01, n = 1, default = 0),
         MHS01_lag2 = lag(MHS01, n = 2, default = 0),
         MHS01_lag3 = lag(MHS01, n = 3, default = 0),
         # MHS01_lag4 = lag(MHS01, n = 4, default = 0), 
         
         MHS01_high_lag1 = lag(MHS01_high, n = 1, default = 0),
         MHS01_high_lag2 = lag(MHS01_high, n = 2, default = 0),
         MHS01_high_lag3 = lag(MHS01_high, n = 3, default = 0),
         # MHS01_high_lag4 = lag(MHS01_high, n = 4, default = 0), 
         
         MHS01_low_lag1 = lag(MHS01_low, n = 1, default = 0),
         MHS01_low_lag2 = lag(MHS01_low, n = 2, default = 0),
         MHS01_low_lag3 = lag(MHS01_low, n = 3, default = 0),
         # MHS01_low_lag4 = lag(MHS01_low, n = 4, default = 0), 
         
         MHS07_lag1 = lag(MHS07, n = 1, default = 0),
         MHS07_lag2 = lag(MHS07, n = 2, default = 0),
         MHS07_lag3 = lag(MHS07, n = 3, default = 0),
         # MHS07_lag4 = lag(MHS07, n = 4, default = 0),
         
         MHS07_high_lag1 = lag(MHS07_high, n = 1, default = 0),
         MHS07_high_lag2 = lag(MHS07_high, n = 2, default = 0),
         MHS07_high_lag3 = lag(MHS07_high, n = 3, default = 0),
         # MHS07_high_lag4 = lag(MHS07_high, n = 4, default = 0),
         
         MHS07_low_lag1 = lag(MHS07_low, n = 1, default = 0),
         MHS07_low_lag2 = lag(MHS07_low, n = 2, default = 0),
         MHS07_low_lag3 = lag(MHS07_low, n = 3, default = 0),
         # MHS07_low_lag4 = lag(MHS07_low, n = 4, default = 0),
         
         MHS29_lag1 = lag(MHS29, n = 1, default = 0),
         MHS29_lag2 = lag(MHS29, n = 2, default = 0),
         MHS29_lag3 = lag(MHS29, n = 3, default = 0),
         # MHS29_lag4 = lag(MHS29, n = 4, default = 0),
         
         MHS29_high_lag1 = lag(MHS29_high, n = 1, default = 0),
         MHS29_high_lag2 = lag(MHS29_high, n = 2, default = 0),
         MHS29_high_lag3 = lag(MHS29_high, n = 3, default = 0),
         # MHS29_high_lag4 = lag(MHS29_high, n = 4, default = 0),
         
         MHS29_low_lag1 = lag(MHS29_low, n = 1, default = 0),
         MHS29_low_lag2 = lag(MHS29_low, n = 2, default = 0),
         MHS29_low_lag3 = lag(MHS29_low, n = 3, default = 0),
         # MHS29_low_lag4 = lag(MHS29_low, n = 4, default = 0),
         
         ucas_acceptance_lag1 = lag(ucas_acceptance_rate, n = 12, default = 0),
         ucas_acceptance_lag2 = lag(ucas_acceptance_rate, n = 24, default = 0),
         ucas_acceptance_lag3 = lag(ucas_acceptance_rate, n = 36, default = 0),
         ucas_acceptance_lag4 = lag(ucas_acceptance_rate, n = 48, default = 0)) %>%
  
  ungroup()



## join the future lag values to the nurse tidy (2018 - 2023) since new nurse lag omit the previous lag values

nurse1 <- nurse_master %>% filter(year < 2023)

nurse2 <- nurse_lag %>% filter(year > 2022)

nurse_master <- rbind(nurse1, nurse2)


## add full date column

nurse_master <- nurse_master %>% mutate(date = as.Date(date))

write_rds(nurse_master, "data/eng_workforce_master_updated.rds")  ## complete dataset

write_csv(nurse_master, "data/eng_workforce_master_updated.csv")  ## complete dataset
