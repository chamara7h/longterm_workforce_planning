#generate future values of predictors - Eng

library(tidyverse)
library(tsibble)
library(fpp3)


#load final datset

nurse <- read_rds("data/eng_workforce_tidy.rds") 



nurse_tidy <- nurse %>% replace(is.na(.), 0) %>%
  
  select(!c(id, cluster_group, benchmark_group, staff_group, fte, fte_leavers, fte_joiners)) ## for the consistency, I ll only use headcount


options(scipen = 999) ## to disable scientific notation


## create the tsibble 

nurse_tsibble <- nurse_tidy %>% as_tsibble(index = date,
                                           key = c(org_code, nhse_region_code, ics_code))




########################### estimating future values of predictors ########################################


### predict future values for leavers using ETS


leavers_hts <- nurse_tsibble %>%  filter_index(. ~ "2022 Dec")


##generating future values for HC LEAVERS

fit_leavers <- leavers_hts %>% model(ETS = ETS(hc_leavers),
                                     arima = ARIMA(hc_leavers))

fc_leavers <- fit_leavers %>% forecast(h = 72)


#plotting forecasted series

fc_leavers %>%
  
  autoplot(leavers_hts,
           
           alpha = 2, level = 90, lwd = 1.0) +
  
  labs(y = "Headcount", x = "Year",
       title = "5 year forecasts for nurse leavers hc of NHS") +
  
  facet_wrap(vars(org_code), scales = "free_y", ncol = 6)  +
  
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

## by having a look at the graph, it seems ETS would be the suitable method

## now i am extracting forecasted point values for the leavers, so i can update my main df


leavers_pred <- fc_leavers %>% 
  
  filter(.model == "ETS") %>% 
  
  as_tibble %>%
  
  select(date, org_code, ics_code, nhse_region_code, .mean) %>%
  
  mutate(hc_leavers = .mean, id = paste0(date, org_code)) %>%
  
  select(id, hc_leavers)

## i am filling the future values for leavers

nurse_tidy <- nurse_tidy %>% mutate(id = paste0(date, org_code)) %>%
  
  rows_update(leavers_pred, by = "id")



### predict future values for joiners ###

joiners_hts <- nurse_tsibble %>%  filter_index(. ~ "2022 Dec")


fit_joiners <- joiners_hts %>% model(ETS = ETS(hc_joiners),
                                     arima = ARIMA(hc_joiners))

fc_joiners <- fit_joiners %>% forecast(h = 72)


#plotting forecasted series

fc_joiners %>%
  
  autoplot(joiners_hts,
           
           alpha = 2, level = 90, lwd = 1.0) +
  
  labs(y = "Headcount", x = "Year",
       title = "5 year forecasts for nurse joiners hc of NHS") +
  
  facet_wrap(vars(org_code), scales = "free_y", ncol = 6)  +
  
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

## by having a look at the graph, it seems ETS would be the suitable method

## now i am extracting forecasted point values for the joiners, so i can update my main df


joiners_pred <- fc_joiners %>% filter(!is_aggregated(org_code), !is_aggregated(nhse_region_code),
                                      !is_aggregated(ics_code)) %>%
  
  filter(.model == "ETS") %>% 
  
  as_tibble %>%
  
  select(date, org_code, ics_code, nhse_region_code, .mean) %>%
  
  mutate(hc_joiners = .mean, id = paste0(date, org_code)) %>%
  
  select(id, hc_joiners)

## i am filling the future values for leavers

## by exploring the future values of joiners, I notices some negative values due to negative trend. Thus, I am replacing those with 0.

nurse_tidy <- nurse_tidy %>% mutate(id = paste0(date, org_code)) %>%
  
  rows_update(joiners_pred, by = "id") %>%
  
  mutate(hc_joiners = case_when(hc_joiners < 0 ~ 0,
                              TRUE ~ hc_joiners))



### predict future values for absence_rate ###

absence_rate_hts <- nurse_tsibble %>%  filter_index(. ~ "2022 Dec")


fit_absence_rate <- absence_rate_hts %>% model(ETS = ETS(sickness_absence_rate_percent),
                                     arima = ARIMA(sickness_absence_rate_percent))

fc_absence_rate <- fit_absence_rate %>% forecast(h = 72)


#plotting forecasted series

fc_absence_rate %>%
  
  autoplot(absence_rate_hts,
           
           alpha = 2, level = 90, lwd = 1.0) +
  
  labs(y = "Headcount", x = "Year",
       title = "5 year forecasts for nurse absence rate hc of NHS") +
  
  facet_wrap(vars(org_code), scales = "free_y", ncol = 6)  +
  
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

## by having a look at the graph, it seems ARIMA would be the suitable method

## now i am extracting forecasted point values for the joiners, so i can update my main df


absence_rate_pred <- fc_absence_rate %>% filter(!is_aggregated(org_code), !is_aggregated(nhse_region_code),
                                      !is_aggregated(ics_code)) %>%
  
  filter(.model == "arima") %>% 
  
  as_tibble %>%
  
  select(date, org_code, ics_code, nhse_region_code, .mean) %>%
  
  mutate(sickness_absence_rate_percent = .mean, id = paste0(date, org_code)) %>%
  
  select(id, sickness_absence_rate_percent)

## i am filling the future values for leavers

nurse_tidy <- nurse_tidy %>% mutate(id = paste0(date, org_code)) %>%
  
  rows_update(absence_rate_pred, by = "id")



### predict future values for vacancy  ###

vacancy_hts <- nurse_tsibble %>%  filter_index(. ~ "2022 Dec")


fit_vacancy <- vacancy_hts %>% model(ETS = ETS(number_of_vacancies),
                                               arima = ARIMA(number_of_vacancies))

fc_vacancy <- fit_vacancy %>% forecast(h = 72)


#plotting forecasted series

fc_vacancy %>%
  
  autoplot(vacancy_hts,
           
           alpha = 2, level = 90, lwd = 1.0) +
  
  labs(y = "Headcount", x = "Year",
       title = "5 year forecasts for nurse vacancies hc of NHS") +
  
  facet_wrap(vars(org_code), scales = "free_y", ncol = 6)  +
  
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

## by having a look at the graph, it seems ETS would be the suitable method

## now i am extracting forecasted point values for the joiners, so i can update my main df


vacancy_pred <- fc_vacancy %>% 
  
  filter(.model == "ETS") %>% 
  
  as_tibble %>%
  
  select(date, org_code, ics_code, nhse_region_code, .mean) %>%
  
  mutate(number_of_vacancies = .mean, id = paste0(date, org_code)) %>%
  
  select(id, number_of_vacancies)

## i am filling the future values for leavers

nurse_tidy <- nurse_tidy %>% mutate(id = paste0(date, org_code)) %>%
  
  rows_update(vacancy_pred, by = "id")




### predict future values for local spend  ###

local_spend_hts <- nurse_tsibble %>%  filter_index(. ~ "2022 Dec") 


fit_local_spend <- local_spend_hts %>% model(ETS = ETS(local_spend_on_mental_health),
                                     arima = ARIMA(local_spend_on_mental_health))

fc_local_spend <- fit_local_spend %>% forecast(h = 72)


#plotting forecasted series

fc_local_spend %>%
  
  autoplot(local_spend_hts,
           
           alpha = 2, level = 90, lwd = 1.0) +
  
  labs(y = "GBP M", x = "Year",
       title = "5 year forecasts for local spend of NHS") +
  
  facet_wrap(vars(org_code), scales = "free_y", ncol = 6)  +
  
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

## by having a look at the graph, it seems ETS would be the suitable method

local_spend_pred <- fc_local_spend %>% 
  
  filter(.model == "ETS") %>% 
  
  as_tibble %>%
  
  select(date, org_code, ics_code, nhse_region_code, .mean) %>%
  
  mutate(local_spend_on_mental_health = .mean, id = paste0(date, org_code)) %>%
  
  select(id, local_spend_on_mental_health)

## i am filling the future values for leavers

nurse_tidy <- nurse_tidy %>% mutate(id = paste0(date, org_code)) %>%
  
  rows_update(local_spend_pred, by = "id")



### predict future values for commissioning spend  ###

commissioning_spend_hts <- nurse_tsibble %>%  filter_index(. ~ "2022 Dec") 
  



fit_commissioning_spend <- commissioning_spend_hts %>% model(ETS = ETS(nhse_specialised_commissioning_spend),
                                             arima = ARIMA(nhse_specialised_commissioning_spend))

fc_commissioning_spend <- fit_commissioning_spend %>% forecast(h = 72)


#plotting forecasted series

fc_commissioning_spend %>%
  
  autoplot(commissioning_spend_hts,
           
           alpha = 2, level = 90, lwd = 1.0) +
  
  labs(y = "GBP M", x = "Year",
       title = "5 year forecasts for commissioning_spend of NHS") +
  
  facet_wrap(vars(org_code), scales = "free_y", ncol = 6)  +
  
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

## by having a look at the graph, it seems ETS would be the suitable method

commissioning_spend_pred <- fc_local_spend %>% 
  
  filter(.model == "ETS") %>% 
  
  as_tibble %>%
  
  select(date, org_code, ics_code, nhse_region_code, .mean) %>%
  
  mutate(nhse_specialised_commissioning_spend = .mean, id = paste0(date, org_code)) %>%
  
  select(id, nhse_specialised_commissioning_spend)

## i am filling the future values for leavers

nurse_tidy <- nurse_tidy %>% mutate(id = paste0(date, org_code)) %>%
  
  rows_update(commissioning_spend_pred, by = "id")


## update total spend

nurse_tidy <- nurse_tidy %>% mutate(total_nhs_spend_on_mental_health = nhse_specialised_commissioning_spend + local_spend_on_mental_health)



### predict future values for ccg spend  ###

ccg_spend_hts <- nurse_tsibble %>%  filter_index(. ~ "2022 Dec") 


fit_ccg_spend <- ccg_spend_hts %>% model(ETS = ETS(ccg_spend_on_mh_as_a_percent_of_ccg_base_allocations),
                                                             arima = ARIMA(ccg_spend_on_mh_as_a_percent_of_ccg_base_allocations))

fc_ccg_spend <- fit_ccg_spend %>% forecast(h = 72)


#plotting forecasted series

fc_ccg_spend %>%
  
  autoplot(ccg_spend_hts,
           
           alpha = 2, level = 90, lwd = 1.0) +
  
  labs(y = "GBP M", x = "Year",
       title = "5 year forecasts for ccg_spend of NHS") +
  
  facet_wrap(vars(org_code), scales = "free_y", ncol = 6)  +
  
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


## by having a look at the graph, it seems ETS would be the suitable method

ccg_spend_pred <- fc_ccg_spend %>% 
  
  filter(.model == "ETS") %>% 
  
  as_tibble %>%
  
  select(date, org_code, ics_code, nhse_region_code, .mean) %>%
  
  mutate(ccg_spend_on_mh_as_a_percent_of_ccg_base_allocations = .mean, id = paste0(date, org_code)) %>%
  
  select(id, ccg_spend_on_mh_as_a_percent_of_ccg_base_allocations)


## i am filling the future values for leavers

nurse_tidy <- nurse_tidy %>% mutate(id = paste0(date, org_code)) %>%
  
  rows_update(ccg_spend_pred, by = "id")




### predict future values for nursing graduates  ###

graduates_hts <- nurse_tsibble %>%  filter_index(. ~ "2022 Dec") 


fit_graduates <- graduates_hts %>% model(ETS = ETS(garduates_per_1000_inhabitants),
                                         arima = ARIMA(garduates_per_1000_inhabitants))

fc_graduates <- fit_graduates %>% forecast(h = 72)


#plotting forecasted series

fc_graduates %>%
  
  autoplot(graduates_hts,
           
           alpha = 2, level = 90, lwd = 1.0) +
  
  labs(y = "per 1000 people", x = "Year",
       title = "5 year forecasts for graduates of NHS") +
  
  facet_wrap(vars(org_code), scales = "free_y", ncol = 6)  +
  
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


## by having a look at the graph, it seems arima would be the suitable method

graduates_pred <- fc_graduates %>% 
  
  filter(.model == "arima") %>% 
  
  as_tibble %>%
  
  select(date, org_code, ics_code, nhse_region_code, .mean) %>%
  
  mutate(garduates_per_1000_inhabitants = .mean, id = paste0(date, org_code)) %>%
  
  select(id, garduates_per_1000_inhabitants)

## i am filling the future values for graduates

nurse_tidy <- nurse_tidy %>% mutate(id = paste0(date, org_code)) %>%
  
  rows_update(graduates_pred, by = "id")



### predict future values for MHS32  ###

mhs32_hts <- nurse_tsibble %>%  filter_index(. ~ "2022 Dec") 


fit_mhs32 <- mhs32_hts %>% model(ETS = ETS(MHS32),
                                         arima = ARIMA(MHS32))

fc_mhs32 <- fit_mhs32 %>% forecast(h = 72)


#plotting forecasted series

fc_mhs32 %>%
  
  autoplot(mhs32_hts,
           
           alpha = 2, level = 90, lwd = 1.0) +
  
  labs(y = "Number of People", x = "Year",
       title = "5 year forecasts for MHS32") +
  
  facet_wrap(vars(org_code), scales = "free_y", ncol = 6)  +
  
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


## by having a look at the graph, it seems ETS would be the suitable method

mhs32_pred <- fc_mhs32 %>% 
  
  filter(.model == "ETS") %>% 
  
  as_tibble %>%
  
  select(date, org_code, ics_code, nhse_region_code, .mean) %>%
  
  mutate(MHS32 = .mean, id = paste0(date, org_code)) %>%
  
  select(id, MHS32)

## i am filling the future values for mhs32

nurse_tidy <- nurse_tidy %>% mutate(id = paste0(date, org_code)) %>%
  
  rows_update(mhs32_pred, by = "id")




### predict future values for MHS01  ###

mhs01_hts <- nurse_tsibble %>%  filter_index(. ~ "2022 Dec") 


fit_mhs01 <- mhs01_hts %>% model(ETS = ETS(MHS01),
                                 arima = ARIMA(MHS01))

fc_mhs01 <- fit_mhs01 %>% forecast(h = 72)


#plotting forecasted series

fc_mhs01 %>%
  
  autoplot(mhs01_hts,
           
           alpha = 2, level = 90, lwd = 1.0) +
  
  labs(y = "Number of People", x = "Year",
       title = "5 year forecasts for MHS01") +
  
  facet_wrap(vars(org_code), scales = "free_y", ncol = 6)  +
  
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


## by having a look at the graph, it seems ARIMA would be the suitable method

mhs01_pred <- fc_mhs01 %>% 
  
  filter(.model == "arima") %>% 
  
  as_tibble %>%
  
  select(date, org_code, ics_code, nhse_region_code, .mean) %>%
  
  mutate(MHS01 = .mean, id = paste0(date, org_code)) %>%
  
  select(id, MHS01)

## i am filling the future values for mhs01

nurse_tidy <- nurse_tidy %>% mutate(id = paste0(date, org_code)) %>%
  
  rows_update(mhs01_pred, by = "id")



### predict future values for MHS07  ###

mhs07_hts <- nurse_tsibble %>%  filter_index(. ~ "2022 Dec") 


fit_mhs07 <- mhs07_hts %>% model(ETS = ETS(MHS07),
                                 arima = ARIMA(MHS07))

fc_mhs07 <- fit_mhs07 %>% forecast(h = 72)


#plotting forecasted series

fc_mhs07 %>%
  
  autoplot(mhs07_hts,
           
           alpha = 2, level = 90, lwd = 1.0) +
  
  labs(y = "Number of People", x = "Year",
       title = "5 year forecasts for MHS07") +
  
  facet_wrap(vars(org_code), scales = "free_y", ncol = 6)  +
  
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


## by having a look at the graph, it seems ARIMA would be the suitable method

mhs07_pred <- fc_mhs07 %>%
  
  filter(.model == "arima") %>% 
  
  as_tibble %>%
  
  select(date, org_code, ics_code, nhse_region_code, .mean) %>%
  
  mutate(MHS07 = .mean, id = paste0(date, org_code)) %>%
  
  select(id, MHS07)

## i am filling the future values for mhs07

nurse_tidy <- nurse_tidy %>% mutate(id = paste0(date, org_code)) %>%
  
  rows_update(mhs07_pred, by = "id")




### predict future values for MHS29  ###

mhs29_hts <- nurse_tsibble %>%  filter_index(. ~ "2022 Dec") 


fit_mhs29 <- mhs29_hts %>% model(ETS = ETS(MHS29),
                                 arima = ARIMA(MHS29))

fc_mhs29 <- fit_mhs29 %>% forecast(h = 72)


#plotting forecasted series

fc_mhs29 %>%
  
  autoplot(mhs29_hts,
           
           alpha = 2, level = 90, lwd = 1.0) +
  
  labs(y = "Number of People", x = "Year",
       title = "5 year forecasts for MHS29") +
  
  facet_wrap(vars(org_code), scales = "free_y", ncol = 6)  +
  
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


## by having a look at the graph, it seems ETS would be the suitable method

mhs29_pred <- fc_mhs29 %>% 
  
  filter(.model == "ETS") %>% 
  
  as_tibble %>%
  
  select(date, org_code, ics_code, nhse_region_code, .mean) %>%
  
  mutate(MHS29 = .mean, id = paste0(date, org_code)) %>%
  
  select(id, MHS29)

## i am filling the future values for mhs29

nurse_tidy <- nurse_tidy %>% mutate(id = paste0(date, org_code)) %>%
  
  rows_update(mhs29_pred, by = "id")




### predict future values for ucas acceptance rate  ###

ucas_hts <- nurse_tsibble %>%  filter_index(. ~ "2020 Dec")


fit_ucas <- ucas_hts %>% model(ETS = ETS(ucas_acceptance_rate),
                                 arima = ARIMA(ucas_acceptance_rate))

fc_ucas <- fit_ucas %>% forecast(h = 96)


#plotting forecasted series

fc_ucas %>%
  
  autoplot(ucas_hts,
           
           alpha = 2, level = 90, lwd = 1.0) +
  
  labs(y = "Percentage change", x = "Year",
       title = "5 year forecasts for ucas_acceptance_rate") +
  
  facet_wrap(vars(org_code), scales = "free_y", ncol = 6)  +
  
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


## by having a look at the graph, it seems ETS would be the suitable method

ucas_pred <- fc_ucas %>% filter(!is_aggregated(org_code), !is_aggregated(nhse_region_code),
                                  !is_aggregated(ics_code)) %>%
  
  filter(.model == "ETS") %>% 
  
  as_tibble %>%
  
  select(date, org_code, ics_code, nhse_region_code, .mean) %>%
  
  mutate(ucas_acceptance_rate = .mean, id = paste0(date, org_code)) %>%
  
  select(id, ucas_acceptance_rate)

## i am filling the future values for ucas_acceptance_rate

nurse_tidy <- nurse_tidy %>% mutate(id = paste0(date, org_code)) %>%
  
  rows_update(ucas_pred, by = "id")





### predict future values for SFTN rate  ###

SFTN_hts <- nurse_tsibble %>%  filter_index(. ~ "2022 Dec")


fit_SFTN <- SFTN_hts %>% model(ETS = ETS(SFTN),
                               arima = ARIMA(SFTN))

fc_SFTN <- fit_SFTN %>% forecast(h = 72)


#plotting forecasted series

fc_SFTN %>%
  
  autoplot(SFTN_hts,
           
           alpha = 2, level = 90, lwd = 1.0) +
  
  labs(y = "Percentage change", x = "Year",
       title = "5 year forecasts for SFTN_acceptance_rate") +
  
  facet_wrap(vars(org_code), scales = "free_y", ncol = 6)  +
  
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


## by having a look at the graph, it seems ETS would be the suitable method

SFTN_pred <- fc_SFTN %>% 
  
  filter(.model == "ETS") %>% 
  
  as_tibble %>%
  
  select(date, org_code, ics_code, nhse_region_code, .mean) %>%
  
  mutate(SFTN = .mean, id = paste0(date, org_code)) %>%
  
  select(id, SFTN)

## i am filling the future values for SFTN_acceptance_rate

nurse_tidy <- nurse_tidy %>% mutate(id = paste0(date, org_code)) %>%
  
  rows_update(SFTN_pred, by = "id")



### predict future values for IFTN rate  ###

IFTN_hts <- nurse_tsibble %>%  filter_index(. ~ "2022 Dec") 


fit_IFTN <- IFTN_hts %>% model(ETS = ETS(IFTN),
                               arima = ARIMA(IFTN))

fc_IFTN <- fit_IFTN %>% forecast(h = 72)


#plotting forecasted series

fc_IFTN %>%
  
  autoplot(IFTN_hts,
           
           alpha = 2, level = 90, lwd = 1.0) +
  
  labs(y = "Percentage change", x = "Year",
       title = "5 year forecasts for IFTN") +
  
  facet_wrap(vars(org_code), scales = "free_y", ncol = 6)  +
  
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


## by having a look at the graph, it seems ARIMA would be the suitable method

IFTN_pred <- fc_IFTN %>% 
  
  filter(.model == "arima") %>% 
  
  as_tibble %>%
  
  select(date, org_code, ics_code, nhse_region_code, .mean) %>%
  
  mutate(IFTN = .mean, id = paste0(date, org_code)) %>%
  
  select(id, IFTN)

## i am filling the future values for IFTN_acceptance_rate

nurse_tidy <- nurse_tidy %>% mutate(id = paste0(date, org_code)) %>%
  
  rows_update(IFTN_pred, by = "id")





### predict future values for international joiners  ###

international_joiners_hts <- nurse_tsibble %>%  filter_index(. ~ "2022 Mar")

fit_international_joiners <- international_joiners_hts %>% model(ETS = ETS(international_joiners_mental_health),
                               arima = ARIMA(international_joiners_mental_health))

fc_international_joiners <- fit_international_joiners %>% forecast(h = 81)


#plotting forecasted series

fc_international_joiners %>%
  
  autoplot(international_joiners_hts,
           
           alpha = 2, level = 90, lwd = 1.0) +
  
  labs(y = "Number", x = "Year",
       title = "5 year forecasts for international_joiners_mental_health") +
  
  facet_wrap(vars(org_code), scales = "free_y", ncol = 6)  +
  
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


## by having a look at the graph, it seems ARIMA would be the suitable method

international_joiners_pred <- fc_international_joiners %>% 
  
  filter(.model == "arima") %>% 
  
  as_tibble %>%
  
  select(date, org_code, ics_code, nhse_region_code, .mean) %>%
  
  mutate(international_joiners_mental_health = .mean, id = paste0(date, org_code)) %>%
  
  select(id, international_joiners_mental_health) %>%
  
  mutate(international_joiners_mental_health = as.integer(international_joiners_mental_health))


## i am filling the future values for international_joiners

nurse_tidy <- nurse_tidy %>% mutate(id = paste0(date, org_code)) %>%
  
  rows_update(international_joiners_pred, by = "id")
  
  

########################### adding lag for future values #######################

## add year and month as time fetures


nurse_tidy <- nurse_tidy %>% mutate(year = year(date), month = month(date)) %>%
  
  select(date, year, month, !id, everything()) %>%
  
  select(!id)


# creating lagas

nurse_lag <- nurse_tidy %>% 
  
  group_by(org_code) %>%
  
  mutate(hc_lag1 = lag(hc, n = 1, default = 0),
         hc_lag2 = lag(hc, n = 2, default = 0),
         hc_lag3 = lag(hc, n = 3, default = 0),
         hc_lag4 = lag(hc, n = 4, default = 0),
         
         hc_leavers_lag1 = lag(hc_leavers, n = 1, default = 0),
         hc_leavers_lag2 = lag(hc_leavers, n = 2, default = 0),
         hc_leavers_lag3 = lag(hc_leavers, n = 3, default = 0),
         hc_leavers_lag4 = lag(hc_leavers, n = 4, default = 0),
         
         hc_joiners_lag1 = lag(hc_joiners, n = 1, default = 0),
         hc_joiners_lag2 = lag(hc_joiners, n = 2, default = 0),
         hc_joiners_lag3 = lag(hc_joiners, n = 3, default = 0),
         hc_joiners_lag4 = lag(hc_joiners, n = 4, default = 0),
         
         number_of_vacancies_lag1 = lag(number_of_vacancies, n = 1, default = 0),
         
         # number_of_vacancies_lag1 = number_of_vacancies_lag1/3,
         
         graduates_per_1000_inhabitants_lag1 = lag(garduates_per_1000_inhabitants, n = 1, default = 0),
         
         # graduates_per_1000_inhabitants_lag1 = graduates_per_1000_inhabitants_lag1/12,
         
         MHS32_lag1 = lag(MHS32, n = 1, default = 0),
         MHS32_lag2 = lag(MHS32, n = 2, default = 0),
         MHS32_lag3 = lag(MHS32, n = 3, default = 0),
         # MHS32_lag4 = lag(MHS32, n = 4, default = 0),
         
         MHS01_lag1 = lag(MHS01, n = 1, default = 0),
         MHS01_lag2 = lag(MHS01, n = 2, default = 0),
         MHS01_lag3 = lag(MHS01, n = 3, default = 0),
         # MHS01_lag4 = lag(MHS01, n = 4, default = 0), 
         
         MHS07_lag1 = lag(MHS07, n = 1, default = 0),
         MHS07_lag2 = lag(MHS07, n = 2, default = 0),
         MHS07_lag3 = lag(MHS07, n = 3, default = 0),
         # MHS07_lag4 = lag(MHS07, n = 4, default = 0),
         
         MHS29_lag1 = lag(MHS29, n = 1, default = 0),
         MHS29_lag2 = lag(MHS29, n = 2, default = 0),
         MHS29_lag3 = lag(MHS29, n = 3, default = 0),
         # MHS29_lag4 = lag(MHS29, n = 4, default = 0),
         
         ucas_acceptance_lag1 = lag(ucas_acceptance_rate, n = 12, default = 0),
         ucas_acceptance_lag2 = lag(ucas_acceptance_rate, n = 24, default = 0),
         ucas_acceptance_lag3 = lag(ucas_acceptance_rate, n = 36, default = 0),
         ucas_acceptance_lag4 = lag(ucas_acceptance_rate, n = 48, default = 0)) %>%
  
  ungroup()



## join the future lag values to the nurse tidy (2018 - 2023) since new nurse lag omit the previous lag values

nurse1 <- nurse_tidy %>% filter(year < 2023)

nurse2 <- nurse_lag %>% filter(year > 2022)

nurse_master <- rbind(nurse1, nurse2)


## update: In exploratory analysis, we found that hc lag 1 is the significant lag for hc. Thus, we decide to use ETS to interpolate future values of hc LAG 1



### predict future values for hc_lag1  ###

hc_lag_hts <- nurse_master %>%  as_tsibble(index = date,
                                           key = c(org_code, nhse_region_code, ics_code)) %>%
  
  filter_index(. ~ "2023 Apr")

fit_hc_lag <- hc_lag_hts %>% model(ETS = ETS(hc_lag1))

fc_hc_lag <- fit_hc_lag %>% forecast(h = 68)



hc_lag_pred <- fc_hc_lag %>% 
  
  as_tibble %>%
  
  select(date, org_code, ics_code, nhse_region_code, .mean) %>%
  
  mutate(hc_lag1 = .mean, id = paste0(date, org_code)) %>%
  
  select(id, hc_lag1)


## i am filling the future values for international_joiners

nurse_master <- nurse_master %>% mutate(id = paste0(date, org_code)) %>%
  
  rows_update(hc_lag_pred, by = "id")
   

# ## add covid period (we define 2020 and 2021 as the covid period)
# 
# 
# a <- nurse_master %>% mutate(covid = 0,
# 
#                              covid = case_when(year %in% 2020:2021 ~ 1,
# 
#                                                TRUE ~ covid))



## create time since variable to map trend features

nurse_master <- nurse_master %>% mutate(time_window = date - min(date))


## add full date column and one hot coding

nurse_master <- nurse_master %>% mutate(date = as.Date(date))

#convert categorical variables to numerical variables

# org <- model.matrix(~ org_code -1, nurse_master) ## # convert org code into one-hot encoding
# 
# region <- model.matrix(~ nhse_region_code -1, nurse_master) ## # convert region code into one-hot encoding
# 
# ics <- model.matrix(~ ics_code -1, nurse_master) ## # convert ics code into one-hot encoding



#prepare one dataframe and convert it to matrix

# nurse_master <- cbind(nurse_master, org, ics, region) ## for ml models

write_rds(nurse_master, "data/eng_workforce_master.rds")  ## complete dataset

write_csv(nurse_master, "data/eng_workforce_master.csv")  ## complete dataset
