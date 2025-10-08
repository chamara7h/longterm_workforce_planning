## Univariate models - Eng

#Load libraries

library(fpp3)
library(tidyverse)

#Load data

nurse_df <- read_rds("data/eng_workforce_master.rds")



## create the tsibble 

nurse_tsibble <- nurse_df %>% as_tsibble(index = date,
                                           key = c(org_code, nhse_region_code, ics_code))



#add organisational hierarchies to create models for each hierarchy

nurse_hts <- nurse_tsibble %>%
  
  aggregate_key((org_code/nhse_region_code), headcount = sum(hc)) 



##model fitting without considering timeseries cross validation

fit_all <- nurse_hts %>% filter_index(. ~ "2022 Dec") %>%
  
  model(naive = NAIVE(headcount),
        ETS = ETS(headcount),
        arima = ARIMA(headcount)) %>%
  
  reconcile(
    bottom_up_naive = bottom_up(naive),
    ols_naive = min_trace(naive, method = "ols"),
    MinT_base_naive = min_trace(naive, method = "mint_shrink"),
    
    bottom_up_ETS = bottom_up(ETS),
    ols_ETS = min_trace(arima, method = "ols"),
    MinT_base_ETS = min_trace(arima, method = "mint_shrink"),
    
    bottom_up_arima = bottom_up(arima),
    ols_arima = min_trace(arima, method = "ols"),
    MinT_base_arima = min_trace(arima, method = "mint_shrink")
  )

write_rds(fit_all, "results/univariate_models_eng.rds")



#Forecasting for next 5 years

fc_all <- fit_all %>% forecast(h = 72)

write_rds(fc_all, "results/univariate_forecasts_eng.rds")





#plotting forecasted series

##Overall level

fc_all %>%
  
  filter(is_aggregated(org_code), is_aggregated(nhse_region_code)) %>%
  
  autoplot(nurse_hts %>% filter_index(. ~ "2022 Dec"),
           
           alpha = 2, level = 90, lwd = 1.0) +
  
  labs(y = "Headcount", x = "Year",
       title = "6 year forecasts for nurse demand for NHS") +
  
    theme(axis.text.x = element_text(angle = 90, hjust = 1))


##regional level

fc_all %>%
  
  filter(.model %in% c("naive", 
                       "arima", "bottom_up_arima", "ols_arima", "MinT_base_arima",
                       "ETS", "bottom_up_ETS", "ols_ETS", "MinT_base_ETS"),
         
         !is_aggregated(org_code), is_aggregated(nhse_region_code)) %>%
  
  autoplot(nurse_hts %>% filter_index(. ~ "2022 Dec"),
           
           alpha = 2, level = 90, lwd = 1.0) +
  
  labs(y = "Headcount", x = "Year",
       title = "6 year forecasts for nurse demand for NHS") +
  
  facet_wrap(vars(org_code), scales = "free_y", ncol = 6)  +
  
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



##Org level

fc_all %>%
  
  filter(.model %in% c("naive", 
                       "arima", "bottom_up_arima", "ols_arima", "MinT_base_arima",
                       "ETS", "bottom_up_ETS", "ols_ETS", "MinT_base_ETS"),
         
         !is_aggregated(org_code), !is_aggregated(nhse_region_code),
         !is_aggregated(ics_code)) %>%
  
  autoplot(nurse_hts %>% filter_index(. ~ "2022 Dec"),
           
           alpha = 2, level = 90, lwd = 1.0) +
  
  labs(y = "Headcount", x = "Year",
       title = "6 year forecasts for nurse demand for NHS") +
  
  facet_wrap(vars(org_code), scales = "free_y", ncol = 6)  +
  
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



# adding forecasted values to the original dataframe

nurse_tidy <- nurse_tsibble %>%
  
  as_tibble() %>%
  
  mutate(id = paste0(date, org_code))



## by having a look at the graph, it seems ETS would be the suitable method

nurse_pred <- fc_all %>% filter(!is_aggregated(org_code), !is_aggregated(nhse_region_code),
                                      !is_aggregated(ics_code)) %>%
  
  filter(.model == "ETS") %>% 
  
  as_tibble %>%
  
  select(date, org_code, ics_code, nhse_region_code, .mean) %>%
  
  mutate(hc = .mean, id = paste0(date, org_code)) %>%
  
  select(id, hc)



## i am filling the future values for leavers

nurse_tidy <- nurse_tidy %>% mutate(id = paste0(date, org_code)) %>%
  
  rows_update(nurse_pred, by = "id")



## save final dataset

write_rds(nurse_tidy, "results/univariate_forecasts_eng_completed.rds")


