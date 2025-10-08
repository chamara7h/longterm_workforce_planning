##Univariate methods

#Load libraries

library(fpp3)
library(tidyverse)


#Load data

nurse_yearly <- read_rds("data/wales_workforce_tidy_annual.rds")

nurse_monthly <- read_rds("data/wales_workforce_tidy_monthly.rds")

##I am using annual data without breaking into monthly basis as I only interested in univariate time series modelling. This way allows me to use data from 2009.

#create the tisbble

# nurse_df <- nurse_yearly %>% select(!organisation_name) %>%
#   
#   as_tsibble(index = year, key = organisation_code)


nurse_df <- nurse_monthly %>% select(!c("id", "grade", "organisation_name")) %>%
  
  as_tsibble(index = date, key = c("grade_code", "grade_hierarchy", "organisation_code"))

  
  #plot the timeseries

##selcting only N6 category as it has the consistent data pattern

nurse_df %>% filter(grade_hierarchy == "N6") %>%
  
  group_by(grade_hierarchy) %>%
  
  summarise(headcount = sum(headcount)) %>%
  
  autoplot(headcount, alpha = 2, level = 90, lwd = 1.0) +
  
  facet_wrap(vars(grade_hierarchy), scales = "free_y", ncol = 6)  +
  
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none")



#add organisational hierarchies. This allows me to prepare models for each LHB at the same time

nurse_hts <- nurse_df %>%
  
  filter(grade_hierarchy == "N6") %>%
  
  aggregate_key(organisation_code*grade_code, headcount = sum(headcount)) 


#evaluate the trends and seasonality 

nurse_hts %>%

  features(headcount, feat_stl) %>%

  ggplot(aes(x = trend_strength, y = seasonal_strength_year)) +

  geom_point(alpha = 1.5) +

  labs(x = "Strength of trend", y = "Strength of yearly seasonality")+

  ggthemes::theme_few()



##model fitting without considering timeseries cross validation

fit_all <- nurse_hts %>%
  
  model(naive = NAIVE(headcount),
        ETS = ETS(headcount),
        arima = ARIMA(headcount)) %>%
  
  reconcile(bottom_up_ETS = bottom_up(ETS),
            bottom_up_arima = bottom_up(arima))


#Forecasting for next 5 years

fc_all <- fit_all %>% forecast(h = 60)


#plotting forecasted series

fc_all %>%
  
  filter(is_aggregated(organisation_code), is_aggregated(grade_code)) %>%
  
  autoplot(nurse_hts,
           
           alpha = 2, level = 90, lwd = 1.0) +
  
  labs(y = "Headcount", x = "Year",
       title = "5 year forecasts for nurse demand for NHS") +
  
  facet_wrap(vars(grade_code), scales = "free_y", ncol = 6)  +
  
  theme(axis.text.x = element_text(angle = 90, hjust = 1))







