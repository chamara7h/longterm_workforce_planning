## sensitivity analysis charts

## result analysis

library(tidyverse)
library(tsibble)
library(ggthemes)
library(ggplot2)
library(rstatix)


## load data 

pred <- read_rds("results/nurse_pred_master_eng.rds")

nurse_history <- read_rds("data/eng_workforce_master_updated.rds")

pred_tsibble <- pred %>% mutate(date = yearmonth(date)) %>%
  
  as_tsibble(index = date, key = c("org_code", "org_name", "nhse_region_code", "nhse_region_name", "model"))

sd <- read.csv("results/scenario_analysis_eng.csv")


##################### Create annual, quaterly  and monthly data for base case ###################

### annual

pred_annual <- pred_tsibble %>% as_tibble() %>% mutate(year = year(date)) %>% filter(model %in% c("lgbm", "snaive", "arima",  "ets", "xgbm")) %>%
  
  group_by(year, org_code, org_name, nhse_region_code, nhse_region_name, model) %>% select(-date) %>%
  
  summarise(across(everything(), mean), .groups = "drop")




### quarterly

pred_quarter <- pred_tsibble %>% as_tibble() %>% mutate(quarter = yearquarter(date)) %>% filter(model %in% c("lgbm", "snaive", "arima",  "ets", "xgbm")) %>%
  
  group_by(quarter, org_code, org_name, nhse_region_code, nhse_region_name, model) %>% select(-date) %>%
  
  summarise(across(everything(), mean), .groups = "drop")


### monthly

pred_monthly <- pred_tsibble %>% as_tibble() %>% filter(model %in% c("lgbm", "snaive", "arima",  "ets", "xgbm"))



## AGREGRATE 

### yearly

pred_annual_dist_total <- pred_annual %>%
  
  select(-org_code, -org_name, -nhse_region_code) %>%
  
  group_by(year, nhse_region_name, model) %>%
  
  summarise(across(everything(), sum), .groups = 'drop') %>% select(-model) %>%
  
  group_by(year, nhse_region_name) %>%
  
  summarise(across(everything(), mean), .groups = 'drop') %>% select(-nhse_region_name) %>%
  
  group_by(year) %>%
  
  summarise(across(everything(), sum), .groups = 'drop') %>%
  
  pivot_longer(cols = -c(year), names_to = "variable", values_to = "forecast") %>%
  
  mutate(scenario = "base case")



### quarter

pred_quarter_dist_total <- pred_quarter %>%
  
  select(-org_code, -org_name, -nhse_region_code) %>%
  
  group_by(quarter, nhse_region_name, model) %>%
  
  summarise(across(everything(), sum), .groups = 'drop') %>% select(-model) %>%
  
  group_by(quarter, nhse_region_name) %>%
  
  summarise(across(everything(), mean), .groups = 'drop') %>% select(-nhse_region_name) %>%
  
  group_by(quarter) %>%
  
  summarise(across(everything(), sum), .groups = 'drop') %>%
  
  pivot_longer(cols = -c(quarter), names_to = "variable", values_to = "forecast") %>%
  
  mutate(scenario = "base case")



### monthly

pred_monthly_dist_total <- pred_monthly %>%
  
  select(-org_name, -nhse_region_name, -nhse_region_code, -model) %>%
  
  group_by(date, org_code) %>%
  
  summarise(across(everything(), mean), .groups = 'drop') %>% select(-org_code) %>%
  
  group_by(date) %>%
  
  summarise(across(everything(), sum), .groups = 'drop') %>%
  
  pivot_longer(cols = -c(date), names_to = "variable", values_to = "forecast") %>%
  
  mutate(scenario = "base case")



## REGION

### yearly

pred_annual_dist_region <- pred_annual %>%
  
  select(-org_code, -org_name, -nhse_region_code) %>%
  
  group_by(year, nhse_region_name, model) %>%
  
  summarise(across(everything(), sum), .groups = 'drop') %>% select(-model) %>%
  
  group_by(year, nhse_region_name) %>%
  
  summarise(across(everything(), mean), .groups = 'drop') %>%
  
  pivot_longer(cols = -c(year, nhse_region_name), names_to = "variable", values_to = "forecast") %>%
  
  mutate(scenario = "base case")



### quarter

pred_quarter_dist_region <- pred_quarter %>%
  
  select(-org_code, -org_name, -nhse_region_code) %>%
  
  group_by(quarter, nhse_region_name, model) %>%
  
  summarise(across(everything(), sum), .groups = 'drop') %>% select(-model) %>%
  
  group_by(quarter, nhse_region_name) %>%
  
  summarise(across(everything(), mean), .groups = 'drop') %>%
  
  pivot_longer(cols = -c(quarter, nhse_region_name), names_to = "variable", values_to = "forecast") %>%
  
  mutate(scenario = "base case")



### monthly

pred_monthly_dist_region <- pred_monthly %>%
  
  select(-org_code, -org_name, -nhse_region_code) %>%
  
  group_by(date, nhse_region_name, model) %>%
  
  summarise(across(everything(), sum), .groups = 'drop') %>% select(-model) %>%
  
  group_by(date, nhse_region_name) %>%
  
  summarise(across(everything(), mean), .groups = 'drop') %>%
  
  pivot_longer(cols = -c(date, nhse_region_name), names_to = "variable", values_to = "forecast") %>%
  
  mutate(scenario = "base case")


## org level


### create box plots for randomly selected org levels

orgs = c("RDY", "RVN", "RJ8", "RW1", "RWV", "RRP", "RW5", "RXT", "RTQ", "RPG")


### yearly


pred_annual_dist_org <- pred_annual %>%
  
  select(-org_name, -nhse_region_code, -nhse_region_name, -model) %>%
  
  filter(org_code %in% orgs) %>%
  
  group_by(year, org_code) %>%
  
  summarise(across(everything(), mean), .groups = 'drop') %>%
  
  pivot_longer(cols = -c(year, org_code), names_to = "variable", values_to = "forecast") %>%
  
  mutate(scenario = "base case")



### quarter

pred_quarter_dist_org <- pred_quarter %>%
  
  select(-org_name, -nhse_region_code, -nhse_region_name, -model) %>%
  
  filter(org_code %in% orgs) %>%
  
  group_by(quarter, org_code) %>%
  
  summarise(across(everything(), mean), .groups = 'drop') %>%
  
  pivot_longer(cols = -c(quarter, org_code), names_to = "variable", values_to = "forecast") %>%
  
  mutate(scenario = "base case")



### monthly

pred_monthly_dist_org <- pred_monthly %>%
  
  select(-org_name, -nhse_region_code, -nhse_region_name, -model) %>%
  
  filter(org_code %in% orgs) %>%
  
  group_by(date, org_code) %>%
  
  summarise(across(everything(), mean), .groups = 'drop') %>%
  
  pivot_longer(cols = -c(date, org_code), names_to = "variable", values_to = "forecast") %>%
  
  mutate(scenario = "base case")

#############################################################################################


##################### Create annual, quaterly  and monthly data for high case ###################

### annual

pred_annual_high <- pred_tsibble %>% as_tibble() %>% mutate(year = year(date)) %>% filter(model %in% c("lgbm_high", "snaive", "arima", "ets", "xgbm_high")) %>%
  
  group_by(year, org_code, org_name, nhse_region_code, nhse_region_name, model) %>% select(-date) %>%
  
  summarise(across(everything(), mean), .groups = "drop")




### quarterly

pred_quarter_high <- pred_tsibble %>% as_tibble() %>% mutate(quarter = yearquarter(date)) %>% filter(model %in% c("lgbm_high", "snaive", "arima", "ets", "xgbm_high")) %>%
  
  group_by(quarter, org_code, org_name, nhse_region_code, nhse_region_name, model) %>% select(-date) %>%
  
  summarise(across(everything(), mean), .groups = "drop")


### monthly

pred_monthly_high <- pred_tsibble %>% as_tibble() %>% filter(model %in% c("lgbm_high", "snaive", "arima", "ets", "xgbm_high"))



## AGREGRATE 

### yearly

pred_annual_high_dist_total <- pred_annual_high %>%
  
  select(-org_code, -org_name, -nhse_region_code) %>%
  
  group_by(year, nhse_region_name, model) %>%
  
  summarise(across(everything(), sum), .groups = 'drop') %>% select(-model) %>%
  
  group_by(year, nhse_region_name) %>%
  
  summarise(across(everything(), mean), .groups = 'drop') %>% select(-nhse_region_name) %>%
  
  group_by(year) %>%
  
  summarise(across(everything(), sum), .groups = 'drop') %>%
  
  pivot_longer(cols = -c(year), names_to = "variable", values_to = "forecast") %>%
  
  mutate(scenario = "high demand")



### quarter

pred_quarter_high_dist_total <- pred_quarter_high %>%
  
  select(-org_code, -org_name, -nhse_region_code) %>%
  
  group_by(quarter, nhse_region_name, model) %>%
  
  summarise(across(everything(), sum), .groups = 'drop') %>% select(-model) %>%
  
  group_by(quarter, nhse_region_name) %>%
  
  summarise(across(everything(), mean), .groups = 'drop') %>% select(-nhse_region_name) %>%
  
  group_by(quarter) %>%
  
  summarise(across(everything(), sum), .groups = 'drop') %>%
  
  pivot_longer(cols = -c(quarter), names_to = "variable", values_to = "forecast") %>%
  
  mutate(scenario = "high demand")



### monthly

pred_monthly_high_dist_total <- pred_monthly_high %>%
  
  select(-org_name, -nhse_region_name, -nhse_region_code, -model) %>%
  
  group_by(date, org_code) %>%
  
  summarise(across(everything(), mean), .groups = 'drop') %>% select(-org_code) %>%
  
  group_by(date) %>%
  
  summarise(across(everything(), sum), .groups = 'drop') %>%
  
  pivot_longer(cols = -c(date), names_to = "variable", values_to = "forecast") %>%
  
  mutate(scenario = "high demand")



## REGION

### yearly

pred_annual_high_dist_region <- pred_annual_high %>%
  
  select(-org_code, -org_name, -nhse_region_code) %>%
  
  group_by(year, nhse_region_name, model) %>%
  
  summarise(across(everything(), sum), .groups = 'drop') %>% select(-model) %>%
  
  group_by(year, nhse_region_name) %>%
  
  summarise(across(everything(), mean), .groups = 'drop') %>%
  
  pivot_longer(cols = -c(year, nhse_region_name), names_to = "variable", values_to = "forecast") %>%
  
  mutate(scenario = "high demand")



### quarter

pred_quarter_high_dist_region <- pred_quarter_high %>%
  
  select(-org_code, -org_name, -nhse_region_code) %>%
  
  group_by(quarter, nhse_region_name, model) %>%
  
  summarise(across(everything(), sum), .groups = 'drop') %>% select(-model) %>%
  
  group_by(quarter, nhse_region_name) %>%
  
  summarise(across(everything(), mean), .groups = 'drop') %>%
  
  pivot_longer(cols = -c(quarter, nhse_region_name), names_to = "variable", values_to = "forecast") %>%
  
  mutate(scenario = "high demand")



### monthly

pred_monthly_high_dist_region <- pred_monthly_high %>%
  
  select(-org_code, -org_name, -nhse_region_code) %>%
  
  group_by(date, nhse_region_name, model) %>%
  
  summarise(across(everything(), sum), .groups = 'drop') %>% select(-model) %>%
  
  group_by(date, nhse_region_name) %>%
  
  summarise(across(everything(), mean), .groups = 'drop') %>%
  
  pivot_longer(cols = -c(date, nhse_region_name), names_to = "variable", values_to = "forecast") %>%
  
  mutate(scenario = "high demand")


## org level


### create box plots for randomly selected org levels

orgs = c("RDY", "RVN", "RJ8", "RW1", "RWV", "RRP", "RW5", "RXT", "RTQ", "RPG")


### yearly


pred_annual_high_dist_org <- pred_annual_high %>%
  
  select(-org_name, -nhse_region_code, -nhse_region_name, -model) %>%
  
  filter(org_code %in% orgs) %>%
  
  group_by(year, org_code) %>%
  
  summarise(across(everything(), mean), .groups = 'drop') %>%
  
  pivot_longer(cols = -c(year, org_code), names_to = "variable", values_to = "forecast") %>%
  
  mutate(scenario = "high demand")



### quarter

pred_quarter_high_dist_org <- pred_quarter_high %>%
  
  select(-org_name, -nhse_region_code, -nhse_region_name, -model) %>%
  
  filter(org_code %in% orgs) %>%
  
  group_by(quarter, org_code) %>%
  
  summarise(across(everything(), mean), .groups = 'drop') %>%
  
  pivot_longer(cols = -c(quarter, org_code), names_to = "variable", values_to = "forecast") %>%
  
  mutate(scenario = "high demand")



### monthly

pred_monthly_high_dist_org <- pred_monthly_high %>%
  
  select(-org_name, -nhse_region_code, -nhse_region_name, -model) %>%
  
  filter(org_code %in% orgs) %>%
  
  group_by(date, org_code) %>%
  
  summarise(across(everything(), mean), .groups = 'drop') %>%
  
  pivot_longer(cols = -c(date, org_code), names_to = "variable", values_to = "forecast") %>%
  
  mutate(scenario = "high demand")

#############################################################################################


##################### Create annual, quaterly  and monthly data for low case ###################

### annual

pred_annual_low <- pred_tsibble %>% as_tibble() %>% mutate(year = year(date)) %>% filter(model %in% c("lgbm_low", "snaive", "arima", "ets", "xgbm_low")) %>%
  
  group_by(year, org_code, org_name, nhse_region_code, nhse_region_name, model) %>% select(-date) %>%
  
  summarise(across(everything(), mean), .groups = "drop")




### quarterly

pred_quarter_low <- pred_tsibble %>% as_tibble() %>% mutate(quarter = yearquarter(date)) %>% filter(model %in% c("lgbm_low", "snaive", "arima", "ets", "xgbm_low")) %>%
  
  group_by(quarter, org_code, org_name, nhse_region_code, nhse_region_name, model) %>% select(-date) %>%
  
  summarise(across(everything(), mean), .groups = "drop")


### monthly

pred_monthly_low <- pred_tsibble %>% as_tibble() %>% filter(model %in% c("lgbm_low", "snaive", "arima", "ets", "xgbm_low"))



## AGREGRATE 

### yearly

pred_annual_low_dist_total <- pred_annual_low %>%
  
  select(-org_code, -org_name, -nhse_region_code) %>%
  
  group_by(year, nhse_region_name, model) %>%
  
  summarise(across(everything(), sum), .groups = 'drop') %>% select(-model) %>%
  
  group_by(year, nhse_region_name) %>%
  
  summarise(across(everything(), mean), .groups = 'drop') %>% select(-nhse_region_name) %>%
  
  group_by(year) %>%
  
  summarise(across(everything(), sum), .groups = 'drop') %>%
  
  pivot_longer(cols = -c(year), names_to = "variable", values_to = "forecast") %>%
  
  mutate(scenario = "low demand")



### quarter

pred_quarter_low_dist_total <- pred_quarter_low %>%
  
  select(-org_code, -org_name, -nhse_region_code) %>%
  
  group_by(quarter, nhse_region_name, model) %>%
  
  summarise(across(everything(), sum), .groups = 'drop') %>% select(-model) %>%
  
  group_by(quarter, nhse_region_name) %>%
  
  summarise(across(everything(), mean), .groups = 'drop') %>% select(-nhse_region_name) %>%
  
  group_by(quarter) %>%
  
  summarise(across(everything(), sum), .groups = 'drop') %>%
  
  pivot_longer(cols = -c(quarter), names_to = "variable", values_to = "forecast") %>%
  
  mutate(scenario = "low demand")



### monthly

pred_monthly_low_dist_total <- pred_monthly_low %>%
  
  select(-org_name, -nhse_region_name, -nhse_region_code, -model) %>%
  
  group_by(date, org_code) %>%
  
  summarise(across(everything(), mean), .groups = 'drop') %>% select(-org_code) %>%
  
  group_by(date) %>%
  
  summarise(across(everything(), sum), .groups = 'drop') %>%
  
  pivot_longer(cols = -c(date), names_to = "variable", values_to = "forecast") %>%
  
  mutate(scenario = "low demand")



## REGION

### yearly

pred_annual_low_dist_region <- pred_annual_low %>%
  
  select(-org_code, -org_name, -nhse_region_code) %>%
  
  group_by(year, nhse_region_name, model) %>%
  
  summarise(across(everything(), sum), .groups = 'drop') %>% select(-model) %>%
  
  group_by(year, nhse_region_name) %>%
  
  summarise(across(everything(), mean), .groups = 'drop') %>%
  
  pivot_longer(cols = -c(year, nhse_region_name), names_to = "variable", values_to = "forecast") %>%
  
  mutate(scenario = "low demand")



### quarter

pred_quarter_low_dist_region <- pred_quarter_low %>%
  
  select(-org_code, -org_name, -nhse_region_code) %>%
  
  group_by(quarter, nhse_region_name, model) %>%
  
  summarise(across(everything(), sum), .groups = 'drop') %>% select(-model) %>%
  
  group_by(quarter, nhse_region_name) %>%
  
  summarise(across(everything(), mean), .groups = 'drop') %>%
  
  pivot_longer(cols = -c(quarter, nhse_region_name), names_to = "variable", values_to = "forecast") %>%
  
  mutate(scenario = "low demand")



### monthly

pred_monthly_low_dist_region <- pred_monthly_low %>%
  
  select(-org_code, -org_name, -nhse_region_code) %>%
  
  group_by(date, nhse_region_name, model) %>%
  
  summarise(across(everything(), sum), .groups = 'drop') %>% select(-model) %>%
  
  group_by(date, nhse_region_name) %>%
  
  summarise(across(everything(), mean), .groups = 'drop') %>%
  
  pivot_longer(cols = -c(date, nhse_region_name), names_to = "variable", values_to = "forecast") %>%
  
  mutate(scenario = "low demand")


## org level


### create box plots for randomly selected org levels

orgs = c("RDY", "RVN", "RJ8", "RW1", "RWV", "RRP", "RW5", "RXT", "RTQ", "RPG")


### yearly


pred_annual_low_dist_org <- pred_annual_low %>%
  
  select(-org_name, -nhse_region_code, -nhse_region_name, -model) %>%
  
  filter(org_code %in% orgs) %>%
  
  group_by(year, org_code) %>%
  
  summarise(across(everything(), mean), .groups = 'drop') %>%
  
  pivot_longer(cols = -c(year, org_code), names_to = "variable", values_to = "forecast") %>%
  
  mutate(scenario = "low demand")



### quarter

pred_quarter_low_dist_org <- pred_quarter_low %>%
  
  select(-org_name, -nhse_region_code, -nhse_region_name, -model) %>%
  
  filter(org_code %in% orgs) %>%
  
  group_by(quarter, org_code) %>%
  
  summarise(across(everything(), mean), .groups = 'drop') %>%
  
  pivot_longer(cols = -c(quarter, org_code), names_to = "variable", values_to = "forecast") %>%
  
  mutate(scenario = "low demand")



### monthly

pred_monthly_low_dist_org <- pred_monthly_low %>%
  
  select(-org_name, -nhse_region_code, -nhse_region_name, -model) %>%
  
  filter(org_code %in% orgs) %>%
  
  group_by(date, org_code) %>%
  
  summarise(across(everything(), mean), .groups = 'drop') %>%
  
  pivot_longer(cols = -c(date, org_code), names_to = "variable", values_to = "forecast") %>%
  
  mutate(scenario = "low demand")

#############################################################################################


####################### create master dataframes ############################################

### Aggregate 

pred_annual_dist_total <- pred_annual_dist_total %>% bind_rows(pred_annual_high_dist_total) %>% bind_rows(pred_annual_low_dist_total)

pred_quarter_dist_total <- pred_quarter_dist_total %>% bind_rows(pred_quarter_high_dist_total) %>% bind_rows(pred_quarter_low_dist_total)

pred_monthly_dist_total <- pred_monthly_dist_total %>% bind_rows(pred_monthly_high_dist_total) %>% bind_rows(pred_monthly_low_dist_total)


write_rds(pred_annual_dist_total, "results/pred_annual_dist_total.rds")
write_rds(pred_quarter_dist_total, "results/pred_quarter_dist_total.rds")
write_rds(pred_monthly_dist_total, "results/pred_monthly_dist_total.rds")


### Region 

pred_annual_dist_region <- pred_annual_dist_region %>% bind_rows(pred_annual_high_dist_region) %>% bind_rows(pred_annual_low_dist_region)

pred_quarter_dist_region <- pred_quarter_dist_region %>% bind_rows(pred_quarter_high_dist_region) %>% bind_rows(pred_quarter_low_dist_region)

pred_monthly_dist_region <- pred_monthly_dist_region %>% bind_rows(pred_monthly_high_dist_region) %>% bind_rows(pred_monthly_low_dist_region)


write_rds(pred_annual_dist_region, "results/pred_annual_dist_region.rds")
write_rds(pred_quarter_dist_region, "results/pred_quarter_dist_region.rds")
write_rds(pred_monthly_dist_region, "results/pred_monthly_dist_region.rds")


###  Org

pred_annual_dist_org <- pred_annual_dist_org %>% bind_rows(pred_annual_high_dist_org) %>% bind_rows(pred_annual_low_dist_org)

pred_quarter_dist_org <- pred_quarter_dist_org %>% bind_rows(pred_quarter_high_dist_org) %>% bind_rows(pred_quarter_low_dist_org)

pred_monthly_dist_org <- pred_monthly_dist_org %>% bind_rows(pred_monthly_high_dist_org) %>% bind_rows(pred_monthly_low_dist_org)

write_rds(pred_annual_dist_org, "results/pred_annual_dist_org.rds")
write_rds(pred_quarter_dist_org, "results/pred_quarter_dist_org.rds")
write_rds(pred_monthly_dist_org, "results/pred_monthly_dist_org.rds")

##################################################################################################################################

## read data

pred_annual_dist_total <- read_rds("results/pred_annual_dist_total.rds")
pred_quarter_dist_total <- read_rds("results/pred_quarter_dist_total.rds")
pred_monthly_dist_total <- read_rds("results/pred_monthly_dist_total.rds")

pred_annual_dist_region <- read_rds("results/pred_annual_dist_region.rds")
pred_quarter_dist_region <- read_rds("results/pred_quarter_dist_region.rds")
pred_monthly_dist_region <- read_rds("results/pred_monthly_dist_region.rds")

pred_annual_dist_org <- read_rds("results/pred_annual_dist_org.rds")
pred_quarter_dist_org <- read_rds("results/pred_quarter_dist_org.rds")
pred_monthly_dist_org <- read_rds("results/pred_monthly_dist_org.rds")



########### plot predictions ###############################


### aggregate 

## annual

# pred_annual_dist_total %>%
# 
#   filter(variable == "prediction" | variable == "lo_95_0" | variable == "hi_95_0", scenario == "base case") %>%
# 
#   pivot_wider(id_cols = year, names_from = variable, values_from = forecast) %>%
# 
#   rename(upper_bound = hi_95_0, lower_bound = lo_95_0) %>%
# 
#   ggplot(aes(year, prediction)) +
# 
#   geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound), fill = "lightgray", alpha=0.3) +
# 
#   geom_line(lwd = 0.75, group = 1) +
# 
#   geom_line(nurse_history %>% filter(year < 2023) %>%
# 
#               group_by(date, year, nhse_region_name) %>%
# 
#               summarise(hc = sum(hc), .groups = 'drop') %>%
# 
#               select(-date, -nhse_region_name) %>%
# 
#               group_by(year) %>%
# 
#               summarise(hc = mean(hc), .groups = 'drop'),
# 
#             mapping = aes(x = year, y = hc, group=1),
# 
#             color = "#0072B2", lwd = 0.75) +
#   
#   scale_x_continuous(breaks = 1:6, labels = levels(as.factor(pred_annual_dist_total$year))) +
# 
#   labs(x = "Year", y = "Headcount", color = "Forecasting scenario") +
# 
#   theme_bw() +
# 
#   #scale_color_manual(labels = c("Upper bound", "Lower bound", "Prediction"), values=c("#D55E00", "#009E73", "gray45")) +
# 
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))


total_2022 <- nurse_history %>% filter(year == 2022) %>% 
  
  group_by(org_code, year, nhse_region_name) %>%
  
  summarise(hc = mean(hc), .groups = 'drop') %>%
  
  select(-org_code, -nhse_region_name) %>%
  
  group_by(year) %>%
  
  summarise(hc = sum(hc), .groups = 'drop') %>%
  
  mutate(variable = "prediction", scenario = "base case") %>%
  
  rename(forecast = hc) %>%
  
  select(year, variable, forecast, scenario)



total_2022 |> 
  bind_rows(pred_annual_dist_total %>%
  
  filter(variable == "prediction" | variable == "lo_95_0" | variable == "hi_95_0", scenario == "base case")) %>%
  
  ggplot(aes(year, forecast, color = variable)) + 
  
  geom_line(aes(group = variable), lwd = 0.75) +
  
  geom_line(nurse_history %>% filter(year < 2023) %>% 
              
              group_by(org_code, year, nhse_region_name) %>%
              
              summarise(hc = mean(hc), .groups = 'drop') %>%
              
              select(-org_code, -nhse_region_name) %>%
              
              group_by(year) %>%
              
              summarise(hc = sum(hc), .groups = 'drop'), 
            
            mapping = aes(x = year, y = hc, group=1),
            
            color = "#0072B2", lwd = 0.75) +
  
  labs(x = "Year", y = "Headcount", color = "Forecasting scenario") +
  
  theme_few() +
  
  scale_x_continuous(breaks = 2018:2028) +
  
  scale_color_manual(labels = c("Upper bound", "Lower bound", "Prediction"), values=c("#D55E00", "#009E73", "gray45")) +
  
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


## calculation


pred_annual_dist_region %>%
  
  filter(variable == "prediction" | variable == "lo_95_0" | variable == "hi_95_0", scenario == "base case") %>%
  
  group_by(year, variable) |> 
  
  summarise(forecast = sum(forecast), .groups = 'drop') |> 
  
  bind_rows(total_2022 %>% bind_rows(total_2022 %>% mutate(variable = "lo_95_0"), total_2022 %>% mutate(variable = "hi_95_0"))) %>%
  
  group_by(variable) %>%
  
  arrange(year) %>%
  
  mutate(rate = (forecast - first(forecast))/first(forecast) * 100) %>%
  
  select(-scenario) %>%
  
  pivot_wider(id_cols = year, names_from = variable, values_from = c(forecast, rate)) %>%
  
  rename("Prediction" = "forecast_prediction", "Lower bound" = "forecast_lo_95_0", "Upper bound" =  "forecast_hi_95_0",
         "Cahange rate % (base)" = "rate_prediction", "Cahange rate % (low)" = "rate_lo_95_0", "Cahange rate % (high)" = "rate_hi_95_0")



## quarter


total_2022_q <- nurse_history %>% mutate(quarter = yearquarter(date)) %>%
  
    filter(as.character(quarter) == "2022 Q4") %>%
  
    mutate(quarter = yearquarter(date)) %>%
    
    group_by(date, quarter, nhse_region_name) %>%
    
    summarise(hc = sum(hc), .groups = 'drop') %>%
    
    select(-date) %>%
    
    group_by(quarter, nhse_region_name) %>%
    
    summarise(hc = mean(hc), .groups = 'drop') %>%
    
    select(-nhse_region_name) %>%
    
    group_by(quarter) %>%
    
    summarise(hc = sum(hc), .groups = 'drop') %>%
  
    mutate(variable = "prediction", scenario = "base case") %>%
    
    rename(forecast = hc) %>%
    
    select(quarter, variable, forecast, scenario)



pred_quarter_dist_total %>%
  
  filter(variable == "prediction" | variable == "lo_95_0" | variable == "hi_95_0", scenario == "base case") %>%
  
  bind_rows(total_2022_q) %>%
  
  ggplot(aes(as.factor(quarter), forecast, color = variable)) + 
  
  geom_line(aes(group=variable), lwd = 0.75) +
  
  geom_line(nurse_history %>% filter(year < 2023) %>%
              
              mutate(quarter = yearquarter(date)) %>%
              
              group_by(date, quarter, nhse_region_name) %>%
              
              summarise(hc = sum(hc), .groups = 'drop') %>%
              
              select(-date) %>%
              
              group_by(quarter, nhse_region_name) %>%
              
              summarise(hc = mean(hc), .groups = 'drop') %>%
              
              select(-nhse_region_name) %>%
              
              group_by(quarter) %>%
              
              summarise(hc = sum(hc), .groups = 'drop'),
            
            mapping = aes(x = as.factor(quarter), y = hc, group=1),
            
            color = "#0072B2", lwd = 0.75) +
  
  labs(x = "Quarter", y = "Headcount", color = "Forecasting scenario") +
  
  scale_color_manual(labels = c("Upper bound", "Lower bound", "Prediction"), values=c("#D55E00", "#009E73", "gray45")) +
  
  theme_bw() +
  
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



## monthly

pred_monthly_dist_total %>%
  
  filter(variable == "prediction" | variable == "lo_95_0" | variable == "hi_95_0", scenario == "base case") %>%
  
  ggplot(aes(as.factor(date), forecast, color = variable)) + 
  
  geom_line(aes(group=variable), lwd = 0.75) +
  
  geom_line(nurse_history %>% filter(year >= 2020, year < 2023) %>% 
              
              mutate(date = yearmonth(date)) %>%
              
              group_by(date, nhse_region_name) %>%
              
              summarise(hc = sum(hc), .groups = 'drop') %>%
              
              select(-nhse_region_name) %>%
              
              group_by(date) %>%
              
              summarise(hc = sum(hc), .group = 'drop'),
            
            mapping = aes(x = as.factor(date), y = hc, group=1),
            
            color = "#0072B2", lwd = 0.75) +
  
  labs(x = "Month", y = "Headcount", color = "Forecasting scenario") +
  
  scale_color_manual(labels = c("Upper bound", "Lower bound", "Prediction"), values=c("#D55E00", "#009E73", "gray45")) +
  
  theme_bw() +
  
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=5))


### REGION



region_2022_t <- nurse_history %>% filter(year == 2022) %>% 
  
  group_by(date, year, nhse_region_name) %>%
  
  summarise(hc = sum(hc), .groups = 'drop') %>%
  
  select(-date) %>%
  
  group_by(year, nhse_region_name) %>%
  
  summarise(hc = mean(hc), .groups = 'drop') %>%
  
  mutate(variable = "prediction", scenario = "base case") %>%
  
  rename(forecast = hc) %>%
  
  select(year, nhse_region_name, variable, forecast, scenario)




## Annual

pred_annual_dist_region %>%
  
  filter(variable == "prediction" | variable == "lo_95_0" | variable == "hi_95_0", scenario == "base case") %>%
  
  bind_rows(region_2022_t) %>%
  
  ggplot(aes(year, forecast, color = variable)) + 
  
  geom_line(aes(group=variable), lwd = 0.75) +
  
  geom_line(nurse_history %>% filter(year < 2023) %>% 
              
              group_by(date, year, nhse_region_name) %>%
              
              summarise(hc = sum(hc), .groups = 'drop') %>%
              
              select(-date) %>%
              
              group_by(year, nhse_region_name) %>%
              
              summarise(hc = mean(hc), .groups = 'drop'), 
            
            mapping = aes(x = year, y = hc, group=1),
            
            color = "#0072B2", lwd = 0.75) +
  
  labs(x = "Year", y = "Headcount", color = "Forecasting scenario") +
  
  scale_color_manual(labels = c("Upper bound", "Lower bound", "Prediction"), values=c("#D55E00", "#009E73", "gray45")) +
  
  theme_bw() +
  scale_x_continuous(breaks = 2018:2028) +
  
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  
  facet_wrap(vars(factor(nhse_region_name)), scales = "free_y")


rate_region <-  pred_annual_dist_region %>%
  
  filter(variable == "prediction" | variable == "lo_95_0" | variable == "hi_95_0", scenario == "base case", year == 2028) %>%
  
  bind_rows(region_2022_t %>% bind_rows(region_2022_t %>% mutate(variable = "lo_95_0"), region_2022_t %>% mutate(variable = "hi_95_0"))) %>%
  
  group_by(nhse_region_name, variable) %>%
  
  arrange(year) %>%
  
  mutate(rate = (forecast - first(forecast))/first(forecast) * 100)  %>%
  
  select(-scenario) %>%
  
  pivot_wider(id_cols = c(year, nhse_region_name), names_from = variable, values_from = c(forecast, rate)) %>%
  
  rename("Prediction" = "forecast_prediction", "Lower bound" = "forecast_lo_95_0", "Upper bound" =  "forecast_hi_95_0",
         "Cahange rate % (base)" = "rate_prediction", "Cahange rate % (low)" = "rate_lo_95_0", "Cahange rate % (high)" = "rate_hi_95_0")



## quarterly


region_2022_q <- nurse_history %>% filter(year < 2023) %>%
  
  mutate(quarter = yearquarter(date)) %>%
  
  group_by(date, quarter, nhse_region_name) %>%
  
  summarise(hc = sum(hc), .groups = 'drop') %>%
  
  select(-date) %>%
  
  group_by(quarter, nhse_region_name) %>%
  
  summarise(hc = mean(hc), .groups = 'drop') %>%
  
  mutate(variable = "prediction", scenario = "base case") %>%
  
  rename(forecast = hc) %>%
  
  select(quarter, nhse_region_name, variable, forecast, scenario)





pred_quarter_dist_region %>%
  
  filter(variable == "prediction" | variable == "lo_95_0" | variable == "hi_95_0", scenario == "base case") %>%
  
  bind_rows(region_2022_q) %>%
  
  ggplot(aes(as.factor(quarter), forecast, color = variable)) + 

  geom_line(aes(group=variable), lwd = 0.75) +
  
  geom_line(nurse_history %>% filter(year < 2023) %>%

            mutate(quarter = yearquarter(date)) %>%

            group_by(date, quarter, nhse_region_name) %>%

            summarise(hc = sum(hc), .groups = 'drop') %>%

            select(-date) %>%

            group_by(quarter, nhse_region_name) %>%

            summarise(hc = mean(hc), .groups = 'drop'),

          mapping = aes(x = as.factor(quarter), y = hc, group=1),

          color = "#0072B2", lwd = 0.75) +

  labs(x = "Quarter", y = "Headcount", color = "Forecasting scenario") +
  
  scale_color_manual(labels = c("Upper bound", "Lower bound", "Prediction"), values=c("#D55E00", "#009E73", "gray45")) +
  
  theme_bw() +
  
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 6)) +
  
  facet_wrap(vars(factor(nhse_region_name)), scales = "free_y")


## monthly

pred_monthly_dist_region %>%
  
  filter(variable == "prediction" | variable == "lo_95_0" | variable == "hi_95_0", scenario == "base case") %>%
  
  ggplot(aes(as.factor(date), forecast, color = variable)) + 
  
  geom_line(aes(group=variable), lwd = 0.75) +
  
  geom_line(nurse_history %>% filter(year >= 2020, year < 2023) %>%
  
              mutate(date = yearmonth(date)) %>%
  
              group_by(date, nhse_region_name) %>%
  
              summarise(hc = sum(hc), .groups = 'drop'),
  
            mapping = aes(x = as.factor(date), y = hc, group=1),
  
            color = "#0072B2", lwd = 0.75) +

  labs(x = "Month", y = "Headcount", color = "Forecasting scenario") +
  
  scale_color_manual(labels = c("Upper bound", "Lower bound", "Prediction"), values=c("#D55E00", "#009E73", "gray45")) +
  
  theme_bw() +
  
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=4)) +
  
  facet_wrap(vars(factor(nhse_region_name)), scales = "free_y")



### Org level

### create box plots for randomly selected org levels

# orgs = sample(nurse_history$org_code,6)

orgs = c("RDY", "RJ8", "RW1", "RW5", "RXT", "RPG")


### annual


org_2022_t <- nurse_history %>% 
  
  filter(org_code %in% orgs, year == 2022) %>% 
  
  group_by(year, org_code) %>%
  
  summarise(hc = mean(hc), .groups = 'drop') %>%
  
  mutate(variable = "prediction", scenario = "base case") %>%
  
  rename(forecast = hc) %>%
  
  select(year, org_code, variable, forecast, scenario)


pred_annual_dist_org %>%
  
  filter(variable == "prediction" | variable == "lo_95_0" | variable == "hi_95_0", scenario == "base case") %>%
  
  filter(org_code %in% orgs) %>%
  
  bind_rows(org_2022_t) %>%
  
  ggplot(aes(as.factor(year), forecast, color = variable)) + 
  
  geom_line(aes(group=variable), lwd = 0.75) +
  
  geom_line(nurse_history %>% filter(org_code %in% orgs, year < 2023) %>% 
              
              group_by(year, org_code) %>%
              
              summarise(hc = mean(hc)), 
            
            mapping = aes(x = as.factor(year), y = hc, group=1),
            
            color = "#0072B2", lwd = 0.75) +
  
  labs(x = "Year", y = "Headcount", color = "Forecasting scenario") +
  
  scale_color_manual(labels = c("Upper bound", "Lower bound", "Prediction"), values=c("#D55E00", "#009E73", "gray45")) +
  
  theme_bw() +
  
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  
  facet_wrap(vars(factor(org_code)), scales = "free_y")


rate_org <-  pred_annual_dist_org %>%
  
  filter(variable == "prediction" | variable == "lo_95_0" | variable == "hi_95_0", scenario == "base case", year == 2028) %>%
  
  filter(org_code %in% orgs)  %>%
  
  bind_rows(org_2022_t %>% bind_rows(org_2022_t %>% mutate(variable = "lo_95_0"), org_2022_t %>% mutate(variable = "hi_95_0"))) %>%
  
  group_by(org_code, variable) %>%
  
  arrange(year) %>%
  
  mutate(rate = (forecast - first(forecast))/first(forecast) * 100) %>%
  
  select(-scenario) %>%
  
  pivot_wider(id_cols = c(year, org_code), names_from = variable, values_from = c(forecast, rate)) %>%
  
  rename("Prediction" = "forecast_prediction", "ICB code" = "org_code",  "Lower bound" = "forecast_lo_95_0", "Upper bound" =  "forecast_hi_95_0",
         "Cahange rate % (base)" = "rate_prediction", "Cahange rate % (low)" = "rate_lo_95_0", "Cahange rate % (high)" = "rate_hi_95_0")




## quarter


org_2022_q <- nurse_history %>% mutate(quarter = yearquarter(date)) %>%
  
  mutate(quarter = yearquarter(date)) %>%
  
  filter(org_code %in% orgs, as.character(quarter) == "2022 Q4") %>%
  
  group_by(quarter, org_code) %>%
  
  summarise(hc = mean(hc), .groups = "drop") %>%
  
  mutate(variable = "prediction", scenario = "base case") %>%
  
  rename(forecast = hc) %>%
  
  select(quarter, org_code, variable, forecast, scenario)



pred_quarter_dist_org %>%
  
  filter(variable == "prediction" | variable == "lo_95_0" | variable == "hi_95_0", scenario == "base case") %>%
  
  filter(org_code %in% orgs) %>%
  
  bind_rows(org_2022_q) %>%
  
  ggplot(aes(as.factor(quarter), forecast, color = variable)) + 
  
  geom_line(aes(group=variable), lwd = 0.75) +
  
  geom_line(nurse_history %>% filter(org_code %in% orgs, year < 2023) %>%

              mutate(quarter = yearquarter(date)) %>%

              group_by(quarter, org_code) %>%

              summarise(hc = mean(hc)), mapping = aes(x = as.factor(quarter), y = hc, group=1),

            color = "#0072B2", lwd = 0.75) +
  
  labs(x = "Quarter", y = "Headcount", color = "Forecasting scenario") +
  
    scale_color_manual(labels = c("Upper bound", "Lower bound", "Prediction"), values=c("#D55E00", "#009E73", "gray45")) +
    
    theme_bw() +
    
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 6)) +
    
    facet_wrap(vars(factor(org_code)), scales = "free_y")


## Monthly

pred_monthly_dist_org %>%
  
  filter(variable == "prediction" | variable == "lo_95_0" | variable == "hi_95_0", scenario == "base case") %>%
  
  filter(org_code %in% orgs) %>%
  
  ggplot(aes(as.factor(date), forecast, color = variable)) + 
  
  geom_line(aes(group=variable), lwd = 0.75) +
  
  geom_line(nurse_history %>% filter(org_code %in% orgs, 2020 <= year, year < 2023) %>% mutate(date = yearmonth(date)), mapping = aes(x = as.factor(date), y = hc, group=1),

            color = "#0072B2", lwd = 0.75) +
  
  
  labs(x = "Month", y = "Headcount", color = "Forecasting scenario") +
  
  scale_color_manual(labels = c("Upper bound", "Lower bound", "Prediction"), values=c("#D55E00", "#009E73", "gray45")) +
  
  theme_bw() +
  
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=5)) +
  
  facet_wrap(vars(factor(org_code)), scales = "free_y")


#########################################################################################


########### plot distribution charts for sensitivity analysis ###############################


### aggregate 

## annual

pred_annual_dist_region %>%
  
  group_by(year, variable, scenario) |> 
  
  summarise(forecast = sum(forecast), .groups = 'drop') %>% 
  
  ggplot(aes(as.factor(year), forecast, color = scenario)) + 
  
  geom_boxplot(fatten = NULL) +
  
  geom_line(pred_annual_dist_total %>% filter(variable == "prediction", scenario == "base case"), 
            mapping = aes(x = as.factor(year), y = forecast, group=1), lwd = 0.5) +
  
  geom_line(pred_annual_dist_total %>% filter(variable == "prediction", scenario == "high demand"), 
            mapping = aes(x = as.factor(year), y = forecast, group=1), lwd = 0.5) +
  
  geom_line(pred_annual_dist_total %>% filter(variable == "prediction", scenario == "low demand"), 
            mapping = aes(x = as.factor(year), y = forecast, group=1), lwd = 0.5) +
  
  labs(x = "Year", y = "Headcount") +
  
  theme_few() +
  
  scale_color_manual(values=c("gray45", "#D55E00", "#009E73")) +
  
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


pred_annual_dist_region %>%
  
  group_by(year, variable, scenario) |> 
  
  summarise(forecast = sum(forecast), .groups = 'drop') %>% 
  filter(variable == "prediction") %>%
  
  bind_rows(total_2022,
            total_2022 |> mutate(scenario = 'high demand'),
            total_2022 |> mutate(scenario = 'low demand')) |> 
  
  arrange(year) %>%
  
  group_by(year) %>%
  
  mutate(rate = (forecast - first(forecast))/first(forecast) * 100) %>%
  
  select(-variable) %>%
  
  pivot_wider(id_cols = year, names_from = scenario, values_from = c(forecast, rate)) %>%
  
  rename("Year" = "year", "Prediction (base case)" = "forecast_base case", "Prediction (high demand)" =  "forecast_low demand", "Prediction (low demand)" = "forecast_low demand",
         "Cahange rate % (base)" = "rate_base case", "Cahange rate % (high)" = "rate_high demand", "Cahange rate % (low)" = "rate_low demand") |> view()




## quarter

pred_quarter_dist_total %>%
  
  mutate(scenario = case_when(variable == "hi_95_0" ~ "high demand",
                              variable == "lo_95_0" ~ "low demand",
                              TRUE ~ as.character(scenario))) %>%
  
  ggplot(aes(as.factor(quarter), forecast, color = scenario)) + 
  
  geom_boxplot(fatten = NULL) +
  
  geom_line(pred_quarter_dist_total %>% filter(variable == "prediction", scenario == "base case"), 
            mapping = aes(x = as.factor(quarter), y = forecast, group=1), lwd = 0.75) +
  
  geom_line(pred_quarter_dist_total %>% filter(variable == "prediction", scenario == "high demand"), 
            mapping = aes(x = as.factor(quarter), y = forecast, group=1), lwd = 0.75) +
  
  geom_line(pred_quarter_dist_total %>% filter(variable == "prediction", scenario == "low demand"), 
            mapping = aes(x = as.factor(quarter), y = forecast, group=1), lwd = 0.75) +
  
  labs(x = "Quarter", y = "Headcount") +
  
  scale_color_manual(values=c("gray45", "#D55E00", "#009E73")) +
  
  theme_few() +
  
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



## monthly

pred_monthly_dist_total %>%
  
  ggplot(aes(as.factor(date), forecast, color = scenario)) + 
  
  geom_boxplot(fatten = NULL) +
  
  geom_line(pred_monthly_dist_total %>% filter(variable == "prediction", scenario == "base case"), 
            mapping = aes(x = as.factor(date), y = forecast, group=1), lwd = 0.75) +
  
  geom_line(pred_monthly_dist_total %>% filter(variable == "prediction", scenario == "high demand"), 
            mapping = aes(x = as.factor(date), y = forecast, group=1), lwd = 0.75) +
  
  geom_line(pred_monthly_dist_total %>% filter(variable == "prediction", scenario == "low demand"), 
            mapping = aes(x = as.factor(date), y = forecast, group=1), lwd = 0.75) +
  
  # geom_line(nurse_history %>% filter(year >= 2020, year < 2023) %>% 
  #             
  #             mutate(date = yearmonth(date)) %>%
  #             
  #             group_by(date, nhse_region_name) %>%
  #             
  #             summarise(hc = sum(hc), .groups = 'drop') %>%
  #             
  #             select(-nhse_region_name) %>%
  #             
  #             group_by(date) %>%
  #             
  #             summarise(hc = mean(hc), .group = 'drop'),
  #           
  #           mapping = aes(x = as.factor(date), y = hc, group=1),
  #           
  #           color = "#0072B2", lwd = 0.75) +
  
  labs(x = "Month", y = "Headcount") +
  
  scale_color_manual(values=c("gray45", "#D55E00", "#009E73")) +
  
  theme_few() +
  
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=5))


### REGION

## Annual

pred_annual_dist_region %>%
  
  ggplot(aes(as.factor(year), forecast, color = scenario)) + 
  
  geom_boxplot(fatten = NULL) +
  
  geom_line(pred_annual_dist_region %>% filter(variable == "prediction", scenario == "base case"), 
            mapping = aes(x = as.factor(year), y = forecast, group=1), lwd = 0.75) +
  
  geom_line(pred_annual_dist_region %>% filter(variable == "prediction", scenario == "high demand"), 
            mapping = aes(x = as.factor(year), y = forecast, group=1), lwd = 0.75) +
  
  geom_line(pred_annual_dist_region %>% filter(variable == "prediction", scenario == "low demand"), 
            mapping = aes(x = as.factor(year), y = forecast, group=1), lwd = 0.75) +
  
  labs(x = "Year", y = "Headcount") +
  
  scale_color_manual(values=c("gray45", "#D55E00", "#009E73")) +
  
  theme_few() +
  
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  
  facet_wrap(vars(factor(nhse_region_name)), scales = "free_y")



pred_annual_dist_region %>% 
  filter(variable == "prediction") %>%
  
  bind_rows(region_2022_t,
            region_2022_t |> mutate(scenario = 'high demand'),
            region_2022_t |> mutate(scenario = 'low demand')) |>
  
  group_by(nhse_region_name, scenario) %>%
  arrange(year, .by_group = TRUE) %>%
  
  # mutate(forecast = (forecast - first(forecast))/first(forecast)*100) %>% 
  
  group_by(scenario, nhse_region_name) %>%

  summarise(forecast = mean(forecast)) %>%

  pivot_wider(id_cols = nhse_region_name, names_from = scenario, values_from =forecast) %>%
  
  rename("NHSE region" = "nhse_region_name", "Change rate % (base)" = "base case", "Change rate % (high)" = "high demand", "Change rate % (low)" = "low demand") |> view()
  



## quarterly

pred_quarter_dist_region %>%
  
  ggplot(aes(as.factor(quarter), forecast, color = scenario)) + 
  
  geom_boxplot(fatten = NULL) +
  
  geom_line(pred_quarter_dist_region %>% filter(variable == "prediction", scenario == "base case"), 
            mapping = aes(x = as.factor(quarter), y = forecast, group=1), lwd = 0.75) +
  
  geom_line(pred_quarter_dist_region %>% filter(variable == "prediction", scenario == "high demand"), 
            mapping = aes(x = as.factor(quarter), y = forecast, group=1), lwd = 0.75) +
  
  geom_line(pred_quarter_dist_region %>% filter(variable == "prediction", scenario == "low demand"), 
            mapping = aes(x = as.factor(quarter), y = forecast, group=1), lwd = 0.75) +
  
  labs(x = "Quarter", y = "Headcount") +
  
  scale_color_manual(values=c("gray45", "#D55E00", "#009E73")) +
  
  theme_few() +
  
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  
  facet_wrap(vars(factor(nhse_region_name)), scales = "free_y")


## monthly

pred_monthly_dist_region %>%
  
  ggplot(aes(as.factor(date), forecast, color = scenario)) + 
  
  geom_boxplot(fatten = NULL) +
  
  geom_line(pred_monthly_dist_region %>% filter(variable == "prediction", scenario == "base case"), 
            mapping = aes(x = as.factor(date), y = forecast, group=1), lwd = 0.75) +
  
  geom_line(pred_monthly_dist_region %>% filter(variable == "prediction", scenario == "high demand"), 
            mapping = aes(x = as.factor(date), y = forecast, group=1), lwd = 0.75) +
  
  geom_line(pred_monthly_dist_region %>% filter(variable == "prediction", scenario == "low demand"), 
            mapping = aes(x = as.factor(date), y = forecast, group=1), lwd = 0.75) +
  
  # geom_line(nurse_history %>% filter(year >= 2020, year < 2023) %>% 
  #             
  #             mutate(date = yearmonth(date)) %>%
  #             
  #             group_by(date, nhse_region_name) %>%
  #             
  #             summarise(hc = sum(hc), .groups = 'drop'),
  #           
  #           mapping = aes(x = as.factor(date), y = hc, group=1),
  #           
  #           color = "#0072B2", lwd = 0.75) +
  
  labs(x = "Month", y = "Headcount") +
  
  scale_color_manual(values=c("gray45", "#D55E00", "#009E73")) +
  
  theme_few() +
  
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=5)) +
  
  facet_wrap(vars(factor(nhse_region_name)), scales = "free_y")



### Org level

### create box plots for randomly selected org levels

orgs = c("RDY", "RPG", "RRP", "RWV", "RW5", "RXT")


### annual

pred_annual_dist_org %>%
  
  filter(org_code %in% orgs) %>%
  
  ggplot(aes(as.factor(year), forecast, color = scenario)) + 
  
  geom_boxplot(fatten = NULL) +
  
  geom_line(pred_annual_dist_org %>% filter(org_code %in% orgs) %>% filter(variable == "prediction", scenario == "base case"), 
            mapping = aes(x = as.factor(year), y = forecast, group=1), lwd = 0.75) +
  
  geom_line(pred_annual_dist_org %>% filter(org_code %in% orgs) %>% filter(variable == "prediction", scenario == "high demand"), 
            mapping = aes(x = as.factor(year), y = forecast, group=1), lwd = 0.75) +
  
  geom_line(pred_annual_dist_org %>% filter(org_code %in% orgs) %>% filter(variable == "prediction", scenario == "low demand"), 
            mapping = aes(x = as.factor(year), y = forecast, group=1), lwd = 0.75) +
  
  labs(x = "Year", y = "Headcount") +
  
  scale_color_manual(values=c("gray45", "#D55E00", "#009E73")) +
  
  theme_few() +
  
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  
  facet_wrap(vars(factor(org_code)), scales = "free_y")


org_dist <- pred_annual_dist_org %>% filter(variable == "prediction") %>%
  
  filter(org_code == "RW5") %>%
  
  group_by(year, org_code) %>%
  
  arrange(year) %>%
  
  mutate(rate = (forecast - first(forecast))/first(forecast) * 100)
  
  
pairwise.t.test(org_dist$rate, org_dist$scenario,
                  p.adjust.method = "bonferroni")




pred_annual_dist_org %>% filter(variable == "prediction") %>%
  
  group_by(year, org_code) %>%
  
  arrange(year) %>%
  
  mutate(rate = (forecast - first(forecast))/first(forecast) * 100) %>%
  
  group_by(scenario, org_code) %>%
  
  filter(scenario != "base case") %>%
  
  summarise(rate = mean(rate)) %>% print(n = 30) %>%
  
  pivot_wider(id_cols = org_code, names_from = scenario, values_from =rate) %>%
  
  rename("ICB code" = "org_code", "Cahange rate % (high demand)" = "high demand", "Cahange rate % (low demand)" = "low demand")





## quarter

pred_quarter_dist_org %>%
  
  filter(org_code %in% orgs) %>%
  
  ggplot(aes(as.factor(quarter), forecast, color = scenario)) + 
  
  geom_boxplot(fatten = NULL) +
  
  geom_line(pred_quarter_dist_org %>% filter(variable == "prediction", scenario == "base case"), 
            mapping = aes(x = as.factor(quarter), y = forecast, group=1), lwd = 0.75) +
  
  geom_line(pred_quarter_dist_org %>% filter(variable == "prediction", scenario == "high demand"), 
            mapping = aes(x = as.factor(quarter), y = forecast, group=1), lwd = 0.75) +
  
  geom_line(pred_quarter_dist_org %>% filter(variable == "prediction", scenario == "low demand"), 
            mapping = aes(x = as.factor(quarter), y = forecast, group=1), lwd = 0.75) +
  
  labs(x = "Quarter", y = "Headcount") +
  
  scale_color_manual(values=c("gray45", "#D55E00", "#009E73")) +
  
  theme_few() +
  
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  
  facet_wrap(vars(factor(org_code)), scales = "free_y")


## Monthly

pred_monthly_dist_org %>%
  
  filter(org_code %in% orgs) %>%
  
  ggplot(aes(as.factor(date), forecast, color = scenario)) + 
  
  geom_boxplot(fatten = NULL) +
  
  geom_line(pred_monthly_dist_org %>% filter(variable == "prediction", scenario == "base case"), 
            mapping = aes(x = as.factor(date), y = forecast, group=1), lwd = 0.75) +
  
  geom_line(pred_monthly_dist_org %>% filter(variable == "prediction", scenario == "high demand"), 
            mapping = aes(x = as.factor(date), y = forecast, group=1), lwd = 0.75) +
  
  geom_line(pred_monthly_dist_org %>% filter(variable == "prediction", scenario == "low demand"), 
            mapping = aes(x = as.factor(date), y = forecast, group=1), lwd = 0.75) +
  
  
  # geom_line(nurse_history %>% filter(org_code %in% orgs, 2020 <= year, year < 2023) %>% mutate(date = yearmonth(date)), mapping = aes(x = as.factor(date), y = hc, group=1),
  #           
  #           color = "#0072B2", lwd = 0.75) +
  
  
  labs(x = "Month", y = "Headcount") +
  
  scale_color_manual(values=c("gray45", "#D55E00", "#009E73")) +
  
  theme_few() +
  
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=5)) +
  
  facet_wrap(vars(factor(org_code)), scales = "free_y")


#########################################################################################




########################## scenario analysis #############################################

sd <- read.csv("results/scenario_analysis_eng.csv")

sd <- sd %>% mutate(nurse_balance = actual_headcount - prediction_base_case)



## original scenario agregrate

sd %>% filter(scenario == "scenario 0") %>%
  
  select(-ucase_acceptance_rate, -recruitment_rate, -scenario) %>%
  
  pivot_longer(c(-year, -nhse_region_name), names_to = "variable", values_to = "value") %>%
  
  filter(variable != "nurse_balance") %>%
  
  group_by(year, variable) %>%
  
  summarise(value = sum(value)) %>%
  
  ggplot(aes(as.factor(year), value, color = variable)) + 
  
  geom_line(aes(group=variable), lwd = 0.75) +
  
  labs(x = "Year", y = "Headcount", color = "Scenario") +
  
  scale_color_manual(labels = c("Actual Headcount", "Prediction"), values=c("#D55E00", "#009E73")) +
  
  theme_bw() +
  
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

## calculation


total_2022 <- nurse_history %>% filter(year == 2022) %>% 
  
  group_by(org_code, year, nhse_region_name) %>%
  
  summarise(hc = mean(hc), .groups = 'drop') %>%
  
  select(-org_code, -nhse_region_name) %>%
  
  group_by(year) %>%
  
  summarise(hc = sum(hc), .groups = 'drop') %>%
  
  mutate(variable = "prediction", scenario = "base case") %>%
  
  rename(forecast = hc) %>%
  
  select(year, forecast) %>%
  
  rename(actual_headcount = forecast) %>%
  
  mutate(prediction_base_case = actual_headcount, nurse_balance = 0)



 sd %>% filter(scenario == "scenario 0") %>%
  
  select(-ucase_acceptance_rate, -recruitment_rate, -nhse_region_name) %>%
  
  group_by(year, scenario) %>%
  
  summarise(across(everything(), sum), .groups = "drop") %>%
   
  select(-scenario) %>%
   
  arrange(year) %>%
   
  mutate(rate = round((actual_headcount - first(actual_headcount))/first(actual_headcount) * 100, 2))
 

## original scenario - Region


sd %>% filter(scenario == "scenario 0") %>%
  
  select(-ucase_acceptance_rate, -recruitment_rate, -scenario) %>%
  
  pivot_longer(c(-year, -nhse_region_name), names_to = "variable", values_to = "value") %>%
  
  filter(variable != "nurse_balance") %>%
  
  ggplot(aes(as.factor(year), value, color = variable)) + 
  
  geom_line(aes(group=variable), lwd = 0.75) +
  
  labs(x = "Year", y = "Headcount", color = "Scenario") +
  
  scale_color_manual(labels = c("Actual Headcount", "Prediction"), values=c("#D55E00", "#009E73")) +
  
  theme_bw() +
  
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=5)) +
  
  facet_wrap(vars(factor(nhse_region_name)), scales = "free_y")


sd %>% filter(scenario == "scenario 0") %>%
  
  select(-ucase_acceptance_rate, -recruitment_rate) %>%
  
  pivot_longer(c(-year, -nhse_region_name, -scenario), names_to = "variable", values_to = "value") %>%
  
  filter(variable == "nurse_balance") %>%
  
  ggplot(aes(as.factor(year), value, color = "#0072B2")) + 
  
  geom_line(aes(group=1), lwd = 0.75) +
  
  labs(x = "Year", y = "Headcount", color = "Scenario") +
  
  scale_color_manual(labels = c("Base case"), values=c("#0072B2")) +
  
  theme_bw() +
  
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=5)) +
  
  facet_wrap(vars(factor(nhse_region_name)), scales = "free_y")



## calculation

sd %>% filter(scenario == "scenario 0") %>%
  
  select(-ucase_acceptance_rate, -recruitment_rate, -scenario) %>%
  
  group_by(nhse_region_name) %>%
  
  mutate(rate = round((actual_headcount - first(actual_headcount))/first(actual_headcount) * 100, 2)) %>%
  
  select(-year) %>%
  
  summarise(across(everything(), mean), .groups = "drop") %>%
  
  rename("NHSE region" = nhse_region_name, "Supply prejection" = actual_headcount, "Demand projection" = prediction_base_case, "Shortfall" = "nurse_balance", "Annual rate %" = "rate")


## Recruitment changes with constant UCAS scenario



sd %>% filter(scenario == "scenario 1"| scenario == "scenario 2" | scenario == "scenario 3") %>%
  
  select(-ucase_acceptance_rate, -recruitment_rate, -nurse_balance) %>%
  
  pivot_longer(c(-year, -nhse_region_name, -scenario), names_to = "variable", values_to = "value") %>%
  
  filter(scenario == "scenario 1") %>%
  
  ggplot(aes(as.factor(year), value, color = variable)) + 
  
  geom_line(aes(group=variable), lwd = 0.75) +
  
  labs(x = "Year", y = "Headcount", color = "Scenario (recruitment rate = 0.1)") +
  
  scale_color_manual(labels = c("Actual Headcount", "Prediction"), values=c("#D55E00", "#009E73")) +
  
  theme_bw() +
  
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=5)) +
  
  facet_wrap(vars(factor(nhse_region_name)), scales = "free_y")



sd %>% filter(scenario == "scenario 1"| scenario == "scenario 2" | scenario == "scenario 3") %>%
  
  select(-ucase_acceptance_rate, -recruitment_rate, -nurse_balance) %>%
  
  pivot_longer(c(-year, -nhse_region_name, -scenario), names_to = "variable", values_to = "value") %>%
  
  filter(scenario == "scenario 2") %>%
  
  ggplot(aes(as.factor(year), value, color = variable)) + 
  
  geom_line(aes(group=variable), lwd = 0.75) +
  
  labs(x = "Year", y = "Headcount", color = "Scenario (recruitment rate = 0.2)") +
  
  scale_color_manual(labels = c("Actual Headcount", "Prediction"), values=c("#D55E00", "#009E73")) +
  
  theme_bw() +
  
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=5)) +
  
  facet_wrap(vars(factor(nhse_region_name)), scales = "free_y")


sd %>% filter(scenario == "scenario 1"| scenario == "scenario 2" | scenario == "scenario 3") %>%
  
  select(-ucase_acceptance_rate, -recruitment_rate, -nurse_balance) %>%
  
  pivot_longer(c(-year, -nhse_region_name, -scenario), names_to = "variable", values_to = "value") %>%
  
  filter(scenario == "scenario 3") %>%
  
  ggplot(aes(as.factor(year), value, color = variable)) + 
  
  geom_line(aes(group=variable), lwd = 0.75) +
  
  labs(x = "Year", y = "Headcount", color = "Scenario (recruitment rate = 0.3)") +
  
  scale_color_manual(labels = c("Actual Headcount", "Prediction"), values=c("#D55E00", "#009E73")) +
  
  theme_bw() +
  
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=5)) +
  
  facet_wrap(vars(factor(nhse_region_name)), scales = "free_y")



sd %>% filter(scenario == "scenario 1"| scenario == "scenario 2" | scenario == "scenario 3") %>%
  
  select(-ucase_acceptance_rate, -recruitment_rate) %>%
  
  pivot_longer(c(-year, -nhse_region_name, -scenario), names_to = "variable", values_to = "value") %>%
  
  filter(variable == "nurse_balance") %>%
  
  ggplot(aes(as.factor(year), value, color = scenario)) + 
  
  geom_line(aes(group=scenario), lwd = 0.75) +
  
  labs(x = "Year", y = "Headcount", color = "Scenario") +
  
  scale_color_manual(labels = c("Recruitment rate - 0.1", "Recruitment rate - 0.2", "Recruitment rate - 0.3"), values=c("#0072B2", "#CC79A7", "#E69F00")) +
  
  theme_bw() +
  
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=5)) +
  
  facet_wrap(vars(factor(nhse_region_name)), scales = "free_y")


## calculation

sd %>% filter(scenario == "scenario 1"| scenario == "scenario 2" | scenario == "scenario 3") %>%
  
  select(-ucase_acceptance_rate, -recruitment_rate) %>%
  
  group_by(nhse_region_name, scenario) %>%
  
  mutate(rate = round((actual_headcount - first(actual_headcount))/first(actual_headcount) * 100, 2)) %>%
  
  select(-year) %>%
  
  summarise(across(everything(), mean), .groups = "drop") %>%
  
  rename(scenario = "Scenario", "NHSE region" = nhse_region_name, "Supply prejection" = actual_headcount, "Demand projection" = prediction_base_case, "Shortfall" = "nurse_balance", "Annual rate %" = "rate")




## Recruitment changes with ucas changes


sd %>% filter(scenario == "scenario 4"| scenario == "scenario 5" | scenario == "scenario 6" | scenario == "scenario 7") %>%
  
  select(-ucase_acceptance_rate, -recruitment_rate, -nurse_balance) %>%
  
  pivot_longer(c(-year, -nhse_region_name, -scenario), names_to = "variable", values_to = "value") %>%
  
  filter(scenario == "scenario 4") %>%
  
  ggplot(aes(as.factor(year), value, color = variable)) + 
  
  geom_line(aes(group=variable), lwd = 0.75) +
  
  labs(x = "Year", y = "Headcount", color = "Scenario (recruitment rate = 0)") +
  
  scale_color_manual(labels = c("Actual Headcount", "Prediction"), values=c("#D55E00", "#009E73")) +
  
  theme_bw() +
  
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=5)) +
  
  facet_wrap(vars(factor(nhse_region_name)), scales = "free_y")


sd %>% filter(scenario == "scenario 4"| scenario == "scenario 5" | scenario == "scenario 6" | scenario == "scenario 7") %>%
  
  select(-ucase_acceptance_rate, -recruitment_rate, -nurse_balance) %>%
  
  pivot_longer(c(-year, -nhse_region_name, -scenario), names_to = "variable", values_to = "value") %>%
  
  filter(scenario == "scenario 5") %>%
  
  ggplot(aes(as.factor(year), value, color = variable)) + 
  
  geom_line(aes(group=variable), lwd = 0.75) +
  
  labs(x = "Year", y = "Headcount", color = "Scenario (recruitment rate = 0.1)") +
  
  scale_color_manual(labels = c("Actual Headcount", "Prediction"), values=c("#D55E00", "#009E73")) +
  
  theme_bw() +
  
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=5)) +
  
  facet_wrap(vars(factor(nhse_region_name)), scales = "free_y")



sd %>% filter(scenario == "scenario 4"| scenario == "scenario 5" | scenario == "scenario 6"| scenario == "scenario 7") %>%
  
  select(-ucase_acceptance_rate, -recruitment_rate, -nurse_balance) %>%
  
  pivot_longer(c(-year, -nhse_region_name, -scenario), names_to = "variable", values_to = "value") %>%
  
  filter(scenario == "scenario 6") %>%
  
  ggplot(aes(as.factor(year), value, color = variable)) + 
  
  geom_line(aes(group=variable), lwd = 0.75) +
  
  labs(x = "Year", y = "Headcount", color = "Scenario (recruitment rate = 0.2)") +
  
  scale_color_manual(labels = c("Actual Headcount", "Prediction"), values=c("#D55E00", "#009E73")) +
  
  theme_bw() +
  
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=5)) +
  
  facet_wrap(vars(factor(nhse_region_name)), scales = "free_y")


sd %>% filter(scenario == "scenario 4"| scenario == "scenario 5" | scenario == "scenario 6"| scenario == "scenario 7") %>%
  
  select(-ucase_acceptance_rate, -recruitment_rate, -nurse_balance) %>%
  
  pivot_longer(c(-year, -nhse_region_name, -scenario), names_to = "variable", values_to = "value") %>%
  
  filter(scenario == "scenario 7") %>%
  
  ggplot(aes(as.factor(year), value, color = variable)) + 
  
  geom_line(aes(group=variable), lwd = 0.75) +
  
  labs(x = "Year", y = "Headcount", color = "Scenario (recruitment rate = 0.3)") +
  
  scale_color_manual(labels = c("Actual Headcount", "Prediction"), values=c("#D55E00", "#009E73")) +
  
  theme_bw() +
  
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=5)) +
  
  facet_wrap(vars(factor(nhse_region_name)), scales = "free_y")



sd %>% filter(scenario == "scenario 4"| scenario == "scenario 5" | scenario == "scenario 6"| scenario == "scenario 7") %>%
  
  select(-ucase_acceptance_rate, -recruitment_rate) %>%
  
  pivot_longer(c(-year, -nhse_region_name, -scenario), names_to = "variable", values_to = "value") %>%
  
  filter(variable == "nurse_balance") %>%
  
  ggplot(aes(as.factor(year), value, color = scenario)) + 
  
  geom_line(aes(group=scenario), lwd = 0.75) +
  
  labs(x = "Year", y = "Headcount", color = "Scenario") +
  
  scale_color_manual(labels = c("Recruitment rate - 0", "Recruitment rate - 0.1", "Recruitment rate - 0.2", "Recruitment rate - 0.3"), values=c("gray32", "#0072B2", "#CC79A7", "#E69F00")) +

  theme_bw() +

  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=5)) +
  
  facet_wrap(vars(factor(nhse_region_name)), scales = "free_y")


## calculation

a <- sd %>% filter(scenario == "scenario 4"| scenario == "scenario 5" | scenario == "scenario 6"| scenario == "scenario 7") %>%
  
  select(-ucase_acceptance_rate, -recruitment_rate) %>%
  
  group_by(nhse_region_name, scenario) %>%
  
  mutate(rate = round((actual_headcount - first(actual_headcount))/first(actual_headcount) * 100, 2)) %>%
  
  select(-year) %>%
  
  summarise(across(everything(), mean), .groups = "drop") %>%
  
  rename("Scenario" = scenario, "NHSE region" = nhse_region_name, "Supply prejection" = actual_headcount, "Demand projection" = prediction_base_case, "Shortfall" = "nurse_balance", "Annual rate %" = "rate")



## recruitment rate 0.3 and ucas 1.3


sd %>% filter(scenario == "scenario 8") %>%
  
  select(-ucase_acceptance_rate, -recruitment_rate, -nurse_balance) %>%
  
  pivot_longer(c(-year, -nhse_region_name, -scenario), names_to = "variable", values_to = "value") %>%
  
  ggplot(aes(as.factor(year), value, color = variable)) + 
  
  geom_line(aes(group=variable), lwd = 0.75) +
  
  labs(x = "Year", y = "Headcount", color = "Scenario (recruitment rate = 0.3, UCAS rate = 0.3)") +
  
  scale_color_manual(labels = c("Actual Headcount", "Prediction"), values=c("#D55E00", "#009E73")) +
  
  theme_bw() +
  
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=5)) +
  
  facet_wrap(vars(factor(nhse_region_name)), scales = "free_y")


sd %>% filter(scenario == "scenario 8") %>%
  
  select(-ucase_acceptance_rate, -recruitment_rate) %>%
  
  pivot_longer(c(-year, -nhse_region_name, -scenario), names_to = "variable", values_to = "value") %>%
  
  filter(variable == "nurse_balance") %>%
  
  ggplot(aes(as.factor(year), value, color = "#0072B2")) + 
  
  geom_line(aes(group=1), lwd = 0.75) +
  
  labs(x = "Year", y = "Headcount", color = "Scenario") +
  
  scale_color_manual(labels = c("Recruitment rate - 0.3, UCAS rate - 0.3"), values=c("#0072B2")) +
  
  theme_bw() +
  
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=5)) +
  
  facet_wrap(vars(factor(nhse_region_name)), scales = "free_y")



## calculation

sd %>% filter(scenario == "scenario 8") %>%
  
  select(-ucase_acceptance_rate, -recruitment_rate) %>%
  
  group_by(nhse_region_name, scenario) %>%
  
  mutate(rate = round((actual_headcount - first(actual_headcount))/first(actual_headcount) * 100, 2)) %>%
  
  select(-year) %>%
  
  summarise(across(everything(), mean), .groups = "drop")


## all regions nurse balance

sd %>%
  
  filter(scenario != "scenario 8") %>%
  
  select(-ucase_acceptance_rate, -recruitment_rate) %>%
  
  pivot_longer(c(-year, -nhse_region_name, -scenario), names_to = "variable", values_to = "value") %>%
  
  filter(variable == "nurse_balance") %>%
  
  ggplot(aes(as.factor(year), value, color = scenario)) + 
  
  geom_line(aes(group=scenario), lwd = 0.75) +
  
  labs(x = "Year", y = "Headcount", color = "Scenario") +
  
  scale_color_manual(values=c("lightgray", "lightgray", "lightgray","lightgray", "lightgray", "lightgray", "lightgray", "lightgray")) +
  
  theme_bw() +
  
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=5)) +
  
  facet_wrap(vars(factor(nhse_region_name)), scales = "free_y")


## select the best option for each region

sd %>%
  
  filter(scenario != "scenario 8") %>%
  
  select(-ucase_acceptance_rate, -recruitment_rate) %>%
  
  group_by(scenario, nhse_region_name) %>%
  
  summarise(nurse_balance = mean(nurse_balance)) %>%
  
  group_by(nhse_region_name) %>%
  
  slice(which.min(abs(nurse_balance))) %>%
  
  rename(Scenario = scenario, "NHSE region" = nhse_region_name, "Average shortfall" = "nurse_balance")



## create optimum solution table

plan <- sd %>% filter(scenario == "scenario 6" & nhse_region_name == "East of England" |
                scenario == "scenario 3" & nhse_region_name == "London" |
                scenario == "scenario 0" & nhse_region_name == "Midlands" |
                scenario == "scenario 0" & nhse_region_name == "North East and Yorkshire" |
                scenario == "scenario 7" & nhse_region_name == "North West" |
                scenario == "scenario 5" & nhse_region_name == "South East" |
                scenario == "scenario 0" & nhse_region_name == "South West" )

plan %>%
  
  select(-ucase_acceptance_rate, -recruitment_rate) %>%
  
  pivot_longer(c(-year, -nhse_region_name, -scenario), names_to = "variable", values_to = "value") %>%
  
  filter(variable == "nurse_balance") %>%
  
  ggplot(aes(as.factor(year), value, color = scenario)) + 
  
  geom_line(aes(group=scenario), lwd = 1.25) +
  
  labs(x = "Year", y = "Headcount", color = "Scenario") +
  
  scale_color_manual(labels = c("Other scenarios", "Scenario 0", "Scenario 3", "Scenario 5", "Scenario 1", "Scenario 4", "Scenario 2", "Scenario 6", "Scenario 7"), 
                     
                     values=c("lightgray", "#D55E00", "#0072B2","#009E73", "#CC79A7", "#E69F00", "#56B4E9", "#F0E442", "gray36")) +
  
  geom_line(sd %>%
              
              filter(scenario != "scenario 8") %>%
              
              select(-ucase_acceptance_rate, -recruitment_rate) %>%
              
              pivot_longer(c(-year, -nhse_region_name, -scenario), names_to = "variable", values_to = "value") %>%
              
              filter(variable == "nurse_balance"),
            
            mapping = aes(as.factor(year), value, group = scenario, color = "lightgray")) +
  
  
  
  theme_few() +
  
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  
  facet_wrap(vars(factor(nhse_region_name)), scales = "free_y")


plan %>%
  
  select(-ucase_acceptance_rate, -recruitment_rate, -scenario) %>%
  
  pivot_longer(c(-year, -nhse_region_name), names_to = "variable", values_to = "value") %>%
  
  filter(variable != "nurse_balance") %>%
  
  group_by(year, variable) %>%
  
  summarise(value = sum(value)) %>%
  
  ggplot(aes(as.factor(year), value, color = variable)) + 
  
  geom_line(aes(group=variable), lwd = 0.75) +
  
  labs(x = "Year", y = "Headcount", color = "Scenario") +
  
  scale_color_manual(labels = c("Actual Headcount", "Prediction"), values=c("#D55E00", "#009E73")) +
  
  theme_bw() +
  
  theme(axis.text.x = element_text(angle = 90, hjust = 1))







  
  
## calculation


plan %>%
  
  select(-ucase_acceptance_rate, -recruitment_rate, -nhse_region_name, -scenario) %>%
  
  group_by(year) %>%
  
  summarise(across(everything(), sum), .groups = "drop") %>%
  
  mutate(rate = round((actual_headcount - first(actual_headcount))/first(actual_headcount) * 100, 2)) %>%
  
  select(-rate) %>%
  
  rename(Year = year, "Supply projection" = actual_headcount, "Demand projection" = "prediction_base_case", "Shortfall/ surplus" = "nurse_balance")


summary <- nurse_history %>% filter(year == 2022) %>% 
  
  group_by(date, year, nhse_region_name) %>%
  
  summarise(hc = sum(hc), .groups = 'drop') %>%
  
  select(-date) %>%
  
  group_by(year, nhse_region_name) %>%
  
  summarise(hc = mean(hc), .groups = 'drop') %>%
  
  mutate(Demand = hc) %>%
  
  rename(Supply = hc, Region = nhse_region_name) %>%
  
  bind_rows(plan %>%
              
              select(-ucase_acceptance_rate, -recruitment_rate, -scenario) %>%
              
              select(year, nhse_region_name, actual_headcount, prediction_base_case) %>%
              
              rename(Supply = actual_headcount, Demand = prediction_base_case, Region = nhse_region_name)) %>%
  
  group_by(Region) %>%
  
  mutate("Supply growth rate" = round((Supply - first(Supply))/first(Supply) * 100, 2),
         
         "Demand growth rate" = round((Demand - first(Demand))/first(Demand) * 100, 2),
         
         "Shortfall/ surplus" = Supply - Demand) %>%
  
  rename("Supply projection" = Supply, "Demand projection" = Demand, "NHSE region" = Region)


summary %>%
  
  group_by(year) %>%
  
  summarise(supply = sum(Supply), demand = sum(Demand))
  




#########################################################################