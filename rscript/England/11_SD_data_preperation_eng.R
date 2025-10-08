## Sytem dynamic data 

library(tidyverse)

## Load data

pred <- read_rds("results/pred_annual_dist_region.rds") |> 
  as_tibble()

nurse_history <- read_rds("data/eng_workforce_master_updated.rds")

ucas <- read.csv("data/system/England_data/UCAS_acceptance.csv")


## create region wise predictions

pred_sd <- pred %>% filter(variable == "prediction") %>%
  
  pivot_wider(id_cols = year, names_from = c(nhse_region_name, scenario), values_from = forecast)

write.csv(pred_sd, "system_dynamics/predictions_eng.csv", row.names = FALSE)


## create stock values of nurses for 2022

nurse_stock <- nurse_history %>% filter(year == 2022) %>% 
  
  group_by(date, year, nhse_region_name) %>%
  
  summarise(hc = sum(hc), .groups = 'drop') %>%
  
  select(-date) %>%
  
  group_by(year, nhse_region_name) %>%
  
  summarise(hc = mean(hc), .groups = 'drop')

write.csv(nurse_stock, "system_dynamics/nurse_stock_eng.csv", row.names = FALSE)


## calculate average ucas acceptance rate

ucas_acceptance_rate <- ucas %>% rename(nhse_region_name = Region) %>%
  
  group_by(nhse_region_name) %>%
  
  mutate(ucas_rate_change = ((UCAS_acceptance - first(UCAS_acceptance))/first(UCAS_acceptance))) %>%
  
  summarise(average_ucas_acceptance_rate = round(mean(ucas_rate_change), 2))

write.csv(ucas_acceptance_rate, "system_dynamics/ucas_acceptance_rate_eng.csv", row.names = FALSE)
  

## leavers stock

leavers_stock <- nurse_history %>% filter(year >2021) %>%
  
  group_by(date, year, nhse_region_name) %>%
  
  summarise(hc_leavers = sum(hc_leavers), .groups = 'drop') %>%
  
  select(-date) %>%
  
  group_by(year, nhse_region_name) %>%
  
  summarise(hc_leavers = round(sum(hc_leavers)),.groups = "drop") %>%
  
  pivot_wider(id_cols = year, names_from = nhse_region_name, values_from = hc_leavers)


write.csv(leavers_stock, "system_dynamics/leavers_stock_eng.csv", row.names = FALSE)


## Joiners rate

joiners_rate <- nurse_history %>% filter(year >2021) %>%
  
  group_by(date, year, nhse_region_name) %>%
  
  summarise(hc_joiners = sum(hc_joiners), .groups = 'drop') %>%
  
  select(-date) %>%
  
  group_by(year, nhse_region_name) %>%
  
  summarise(hc_joiners = round(sum(hc_joiners)),.groups = "drop") %>%
  
  group_by(nhse_region_name) %>%
  
  mutate(joiners_rate = ((hc_joiners - first(hc_joiners))/first(hc_joiners))) %>%
  
  summarise(joiners_rate = round(mean(joiners_rate), 2))

write.csv(joiners_rate, "system_dynamics/joiners_rate_eng.csv", row.names = FALSE)


joiners_stock <- nurse_history %>% filter(year >2021) %>%
  
  group_by(date, year, nhse_region_name) %>%
  
  summarise(hc_joiners = sum(hc_joiners), .groups = 'drop') %>%
  
  select(-date) %>%
  
  group_by(year, nhse_region_name) %>%
  
  summarise(hc_joiners = round(sum(hc_joiners)),.groups = "drop") %>%
  
  pivot_wider(id_cols = year, names_from = nhse_region_name, values_from = hc_joiners)


write.csv(joiners_stock, "system_dynamics/joiners_stock_eng.csv", row.names = FALSE)


## mental healthcare service contacts

MHS01 <- nurse_history %>% filter(year >2021) %>%
  
  group_by(year, nhse_region_name) %>%
  
  summarise(MHS01 = sum(MHS01), .groups = 'drop') %>%
  
  pivot_wider(id_cols = year, names_from = nhse_region_name, values_from = MHS01)


MHS01_high <- nurse_history %>% filter(year >2021) %>%
  
  group_by(year, nhse_region_name) %>%
  
  summarise(MHS01_high = sum(MHS01_high), .groups = 'drop') %>%
  
  pivot_wider(id_cols = year, names_from = nhse_region_name, values_from = MHS01_high)


MHS01_low <- nurse_history %>% filter(year >2021) %>%
  
  group_by(year, nhse_region_name) %>%
  
  summarise(MHS01_low = sum(MHS01_low), .groups = 'drop') %>%
  
  pivot_wider(id_cols = year, names_from = nhse_region_name, values_from = MHS01_low)


write.csv(MHS01, "system_dynamics/MHS01_eng.csv", row.names = FALSE)

write.csv(MHS01_high, "system_dynamics/MHS01_high_eng.csv", row.names = FALSE)

write.csv(MHS01_low, "system_dynamics/MHS01_low_eng.csv", row.names = FALSE)





  