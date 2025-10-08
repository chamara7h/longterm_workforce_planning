## create master files for predictions 

library(tidyverse)


## load data

# pred_snaive <- read.csv("results/pred_snaive_eng.csv")
# pred_arima <- read.csv("results/pred_arima_eng.csv")
# pred_ets <- read.csv("results/pred_ets_eng.csv")
# 
# pred_lm <- read.csv("results/pred_lm_eng.csv")
# pred_lm_low <- read.csv("results/pred_lm_low_eng.csv")
# pred_lm_high <- read.csv("results/pred_lm_high_eng.csv")
# 
# pred_lgbm <- read.csv("results/pred_lgbm_eng.csv")
# pred_lgbm_low <- read.csv("results/pred_lgbm_low_eng.csv")
# pred_lgbm_high <- read.csv("results/pred_lgbm_high_eng.csv")
# 
# pred_xgbm <- read.csv("results/pred_xgbm_eng.csv")
# pred_xgbm_low <- read.csv("results/pred_xgbm_low_eng.csv")
# pred_xgbm_high <- read.csv("results/pred_xgbm_high_eng.csv")


## write RDS files

# write_rds(pred_snaive, "results/pred_snaive_eng.rds")
# write_rds(pred_arima, "results/pred_arima_eng.rds")
# write_rds(pred_ets, "results/pred_ets_eng.rds")
# 
# write_rds(pred_lm, "results/pred_lm_eng.rds")
# write_rds(pred_lm_low, "results/pred_lm_low_eng.rds")
# write_rds(pred_lm_high, "results/pred_lm_high_eng.rds")
# 
# write_rds(pred_lgbm, "results/pred_lgbm_eng.rds")
# write_rds(pred_lgbm_low, "results/pred_lgbm_low_eng.rds")
# write_rds(pred_lgbm_high, "results/pred_lgbm_high_eng.rds")
# 
# write_rds(pred_xgbm, "results/pred_xgbm_eng.rds")
# write_rds(pred_xgbm_low, "results/pred_xgbm_low_eng.rds")
# write_rds(pred_xgbm_high, "results/pred_xgbm_high_eng.rds")


## load data

pred_snaive <- read_rds("results/pred_snaive_eng.rds")
pred_arima <- read_rds("results/pred_arima_eng.rds")
pred_ets <- read_rds("results/pred_ets_eng.rds")

pred_lm <- read_rds("results/pred_lm_eng.rds")
pred_lm_low <- read_rds("results/pred_lm_low_eng.rds")
pred_lm_high <- read_rds("results/pred_lm_high_eng.rds")

pred_lgbm <- read_rds("results/pred_lgbm_eng.rds")
pred_lgbm_low <- read_rds("results/pred_lgbm_low_eng.rds")
pred_lgbm_high <- read_rds("results/pred_lgbm_high_eng.rds")

pred_xgbm <- read_rds("results/pred_xgbm_eng.rds")
pred_xgbm_low <- read_rds("results/pred_xgbm_low_eng.rds")
pred_xgbm_high <- read_rds("results/pred_xgbm_high_eng.rds")


nurse <- read_rds("data/eng_workforce_master_updated.rds") ## to get levels


## change the column names

#snaive

new_col_names <- c("org_code", "date", "prediction") 

for (i in 4:2001) {
  new_col_names[i] <- str_sub(colnames(pred_snaive[i]), -7, -1)
}

names(pred_snaive) <- new_col_names

pred_snaive <- pred_snaive %>% mutate(model = "snaive")


#arima

new_col_names <- c("org_code", "date", "prediction") 

for (i in 4:2001) {
  new_col_names[i] <- str_sub(colnames(pred_arima[i]), -7, -1)
}

names(pred_arima) <- new_col_names

pred_arima <- pred_arima %>% mutate(model = "arima")


#ets

new_col_names <- c("org_code", "date", "prediction") 

for (i in 4:2001) {
  new_col_names[i] <- str_sub(colnames(pred_ets[i]), -7, -1)
}

names(pred_ets) <- new_col_names

pred_ets <- pred_ets %>% mutate(model = "ets")


#lm

new_col_names <- c("org_code", "date", "prediction") 

for (i in 4:2001) {
  new_col_names[i] <- str_sub(colnames(pred_lm[i]), -7, -1)
}

names(pred_lm) <- new_col_names

pred_lm <- pred_lm %>% mutate(model = "lm")


#lm_high

new_col_names <- c("org_code", "date", "prediction") 

for (i in 4:2001) {
  new_col_names[i] <- str_sub(colnames(pred_lm_high[i]), -7, -1)
}

names(pred_lm_high) <- new_col_names

pred_lm_high <- pred_lm_high %>% mutate(model = "lm_high")


#lm_low

new_col_names <- c("org_code", "date", "prediction") 

for (i in 4:2001) {
  new_col_names[i] <- str_sub(colnames(pred_lm_low[i]), -7, -1)
}

names(pred_lm_low) <- new_col_names

pred_lm_low <- pred_lm_low %>% mutate(model = "lm_low")


#lgbm

new_col_names <- c("org_code", "date", "prediction") 

for (i in 4:2001) {
  new_col_names[i] <- str_sub(colnames(pred_lgbm[i]), -7, -1)
}

names(pred_lgbm) <- new_col_names

pred_lgbm <- pred_lgbm %>% mutate(model = "lgbm")


#lgbm_high

new_col_names <- c("org_code", "date", "prediction") 

for (i in 4:2001) {
  new_col_names[i] <- str_sub(colnames(pred_lgbm_high[i]), -7, -1)
}

names(pred_lgbm_high) <- new_col_names

pred_lgbm_high <- pred_lgbm_high %>% mutate(model = "lgbm_high")


#lgbm_low

new_col_names <- c("org_code", "date", "prediction") 

for (i in 4:2001) {
  new_col_names[i] <- str_sub(colnames(pred_lgbm_low[i]), -7, -1)
}

names(pred_lgbm_low) <- new_col_names

pred_lgbm_low <- pred_lgbm_low %>% mutate(model = "lgbm_low")


#xgbm

new_col_names <- c("org_code", "date", "prediction") 

for (i in 4:2001) {
  new_col_names[i] <- str_sub(colnames(pred_xgbm[i]), -7, -1)
}

names(pred_xgbm) <- new_col_names

pred_xgbm <- pred_xgbm %>% mutate(model = "xgbm")


#xgbm_high

new_col_names <- c("org_code", "date", "prediction") 

for (i in 4:2001) {
  new_col_names[i] <- str_sub(colnames(pred_xgbm_high[i]), -7, -1)
}

names(pred_xgbm_high) <- new_col_names

pred_xgbm_high <- pred_xgbm_high %>% mutate(model = "xgbm_high")


#xgbm_low

new_col_names <- c("org_code", "date", "prediction") 

for (i in 4:2001) {
  new_col_names[i] <- str_sub(colnames(pred_xgbm_low[i]), -7, -1)
}

names(pred_xgbm_low) <- new_col_names

pred_xgbm_low <- pred_xgbm_low %>% mutate(model = "xgbm_low")



### create master file

# crate levels

org_levels <- nurse %>% 
  
  select("org_code", "org_name", "nhse_region_code", "nhse_region_name") %>%
  
  unique()



nurse_pred <- bind_rows(pred_snaive, pred_arima, pred_ets,
                        pred_lm, pred_lm_high, pred_lm_low,
                        pred_lgbm, pred_lgbm_high, pred_lgbm_low)
  
  
  
nurse_final_pred <- nurse_pred %>% left_join(org_levels, by = "org_code") %>%
  
  select(date, org_code, org_name, nhse_region_code, nhse_region_name, model, everything()) %>%
  
  janitor::clean_names()


write_rds(nurse_final_pred, "results/nurse_pred_master_eng.rds")
