### exploratory analysis - Eng

library(tidyverse)
library(tsibble)
library(fpp3)
library(corrplot)
library(GGally)
library(timetk)
library(astsa)

#load final datset


nurse <- read_rds("data/eng_workforce_master.rds") 


## create the tsibble 

nurse_tsibble <- nurse %>% as_tsibble(index = date,
                                           key = c(org_name, nhse_region_name))

nurse_hts <- nurse_tsibble %>%
  
  aggregate_key((nhse_region_name/org_name), headcount = sum(hc)) 



############# plot the timeseries ###################

no_x_axis <- theme(
  axis.title.x = element_blank(),
  axis.text.x = element_blank(),
  axis.ticks.x = element_blank()
)

p_total <- nurse_hts %>% filter_index(. ~ "2022 Dec") %>%
    filter(is_aggregated(nhse_region_name), is_aggregated(org_name)) %>%
    autoplot(headcount) +
    labs(x = "", y = "Headcount") +
    ggthemes::scale_color_colorblind() +
    ggthemes::theme_few() +
    no_x_axis

p_region <- nurse_hts %>% filter_index(. ~ "2022 Dec") %>%
    filter(!is_aggregated(nhse_region_name), is_aggregated(org_name)) %>%
    as_tibble() %>%
    select(-org_name) %>%
    group_by(date, nhse_region_name) %>%
    summarise(headcount = sum(headcount), .groups = "drop") %>%
    ggplot(aes(x = date, y = headcount, color = factor(nhse_region_name))) +
    geom_line() +
    labs(y = "Headcount", color = "NHSE Region")  +
    ggthemes::scale_color_colorblind() +
    ggthemes::theme_few() +
    no_x_axis

p_board <- nurse_tsibble %>% filter_index(. ~ "2022 Dec") %>%
  filter(org_name %in% sample(nurse_tsibble$org_name,8)) %>%
  group_by(org_name) %>%
  ggplot(aes(x = date, y = hc, color = factor(org_name))) +
  geom_line() +
  # facet_wrap(vars(factor(region)), scales = "free_y") +
  labs(y = "Headcount", color = "Integrated Care Board") +
  ggthemes::scale_color_colorblind() +
  ggthemes::theme_few() +
  no_x_axis

p_board_percent <- nurse_tsibble %>% filter_index(. ~ "2022 Dec") %>%
  group_by(nhse_region_name) %>%
  summarise(hc = sum(hc)) %>%
  group_by(nhse_region_name) %>%
  mutate(hc_change = ((hc - first(hc))/first(hc)) * 100) %>%
  ggplot(aes(x = date, y = hc_change, color = factor(nhse_region_name))) +
  geom_line() +
  labs(y = "Percentage change in headcount compared to June 2018", color = "NHSE Region")  +
  #facet_wrap(vars(factor(nhse_region_name)), scales = "free_y") +
  ggthemes::scale_color_colorblind() +
  ggthemes::theme_few() +
  no_x_axis

p_org_percent <- nurse_tsibble %>% filter_index(. ~ "2022 Dec") %>%
  group_by(org_name) %>%
  summarise(hc = sum(hc)) %>%
  group_by(org_name) %>%
  mutate(hc_change = ((hc - first(hc))/first(hc)) * 100) %>%
  filter(org_name %in% sample(nurse_tsibble$org_name,7)) %>%
  ggplot(aes(x = date, y = hc_change, color = factor(org_name))) +
  geom_line() +
  labs(y = "Percentage change in headcount compared to June 2018", color = "Integrated Care Board")  +
  #facet_wrap(vars(factor(org_name)), scales = "free_y") +
  ggthemes::scale_color_colorblind() +
  ggthemes::theme_few() +
  no_x_axis

p_total /
  p_region /
  p_board /
  p_board_percent /
  p_org_percent /
  
###########################################

## calculate summary

nurse_growth <- nurse_tsibble %>% ungroup() %>% filter_index(. ~ "2022 Dec") %>%
  summarise(hc = sum(hc)) %>%
  mutate(hc_change = ((hc - first(hc))/first(hc)) * 100) %>% summarise(mean_hc_change = mean(hc_change))

nurse_growth_mean <- mean(nurse_growth$mean_hc_change)

########################################################


#evaluate the trends and seasonality

nurse_hts %>% mutate(date = yearmonth(date)) %>%
  features(headcount, feat_stl) %>%
  ggplot(aes(x = trend_strength, y = seasonal_strength_year)) +
  geom_point(alpha = 0.25) +
  labs(x = "Strength of trend", y = "Strength of yearly seasonality")+
  ggthemes::theme_few()


#########################################################


## plot acf, pacf, CCFs plots


# orgs = sample(nurse_tsibble$org_code,6)

orgs = c("RXX", "RWX", "RP1", "RGD", "R1L", "RXY")

## plot CCFs

## staff related

nurse_lags <- nurse %>% filter(year < 2023) %>% mutate(date = yearmonth(date)) %>%
  
  select(date, hc, org_code, nhse_region_code, hc_leavers, hc_joiners, sickness_absence_rate_percent,
         number_of_vacancies, garduates_per_1000_inhabitants, MHS01, MHS32, MHS29, MHS07, ucas_acceptance_rate,
         IFTN, SFTN, international_joiners_mental_health) 


nurse_lags %>%
  
  filter(org_code %in% orgs) %>%
  
  group_by(org_code) %>% rename(absent_rate = sickness_absence_rate_percent, vacancies = number_of_vacancies) %>%
  
  plot_acf_diagnostics(
    date, hc,                        # ACF & PACF
    .ccf_vars    = c(hc_leavers, hc_joiners, absent_rate,
                     vacancies), # CCFs
    .lags        = 48,
    .interactive = FALSE
  )


## Graduates

nurse_lags %>%
  
  filter(org_code %in% orgs) %>%
  
  group_by(org_code) %>% rename(graduates = garduates_per_1000_inhabitants, ucas = ucas_acceptance_rate) %>%
  
  plot_acf_diagnostics(
    date, hc,                        # ACF & PACF
    .ccf_vars    = c(graduates, ucas), # CCFs
    .lags        = 48,
    .interactive = FALSE
  )

## foreign staff related

nurse_lags %>%
  
  filter(org_code %in% orgs) %>%
  
  group_by(org_code) %>% rename(joiners = international_joiners_mental_health) %>%
  
  plot_acf_diagnostics(
    date, hc,                        # ACF & PACF
    .ccf_vars    = c(IFTN, SFTN, joiners), # CCFs
    .lags        = 48,
    .interactive = FALSE
  )


## patient realted

nurse_lags %>%
  
  filter(org_code %in% orgs) %>%
  
  group_by(org_code) %>%
  
  plot_acf_diagnostics(
    date, hc,                        # ACF & PACF
    .ccf_vars    = c(MHS01, MHS07, MHS29, MHS32), # CCFs
    .lags        = 48,
    .interactive = FALSE
  )


#### Plot lag plots

## Create indivial series for plotting

headcount <- (nurse_lags$hc)

leavers <- (nurse_lags$hc_leavers)

joiners <- (nurse_lags$hc_joiners)

absence_rate <- (nurse_lags$sickness_absence_rate_percent)

vacancies <- (nurse_lags$number_of_vacancies)

graduates <- (nurse_lags$garduates_per_1000_inhabitants)

MHS01 <- (nurse_lags$MHS01)

MHS32 <- (nurse_lags$MHS32)

MHS07 <- (nurse_lags$MHS07)

MHS29 <- (nurse_lags$MHS29)

ucas <- (nurse_lags$ucas_acceptance_rate)

IFTN <- (nurse_lags$IFTN)

SFTN <- (nurse_lags$SFTN)

Intr_joiners <- (nurse_lags$international_joiners_mental_health)



lag1.plot(headcount, 6, col=5, corr = F) ## Headcount

lag2.plot(leavers, headcount, 5,col=5, corr = T, smooth = F) ## Headcount vs leavers

lag2.plot(joiners, headcount, 4,col=5, corr = T, smooth = F) ## Headcount vs joiners

lag2.plot(absence_rate, headcount, 4,col=5, corr = T, smooth = F) ## Headcount vs absence_rate

lag2.plot(vacancies, headcount, 4,col=5, corr = T, smooth = F) ## Headcount vs vacancies

lag2.plot(graduates, headcount, 4,col=5, corr = T, smooth = F) ## Headcount vs graduates

lag2.plot(MHS01, headcount, 4,col=5, corr = T, smooth = T) ## Headcount vs MHS01

lag2.plot(MHS32, headcount, 4,col=5, corr = T, smooth = T) ## Headcount vs MHS32

lag2.plot(MHS07, headcount, 4,col=5, corr = T, smooth = T) ## Headcount vs MHS07

lag2.plot(MHS29, headcount, 4,col=5, corr = T, smooth = T) ## Headcount vs MHS29

lag2.plot(ucas, headcount, 4,col=5, corr = T, smooth = F) ## Headcount vs ucas

lag2.plot(IFTN, headcount, 4,col=5, corr = T, smooth = F) ## Headcount vs IFTN

lag2.plot(SFTN, headcount, 4,col=5, corr = T, smooth = F) ## Headcount vs SFTN

lag2.plot(Intr_joiners, headcount, 4,col=5, corr = T, smooth = F) ## Headcount vs Intr_joiners


## based on the observations, it seems lag 1 is the best lag feature for headcount as well as for predictors, also the lag 1


## lasso plot

lasso <- read.csv("results/lasso_features_eng.csv")

lasso %>%
  
  arrange(Importance) %>% 
  
  ggplot(aes(x = reorder(Feature, Importance), y = Importance)) +
  
  geom_bar(stat="identity") +
  
  labs(y = "Feature importance", x = "Factor")  +

  ggthemes::theme_few() +
  
  theme(axis.text.y = element_text(size = 7)) +
  
  coord_flip()


#######################################

## plot the correlation matrix to see the correlation between target variable and predictors

## selecting only the numeric values

nurse_corr <- nurse_tsibble %>% filter_index(. ~ "2022 Dec") %>% 
  
  select(!c(hc_lag2, hc_lag3, hc_lag4,
            hc_leavers_lag2, hc_leavers_lag3, hc_leavers_lag4,
            hc_joiners_lag2, hc_joiners_lag3, hc_joiners_lag3,
            MHS32_lag2,MHS32_lag3,
            MHS01_lag2, MHS01_lag3,
            MHS07_lag2, MHS07_lag3,
            MHS29_lag2, MHS29_lag3,
            ucas_acceptance_lag1, ucas_acceptance_lag2, ucas_acceptance_lag3,
            time_window)) %>%
  
  rename(absence_rate = sickness_absence_rate_percent,
         local_spend = local_spend_on_mental_health,
         commissioning_spend = nhse_specialised_commissioning_spend,
         ccg_spend = ccg_spend_on_mh_as_a_percent_of_ccg_base_allocations,
         total_spend = total_nhs_spend_on_mental_health,
         graduates = garduates_per_1000_inhabitants,
         graduates_lag1 = graduates_per_1000_inhabitants_lag1) %>%
  
  as_tibble() %>%
  
  ungroup() %>%
  
  select_if(is.numeric) %>%
  
  select(!c(year, month))


corr <- cor(nurse_corr) ## correlation matrix

ggcorr(nurse_corr, hjust = 1, size = 2.5, color = "black", palette = "Blues",
       
       nbreaks = 8, label = T, label_size = 2, label_color = "black") ## corr plot


# ##create scatterplot matrix with target variable and predictors to see the correlation
# 
# nurse_corr %>% GGally::ggpairs(columns = c(1, 4:7)) #hc with its lags
# 
# nurse_corr %>% GGally::ggpairs(columns = c(1, 2, 8:11)) #hc with leavers and its lags
# 
# nurse_corr %>% GGally::ggpairs(columns = c(1, 3, 12:15)) #hc with joiners and its lags
# 
# nurse_corr %>% GGally::ggpairs(columns = c(1, 16:18)) #hc with other staff related predictors
# 
# nurse_corr %>% GGally::ggpairs(columns = c(1, 19:22)) #hc with investments
# 
# nurse_corr %>% GGally::ggpairs(columns = c(1, 23:24, 53:57)) #hc with nursing education
# 
# nurse_corr %>% GGally::ggpairs(columns = c(1, 25:40)) #hc with nursing education
# 
# nurse_corr %>% GGally::ggpairs(columns = c(1, 41:50)) #hc with population
# 
# nurse_corr %>% GGally::ggpairs(columns = c(1, 51, 52, 58)) #hc with international joiners


## plot future values of predictors

pred <- read_rds("data/eng_workforce_master_updated.rds") 

orgs <- c("RW1", "RPG", "RXT")


## hc leavers and joiners

pred %>%
  
  select(year, nhse_region_name, hc_leavers, hc_joiners) %>%
  
  group_by(year, nhse_region_name) %>%
  
  summarise(hc_leavers = sum(hc_leavers), hc_joiners = sum(hc_joiners)) %>%
  
  pivot_longer(c(-year, -nhse_region_name), names_to = "variable", values_to = "value") %>%
  
  ggplot(aes(as.factor(year), value, color = variable)) + 
  
  geom_line(aes(group=variable), lwd = 0.75) +
  
  labs(x = "Year", y = "Headcount", color = "Predictor") +
  
  scale_color_manual(labels = c("Leavers", "Joiners"), values=c("#D55E00", "#0072B2")) +
  
  theme_bw() +
  
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=5)) +
  
  facet_wrap(vars(factor(nhse_region_name)), scales = "free_y")



## vacancies

pred %>% mutate(date = yearmonth(date)) %>%
  
  group_by(year, nhse_region_name) %>%
  
  summarise(number_of_vacancies = mean(number_of_vacancies)*4) %>%
  
  pivot_longer(c(-year, -nhse_region_name), names_to = "variable", values_to = "value") %>%
  
  ggplot(aes(as.factor(year), value, color = variable)) + 
  
  geom_line(aes(group=1), lwd = 0.75) +
  
  labs(x = "Year", y = "Number", color = "Predictor") +
  
  scale_color_manual(labels = c("Vacancies"), values=c("#E69F00")) +
  
  theme_bw() +
  
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  
  facet_wrap(vars(factor(nhse_region_name)), scales = "free_y")



## absent rate


pred %>% mutate(date = yearmonth(date)) %>%
  
  group_by(year, nhse_region_name) %>%
  
  summarise(sickness_absence_rate_percent = mean(sickness_absence_rate_percent)) %>%
  
  pivot_longer(c(-year, -nhse_region_name), names_to = "variable", values_to = "value") %>%
  
  ggplot(aes(as.factor(year), value, color = variable)) + 
  
  geom_line(aes(group=1), lwd = 0.75) +
  
  labs(x = "Year", y = "Percentage", color = "Predictor") +
  
  scale_color_manual(labels = c("Absence rate"), values=c("#009E73")) +
  
  theme_bw() +
  
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  
  facet_wrap(vars(factor(nhse_region_name)), scales = "free_y")



## graduate

pred %>% mutate(date = yearmonth(date)) %>%
  
  group_by(year, nhse_region_name) %>%
  
  summarise(garduates_per_1000_inhabitants = mean(garduates_per_1000_inhabitants)) %>%
  
  pivot_longer(c(-year, -nhse_region_name), names_to = "variable", values_to = "value") %>%
  
  ggplot(aes(as.factor(year), value, color = variable)) + 
  
  geom_line(aes(group=1), lwd = 0.75) +
  
  labs(x = "Year", y = "Graduants per 1000 people", color = "Predictor") +
  
  scale_color_manual(labels = c("Nursing Graduates"), values=c("#D55E00")) +
  
  theme_bw() +
  
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  
  facet_wrap(vars(factor(nhse_region_name)), scales = "free_y")


## UCAS

pred %>% filter(year>2017) %>%
  
  mutate(date = yearmonth(date)) %>%
  
  group_by(year, nhse_region_name) %>%
  
  summarise(ucas_acceptance_rate = mean(ucas_acceptance_rate)/12, .groups = "drop") %>%
  
  pivot_longer(c(-year, -nhse_region_name), names_to = "variable", values_to = "value") %>%
  
  ggplot(aes(as.factor(year), value, color = variable)) + 
  
  geom_line(aes(group=1), lwd = 0.75) +
  
  labs(x = "Year", y = "UCAS acceptance rate", color = "Predictor") +
  
  scale_color_manual(labels = c("UCAS acceptance rate"), values=c("#0072B2")) +
  
  theme_bw() +
  
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  
  facet_wrap(vars(factor(nhse_region_name)), scales = "free_y")


## IFTN


pred %>% mutate(date = yearmonth(date)) %>%
  
  group_by(year, nhse_region_name) %>%
  
  summarise(IFTN = mean(IFTN)*12) %>%
  
  pivot_longer(c(-year, -nhse_region_name), names_to = "variable", values_to = "value") %>%
  
  ggplot(aes(as.factor(year), value, color = variable)) + 
  
  geom_line(aes(group=1), lwd = 0.75) +
  
  labs(x = "Year", y = "Headcount", color = "Predictor") +
  
  scale_color_manual(labels = c("IFTN"), values=c("#E69F00")) +
  
  theme_bw() +
  
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  
  facet_wrap(vars(factor(nhse_region_name)), scales = "free_y")


## Iternational joiners


pred %>% mutate(date = yearmonth(date)) %>%
  
  group_by(year, nhse_region_name) %>%
  
  summarise(international_joiners_mental_health = mean(international_joiners_mental_health)*12) %>%
  
  pivot_longer(c(-year, -nhse_region_name), names_to = "variable", values_to = "value") %>%
  
  ggplot(aes(as.factor(year), value, color = variable)) + 
  
  geom_line(aes(group=1), lwd = 0.75) +
  
  labs(x = "Year", y = "Headcount", color = "Predictor") +
  
  scale_color_manual(labels = c("Int. joiners"), values=c("#E69F00")) +
  
  theme_bw() +
  
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  
  facet_wrap(vars(factor(nhse_region_name)), scales = "free_y")


## spend


pred %>% mutate(date = yearmonth(date)) %>%
  
  group_by(year, nhse_region_name) %>%
  
  summarise(ccg_spend_on_mh_as_a_percent_of_ccg_base_allocations = mean(ccg_spend_on_mh_as_a_percent_of_ccg_base_allocations)) %>%
  
  pivot_longer(c(-year, -nhse_region_name), names_to = "variable", values_to = "value") %>%
  
  ggplot(aes(as.factor(year), value, color = variable)) + 
  
  geom_line(aes(group=1), lwd = 0.75) +
  
  labs(x = "Year", y = "Percentage", color = "Predictor") +
  
  scale_color_manual(labels = c("CCG spend"), values=c("#E69F00")) +
  
  theme_bw() +
  
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  
  facet_wrap(vars(factor(nhse_region_name)), scales = "free_y")


## MHS01


pred %>%
  
  select(year, nhse_region_name, MHS01, MHS01_high, MHS01_low) %>%
  
  group_by(year, nhse_region_name) %>%
  
  summarise(MHS01 = mean(MHS01)*12, MHS01_high = mean(MHS01_high)*12, MHS01_low = mean(MHS01_low)*12) %>%
  
  pivot_longer(c(-year, -nhse_region_name), names_to = "variable", values_to = "value") %>%
  
  ggplot(aes(as.factor(year), value, color = variable)) + 
  
  geom_line(aes(group=variable), lwd = 0.75) +
  
  labs(x = "Year", y = "Number", color = "Predictor") +
  
  scale_color_manual(labels = c("MHS01", "Upper bound", "Lower bound"), values=c("#009E73", "#D55E00", "gray45")) +
  
  theme_bw() +
  
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=5)) +
  
  facet_wrap(vars(factor(nhse_region_name)), scales = "free_y")


## MHS07


pred %>%
  
  select(year, nhse_region_name, MHS07, MHS07_high, MHS07_low) %>%
  
  group_by(year, nhse_region_name) %>%
  
  summarise(MHS07 = sum(MHS07), MHS07_high = sum(MHS07_high), MHS07_low = sum(MHS07_low)) %>%
  
  pivot_longer(c(-year, -nhse_region_name), names_to = "variable", values_to = "value") %>%
  
  ggplot(aes(as.factor(year), value, color = variable)) + 
  
  geom_line(aes(group=variable), lwd = 0.75) +
  
  labs(x = "Year", y = "Number", color = "Predictor") +
  
  scale_color_manual(labels = c("MHS07", "Upper bound", "Lower bound"), values=c("#009E73", "#D55E00", "gray45")) +
  
  theme_bw() +
  
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=5)) +
  
  facet_wrap(vars(factor(nhse_region_name)), scales = "free_y")


## MHS29


pred %>%
  
  select(year, nhse_region_name, MHS29, MHS29_high, MHS29_low) %>%
  
  group_by(year, nhse_region_name) %>%
  
  summarise(MHS29 = sum(MHS29), MHS29_high = sum(MHS29_high), MHS29_low = sum(MHS29_low)) %>%
  
  pivot_longer(c(-year, -nhse_region_name), names_to = "variable", values_to = "value") %>%
  
  ggplot(aes(as.factor(year), value, color = variable)) + 
  
  geom_line(aes(group=variable), lwd = 0.75) +
  
  labs(x = "Year", y = "Number", color = "Predictor") +
  
  scale_color_manual(labels = c("MHS29", "Upper bound", "Lower bound"), values=c("#009E73", "#D55E00", "gray45")) +
  
  theme_bw() +
  
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=5)) +
  
  facet_wrap(vars(factor(nhse_region_name)), scales = "free_y")


## MHS32


pred %>%
  
  select(year, nhse_region_name, MHS32, MHS32_high, MHS32_low) %>%
  
  group_by(year, nhse_region_name) %>%
  
  summarise(MHS32 = sum(MHS32), MHS32_high = sum(MHS32_high), MHS32_low = sum(MHS32_low)) %>%
  
  pivot_longer(c(-year, -nhse_region_name), names_to = "variable", values_to = "value") %>%
  
  ggplot(aes(as.factor(year), value, color = variable)) + 
  
  geom_line(aes(group=variable), lwd = 0.75) +
  
  labs(x = "Year", y = "Number", color = "Predictor") +
  
  scale_color_manual(labels = c("MHS32", "Upper bound", "Lower bound"), values=c("#009E73", "#D55E00", "gray45")) +
  
  theme_bw() +
  
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=5)) +
  
  facet_wrap(vars(factor(nhse_region_name)), scales = "free_y")





