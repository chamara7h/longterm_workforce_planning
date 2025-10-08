#Tidy data preparation - Eng

library(tidyverse)
library(tsibble)
library(janitor)
library(lubridate)
library(data.table)
library(fpp3)

#Load data

#nurse_df <- read_xlsx("data/system/England_data/HCHS_Mental_Health_Workforce.xlsx", sheet = 2) %>% janitor::clean_names() ## data available only from 2019 onwards

nurse_df <- read.csv("data/system/England_data/NHS Workforce Statistics, March 2023.csv") %>% janitor::clean_names()

turnover <- read.csv("data/system/England_data/Monthly_turnover.csv") %>% janitor::clean_names()

vacancy <- read.csv("data/system/England_data/Vacancy.csv")

investment <- read.csv("data/system/England_data/Investments_mental.csv") %>% janitor::clean_names()

graduates <- read.csv("data/system/England_data/nurse_graduates.csv") %>% janitor::clean_names()

patient_df <- read.csv("data/system/England_data/MHSDS Time_Series_data_Apr_2016_MarPrf_2023.csv") %>% janitor::clean_names()

population <- read.csv("data/system/England_data/Population_projections.csv") %>% janitor::clean_names()

nurse_migration <- read.csv("data/system/nurse_migration.csv") %>% janitor::clean_names()

International_joiners <- read.csv("data/system/England_data/International_joiners.csv") %>% janitor::clean_names()

UCAS_acceptance <- read.csv("data/system/England_data/UCAS_acceptance.csv") %>% janitor::clean_names()


##absent rate

feb23 <- read.csv(url("https://files.digital.nhs.uk/01/C70937/NHS%20Sickness%20Absence%20rates%20CSV%2C%20February%202023.csv")) %>% janitor::clean_names() %>%
  
  filter(org_type == "Mental Health") %>% select(date, org_code, sickness_absence_rate_percent) %>% 
  
  mutate(date = yearmonth(as.Date(date, "%d/%m/%Y")))


Jan23 <- read.csv(url("https://files.digital.nhs.uk/9C/E7E48E/NHS%20Sickness%20Absence%20rates%20CSV%2C%20January%202023.csv")) %>% janitor::clean_names() %>%
  
  filter(org_type == "Mental Health") %>% select(date, org_code, sickness_absence_rate_percent) %>% 
  
  mutate(date = yearmonth(as.Date(date, "%d/%m/%Y")))


Dec22 <- read.csv(url("https://files.digital.nhs.uk/AD/B40BC4/NHS%20Sickness%20Absence%20rates%20CSV%2C%20December%202022.csv")) %>% janitor::clean_names() %>%
  
  filter(org_type == "Mental Health") %>% select(date, org_code, sickness_absence_rate_percent) %>% 
  
  mutate(date = yearmonth(as.Date(date, "%d/%m/%Y")))


Nov22 <- read.csv(url("https://files.digital.nhs.uk/7B/4BA143/NHS%20Sickness%20Absence%20rates%20CSV%2C%20November%202022.csv")) %>% janitor::clean_names() %>%
  
  filter(org_type == "Mental Health") %>% select(date, org_code, sickness_absence_rate_percent) %>% 
  
  mutate(date = yearmonth(as.Date(date, "%d/%m/%Y")))


Oct22 <- read.csv(url("https://files.digital.nhs.uk/DC/CED086/NHS%20Sickness%20Absence%20rates%20CSV%2C%20October%202022.csv")) %>% janitor::clean_names() %>%
  
  filter(org_type == "Mental Health") %>% select(date, org_code, sickness_absence_rate_percent) %>% 
  
  mutate(date = yearmonth(as.Date(date, "%d/%m/%Y")))


Sep22 <- read.csv(url("https://files.digital.nhs.uk/00/B1BCE8/NHS%20Sickness%20Absence%20rates%20CSV%2C%20September%202022.csv")) %>% janitor::clean_names() %>%
  
  filter(org_type == "Mental Health") %>% select(date, org_code, sickness_absence_rate_percent) %>% 
  
  mutate(date = yearmonth(as.Date(date, "%d/%m/%Y")))


Aug22 <- read.csv(url("https://files.digital.nhs.uk/39/B64543/NHS%20Sickness%20Absence%20rates%20CSV%2C%20August%202022.csv")) %>% janitor::clean_names() %>%
  
  filter(org_type == "Mental Health") %>% select(date, org_code, sickness_absence_rate_percent) %>% 
  
  mutate(date = yearmonth(as.Date(date, "%d/%m/%Y")))


Jul22 <- read.csv(url("https://files.digital.nhs.uk/93/CE5B3B/NHS%20Sickness%20Absence%20rates%20CSV%2C%20July%202022.csv")) %>% janitor::clean_names() %>%
  
  filter(org_type == "Mental Health") %>% select(date, org_code, sickness_absence_rate_percent) %>% 
  
  mutate(date = yearmonth(as.Date(date, "%d/%m/%Y")))


Jun22 <- read.csv(url("https://files.digital.nhs.uk/39/163FFC/NHS%20Sickness%20Absence%20rates%20CSV%2C%20June%202022.csv")) %>% janitor::clean_names() %>%
  
  filter(org_type == "Mental Health") %>% select(date, org_code, sickness_absence_rate_percent) %>% 
  
  mutate(date = yearmonth(as.Date(date, "%d/%m/%Y")))


May22 <- read.csv(url("https://files.digital.nhs.uk/0B/9C9B7F/ESR_ABSENCE_CSV_NHSE.csv")) %>% janitor::clean_names() %>%
  
  filter(org_type == "Mental Health") %>% select(date, org_code, sa_rate) %>%
  
  rename(sickness_absence_rate_percent = sa_rate) %>% 
  
  mutate(date = yearmonth(date)) %>%
  
  as_tsibble(index = date, key = org_code) %>%
  
  filter_index("2018 June" ~ .)



absent_list = list(feb23, Jan23, Dec22, Nov22, Oct22, Sep22, Aug22, Jul22, Jun22, May22)


absent_rate <- do.call("rbind", absent_list)





# ##clean nurse df
# 
# ##we only have monthly data 2022 onwards


## clean annual turnover dataset

##leavers

leavers_cleaned <- turnover %>% filter(cluster_group == "Mental Health",
                                        type == "Leavers",
                                        benchmark_group == "Mental Health and Learning Disability",
                                        staff_group == "Nurses & health visitors") %>%
  
  mutate(yearmonth = paste0(str_sub(period, 1,4),"-", str_sub(period, 5,6))) %>% ##to create date column
  
  mutate(yearmonth = yearmonth(yearmonth)) %>%
  
  mutate(id = paste0(yearmonth, org_code)) %>%
  
  rename(hc_leavers = hc, fte_leavers = fte) %>%
  
  select(id, hc_leavers, fte_leavers)


##Joiners

joiners_cleaned <- turnover %>% filter(cluster_group == "Mental Health",
                                       type == "Joiners",
                                       benchmark_group == "Mental Health and Learning Disability",
                                       staff_group == "Nurses & health visitors") %>%
  
  mutate(yearmonth = paste0(str_sub(period, 1,4),"-", str_sub(period, 5,6))) %>% ##to create date column
  
  mutate(yearmonth = yearmonth(yearmonth)) %>%
  
  mutate(id = paste0(yearmonth, org_code)) %>%
  
  rename(hc_joiners = hc, fte_joiners = fte) %>%
  
  select(id, hc_joiners, fte_joiners)



##Monthly workforce - Nurses and health visitors 

#turn over data also consists with monthly staff levels

workforce_cleaned <- turnover %>% filter(cluster_group == "Mental Health",
                                       type == "Denoms",
                                       benchmark_group == "Mental Health and Learning Disability",
                                       staff_group == "Nurses & health visitors") %>%
  
  mutate(yearmonth = yearmonth(as.Date(period, "%d/%m/%Y"))) %>%
  
  mutate(id = paste0(yearmonth, org_code))


## merge workforce, leavers and joiners data together

workforce_tidy <- left_join(workforce_cleaned, leavers_cleaned, by='id') %>%
  
  left_join(., joiners_cleaned, by='id') %>%
  
  mutate(across(where(is.numeric), ~replace(., is.na(.), 0))) %>% ## adding zero assuming there are no joiners or leavers
  
  select(!c("period", "type", "id")) %>%
  
  select(yearmonth, everything()) %>%
  
  mutate(org_name = case_when(org_name == "North West Boroughs Healthcare NHS Foundation Trust" ~ "Mersey Care NHS Foundation Trust",
                              org_name == "Dudley and Walsall Mental Health Partnership NHS Trust" ~ "Black Country Healthcare NHS Foundation Trust",
                              TRUE ~ as.character(org_name)),
         
         org_code = case_when(org_code == "RTV" ~ "RW4",
                              org_code == "RYK-X" ~ "TAJ",
                              TRUE ~ as.character(org_code))) %>%
  
  group_by(yearmonth, org_code, org_name, nhse_region_code, nhse_region_name, ics_code,
           ics_name, cluster_group, benchmark_group, staff_group) %>%
  
  summarise(across(everything(), sum)) %>% ungroup() %>%
  
  mutate(id = paste0(yearmonth, org_code))


## creating future months for the dataset


test <- workforce_tidy %>% mutate(year = year(yearmonth)) %>%
  
  filter(year == 2021) %>%
  
  select(!id) %>% group_by(yearmonth,
                           org_code,
                           org_name,
                           nhse_region_code,
                           nhse_region_name,
                           ics_code,
                           ics_name,
                           cluster_group,
                           benchmark_group,
                           staff_group) %>%
  
  summarise() %>%
  
  mutate(year = year(yearmonth), month = month(yearmonth)) %>% ungroup()


years <- c(2024, 2025, 2026, 2027, 2028)

df <- test %>% mutate(year = 2023)


for (i in years) {
  
  df1 <- test %>% mutate(year = i)
  
  df <- rbind(df, df1)
  
}


## we have data upto Mar 2023. Thus, I am creating a blank dataframe to add future values 

df_bind <- df %>% mutate(date = sprintf("%04d-%02d", year, month), yearmonth = yearmonth(date)) %>%
  
  select(!c("year", "month", "date")) %>%
  
  mutate(id = paste0(yearmonth, org_code), hc = 0, fte = 0, hc_leavers = 0, fte_leavers = 0, hc_joiners = 0, fte_joiners = 0) %>%
  
  select(yearmonth, id, everything()) %>%
  
  filter(as.character(yearmonth) != "2023 Jan" ,
           as.character(yearmonth) != "2023 Feb",
           as.character(yearmonth) != "2023 Mar")


## bind two dataframe together

workforce_tidy <- rbind(workforce_tidy, df_bind)


## creating lag features


workforce_tidy <- as_tibble(workforce_tidy) %>% arrange(yearmonth) %>%
  
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
         hc_joiners_lag4 = lag(hc_joiners, n = 4, default = 0))
  
  
  
## merge workforce, absent rate together

absent_rate_merge <- absent_rate %>% mutate(id = paste0(date, org_code)) %>%
  
  select(id, sickness_absence_rate_percent)


workforce_tidy <- left_join(workforce_tidy, absent_rate_merge, by = "id")


##Vacancy data

vacancy_cleaned <- vacancy %>%
  
  pivot_longer(-date, names_to = "region", values_to = "number_of_vacancies") %>%
  
  mutate(region = gsub("\\.", " ", region)) %>%
  
  mutate(date = yearmonth(as.Date(date, "%d/%m/%Y")),
         
         number_of_vacancies_lag1 = lag(number_of_vacancies, n = 1, default = 0)) %>%
  
  mutate(number_of_vacancies = number_of_vacancies/3,
         
         number_of_vacancies_lag1 = number_of_vacancies_lag1/3) %>% ## convert quarterly data to monthly assuming vacancies are equally distributed over the period

  mutate(id = paste0(date, region)) %>%
  
  select(id, number_of_vacancies, number_of_vacancies_lag1)
  
  

##merge vacancy data to workforce tidy


workforce_tidy <- workforce_tidy %>% mutate(id = paste0(yearmonth, nhse_region_name)) ##chabge the unique id accordingly


workforce_tidy <- left_join(workforce_tidy, vacancy_cleaned, by = "id") %>% rename(date = yearmonth)





##Creatr month tibble to brake yearly data into months


months <- tibble(year=2018:2023) %>% 
  
  mutate(month=list(1:12)) %>% 
  
  unnest(month) %>% 

  mutate(date = sprintf("%04d-%02d", year, month), date = yearmonth(date)) %>% ##create monthly time frame

  select(date, year) %>% as_tsibble(index = date) %>%
  
  filter_index("2018 Jul" ~ "2023 Mar") %>% as_tibble()




##Merging months and investment data 

invesment_cleaned <- left_join(months, investment, by = "year")

#assume annual investment equally distributed through out the year

invesment_cleaned <- invesment_cleaned %>% mutate(local_spend_on_mental_health = local_spend_on_mental_health/12,
                                                  nhse_specialised_commissioning_spend = nhse_specialised_commissioning_spend/12,
                                                  total_nhs_spend_on_mental_health = total_nhs_spend_on_mental_health/12) %>%
  
  select(!year)


##merge months and graduate data


graduates <- graduates %>% 
  
  mutate(graduates_per_1000_inhabitants_lag1 = lag(garduates_per_1000_inhabitants, n = 1, default = 0))


graduates_cleaned <- months  %>%
  
  left_join(graduates, by = "year") %>%
  
  select(date, garduates_per_1000_inhabitants, graduates_per_1000_inhabitants_lag1) %>%
  
  mutate(garduates_per_1000_inhabitants = garduates_per_1000_inhabitants/12,
         graduates_per_1000_inhabitants_lag1 = graduates_per_1000_inhabitants_lag1/12) ##assuming number of graduates are equally distributed through out the year


##prepare patient related data

org_codes_list <- unique(workforce_tidy$org_code) ##creating a list of org codes

patient_cleaned <- patient_df %>% filter(remark != 1,
                                         breakdown == "PROVIDER"|
                                         breakdown == "Provider" & 
                                         (measure_id == "MHS01"|  #MHS01 - People in contact with mental health services at the end of the RP
                                         measure_id == "MHS32"|  #MHS32 - Referrals starting in the RP  
                                         measure_id == "MHS07"|  #MHS07 - People with an open hospital spell at the end of the RP 
                                         measure_id == "MHS29"))  %>%  #MHS29 - Contacts in the RP

  filter(primary_level %in% org_codes_list) %>%
  
  select(reporting_period_start, primary_level, measure_id, measure_value) %>%
  
  rename(date = reporting_period_start, org_code = primary_level) %>%
  
  mutate(date = yearmonth(as.Date(date, "%d/%m/%Y"))) %>%
  
  mutate(id = paste0(date, org_code)) 


patient_regions <- patient_cleaned %>% group_by(date, org_code, id) %>% summarise()


## creating lag values


patient_tidy <- patient_cleaned %>%
  
  pivot_wider(id_cols = id, names_from = measure_id, values_from = measure_value) %>% 
  
  mutate(MHS32 = as.numeric(MHS32),
         MHS01 = as.numeric(MHS01),
         MHS07 = as.numeric(MHS07),
         MHS29 = as.numeric(MHS29)) %>%
  
    replace(is.na(.), 0) %>%
  
  left_join(patient_regions, by = "id") %>%
  
  group_by(org_code) %>%
  
  mutate(MHS32_lag1 = lag(MHS32, n = 1, default = 0),
         MHS32_lag2 = lag(MHS32, n = 2, default = 0),
         MHS32_lag3 = lag(MHS32, n = 3, default = 0),
         MHS32_lag3 = lag(MHS32, n = 4, default = 0),
         
         MHS01_lag1 = lag(MHS01, n = 1, default = 0),
         MHS01_lag2 = lag(MHS01, n = 2, default = 0),
         MHS01_lag3 = lag(MHS01, n = 3, default = 0),
         MHS01_lag3 = lag(MHS01, n = 4, default = 0), 
         
         MHS07_lag1 = lag(MHS07, n = 1, default = 0),
         MHS07_lag2 = lag(MHS07, n = 2, default = 0),
         MHS07_lag3 = lag(MHS07, n = 3, default = 0),
         MHS07_lag3 = lag(MHS07, n = 4, default = 0),
         
         MHS29_lag1 = lag(MHS29, n = 1, default = 0),
         MHS29_lag2 = lag(MHS29, n = 2, default = 0),
         MHS29_lag3 = lag(MHS29, n = 3, default = 0),
         MHS29_lag3 = lag(MHS29, n = 4, default = 0)) %>%
  
  ungroup() %>%
  
  select(!c(date, org_code))
         
         
##prepare population data


months <- tibble(year=2018:2028) %>% 
  
  mutate(month=list(1:12)) %>% 
  
  unnest(month) %>% 
  
  mutate(date = sprintf("%04d-%02d", year, month), date = yearmonth(date)) %>% ##create monthly time frame
  
  select(date, year)


population_cleaned <- population %>% mutate(nhse_region_name = case_when(area %in% c("East", "England") ~ "East of England",
                                                                         area %in% c("North East", "Yorkshire and The Humber") ~ "North East and Yorkshire",
                                                                         area == "North West" ~ "North West",
                                                                         area %in% c("East Midlands", "West Midlands") ~ "Midlands",
                                                                         area == "London" ~ "London",
                                                                         area == "South East" ~ "South East",
                                                                         area == "South West" ~ "South West")) %>%
  
  filter(age_group == "15-19"|
           age_group == "20-24"|
           age_group == "25-29"|
           age_group == "30-34"|
           age_group == "35-39"|
           age_group == "40-44"|
           age_group == "45-49"|
           age_group == "50-54"|
           age_group == "55-59"|
           age_group == "60-64") %>% 
  
  select(!c("code", "area")) %>% group_by(nhse_region_name, age_group) %>%
  
  summarise(across(everything(), sum)) %>%
  
  pivot_longer(-c("nhse_region_name", "age_group"), names_to = "year", values_to = "population") %>%
  
  mutate(year = str_sub(year, 2,5)) %>%
  
  mutate(year = as.integer(year)) %>%
  
  left_join(months, by = "year") %>%
  
  filter(year < 2029) %>%
  
  mutate(id = paste0(date, nhse_region_name)) %>%
  
  select(id, age_group, population) %>%
  
  mutate(age_group = paste0("age_", age_group))



population_merge <- population_cleaned %>% pivot_wider(id_cols = id, names_from = age_group, values_from = population) %>% 
  
  janitor::clean_names() ## to create columns for each age group


##merge workforce tidy with investment, graduate, patient and popultation data

workforce_tidy <- workforce_tidy %>% left_join(invesment_cleaned, by = "date") %>%
  
  left_join(., graduates_cleaned, by='date') %>%
  
  mutate(id = paste0(date, org_code)) %>%
  
  left_join(., patient_tidy, by='id') %>%
  
  mutate(id = paste0(date, nhse_region_name)) %>%
  
  left_join(., population_merge, by = "id")
  




##merge nursing migration data

nurse_migration_cleaned <- nurse_migration %>% filter(var == "SFTN"|
                                                      var == "IFTN") %>%
  
  select(year, var, value) %>%
  
  pivot_wider(names_from = var, values_from = value,
              names_sep = "_") %>%
  
  right_join(months, by = "year")  %>%
  
  select(date, SFTN, IFTN) %>%
  
  mutate(IFTN = IFTN/12)


##merge migration data with workforce data

workforce_tidy <- workforce_tidy %>% left_join(nurse_migration_cleaned, by = "date")



##ucas acceptance date


months <- tibble(year=2011:2021) %>% 
  
  mutate(month=list(1:12)) %>% 
  
  unnest(month) %>% 
  
  mutate(date = sprintf("%04d-%02d", year, month), date = yearmonth(date)) %>% ##create monthly time frame
  
  select(date, year)


##create lags

## I am using 2011 is the base year for creating the rate

UCAS_acceptance <-   UCAS_acceptance %>%
  
  group_by(region) %>%
  
  mutate(ucas_acceptance_lag1 = lag(ucas_acceptance, n = 1, default = 0),
         ucas_acceptance_rate = round(((ucas_acceptance - ucas_acceptance_lag1)/ ucas_acceptance * 100), digits = 2))



##i am assuming ucas acceptance rate is same throughout the year


UCAS_acceptance_tidy <- months %>% left_join(UCAS_acceptance, by = "year") %>%
  
  select(date, region, ucas_acceptance_rate) %>%
  
  group_by(region) %>%
  
  mutate(ucas_acceptance_lag1 = lag(ucas_acceptance_rate, n = 12, default = 0),
         ucas_acceptance_lag2 = lag(ucas_acceptance_rate, n = 24, default = 0),
         ucas_acceptance_lag3 = lag(ucas_acceptance_rate, n = 36, default = 0),
         ucas_acceptance_lag4 = lag(ucas_acceptance_rate, n = 48, default = 0)) %>%
  
  mutate(id = paste0(date, region)) %>% ungroup() %>%
  
  select(!c("date", "region"))


##international joiners

##In 2021, there were 508 international joiners for mental healthcare out of 23444. Since there is no breakdown available, we too this ratio to calculate international joiners for MC.

International_joiners_tidy <- International_joiners %>% 
  
  mutate(date = yearmonth(as.Date(date, "%d/%m/%Y"))) %>% 
  
  select(date, international_joiners_mental_health) %>% 
  
  as_tsibble(index = date) %>%
  
  filter_index("2018 Jul" ~ .)


##join df together

workforce_tidy <- workforce_tidy %>% left_join(UCAS_acceptance_tidy, by = "id") %>%
  
  left_join(., International_joiners_tidy, by='date')


##write data 

write_rds(workforce_tidy, "data/eng_workforce_tidy.rds")  
                                                                    
