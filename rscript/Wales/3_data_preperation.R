#Tidy data preparation

library(tidyverse)
library(tsibble)
library(janitor)
library(lubridate)
library(data.table)

#Load data

nurse_df <- read_rds("data/system/Wales_data/nursing_by_area_tidy.rds") %>% janitor::clean_names()

LPMHSS <- read_rds("data/system/Wales_data/LPMHSS_tidy.rds") %>% janitor::clean_names()

staff_absent <- read_rds("data/system/Wales_data/staff_absent_tidy.rds") %>% janitor::clean_names()

population <- read_rds("data/system/Wales_data/Population_lhb_tidy.rds") %>% janitor::clean_names()

NHS_expenditure <- read_rds("data/system/Wales_data/NHS_expenditure_tidy.rds") %>% janitor::clean_names()

nurse_grad <- read.csv("data/system/nurse_graduates.csv") %>% janitor::clean_names()

nurse_migration <- read.csv("data/system/nurse_migration.csv") %>% janitor::clean_names()



#Create monthly dataset for nurse

##We assume that yearly staff level remains same throughout the year


##Convert quarterly data to monthly


months <- tibble(year=2019:2022) %>% 
  
  mutate(month=list(1:12)) %>% 
  
  unnest(month) %>% ##create monthly time frame

  mutate(quarter = case_when(month %in% c(1,2,3) ~ 1,
                             month %in% c(4,5,6) ~ 2,
                             month %in% c(7,8,9) ~ 3,
                             month %in% c(10,11,12) ~ 4),
           
         id = paste0(year, quarter))
  

nurse_quater <- nurse_df %>% 
  
  filter(date == "2019-DEC"|
         date == "2019-JUN"|
         date == "2019-MAR"|
         date == "2019-SEP"|
         
         date == "2020-DEC"|
         date == "2020-JUN"|
         date == "2020-MAR"|
         date == "2020-SEP"|
         
         date == "2022-DEC"|
         date == "2022-JUN"|
         date == "2022-MAR"|
         date == "2022-SEP"|
         
         date == "2021-DEC"|
         date == "2021-JUN"|
         date == "2021-MAR"|
         date == "2021-SEP") %>%
  
  mutate(date = yearmonth(date), year = year(date), quarter = quarter(date),
         
         id = paste0(year, quarter)) %>%
  
  select(!c("date", "year", "quarter")) %>%
  
  left_join(months, by = "id") %>%
  
  mutate(yearmonth = sprintf("%04d-%02d", year, month), date = yearmonth(yearmonth)) %>%
  
  select(date, grade_code, grade, grade_hierarchy, organisation_code, organisation_name, headcount)


## convert yearly data to monthly

months <- tibble(year=2009:2018) %>% 
  
  mutate(month=list(1:12)) %>% 
  
  unnest(month)


nurse_year <- nurse_df %>% 
  
  filter(date != "2019-DEC",
         date != "2019-JUN",
         date != "2019-MAR",
         date != "2019-SEP",
         
         date != "2020-DEC",
         date != "2020-JUN",
         date != "2020-MAR",
         date != "2020-SEP",
         
         date != "2022-DEC",
         date != "2022-JUN",
         date != "2022-MAR",
         date != "2022-SEP",
         
         date != "2021-DEC",
         date != "2021-JUN",
         date != "2021-MAR",
         date != "2021-SEP",
         
         date != "2018-DEC") %>%
  
  rename(year = date) %>%
  
  mutate(year = as.numeric(year)) %>%
  
  left_join(months, by = "year") %>%
  
  mutate(yearmonth = sprintf("%04d-%02d", year, month), date = yearmonth(yearmonth)) %>%
  
  select(date, grade_code, grade, grade_hierarchy, organisation_code, organisation_name, headcount)
  

##merge the nurse dataframes together

nurse_monthly <- rbind(nurse_year, nurse_quater)


##create montlhy data

months <- tibble(year=2009:2022) %>% 
  
  mutate(month=list(1:12)) %>% 
  
  unnest(month)



##create nurse annual data



nurse_annual <- nurse_monthly %>% #filter(grade_hierarchy == "N7"|
                                      #grade_hierarchy == "N6"|
                                      #grade_hierarchy == "H1") %>% 
  
  mutate(year = year(date),
         
         organisation_code = case_when(organisation_code %in% c("W11000026", "W11000027") ~ "W11000030",
                                       TRUE ~ as.character(organisation_code)),
         
         organisation_name = case_when(organisation_name %in% c("Abertawe Bro Morgannwg University LHB", "Cwm Taf University LHB") ~ "Cwm Taf Morgannwg University LHB",
                                             TRUE ~ as.character(organisation_name))) %>%
  
  group_by(year, organisation_code, organisation_name) %>%
  
  summarise(headcount = mean(headcount))

write_rds(nurse_annual, "data/wales_workforce_tidy_annual.rds") ##only extracting annual data to create univariate models



##tidying nurse monthly

nurse_monthly <- nurse_monthly %>% filter(grade_hierarchy == "N6") %>%
                                     # grade_hierarchy == "N6"|
                                     # grade_hierarchy == "H1") %>% 
  
  mutate(organisation_code = case_when(organisation_code %in% c("W11000026", "W11000027") ~ "W11000030",
                                       TRUE ~ as.character(organisation_code)),
         
         organisation_name = case_when(organisation_name %in% c("Abertawe Bro Morgannwg University LHB", "Cwm Taf University LHB") ~ "Cwm Taf Morgannwg University LHB",
                                       TRUE ~ as.character(organisation_name))) %>%
  
  group_by(date,grade_code, grade, grade_hierarchy,  organisation_code, organisation_name) %>%
  
  summarise(headcount = sum(headcount)) %>%
  
  mutate(id = paste0(date, organisation_code))  #create unique id


write_rds(nurse_monthly, "data/wales_workforce_tidy_monthly.rds") ##only extracting annual data to create univariate models
         


#merge nurse_monthly and LPMHSS

##We didnot consider Welsh ambulance service and NHS trust in Nurse_monthly.


LPMHSS_merge <- LPMHSS %>% #as_tsibble(index = date, key = c("local_health_board_code",
                                                            # "local_health_board_name",
                                                            # "alt_code",
                                                            # "id")) %>%
  
  #filter_index("2013 Apr" ~ "2022 Dec") %>%
  
  filter(alt_code != "W92000004") %>%
  
  select(!id) %>%
  
  mutate(alt_code = case_when(alt_code %in% c("W11000026", "W11000027") ~ "W11000030",
                                       TRUE ~ as.character(alt_code)),
         
         local_health_board_code = case_when(local_health_board_code %in% c("7A3x", "7A5x") ~ "7A5",
                                             TRUE ~ as.character(local_health_board_code)),
         
         local_health_board_name = case_when(local_health_board_name %in% c("Abertawe Bro Morgannwg University Local Health Board", "Cwm Taf University Local Health Board") ~ "Cwm Taf Morgannwg University Local Health Board",
                                             TRUE ~ as.character(local_health_board_name))) %>%
  
  group_by(date, local_health_board_code, local_health_board_name, alt_code) %>%
  
  summarise(across(everything(), sum)) %>%
  
  mutate(id = paste0(date, alt_code)) #create unique id



nurse_monthly_merge <- nurse_monthly %>% #as_tsibble(index = yearmonth, key = c("organisation_code",
  #                                                                              "organisation_name",
  #                                                                              "grade_code",
  #                                                                              "grade",
  #                                                                              "grade_hierarchy",
  #                                                                              "id")) %>%
  # 
  # filter_index("2013 Apr" ~ "2022 Dec") %>%
  
  filter(organisation_code != "RT4",
         organisation_code != "RYT") %>% ungroup() %>%
  
  select(!c("date", "organisation_name", "organisation_code"))
  

  
##prepare combined dataframe

nurse_combined <- left_join(as_tibble(LPMHSS_merge), as_tibble(nurse_monthly_merge), by = 'id')


##select the relevant columns

# nurse_combined <- nurse_combined %>% select(!c("date", "organisation_code", "organisation_name")) %>%
#   
#   select(yearmonth, id, local_health_board_code, local_health_board_name, alt_code, grade_code, grade,
#          grade_hierarchy, headcount, number_of_referrals, number_of_lpmhss_assessments_waited_28_days,
#          number_of_lpmhss_assessments_waited_56_days, number_of_lpmhss_assessments_waited_more_than_56_days,
#          number_of_lpmhss_assessments, number_of_therapeutic_assessments_waited_28_days,
#          number_of_therapeutic_assessments_waited_56_days, number_of_therapeutic_assessments_waited_more_than_56_days,
#          number_of_therapeutic_assessments, percentage_of_lpmhss_assessments_within_28_days, percentage_of_therapeutic_assessments_within_28_days)
#   
  

#Merge staff absent dataset

## this represents the overall percentage not for the individual category


staff_absent_merge <- staff_absent %>% filter(alt_code != "RQF",
                                              alt_code != "RT4",
                                              alt_code != "RYT",
                                              alt_code != "W92000004",
                                              alt_code != "T1530",
                                              alt_code != "FQR") %>%
  
  mutate(alt_code = case_when(alt_code %in% c("W11000026", "W11000027") ~ "W11000030",
                              TRUE ~ as.character(alt_code))) %>%
  
  select(!c("organisation_item_notes_eng", "organisation_code", "organisation_name" )) %>%
  
  group_by(date, alt_code) %>%
  
  summarise(across(everything(), mean)) %>%
  
  mutate(id = paste0(date, alt_code)) %>% 
  
  ungroup() %>%
  
  select(id, percentage_of_nurses) #create the unique id



#merge staff_absent_merge and nurse_combined

nurse_combined <- left_join(as_tibble(nurse_combined), as_tibble(staff_absent_merge), by = 'id')


## merge population data

population_merge <- population %>% filter(age_group == "Aged 0 to 15" |
                                          age_group == "Aged 16 to 24" |
                                          age_group == "Aged 25 to 44"|
                                          age_group == "Aged 45 to 64") %>% 
  
  filter(sex == "Persons") %>% ## assuming persons = male + female
  
  mutate(year = as.numeric(date)) %>%
  
  left_join(months, by = "year") %>%
  
  mutate(yearmonth = sprintf("%04d-%02d", year, month), yearmonth = yearmonth(yearmonth)) %>%
  
  select(yearmonth, area_code, age_group, sex, population) %>%
  
  mutate(id = paste0(yearmonth, area_code))
  
 


population_merge <- population_merge %>% pivot_wider(id_cols = id, names_from = age_group, values_from = population) %>% 
  
  janitor::clean_names() ## to create columns for each age group



## merge population date to nursing_combined

nurse_combined <- left_join(nurse_combined, population_merge, by = 'id')


# adding nhs expenditure variables

## create a new id since here we are using org_code

nurse_combined <- nurse_combined %>% mutate(id = paste0(date, local_health_board_code))

NHS_expenditure_merge <- NHS_expenditure %>% filter(commissioner_code == "Prim",
                                                    programme == "General mental illness"|
                                                    programme == "Child & adolescent mental health services") %>%
  
  mutate(organisation_code = case_when(organisation_code %in% c("7A3x", "7A5x") ~ "7A5",
                                                            TRUE ~ as.character(organisation_code))) %>%
  
  rename(year = date) %>%
  
  left_join(months, by = "year") %>%
  
  mutate(yearmonth = sprintf("%04d-%02d", year, month), yearmonth = yearmonth(yearmonth)) %>%
  
  select(yearmonth, organisation_code, commissioner_code, programme, expenditure_000,
         expenditure_per_head, per_cent_of_total) %>%
  
  group_by(yearmonth, organisation_code, commissioner_code, programme) %>%
  
  summarise(expenditure_000 = sum(expenditure_000), expenditure_per_head = mean(expenditure_per_head), per_cent_of_total = mean(per_cent_of_total)) %>%
  
  mutate(id = paste0(yearmonth, organisation_code))


## prepare separate df for each expenditure variable

NHS_expenditure_merge <- NHS_expenditure_merge %>%
  
  pivot_wider(names_from = c(commissioner_code, programme), values_from = c(expenditure_000, expenditure_per_head, per_cent_of_total),
              names_sep = "_") %>%
  
  janitor::clean_names() %>%
  
  filter(organisation_code != "W") %>% 
  
  ungroup() %>%
  
  select(!c(yearmonth, organisation_code))


## merge nhs expenditure data to nursing_combined
  
nurse_combined <- nurse_combined %>% mutate(id = paste0(date, local_health_board_code)) %>%
  
  
  left_join(NHS_expenditure_merge, by = 'id')



##merge nursing graduate data

nurse_grad_merge <- nurse_grad %>% filter(2012 < year) %>%
  
  left_join(months, by = "year") %>%
  
  mutate(yearmonth = sprintf("%04d-%02d", year, month), yearmonth = yearmonth(yearmonth)) %>%
  
  select(yearmonth, garduates_per_1000_inhabitants) %>%
  
  rename(date = yearmonth)


## merge graduate data to nursing_combined

nurse_combined <- left_join(nurse_combined, nurse_grad_merge, by = 'date')



##merge nursing migration data

nurse_migration_merge <- nurse_migration %>% filter(var == "SFTN"|
                                                    var == "IFTN") %>%
  
  select(year, var, value) %>%
  
  pivot_wider(names_from = var, values_from = value,
              names_sep = "_") %>%
  
  left_join(months, by = "year") %>%
  
  filter(!is.na(month)) %>%
  
  mutate(yearmonth = sprintf("%04d-%02d", year, month), yearmonth = yearmonth(yearmonth)) %>%
  
  select(yearmonth, SFTN, IFTN) %>%
  
  rename(date = yearmonth)


## merge nursing migration data to nursing_combined

nurse_combined <- nurse_combined %>% left_join(nurse_migration_merge, by = 'date')


nurse_tidy <- nurse_combined %>%
  
  select(!id) %>%
  
  mutate(expenditure_000_prim_general_mental_illness = expenditure_000_prim_general_mental_illness/12,
         expenditure_000_prim_child_adolescent_mental_health_services = expenditure_000_prim_child_adolescent_mental_health_services/12,
         IFTN = IFTN/12)

  
  
#write dateset

write_rds(nurse_tidy, "data/wales_workforce_tidy.rds")
  