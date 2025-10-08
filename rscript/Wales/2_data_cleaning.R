#Data cleaning and preparation

library(tidyverse)
library(tsibble)
library(janitor)

#load data

df <- read_rds("data/system/referrals.rds")

ctp <- read_rds("data/system/ctp.rds")

beds <- read_rds("data/system/beds_monthly.rds")

staff_age <- read_rds("data/system/staff_age.rds")

staff_disability <- read_rds("data/system/staff_disability.rds")

staff_ethnicity <- read_rds("data/system/staff_ethnicity.rds")

staff_gender <- read_rds("data/system/staff_gender.rds")

staff_absent <- read_rds("data/system/staff_absent.rds")

nursing_by_area <- read_rds("data/system/nursing_by_area.rds")

NHS_expenditure <- read_rds("data/system/NHS_expenditure.rds")

population <- read_rds("data/system/population.rds")

population_projections <- read_rds("data/system/population_projections.rds")

population_by_health_boards <- read_rds("data/system/population_by_health_boards.rds")

admissions_lhb <- read_rds("data/system/addmissions_local_health_board.rds")


################### data preparation - Referrals ###############################


# ref_all_adult <- df %>% filter(Indicator_SortOrder == 6 & Age_SortOrder == 3)
# 
# ref_all_child <- df %>% filter(Indicator_SortOrder == 6 & Age_SortOrder == 2)

# ref_all <- df %>% filter(Indicator_SortOrder == 6 & Age_SortOrder == 1)  ## choosing the all services since no records found for adult before 2020 April
# 
# ref_all <- ref_all %>% select(Date_ItemName_ENG, LocalHealthBoard_Code, LocalHealthBoard_ItemName_ENG, LocalHealthBoard_AltCode1, Data) %>%
#   rename(Date = Date_ItemName_ENG, Local_Health_Board_Code = LocalHealthBoard_Code, Local_Health_Board_Name = LocalHealthBoard_ItemName_ENG,
#          Alt_Code = LocalHealthBoard_AltCode1, Number_of_referrals = Data)


#data preparation - Referrals 

## choosing the all services since no records found for adult before 2020 April

test <- df %>% filter(Age_SortOrder == 1)  %>% 
  
  select(Date_ItemName_ENG, LocalHealthBoard_Code, LocalHealthBoard_ItemName_ENG, 
                                              LocalHealthBoard_AltCode1, Indicator_ItemName_ENG, Data) %>%
  
  rename(Date = Date_ItemName_ENG, Local_Health_Board_Code = LocalHealthBoard_Code, 
         Local_Health_Board_Name = LocalHealthBoard_ItemName_ENG,
         Alt_Code = LocalHealthBoard_AltCode1, Indicator_Name = Indicator_ItemName_ENG)                               ##creating a tidy data set



test <- test %>% mutate(ID = paste0(Date, Alt_Code))  ## creating a unique ID. This will use to merge datasets


test2 <- test %>% 
  
  pivot_wider(id_cols = ID, names_from = Indicator_Name, values_from = Data)  %>%
  
  rename(Number_of_Referrals = "Number of referrals for a LPMHSS assessment received during the month", 
         Number_of_LPMHSS_Assessments_Waited_28_Days = "Number of patients who had waited up to and including 28 days from referral to a LMPHSS assessment",
         Number_of_LPMHSS_Assessments_Waited_56_Days = "Number of patients who had waited over 28 days and up to and including 56 days from referral to a LPMHSS assessment", 
         Number_of_LPMHSS_Assessments_Waited_MoreThan_56_Days = "Number of patients who had waited over 56 days from referral to a LPMHSS assessment",
         Number_of_LPMHSS_Assessments = "Total number of LPMHSS assessments undertaken during the month", 
         Number_of_Therapeutic_Assessments_Waited_28_Days = "Number of patients who had waited up to and including 28 days from a LPMHSS assessment to the start of a therapeutic intervention",
         Number_of_Therapeutic_Assessments_Waited_56_Days = "Number of patients who had waited over 28 days and up to and including 56 days from a LMPHSS assessment to the start of a therapeutic intervention",
         Number_of_Therapeutic_Assessments_Waited_MoreThan_56_Days = "Number of patients who had waited over 56 days from a LPMHSS assessment to the start of a therapeutic intervention",
         Number_of_Therapeutic_Assessments = "Total number of therapeutic interventions started during the month",
         Percentage_of_LPMHSS_assessments_Within_28_Days = "Percentage of LPMHSS assessments undertaken within 28 days of referral",
         Percentage_of_Therapeutic_Assessments_Within_28_Days = "Percentage of therapeutic interventions started within 28 days following a LPMHSS assessment")


## merge test2 and test3 to add hospital and date data


test3 <- test %>% select(Date, Local_Health_Board_Code, Local_Health_Board_Name,
                         Alt_Code, ID) %>% unique()     


LPMHSS <- left_join(test3, test2, by = 'ID')  ##Local Primary Mental Health Support Services including all the referrals, assessments and waited times 

LPMHSS <- LPMHSS %>% mutate(Date = yearmonth(Date))

write_rds <- write_rds(LPMHSS, "data/system/Wales_data/LPMHSS_tidy.rds") ##save LPMHSS dataset



################### data preparation - CTP ###############################

#choosing all service as other categories do not have consistent data

ctp_test <- ctp %>% filter(Indicator_Code != 5 & ServiceorAge_SortOrder == 1) %>%
  
  select(Date_ItemName_ENG, LocalHealthBoard_Code, LocalHealthBoard_ItemName_ENG, LocalHealthBoard_AltCode1,
         Indicator_ItemName_ENG, Data) %>%
  
  rename(Date = Date_ItemName_ENG, Local_Health_Board_Code = LocalHealthBoard_Code, 
         Local_Health_Board_Name = LocalHealthBoard_ItemName_ENG,
         Alt_Code = LocalHealthBoard_AltCode1, Indicator_Name = Indicator_ItemName_ENG)  


ctp_test <- ctp_test %>% mutate(ID = paste0(Date, Alt_Code))  ## creating a unique ID. This will use to merge datasets  


ctp_test2 <- ctp_test %>% 
  
  pivot_wider(id_cols = ID, names_from = Indicator_Name, values_from = Data) %>%
  
  rename(Number_of_patient_at_LHB = "Total number of patients resident in the LHB with a valid CTP at the end of the month", 
         Number_of_patient_at_Secondary_Services = "Total number of patients resident in the LHB currently in receipt of secondary Mental Health services at the end of the month")


## merge test2 and test3


ctp_test3 <- ctp_test %>% select(Date, Local_Health_Board_Code, Local_Health_Board_Name,
                         Alt_Code, ID) %>% unique()     


CTP <- left_join(ctp_test3, ctp_test2, by = 'ID') 

CTP <- CTP %>% mutate(Date = yearmonth(Date))

write_rds <- write_rds(CTP, "data/system/Wales_data/CTP_tidy.rds") ##save ctp dataset




################### data preparation - beds monthly ###############################

#selecting all categories in mental healthcare services

beds_test <- beds %>% filter((Specialty_SortOrder == 5 |
                               Specialty_SortOrder == 78 |
                               Specialty_SortOrder == 79 |
                               Specialty_SortOrder == 80 |
                               Specialty_SortOrder == 81 |
                               Specialty_SortOrder == 86) &
                               Measure_SortOrder != 3)  %>%
  
  select(Month_ItemName_ENG, Measure_ItemName_ENG,  Measure_Code, Organisation_Code, Organisation_ItemName_ENG, 
         Organisation_Hierarchy, Specialty_ItemName_ENG, Specialty_Code, Data) %>%
  
  rename(Date = Month_ItemName_ENG, Indicator = Specialty_ItemName_ENG, Measure = Measure_ItemName_ENG, Organization_Name = Organisation_ItemName_ENG)


beds_test <- beds_test %>% mutate(id = paste0(Date, Organisation_Code, Specialty_Code))  ## creating a unique ID. This will use to merge datasets 


beds_test2 <- beds_test %>% 
  
  pivot_wider(id_cols = id, names_from = Measure, values_from = Data) %>%
  
  janitor::clean_names()

  
## merge test2 and test3


beds_test3 <- beds_test %>% select(Date, Organization_Name, Organisation_Code,
                                  Organisation_Hierarchy, Indicator, id) %>% unique()     


BEDS <- left_join(beds_test3, beds_test2, by = 'id')  ##average occupied and available beds for all specialties

BEDS <- BEDS %>% mutate(Date = yearmonth(Date))

write_rds <- write_rds(BEDS, "data/system/Wales_data/beds_tidy.rds") ##save beds dataset

  
  
  
################### data preparation - staff age ###############################

## selecting Nursing, midwifery and health visiting staff category. However, data is provided as at 2022


STAFF_AGE <- staff_age %>% filter(Staffgroup_SortOrder == 2) %>%
  
  select(Organisation_Code, Organisation_ItemName_ENG, Ageband_ItemName_ENG, Ageband_Code, Data) %>%
  
  rename(Organisation_Name = Organisation_ItemName_ENG, Ageband = Ageband_ItemName_ENG, Percentage_of_Nurses = Data)


write_rds <- write_rds(STAFF_AGE, "data/system/Wales_data/staff_age_tidy.rds") ##save STAFF_AGE dataset



################### data preparation - staff disability ###############################

## selecting Nursing, midwifery and health visiting staff category. However, data is provided as at 2022


STAFF_DISABILITY <- staff_disability %>% filter(Staffgroup_SortOrder == 2) %>%
  
  select(Organisation_Code, Organisation_ItemName_ENG, Disability_ItemName_ENG, Disability_Code, Data) %>%
  
  rename(Organisation_Name = Organisation_ItemName_ENG, Disability = Disability_ItemName_ENG, Percentage_of_Nurses = Data)


write_rds <- write_rds(STAFF_DISABILITY, "data/system/Wales_data/staff_dissability_tidy.rds") ##save STAFF_DISABILITY dataset



################### data preparation - staff ethnicity ###############################

## selecting Nursing, midwifery and health visiting staff category. However, data is provided as at 2022


STAFF_ETHNICITY <- staff_ethnicity %>% filter(Staffgroup_SortOrder == 2) %>%
  
  select(Organisation_Code, Organisation_ItemName_ENG, Ethnicity_ItemName_ENG, Ethnicity_Code, Data) %>%
  
  rename(Organisation_Name = Organisation_ItemName_ENG, Ethnicity = Ethnicity_ItemName_ENG, Percentage_of_Nurses = Data)


write_rds <- write_rds(STAFF_ETHNICITY, "data/system/Wales_data/staff_ethnicity_tidy.rds") ##save STAFF_ETHNICITY dataset



################### data preparation - staff gender ###############################

## selecting Nursing, midwifery and health visiting staff category. However, data is provided as at 2022


STAFF_GENDER <- staff_gender %>% filter(Staffgroup_SortOrder == 2) %>%
  
  select(Organisation_Code, Organisation_ItemName_ENG, Gender_ItemName_ENG, Data) %>%
  
  rename(Organisation_Name = Organisation_ItemName_ENG, Gender = Gender_ItemName_ENG, Percentage_of_Nurses = Data)


write_rds <- write_rds(STAFF_GENDER, "data/system/Wales_data/staff_gender_tidy.rds") ##save STAFF_ETHNICITY dataset




################### data preparation - staff absent ###############################

## selecting Nursing, midwifery and health visiting staff category.


STAFF_ABSENT <- staff_absent %>% filter(Staffgroup_SortOrder == 2 &
                                             nchar(Date_ItemName_ENG) == 8) %>%
  
  select(Date_ItemName_ENG, Organisation_Code, Organisation_ItemName_ENG, 
         Organisation_AltCode1, Organisation_ItemNotes_ENG, Data) %>%
  
  rename(Date = Date_ItemName_ENG, Organisation_Name = Organisation_ItemName_ENG, Alt_Code = Organisation_AltCode1,
         Percentage_of_Nurses = Data) %>%
  
  mutate(Date = yearmonth(Date))


write_rds <- write_rds(STAFF_ABSENT, "data/system/Wales_data/staff_absent_tidy.rds") ##save STAFF_ABSENT dataset



################### data preparation - Nursing by area yearly ###############################

## I chose the December data as the total nurse count for 2018 onwards as it consists the quarterly data


## Selecting the relevant nursing categories

nurse_levels <- c("Psychiatry| Community Services| Community Matron| Qualified School Nurse| Health visiting|
                  Diploma| Qualified School Nurse| Health Visitor")


nursing_by_area_df <- nursing_by_area %>% filter(grepl(nurse_levels, Grade_ItemName_ENG))



nursing_by_area_tidy <- nursing_by_area_df %>% #filter(Date_Code != "2019-Q1",
                                   # Date_Code != "2019-Q2",
                                   #   Date_Code != "2019-Q3",
                                   #   Date_Code != "2020-Q1",
                                   #   Date_Code != "2020-Q2",
                                   #   Date_Code != "2020-Q3",
                                   #   Date_Code != "2022-Q1",
                                   #   Date_Code != "2022-Q2",
                                   #   Date_Code != "2022-Q3",
                                   #   Date_Code != "2021-Q1",
                                   #   Date_Code != "2021-Q2",
                                   #   Date_Code != "2021-Q3",
                                   #   Date_Code != "2018",
                                   #   Measure_SortOrder == 2) %>%

  filter(Measure_SortOrder == 2) %>%
  
  select(Date_ItemName_ENG, Grade_Code, Grade_ItemName_ENG, Grade_Hierarchy, Organisation_Code, Organisation_ItemName_ENG, Data) %>%
  
  rename(Date = Date_ItemName_ENG, Grade = Grade_ItemName_ENG, Organisation_Name = Organisation_ItemName_ENG,
         Headcount = Data) #%>%
  
  #mutate(Year = substr(Date, 1, 4), Year = year(as.Date(paste(Date, 1, 1), "%Y %m %d"))) 



write_rds(nursing_by_area_tidy, "data/system/Wales_data/nursing_by_area_tidy.rds")



################### data preparation - NHS expenditure ###############################

##filtering the mental healthcare issues

NHS_expenditure_tidy <- NHS_expenditure %>% filter(grepl("mental", ProgrammeBudget_ItemName_ENG)) %>%
  
  select(Year_ItemName_ENG, Organisation_Code, Organisation_ItemName_ENG, Commissioner_Code, Commissioner_ItemName_ENG,
         Measure_Code, Measure_ItemName_ENG, ProgrammeBudget_ItemName_ENG, ProgrammeBudget_Code, Data) %>%
  
  rename(Date = Year_ItemName_ENG, Organisation_Name = Organisation_ItemName_ENG, Commissioner = Commissioner_ItemName_ENG,
         Measure = Measure_ItemName_ENG, Programme = ProgrammeBudget_ItemName_ENG)
  

NHS_expenditure_tidy <- NHS_expenditure_tidy %>% mutate(id = paste0(Date, Organisation_Code, ProgrammeBudget_Code, Commissioner_Code))  ## creating a unique ID. This will use to merge datasets 


exps_pivot <- NHS_expenditure_tidy %>% 
  
  pivot_wider(id_cols = id, names_from = Measure, values_from = Data) %>%
  
  janitor::clean_names()


## merge dataframes

NHS_expenditure_cols <- NHS_expenditure_tidy %>% select(!c(Measure, Measure_Code, ProgrammeBudget_Code, Data)) %>% unique()  

NHS_expenditure_tidy <- left_join(NHS_expenditure_cols, exps_pivot, by = 'id')  

NHS_expenditure_tidy <- NHS_expenditure_tidy %>%
  
  mutate(Date = substr(Date, 1, 4), Date = year(as.Date(paste(Date, 1, 1), "%Y %m %d"))) ##correcting date format

write_rds <- write_rds(NHS_expenditure_tidy, "data/system/Wales_data/NHS_expenditure_tidy.rds") ##save NHS_expenditure_tidy dataset



################### data preparation - Population ###############################


Population_tidy <- population %>% filter(Year_Code > "2008", 
                                         Area_Hierarchy == "W92000004") %>% 
  
  select(Year_Code, Area_Code, Area_ItemName_ENG, Age_ItemName_ENG, Sex_ItemName_ENG, Data) %>%
  
  rename(Date = Year_Code, Area_Name = Area_ItemName_ENG, Age_Group = Age_ItemName_ENG,
         Sex = Sex_ItemName_ENG, Population = Data)


write_rds <- write_rds(Population_tidy, "data/system/Wales_data/population_tidy.rds") ##save Population_tidy dataset


################### data preparation - Population ###############################


Population_lhb_tidy <- population_by_health_boards %>% 
  
  select(Year_Code, Area_Code, Area_ItemName_ENG, Age_ItemName_ENG, Sex_ItemName_ENG, Data) %>%
  
  rename(Date = Year_Code, Area_Name = Area_ItemName_ENG, Age_Group = Age_ItemName_ENG,
         Sex = Sex_ItemName_ENG, Population = Data)


write_rds <- write_rds(Population_lhb_tidy, "data/system/Wales_data/Population_lhb_tidy.rds") ##save Population_tidy dataset



################### data preparation - Population projection ###############################


##Population at the end of year

Population_projections_tidy <- population_projections %>% filter(Component_Code == "EndPop") %>%
  
  select(YearlyChange_ItemName_ENG, Area_AltCode1, Area_ItemName_ENG, Data) %>%
  
  rename(Date = YearlyChange_ItemName_ENG, Area_Code = Area_AltCode1, Area_Name = Area_ItemName_ENG, Population = Data) %>%
  
  mutate(Date = substr(Date, 1, 4), Date = year(as.Date(paste(Date, 1, 1), "%Y %m %d"))) ##correcting date format


write_rds <- write_rds(Population_projections_tidy, "data/system/Wales_data/population_projections_tidy.rds") ##save Population_projections_tidy dataset



################### data preparation - admissions_lhb ###############################

##Choosing the total admissions

admissions_lhb_tidy <- admissions_lhb %>% filter(LegalStatus_Code == 12) %>%
  
  select(Year_ItemName_ENG, Organisation_Code, Organisation_ItemName_ENG, Organisation_AltCode1, Gender_ItemName_ENG, Data) %>%
  
  rename(Date = Year_ItemName_ENG, Organisation_Name = Organisation_ItemName_ENG, Alt_Code = Organisation_AltCode1, Gender = Gender_ItemName_ENG,
         Number_of_Admissions = Data) %>%
  
  mutate(Date = substr(Date, 1, 4), Date = year(as.Date(paste(Date, 1, 1), "%Y %m %d")))


write_rds(admissions_lhb_tidy, "data/system/Wales_data/admissions_lhb_tidy.rds") ##save admissions_lhb_tidy dataset
