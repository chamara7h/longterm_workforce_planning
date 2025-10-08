#Extract data from ststwales R package

# install.packages("statswalesr")

# Load libraries

library(statswalesr)
library(tidyverse)


# Download data

addmissions_local_health_board <- statswales_get_dataset("HLTH0712") #Admissions to mental health facilities by local health board

referrals <- statswales_get_dataset("HLTH3000") #Referrals and Waiting times for a LPMHSS assessment and Waiting times for a therapeutic intervention by LHB, age and month

ctp <- statswales_get_dataset("HLTH3001") #Care and treatment plan (CTP) compliance, by LHB, service, age and month

outcome <- statswales_get_dataset("HLTH3002") #Outcome assessment report compliance, by LHB and month

psychiatric_data <- statswales_get_dataset("Hlth0707") #Patients in mental health hospitals and units in Wales with a mental illness

beds_annual <- statswales_get_dataset("HLTH0309") #NHS beds by organisation and year, 2009-10 onwards

beds_monthly <- statswales_get_dataset("HLTH0310") #Monthly NHS beds data by measure, site and specialty, March 2014 onwards

outpatient_attendance <- statswales_get_dataset("HLTH0313") #Outpatient attendances by organisation and site

staff_age <- statswales_get_dataset("HLTH0466") #Percent of NHS staff by organisation, staff group and age band

staff_disability <- statswales_get_dataset("hlth0467") #Percent of NHS staff by organisation, staff group, and disability

staff_ethnicity <- statswales_get_dataset("hlth0468") #Percent of NHS staff by organisation, staff group, and ethnicity

staff_gender <- statswales_get_dataset("hlth0469") #Percent of NHS staff by organisation, staff group and gender

staff_nationality <- statswales_get_dataset("hlth0470") #Percent of NHS staff by organisation, staff group and nationality

staff_absent <- statswales_get_dataset("HLTH0440") #Percentage absent by staff group and date

nursing_by_area <- statswales_get_dataset("HLTH0441") #Nursing, midwifery and health visiting staff, by grade and area of work

other_staff <- statswales_get_dataset("hlth0443") #Other non medical staff by job type and area of work

staff_group <- statswales_get_dataset("HLTH0431") #NHS staff by staff group and year

staff_grade <- statswales_get_dataset("hlth0432") #Medical and dental staff by grade and year

NHS_expenditure <- statswales_get_dataset("HLTH1901") #NHS expenditure per head by budget category and year

QOF <- statswales_get_dataset("Hlth1111") #Quality and Outcomes Framework (QOF) points by local health board and register

diseases_registered <- statswales_get_dataset("Hlth1112") #Patients on Quality and Outcomes Framework (QOF) disease registers by local health board

population <- statswales_get_dataset("POPU0003") #Population estimates by local authority and year

population_by_health_boards <- statswales_get_dataset("POPU0005") #Population estimates by local authority and year

population_projections <- statswales_get_dataset("POPU6011") #Population projection components of change by local authority and year


#save data

write_rds(addmissions_local_health_board, "data/system/addmissions_local_health_board.rds")

write_rds(referrals, "data/system/referrals.rds")

write_rds(ctp, "data/system/ctp.rds")

write_rds(outcome, "data/system/outcome.rds")

write_rds(psychiatric_data, "data/system/psychiatric_data.rds")

write_rds(beds_annual, "data/system/beds_annual.rds")

write_rds(beds_monthly, "data/system/beds_monthly.rds")

write_rds(outpatient_attendance, "data/system/outpatient_attendance.rds")

write_rds(staff_age, "data/system/staff_age.rds")

write_rds(staff_disability, "data/system/staff_disability.rds")

write_rds(staff_ethnicity, "data/system/staff_ethnicity.rds")

write_rds(staff_gender, "data/system/staff_gender.rds")

write_rds(staff_nationality, "data/system/staff_nationality.rds")

write_rds(staff_absent, "data/system/staff_absent.rds")

write_rds(nursing_by_area, "data/system/nursing_by_area.rds")

write_rds(other_staff, "data/system/other_staff.rds")

write_rds(staff_group, "data/system/staff_group.rds")

write_rds(staff_grade, "data/system/staff_grade.rds")

write_rds(NHS_expenditure, "data/system/NHS_expenditure.rds")

write_rds(QOF, "data/system/QOF.rds")

write_rds(diseases_registered, "data/system/diseases_registered.rds")

write_rds(population, "data/system/population.rds")

write_rds(population_by_health_boards, "data/system/population_by_health_boards.rds")

write_rds(population_projections, "data/system/population_projections.rds")
