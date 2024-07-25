# load SIPP data and compile statistics for retirement demographics.

rm(list = ls())
###########################
###   Load Packages     ###
###########################

library(haven)
library(dplyr)
library(plotly)
library(tidyr)
library(openxlsx)

#################
### Set paths ###
#################

path_project = "/Users/sarah/Library/CloudStorage/GoogleDrive-sarah@eig.org/My Drive/projects/retirement"
path_data = file.path(path_project, "data/SIPP")
path_output = file.path(path_project, "Output/SIPP")

# Set working directory for SIPP data
setwd(path_data)

#####################
# IDENTIFIERS
# SHHADID - Household address ID. Used to differentiate households spawned from an original sample household.
# SPANEL - Panel year
# SSUID - Sample unit identifier. This identifier is created by scrambling together PSU, Sequence #1, Sequence #2, and the Frame Indicator for a case. It may be used in matching sample units from different waves.
# SWAVE - Wave number of interview
# PNUM - Person number
# MONTHCODE - Value of reference month
# WPFINWGT - Final person weight

# DEMOGRAPHICS -
# TAGE - Age as of last birthday
# EEDUC - What is the highest level of school ... completed or the highest degree received by December of (reference year)?
# ESEX - Sex of this person
# ERACE - What race(s) does ... consider herself/himself to be?
# TMETRO_INTV - metropolitian status for interview address

# LABORFORCE
# EJB1_JBORSE - This variable describes the type of work arrangement, whether work for an employer, self employed or other.Respondents who held a job during the reference month
# EJB1_CLWRK - Class of worker

# INCOME
# TFTOTINC - Sum of monthly earnings and income received by family members age 15 and older, as well as SSI payments received by children under age 15

# TRANSFERS
# for access - 
# EMJOB_401
# EMJOB_IRA
# EMJOB_PEN
# EOWN_THR401
# EOWN_IRAKEO
# EOWN_PENSION

# participation -
# ESCNTYN_401

# matching - 
# EECNTYN_401

####################
#### load data #####
####################
sipp_2021 = read.csv("pu2021.csv") # 2021 simplified dataset from stata export.

names(sipp_2021)

sipp_2021 = sipp_2021 %>%
  mutate(
    EDUCATION = case_when(
      EEDUC >=31 & EEDUC <= 39 ~ "High School or less",
      EEDUC >= 40 & EEDUC <=42 ~ "Some college",
      EEDUC >= 43 & EEDUC <= 46 ~ "Bachelor's degree or higher",
      TRUE ~ "Missing"
    ),
    SEX = case_when(
      ESEX == 1 ~ "Male",
      ESEX == 2 ~ "Female",
      TRUE ~ "Missing"
    ),
    RACE = case_when(
      ERACE == 1 & EORIGIN ==2 ~ "Non-Hispanic White",
      ERACE == 2 & EORIGIN ==2 ~ "Non-Hispanic Black",
      ERACE == 3 & EORIGIN ==2 ~ "Asian",
      EORIGIN == 1 ~ "Hispanic",
      ERACE == 4 ~ "Mixed/Other",
      TRUE ~ "Missing"
    ),
    EMPLOYMENT_TYPE = case_when(
      EJB1_JBORSE == 1 ~ "Employer",
      EJB1_JBORSE == 2 ~ "Self-employed (owns a business)",
      EJB1_JBORSE == 3 ~ "Other work arrangement",
      TRUE ~ "Missing"
    ),
    CLASS_OF_WORKER = case_when(
      EJB1_CLWRK == 1 ~ "Federal government employee",
      EJB1_CLWRK == 2 ~ "Active duty military",
      EJB1_CLWRK == 3 ~ "State government employee",
      EJB1_CLWRK == 4 ~ "Local government employee",
      EJB1_CLWRK == 5 ~ "Employee of a private, for-profit company",
      EJB1_CLWRK == 6 ~ "Employee of a private, not-for-profit company",
      EJB1_CLWRK == 7 ~ "Self-employed in own incorporated business",
      EJB1_CLWRK == 8 ~ "Self-employed in own not incorporated business",
      TRUE ~ "Missing"
    ),
    TOTYEARINC = TFTOTINC*12,
    ANY_RETIREMENT_ACCESS = case_when(
      EMJOB_401 == 1 ~ "Yes",
      EMJOB_IRA == 1 ~ "Yes",
      EMJOB_PEN == 1 ~ "Yes",
      EMJOB_401 == 2 ~ "No",
      EMJOB_IRA == 2 ~ "No",
      EMJOB_PEN == 2 ~ "No",
      EOWN_THR401  == 2 ~ "No",
      EOWN_IRAKEO  == 2 ~ "No",
      EOWN_PENSION == 2 ~ "No",
      TRUE ~ "Missing"
    ),
    PARTICIPATING = case_when(
      ESCNTYN_401 == 1 ~ "Yes",
      ESCNTYN_401 == 2 ~ "No",
      is.na(ESCNTYN_401) ~ "No"
    ),
    MATCHING = case_when(
      EECNTYN_401 == 1 ~ "Yes",
      EECNTYN_401 == 2 ~ "No",
      is.na(EECNTYN_401) ~ "No"
    ),
    PARTICIPATING = ifelse(MATCHING=="Yes", "Yes",  PARTICIPATING),
    METRO_STATUS = case_when(
      TMETRO_INTV == 1 ~ "Metropolitan area",
      TMETRO_INTV == 2 ~ "Nonmetropolitan area",
      TMETRO_INTV == 3 ~ "Not identified"
    )
  ) %>%
  select("SHHADID", "SPANEL", "SSUID", "SWAVE", "PNUM", "MONTHCODE", "WPFINWGT",
         "TAGE", "EDUCATION", "SEX", "RACE", "METRO_STATUS",
         "EMPLOYMENT_TYPE", "CLASS_OF_WORKER",
         "TFTOTINC",
         "ANY_RETIREMENT_ACCESS",
         "PARTICIPATING",
         "MATCHING", "MONTHCODE", "TJB1_JOBHRS1", "TOTYEARINC")


# demographic filtering: 
# only 18-65, non-government,
# by full/part time status

sipp_2021 = sipp_2021 %>%
  filter(EMPLOYMENT_TYPE == "Employer" | EMPLOYMENT_TYPE=="Self-employed (owns a business)") %>%
  filter(CLASS_OF_WORKER ==  "Employee of a private, for-profit company" | 
           CLASS_OF_WORKER == "Employee of a private, not-for-profit company") %>%
  filter(MONTHCODE == 12) %>% # avoid double-counting
  filter(TFTOTINC >0) %>% # earning an income
  mutate(in_age_range = ifelse(TAGE >= 18 & TAGE <= 65, "yes","no")) %>% # 18-65 ages
  mutate(FULL_PART_TIME = case_when(
    TJB1_JOBHRS1 >=35 ~ "full time",
    TJB1_JOBHRS1 >0 & TJB1_JOBHRS1< 35 ~ "part time"
  )) %>%
  filter(!is.na(FULL_PART_TIME))

write.csv(sipp_2021, paste(path_output, "sipp_2021_wrangled.csv", sep = "/"))
