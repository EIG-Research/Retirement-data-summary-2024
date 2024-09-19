# SIPP data on retirement access, participation, matching for all years
# for which data is available
# 2021
# 2022
# 2023

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
setwd(path_output)

###### read in each dataset
sipp_2021 = read.csv("sipp_2021_wrangled.csv")

sipp_2022 = read.csv("sipp_2022_wrangled.csv")
  
sipp_2023 = read.csv("sipp_2023_wrangled.csv")


############ access
access_2021 = sipp_2021 %>%
  filter(in_age_range =="yes") %>%
  filter(FULL_PART_TIME=="full time") %>%
  filter(ANY_RETIREMENT_ACCESS!="Missing") %>%
  rename(`Has access to an Employer Retirement Plan` = ANY_RETIREMENT_ACCESS) %>%
  group_by(`Has access to an Employer Retirement Plan`) %>%
  summarise(count = sum(WPFINWGT)) %>%
  ungroup() %>%
  mutate(Share = count / sum(count)*100) %>%
  select(-c(count)) %>%
  mutate(year = 2021)

access_2022 = sipp_2022 %>%
  filter(in_age_range =="yes") %>%
  filter(FULL_PART_TIME=="full time") %>%
  filter(ANY_RETIREMENT_ACCESS!="Missing") %>%
  rename(`Has access to an Employer Retirement Plan` = ANY_RETIREMENT_ACCESS) %>%
  group_by(`Has access to an Employer Retirement Plan`) %>%
  summarise(count = sum(WPFINWGT)) %>%
  ungroup() %>%
  mutate(Share = count / sum(count)*100) %>%
  select(-c(count)) %>%
  mutate(year = 2022)

access_2023 = sipp_2023 %>%
  filter(in_age_range =="yes") %>%
  filter(FULL_PART_TIME=="full time") %>%
  filter(ANY_RETIREMENT_ACCESS!="Missing") %>%
  rename(`Has access to an Employer Retirement Plan` = ANY_RETIREMENT_ACCESS) %>%
  group_by(`Has access to an Employer Retirement Plan`) %>%
  summarise(count = sum(WPFINWGT)) %>%
  ungroup() %>%
  mutate(Share = count / sum(count)*100) %>%
  select(-c(count)) %>%
  mutate(year = 2023)



  
############ participation
participate_2021 = sipp_2021 %>%
  filter(in_age_range =="yes") %>%
  filter(FULL_PART_TIME=="full time") %>%
  filter(PARTICIPATING!="Missing") %>%
  rename(`Participates in Employer Retirement Plan` = PARTICIPATING) %>%
  group_by(`Participates in Employer Retirement Plan`) %>%
  summarise(count = sum(WPFINWGT)) %>%
  ungroup() %>%
  mutate(Share = count / sum(count)*100) %>%
  select(-c(count)) %>%
  mutate(year = 2021)
  
participate_2022 = sipp_2022 %>%
  filter(in_age_range =="yes") %>%
  filter(FULL_PART_TIME=="full time") %>%
  filter(PARTICIPATING!="Missing") %>%
  rename(`Participates in Employer Retirement Plan` = PARTICIPATING) %>%
  group_by(`Participates in Employer Retirement Plan`) %>%
  summarise(count = sum(WPFINWGT)) %>%
  ungroup() %>%
  mutate(Share = count / sum(count)*100) %>%
  select(-c(count)) %>%
  mutate(year = 2022)
  
participate_2023 = sipp_2023 %>%
  filter(in_age_range =="yes") %>%
  filter(FULL_PART_TIME=="full time") %>%
  filter(PARTICIPATING!="Missing") %>%
  rename(`Participates in Employer Retirement Plan` = PARTICIPATING) %>%
  group_by(`Participates in Employer Retirement Plan`) %>%
  summarise(count = sum(WPFINWGT)) %>%
  ungroup() %>%
  mutate(Share = count / sum(count)*100) %>%
  select(-c(count)) %>%
  mutate(year = 2023)


############ matching
match_2021 = sipp_2021 %>%
  filter(in_age_range =="yes") %>%
  filter(FULL_PART_TIME=="full time") %>%
  filter(MATCHING!="Missing") %>%
  rename(`Employer contributes to Employer Retirement Plan`=MATCHING) %>%
  group_by(`Employer contributes to Employer Retirement Plan`) %>%
  summarise(count = sum(WPFINWGT)) %>%
  ungroup() %>%
  mutate(Share = count / sum(count)*100) %>%
  select(-c(count)) %>%
  mutate(year = 2021)
  
match_2022 = sipp_2022 %>%
  filter(in_age_range =="yes") %>%
  filter(FULL_PART_TIME=="full time") %>%
  filter(MATCHING!="Missing") %>%
  rename(`Employer contributes to Employer Retirement Plan`=MATCHING) %>%
  group_by(`Employer contributes to Employer Retirement Plan`) %>%
  summarise(count = sum(WPFINWGT)) %>%
  ungroup() %>%
  mutate(Share = count / sum(count)*100) %>%
  select(-c(count)) %>%
  mutate(year = 2022)
  
match_2023 = sipp_2023 %>%
  filter(in_age_range =="yes") %>%
  filter(FULL_PART_TIME=="full time") %>%
  filter(MATCHING!="Missing") %>%
  rename(`Employer contributes to Employer Retirement Plan`=MATCHING) %>%
  group_by(`Employer contributes to Employer Retirement Plan`) %>%
  summarise(count = sum(WPFINWGT)) %>%
  ungroup() %>%
  mutate(Share = count / sum(count)*100) %>%
  select(-c(count)) %>%
  mutate(year = 2023)


access = bind_rows(access_2021, access_2022, access_2023) %>%
  filter(`Has access to an Employer Retirement Plan`=="Yes") %>%
  select(Share, year) %>%
  rename(`Has access to an Employer Retirement Plan` = Share)

participate = bind_rows(participate_2021, participate_2022, participate_2023) %>%
  filter(`Participates in Employer Retirement Plan`=="Yes") %>%
  select(Share, year) %>%
  rename(`Participates in Employer Retirement Plan` = Share)

match = bind_rows(match_2021, match_2022, match_2023) %>%
  filter(`Employer contributes to Employer Retirement Plan`=="Yes") %>%
  select(Share, year) %>%
  rename(`Employer contributes to Employer Retirement Plan` = Share)

annual = merge(access, participate, by = "year")
annual = merge(annual, match, by = "year")
write.xlsx(annual, "annual_data.xlsx")
