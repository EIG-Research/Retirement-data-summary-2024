# load SIPP data and compile statistics for retirement demographics.

rm(list = ls())
###########################
###   Load Packages     ###
###########################

library(haven)
library(dplyr)
library(tidyr)
library(openxlsx)
library(ggplot2)

#################
### Set paths ###
#################
# Define user-specific project directories
project_directories <- list(
  "bglasner" = "C:/Users/bglasner/EIG Dropbox/Benjamin Glasner/GitHub/Retirement-data-summary-2024",
  "bngla" = "C:/Users/bngla/EIG Dropbox/Benjamin Glasner/GitHub/Retirement-data-summary-2024",
  "Benjamin Glasner" = "C:/Users/Benjamin Glasner/EIG Dropbox/Benjamin Glasner/GitHub/Retirement-data-summary-2024",
  "sarah" = "/Users/sarah/Library/CloudStorage/GoogleDrive-sarah@eig.org/My Drive/projects/retirement"
)

# Setting project path based on current user
current_user <- Sys.info()[["user"]]
if (!current_user %in% names(project_directories)) {
  stop("Root folder for current user is not defined.")
}
path_project <- project_directories[[current_user]]
path_data = file.path(path_project, "Data")
path_output = file.path(path_project, "Output")

# Set working directory for SIPP data
setwd(path_data)

####################
#### load data #####
####################

sipp_2023 <- read.csv("sipp_2023_wrangled.csv") # 2023 simplified dataset from stata export.

########################
#### Summary stats #####
########################

sipp_2023 %>%
  group_by(FULL_PART_TIME,ANY_RETIREMENT_ACCESS) %>%
  summarise(Observations = n(),
            weighted_n = sum(WPFINWGT)) %>%
  ungroup() %>%
  group_by(FULL_PART_TIME) %>%
  mutate(share = weighted_n/sum(weighted_n)) 

sipp_2023 %>%
  filter(PARTICIPATING != "Missing") %>%
  group_by(FULL_PART_TIME,PARTICIPATING) %>%
  summarise(Observations = n(),
            weighted_n = sum(WPFINWGT)) %>%
  ungroup() %>%
  group_by(FULL_PART_TIME) %>%
  mutate(share = weighted_n/sum(weighted_n)) 

sipp_2023 %>%
  filter(MATCHING != "Missing") %>%
  group_by(FULL_PART_TIME,MATCHING) %>%
  summarise(Observations = n(),
            weighted_n = sum(WPFINWGT)) %>%
  ungroup() %>%
  group_by(FULL_PART_TIME) %>%
  mutate(share = weighted_n/sum(weighted_n)) 
