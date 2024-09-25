# summary statistics on matching, participation, access

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

# Define user-specific project directories
project_directories <- list(
  "bglasner" = "C:/Users/bglasner/EIG Dropbox/Benjamin Glasner/GitHub/Retirement-data-summary-2024",
  "bngla" = "C:/Users/bngla/EIG Dropbox/Benjamin Glasner/GitHub/Retirement-data-summary-2024",
  "Benjamin Glasner" = "C:/Users/Benjamin Glasner/EIG Dropbox/Benjamin Glasner/GitHub/Retirement-data-summary-2024",
  "sarah" = "/Users/sarah/Library/CloudStorage/GoogleDrive-sarah@eig.org/My Drive/PROJECTS/ACTIVE/RETIREMENT"
)

# Setting project path based on current user
current_user <- Sys.info()[["user"]]
if (!current_user %in% names(project_directories)) {
  stop("Root folder for current user is not defined.")
}
path_project <- project_directories[[current_user]]
path_data = file.path(path_project, "Data")
path_output = file.path(path_project, "Output/SIPP")

# Set working directory for SIPP data
setwd(path_output)

sipp_2023 = read.csv("sipp_2023_wrangled.csv")



###################################################
## Access, Participation, and Matching - overall ##
###################################################

# access

access = sipp_2023 %>%
  filter(in_age_range =="yes") %>%
  filter(FULL_PART_TIME=="full time") %>% # change to "part time" for alternative cut.
  filter(ANY_RETIREMENT_ACCESS!="Missing") %>%
  rename(`Has access to an Employer Retirement Plan`=ANY_RETIREMENT_ACCESS) %>%
  group_by(`Has access to an Employer Retirement Plan`) %>%
  summarise(count = sum(WPFINWGT)) %>%
  ungroup() %>%
  mutate(Share = count / sum(count)*100) %>%
  select(-c(count))

access_all = sipp_2023 %>% # full and part time workers
  filter(in_age_range =="yes") %>%
  filter(ANY_RETIREMENT_ACCESS!="Missing") %>%
  rename(`Has access to an Employer Retirement Plan`=ANY_RETIREMENT_ACCESS) %>%
  group_by(`Has access to an Employer Retirement Plan`) %>%
  summarise(count = sum(WPFINWGT)) %>%
  ungroup() %>%
  mutate(Share = count / sum(count)*100) %>%
  select(-c(count))

access


# participate

participate = sipp_2023 %>%
  filter(in_age_range =="yes") %>%
  filter(FULL_PART_TIME=="full time") %>%
  filter(PARTICIPATING!="Missing") %>%
  rename(`Participates in Employer Retirement Plan` = PARTICIPATING) %>%
  group_by(`Participates in Employer Retirement Plan`) %>%
  summarise(count = sum(WPFINWGT)) %>%
  ungroup() %>%
  mutate(Share = count / sum(count)*100) %>%
  select(-c(count))

participate


# match

match = sipp_2023 %>%
  filter(in_age_range =="yes") %>%
  filter(FULL_PART_TIME=="full time") %>%
  filter(MATCHING!="Missing") %>%
  rename(`Employer contributes to Employer Retirement Plan`=MATCHING) %>%
  group_by(`Employer contributes to Employer Retirement Plan`) %>%
  summarise(count = sum(WPFINWGT)) %>%
  ungroup() %>%
  mutate(Share = count / sum(count)*100) %>%
  select(-c(count))

match_all = sipp_2023 %>% # full and part time workers
  filter(in_age_range =="yes") %>%
  filter(MATCHING!="Missing") %>%
  rename(`Employer contributes to Employer Retirement Plan`=MATCHING) %>%
  group_by(`Employer contributes to Employer Retirement Plan`) %>%
  summarise(count = sum(WPFINWGT)) %>%
  ungroup() %>%
  mutate(Share = count / sum(count)*100) %>%
  select(-c(count))

match


#######
# based on labor force estimates --
print("total full time workers without matching")
round(match[1,2]/100*93.4,2)

print("total full time workers without access")
round(access[1,2]/100*93.4,2)

print("total full time workers not participating")
round(participate[1,2]/100*93.4,2)


print("all workers without access (full and part)")
round(access_all[1,2]/100*107.9,2)

print("all workers without matching (full and part)")
round(match_all[1,2]/100*107.9,2)

#########################################################
## Access, Participation, Matching - by income deciles ##
#########################################################

ACCESS_decile = sipp_2023 %>%
  filter(in_age_range =="yes") %>%
  mutate(INCOME_DECILE = ntile(TPTOTINC, 10)) %>%
  filter(FULL_PART_TIME=="full time") %>%
  group_by(ANY_RETIREMENT_ACCESS, INCOME_DECILE) %>%
  summarise(count = sum(WPFINWGT)) %>%
  ungroup() %>%
  group_by(INCOME_DECILE) %>%
  mutate(Share = count / sum(count)*100) %>%
  select(-c(count))  %>%
  pivot_wider(names_from = INCOME_DECILE,
              values_from = "Share")


PARTICIPATE_decile = sipp_2023 %>%
  filter(in_age_range =="yes") %>%
  filter(PARTICIPATING !="Missing") %>%
  filter(FULL_PART_TIME=="full time") %>%
  mutate(INCOME_DECILE = ntile(TPTOTINC, 10)) %>%
  group_by(PARTICIPATING, INCOME_DECILE) %>%
  summarise(count = sum(WPFINWGT)) %>%
  ungroup() %>%
  group_by(INCOME_DECILE) %>%
  mutate(Share = count / sum(count)*100) %>%
  select(-c(count))%>%
  pivot_wider(names_from = INCOME_DECILE,
              values_from = "Share")

MATCH_decile = sipp_2023 %>%
  filter(in_age_range =="yes") %>%
  filter(MATCHING!="Missing") %>%
  filter(FULL_PART_TIME=="full time") %>%
  mutate(INCOME_DECILE = ntile(TPTOTINC, 10)) %>%
  group_by(MATCHING, INCOME_DECILE) %>%
  summarise(count = sum(WPFINWGT)) %>%
  ungroup() %>%
  group_by(INCOME_DECILE) %>%
  mutate(Share = count / sum(count)*100) %>%
  select(-c(count))%>%
  pivot_wider(names_from = INCOME_DECILE,
              values_from = "Share")

# export for plotting
write.xlsx(ACCESS_decile, paste(path_output, "ACCESS_decile.xlsx", sep = "/"))
write.xlsx(PARTICIPATE_decile, paste(path_output, "PARTICIPATE_decile.xlsx", sep = "/"))
write.xlsx(MATCH_decile, paste(path_output, "MATCH_decile.xlsx", sep = "/"))


# display the deciles (these are monthly earnings)
earning_Deciles = sipp_2023 %>%
  filter(in_age_range =="yes") %>%
  filter(FULL_PART_TIME=="full time") %>%
  mutate(INCOME_DECILE = ntile(TPTOTINC, 10)) %>%
  ungroup() %>%
  group_by(INCOME_DECILE) %>%
  mutate(val = max(TPTOTINC,na.rm=TRUE)) %>%
  select(val, INCOME_DECILE)

earning_Deciles = unique(earning_Deciles)
earning_Deciles


# access -- top and bottom 50%
sipp_2023 %>%
  filter(in_age_range =="yes") %>%
  mutate(INCOME_DECILE = ntile(TPTOTINC, 2)) %>%
  filter(FULL_PART_TIME=="full time") %>%
  group_by(ANY_RETIREMENT_ACCESS, INCOME_DECILE) %>%
  summarise(count = sum(WPFINWGT)) %>%
  ungroup() %>%
  group_by(INCOME_DECILE) %>%
  mutate(Share = count / sum(count)*100) %>%
  select(-c(count))  %>%
  pivot_wider(names_from = INCOME_DECILE,
              values_from = "Share")



#####################################
# matching conditional on participating

# for the lowest decile.
sipp_2023 %>%
  filter(in_age_range =="yes") %>%
  filter(FULL_PART_TIME=="full time") %>%
  filter(PARTICIPATING =="Yes") %>%
  filter(MATCHING!="Missing") %>%
  filter(TPTOTINC <=2199) %>%
  rename(`Employer contributes to Employer Retirement Plan`=MATCHING) %>%
  group_by(`Employer contributes to Employer Retirement Plan`) %>%
  summarise(count = sum(WPFINWGT)) %>%
  ungroup() %>%
  mutate(Share = count / sum(count)*100)


# for the highest decile.
sipp_2023 %>%
  filter(in_age_range =="yes") %>%
  filter(FULL_PART_TIME=="full time") %>%
  filter(PARTICIPATING =="Yes") %>%
  filter(MATCHING!="Missing") %>%
  filter(TPTOTINC >14525) %>%
  rename(`Employer contributes to Employer Retirement Plan`=MATCHING) %>%
  group_by(`Employer contributes to Employer Retirement Plan`) %>%
  summarise(count = sum(WPFINWGT)) %>%
  ungroup() %>%
  mutate(Share = count / sum(count)*100) %>%
  select(-c(count))



###############################
## Matching - by race x edu ##
###############################

MATCH_race_edu <- sipp_2023 %>% 
  filter(in_age_range =="yes") %>%
  filter(MATCHING!="Missing") %>%
  filter(FULL_PART_TIME=="full time") %>%
  group_by(MATCHING, RACE, EDUCATION) %>%
  summarise(count = sum(WPFINWGT)) %>%
  ungroup() %>%
  group_by(RACE, EDUCATION) %>%
  mutate(Share = round(count / sum(count),3)) %>%
  select(-c(count))

    # education overall.
    MATCH_race_edu_overall <- sipp_2023 %>% 
      filter(in_age_range =="yes") %>%
      filter(MATCHING!="Missing") %>%
      filter(FULL_PART_TIME=="full time") %>%
      group_by(MATCHING, RACE) %>%
      summarise(count = sum(WPFINWGT)) %>%
      ungroup() %>%
      group_by(RACE) %>%
      mutate(Share = round(count / sum(count),3)) %>%
      select(-c(count)) %>%
      mutate(EDUCATION = "Overall")

MATCH_race_edu = rbind(MATCH_race_edu, MATCH_race_edu_overall)

MATCH_race_edu = MATCH_race_edu %>% filter(RACE!="Mixed/Other") %>%
  mutate(MATCHING = case_when(
    MATCHING =="Yes" ~ "Has Matching",
    MATCHING =="No" ~ "Lacks Matching"
  )) %>%
  mutate(label = ifelse(MATCHING == "Has Matching", Share, NA)) %>%
  mutate(order = case_when(
    EDUCATION == "Overall" ~ 1,
    EDUCATION == "High School or less" ~ 2,
    EDUCATION == "Some college" ~ 3,
    EDUCATION == "Bachelor's degree or higher" ~ 4
  )) %>%
  mutate(EDUCATION = ifelse(EDUCATION == "Bachelor's degree or higher","BA+",EDUCATION),
         EDUCATION = ifelse(EDUCATION == "High School or less" , "HS or less",EDUCATION))
     
write.xlsx(MATCH_race_edu,
           paste(path_output, "race_education_plot.xlsx",sep="/"))
 
# by edu only
sipp_2023 %>% 
  filter(in_age_range =="yes") %>%
  filter(MATCHING!="Missing") %>%
  filter(FULL_PART_TIME=="full time") %>%
  group_by(MATCHING, EDUCATION) %>%
  summarise(count = sum(WPFINWGT)) %>%
  ungroup() %>%
  group_by(EDUCATION) %>%
  mutate(hare = round(count / sum(count)*100,1)) %>%
  select(-c(count))

# by race only
sipp_2023 %>% 
  filter(in_age_range =="yes") %>%
  filter(MATCHING!="Missing") %>%
  filter(FULL_PART_TIME=="full time") %>%
  group_by(MATCHING, RACE) %>%
  summarise(count = sum(WPFINWGT)) %>%
  ungroup() %>%
  group_by(RACE) %>%
  mutate(Share = round(count / sum(count)*100,1)) %>%
  select(-c(count))


# R ggplot bar graphs for matching
MATCH_race_edu %>%
  ggplot(aes(fill=MATCHING, y=Share, x=reorder(EDUCATION,order))) + 
  geom_bar(position="stack", stat="identity") +
  facet_wrap(~RACE) +
  labs(title = "Matching by Race and Education, 2023") +
  theme_classic() +
  theme(plot.title = element_text(face = "bold", color = "#1a654d"),
        legend.position = "top",
        legend.title= element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 7),
        axis.text.y = element_blank(),
        legend.text=element_text(size=7)) +
  scale_fill_manual(values = c("#e1ad28","#b3d6dd")) +
  geom_text(aes(label = scales::percent(round(label/100,3)),accuracy = 1L),vjust = 1.2,size=2.5)



###############################################
## Matching, Participation, Access - by sex ##
###############################################

sipp_2023 %>% 
  filter(in_age_range =="yes") %>%
  filter(FULL_PART_TIME=="full time") %>%
  group_by(ANY_RETIREMENT_ACCESS, SEX) %>%
  summarise(count = sum(WPFINWGT)) %>%
  ungroup() %>%
  group_by(SEX) %>%
  mutate(Share = round(count / sum(count)*100,1)) %>%
  select(-c(count)) %>%
  filter(ANY_RETIREMENT_ACCESS == "No") %>%
  pivot_wider(names_from = SEX,
              values_from = Share) %>%
  mutate(`Female - Male Participating Gap` = Female - Male)


sipp_2023 %>% 
  filter(in_age_range =="yes") %>%
  filter(FULL_PART_TIME=="full time") %>%
  group_by(PARTICIPATING, SEX) %>%
  summarise(count = sum(WPFINWGT)) %>%
  ungroup() %>%
  group_by(SEX) %>%
  mutate(Share = round(count / sum(count)*100,1)) %>%
  select(-c(count)) %>%
  filter(PARTICIPATING != "Missing") %>%
  pivot_wider(names_from = SEX,
              values_from = Share) %>%
  mutate(`Female - Male Participating Gap` = Female - Male)

sipp_2023 %>% 
  filter(in_age_range =="yes") %>%
  filter(FULL_PART_TIME=="full time") %>%
  group_by(MATCHING, SEX) %>%
  summarise(count = sum(WPFINWGT)) %>%
  ungroup() %>%
  group_by(SEX) %>%
  mutate(Share = round(count / sum(count)*100,1)) %>%
  select(-c(count)) %>%
  filter(MATCHING != "Missing") %>%
  pivot_wider(names_from = SEX,
              values_from = Share) %>%
  mutate(`Female - Male Participating Gap` = Female - Male)


#gap by low education level
sipp_2023 %>% 
  filter(in_age_range =="yes") %>%
  filter(FULL_PART_TIME=="full time") %>%
  filter(EDUCATION == "High School or less") %>%
  group_by(ANY_RETIREMENT_ACCESS, SEX) %>%
  summarise(count = sum(WPFINWGT)) %>%
  ungroup() %>%
  group_by(SEX) %>%
  mutate(Share = round(count / sum(count)*100,1)) %>%
  select(-c(count)) %>%
  filter(ANY_RETIREMENT_ACCESS != "Missing") %>%
  pivot_wider(names_from = SEX,
              values_from = Share) %>%
  mutate(`Female - Male Participating Gap` = Female - Male)


sipp_2023 %>% 
  filter(in_age_range =="yes") %>%
  filter(FULL_PART_TIME=="full time") %>%
  filter(EDUCATION == "High School or less") %>%
  group_by(PARTICIPATING, SEX) %>%
  summarise(count = sum(WPFINWGT)) %>%
  ungroup() %>%
  group_by(SEX) %>%
  mutate(Share = round(count / sum(count)*100,1)) %>%
  select(-c(count)) %>%
  filter(PARTICIPATING != "Missing") %>%
  pivot_wider(names_from = SEX,
              values_from = Share) %>%
  mutate(`Female - Male Participating Gap` = Female - Male)

sipp_2023 %>% 
  filter(in_age_range =="yes") %>%
  filter(FULL_PART_TIME=="full time") %>%
  filter(EDUCATION == "High School or less") %>%
  group_by(MATCHING, SEX) %>%
  summarise(count = sum(WPFINWGT)) %>%
  ungroup() %>%
  group_by(SEX) %>%
  mutate(Share = round(count / sum(count)*100,1)) %>%
  select(-c(count)) %>%
  filter(MATCHING != "Missing") %>%
  pivot_wider(names_from = SEX,
              values_from = Share) %>%
  mutate(`Female - Male Participating Gap` = Female - Male)


########
# RSAA #
########
# $42,200 is median income for 16+ workers, full & part time.


# how many earning < $42,200 have access to retirement plan?
sipp_2023 %>%
  filter(TPTOTINC < 42200/12) %>%
  filter(TAGE>15) %>%
  group_by(ANY_RETIREMENT_ACCESS) %>%
  summarise(count = sum(WPFINWGT)) %>%
  ungroup() %>%
  mutate(Share = count / sum(count)*100) %>%
  select(-c(count))


# what's the share of those without access earn < $42,200?
sipp_2023 %>% 
  filter(TAGE>15) %>%
  mutate(under42_no_access = case_when(
    TPTOTINC < 42200/12 & ANY_RETIREMENT_ACCESS == "No" ~ "targeted group",
    TRUE ~ "not targeted group"
  )) %>%
  group_by(under42_no_access) %>%
  summarise(count = sum(WPFINWGT)) %>%
  ungroup() %>%
  mutate(Share = count / sum(count)*100) %>%
  select(-c(count))
  
