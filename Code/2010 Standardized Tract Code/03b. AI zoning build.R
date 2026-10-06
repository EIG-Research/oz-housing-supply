# Ben Glasner 01/03/2025

# crosswalk OZ eligibility from 2010 to 2020 census tract definitions;
# identify full or partial matches, and composition of tract eligibility qualifications.

# remove dependencies
rm(list = ls())

# packages
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
#################
### Set paths ###
#################
# Define user-specific project directories
project_directories <- list(
  "name" = "PATH TO GITHUB REPO",
  "Benjamin Glasner" = "C:/Users/Benjamin Glasner/EIG Dropbox/Benjamin Glasner/GitHub/oz-housing-supply",
  "bngla" = "C:/Users/bngla/EIG Dropbox/Benjamin Glasner/GitHub/oz-housing-supply",
  "Research" = "C:/Users/Research/EIG Dropbox/Benjamin Glasner/GitHub/oz-housing-supply"
)

# Setting project path based on current user
current_user <- Sys.info()[["user"]]
if (!current_user %in% names(project_directories)) {
  stop("Root folder for current user is not defined.")
}

path_project <- project_directories[[current_user]]

path_data <- file.path(path_project, "Data")
path_data_USPS <- file.path(path_data, "HUD USPS Data Standardized") # not publicly available.
path_data_tract <- file.path(path_data, "Tract Characteristics")
path_data_zoning <- file.path(path_data, "AI Zoning project")
path_data_crosswalks <- file.path(path_data, "HUD crosswalks")
path_output <- file.path(path_project, "Output")

  
  
#######################################################################
# read in ai zoning data from https://github.com/dmilo75/ai-zoning
# NYU AI-Zoning Project
# Contact Information
# For any inquiries, please contact dm4766@stern.nyu.edu.

setwd(path_data_zoning)

# 
tract <- read_csv("tract.csv") %>% mutate(geoid = as.double(tract)) %>% select(geoid,Overall_Index)
state <- read_csv("state.csv") %>% mutate(STATEFP = as.character(state)) %>% select(STATEFP, state_name, Overall_Index) %>% rename(Overall_Index_State = Overall_Index)
csa <- read_csv("csa.csv")
cbsa <- read_csv("cbsa.csv")
zcta <- read_csv("zcta.csv")

CENSUS_TRACT_CROSSWALK <- readxl::read_excel(file.path(path_data_crosswalks, "CENSUS_TRACT_CROSSWALK_2010_to_2020_2020.xlsx"))

# Tract zoning is currently in 202 tract boundaries. Let's convert to 2010.
tract_2010 <- tract %>%
  # turn numeric geoid into an 11-char string with leading zeros
  mutate(GEOID_2020 = str_pad(as.character(geoid), 
                              width = 11, 
                              side  = "left", 
                              pad   = "0")) %>%
  # join to your crosswalk, matching GEOID_2020 to the character geoid in CENSUS_TRACT_CROSSWALK
  right_join(CENSUS_TRACT_CROSSWALK, 
             # by           = c("GEOID_2020" = "geoid"), 
             relationship = "many-to-many") %>%
  na.omit() %>%
  mutate(
    Overall_Index = Overall_Index*RES_RATIO
    ) %>%
  group_by(GEOID_2010) %>%
  summarise(
    Overall_Index = sum(Overall_Index, na.rm = TRUE)
    ) %>%
  ungroup() %>%
  mutate(geoid = as.numeric(GEOID_2010)) %>%
  select(geoid, Overall_Index)

setwd(path_data_tract)
load(file = "LIC_OZ_neighbor_data_2010.RData")

# Merge Tract level zoning data in, then progress up by geography 

tract_ai_zoning <- neighbor_data %>%
  left_join(tract_2010) %>%
  left_join(state) %>%
  mutate(Zoning_Index = if_else(!is.na(Overall_Index),Overall_Index,Overall_Index_State))


write.csv(tract_ai_zoning, "2010_tracts_with_ai_zoning_and_neighboring.csv")
save(tract_ai_zoning, file = "2010_tracts_with_ai_zoning_and_neighboring.RData")
