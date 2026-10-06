# HUD Aggregated USPS Administrative Data on Vacancies - 2020 standardized version
# final dataset construction step.

# NOTE FOR USERS: the USPS data was shared generously by Alexander Din at the
# US Department of Housing and Urban Development, and is not authorized for 
# public release.
# you may access a public facing version of this data by signing up for
# an account through HUD here: 
# https://www.huduser.gov/portal/home.html
# keep in mind that the use of this public facing data will impact the generated
# results.

# Ben Glasner 01/03/2025

rm(list = ls())
options(scipen = 999)
set.seed(42)

###########################
###   Load Packages     ###
###########################

library(dplyr)
library(tidyr)
library(readxl)
library(foreign)
library(janitor)  # Ensure janitor is loaded
library(plm)
library(ggplot2)
library(tigris)
library(stringr)

#################
### Set paths ###
#################
# Define user-specific project directories
project_directories <- list(
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
path_data_USPS <- file.path(path_data, "2020 Standardized")
path_data_tract <- file.path(path_data, "Tract Characteristics")
path_data_crosswalks <- file.path(path_data, "HUD crosswalks")
path_data_lodes <- file.path(path_data, "LODES") # Longitudinal Employer-Household Dynamics https://lehd.ces.census.gov/

path_export <- file.path(path_data, "2020 standardized to 2010")


#################
### Data load ###
#################
# tract - crosswalk

CENSUS_TRACT_CROSSWALK <- read_excel(file.path(path_data_crosswalks, "CENSUS_TRACT_CROSSWALK_2010_to_2020_2020.xlsx"))

#####################
### Load HUD data ###
#####################

setwd(path_data_USPS)
data_list <- list()

# List all .xlsx files
excel_files <- list.files(path = path_data_USPS, pattern = "\\.xlsx$", full.names = TRUE)

# Read each Excel file into a list of data frames
data_list_xlsx <- lapply(excel_files, read_excel)


for(i in seq_along(data_list_xlsx)){
  data_list_xlsx[[i]] <- data_list_xlsx[[i]] %>%
    rename(GEOID_2020 = TRACT20) %>%
    mutate(geoid = as.numeric(GEOID_2020)) %>%
    right_join(CENSUS_TRACT_CROSSWALK, relationship = "many-to-many") %>%
    mutate(TOTAL_RESIDENTIAL_ADDRESSES = TOTAL_RESIDENTIAL_ADDRESSES*RES_RATIO,
           ACTIVE_RESIDENTIAL_ADDRESSES = ACTIVE_RESIDENTIAL_ADDRESSES*RES_RATIO,
           STV_RESIDENTIAL_ADDRESSES = STV_RESIDENTIAL_ADDRESSES*RES_RATIO,
           LTV_RESIDENTIAL_ADDRESSES = LTV_RESIDENTIAL_ADDRESSES*RES_RATIO,
           NO_STAT_RESIDENTIAL_ADDRESSES = NO_STAT_RESIDENTIAL_ADDRESSES*RES_RATIO
    ) %>%
    group_by(GEOID_2010, YEAR,MONTH,YEAR_MONTH) %>%
    summarise(TOTAL_RESIDENTIAL_ADDRESSES = sum(TOTAL_RESIDENTIAL_ADDRESSES, na.rm = TRUE),
              ACTIVE_RESIDENTIAL_ADDRESSES = sum(ACTIVE_RESIDENTIAL_ADDRESSES, na.rm = TRUE),
              STV_RESIDENTIAL_ADDRESSES = sum(STV_RESIDENTIAL_ADDRESSES, na.rm = TRUE),
              LTV_RESIDENTIAL_ADDRESSES = sum(LTV_RESIDENTIAL_ADDRESSES, na.rm = TRUE),
              NO_STAT_RESIDENTIAL_ADDRESSES = sum(NO_STAT_RESIDENTIAL_ADDRESSES, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(geoid = as.numeric(GEOID_2010)) %>%
    select(geoid, YEAR,MONTH,YEAR_MONTH, TOTAL_RESIDENTIAL_ADDRESSES:NO_STAT_RESIDENTIAL_ADDRESSES)
  
  
}


########
# Export
setwd(path_export)
for(i in seq_along(data_list_xlsx)){
  month <- data_list_xlsx[[i]] %>% select(MONTH) %>% distinct() %>% na.omit() %>% pull()
  year <- data_list_xlsx[[i]] %>% select(YEAR) %>% distinct() %>% na.omit() %>% pull()
  
  writexl::write_xlsx(data_list_xlsx[[i]],path = paste0("TRACT10_",month,year,".xlsx"))
}

