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
# path_data_USPS <- file.path(path_data, "2010 Standardized")
path_data_USPS <- file.path(path_data, "2020 standardized to 2010")
path_data_tract <- file.path(path_data, "Tract Characteristics")
path_data_crosswalks <- file.path(path_data, "HUD crosswalks")
path_data_lodes <- file.path(path_data, "LODES") # Longitudinal Employer-Household Dynamics https://lehd.ces.census.gov/

path_output <- file.path(path_project, "Output")


#################
### Data load ###
#################
# tract - crosswalk

# CENSUS_TRACT_CROSSWALK <- read_excel(file.path(path_data_crosswalks, "CENSUS_TRACT_CROSSWALK_2010_to_2020_2020.xlsx"))
# 
# # time invariant characteristics
# NCWM_Economic_Development2 <- read_excel(file.path(path_data, "NCWM_Economic_Development2.xlsx")) %>%
#   mutate(geoid = as.numeric(GEOID))

#####################
### Load HUD data ###
#####################

setwd(path_data_USPS)
data_list <- list()

# List all .xlsx files
excel_files <- list.files(path = path_data_USPS, pattern = "\\.xlsx$", full.names = TRUE)

# Read each Excel file into a list of data frames
data_list_xlsx <- lapply(excel_files, read_excel)

# Combine all data frames into one using bind_rows
USPS_data_xlsx <- bind_rows(data_list_xlsx)


USPS_data <- USPS_data_xlsx %>%
  mutate(date = as.Date(paste0(YEAR, "/", MONTH, "/01"), format = "%Y/%m/%d"))

# simplify the variable list:
USPS_data <- USPS_data %>%
  select(
    YEAR:YEAR_MONTH,date,
    # , TRACT10
    , geoid
    , TOTAL_RESIDENTIAL_ADDRESSES:NO_STAT_RESIDENTIAL_ADDRESSES
    )

USPS_data <- USPS_data %>% 
  mutate(GEOID_char = sprintf("%011.0f", as.numeric(geoid)))  # Adjust column name if needed

# Include DC explicitly
states_to_use <- c(state.abb, "DC")

# Download tracts for all specified states, including DC
tracts_all <- rbind_tigris(lapply(states_to_use, function(state) {
  tryCatch({
    tracts(state, year = 2017)
  }, error = function(e) NULL)
}))

# Drop geometry and select key identifiers
tracts_data <- tracts_all %>%
  sf::st_drop_geometry() %>%
  select(GEOID, STATEFP, COUNTYFP)

# Get state-level metadata
states <- tigris::states(year = 2017)

# Join state metadata to tract data
tracts_data <- states %>%
  as.data.frame() %>%
  select(REGION, DIVISION, STATEFP, NAME) %>%
  inner_join(tracts_data, by = "STATEFP") %>%
  rename(GEOID_char = GEOID)

USPS_data <- USPS_data %>%
  left_join(tracts_data)

#################################
# Merge in Tract Characteristics
# There were 84,414 census tracts in the United States in 2022
# There were 74,134 census tracts in the United States and its territories in 2010.

setwd(path_data_tract)

# see 2. OZ eligible tracts build.R
tract_designation <- read.csv("2010_tracts_with_2010_qualifications.csv") %>%
  mutate(geoid = as.numeric(GEOID)) %>% 
  select(-X) %>%
  select(-GEOID)
  
tract_designation_geoid <- sort(unique(tract_designation$geoid))

# read National Center for Education Statistics 2021 classifications,
# for urban-rural tract classification schema
NCES_Locales_Tract_2020 <- readr::read_csv("NCES Locales Tract 2010 CSV_v1.csv") %>%
  rename(geoid = FIPS) %>%
  select(geoid, `Type tract`) 


# For the ACS data, crosswalk using the residential ratio from the HUD data on
# addresses since the variables are at the person level
load("ACS_2012_2023_2010tracts.RData")

ACS_2012_2023 <- ACS_2012_2023 %>% rename(YEAR = year) %>% mutate(YEAR = as.character(YEAR))
# openxlsx::write.xlsx(ACS_2012_2023, file = "ACS_2012_2023_2010tracts.xlsx") # save as xlsx.


# tract neighboring info
# load("LIC_OZ_neighbor_data_2010.RData")
# tract neighboring and zoning data

load("2010_tracts_with_ai_zoning_and_neighboring.RData")
#########################################
# time invariant tract characteristics

time_inv <- NCES_Locales_Tract_2020  %>%
  left_join(tract_designation) %>%
  left_join(tract_ai_zoning) %>%
  mutate(
    `OZ Designation` = case_when(
      Designation %in% c("LIC selected",
                                  "Contiguous selected") 
      ~ 1,
      TRUE ~ 0
    )
  
  ) 

# Should be 84122 geoids
time_inv <- time_inv %>%
  distinct(geoid, .keep_all = TRUE)

setwd(path_data)
save(time_inv, file = "Tract_2010_time_invariant_characteristics.RData")

# Step 1: Summarize and pivot the data
pivot_tbl <- time_inv %>%
  group_by(Designation, `OZ Designation`) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = `OZ Designation`,
    values_from = count,
    values_fill = list(count = 0)
  )

# Step 2: Add a totals column by summing across each row
pivot_tbl <- pivot_tbl %>%
  mutate(Total = rowSums(across(where(is.numeric))))

# Step 3: Create a summary row that sums each numeric column
total_row <- pivot_tbl %>%
  summarise(across(where(is.numeric), sum)) %>%
  mutate(Designation_category = "Total")

# Step 4: Append the summary row to the pivot table
final_table <- bind_rows(pivot_tbl, total_row)

final_table


USPS_data <- USPS_data  %>%
  # mutate(geoid = as.numeric(as.character(TRACT10))) %>% 
  filter(YEAR >= 2014) %>%
  left_join(time_inv) %>%
  filter(!is.na(STATEFP)) %>%
  left_join(ACS_2012_2023) 

USPS_data <- USPS_data %>%
  mutate(Designation_category_detailed = case_when(
    Designation == "Contiguous not selected" & LIC_oz_neighbor == 1 ~ "Contiguous not selected, border tract",
    Designation == "Contiguous not selected" & LIC_oz_neighbor == 0 ~ "Contiguous not selected, not a border tract",
    
    Designation == "Ineligible" & LIC_oz_neighbor == 1 ~ "Ineligible, border tract",
    Designation == "Ineligible" & LIC_oz_neighbor == 0 ~ "Ineligible, not a border tract",
    
    Designation == "LIC not selected" & LIC_oz_neighbor == 1 ~ "LIC not selected, border tract",
    Designation == "LIC not selected" & LIC_oz_neighbor == 0 ~ "LIC not selected, not a border tract",
    
    Designation == "LIC selected" ~ "LIC selected",
    Designation == "Contiguous selected" ~ "Contiguous selected",
    
    TRUE ~ NA
  ))


# Count the number of quarters covered by the data set for each tract
USPS_data <- USPS_data %>%
  distinct(geoid, date, .keep_all = TRUE) %>%
  group_by(geoid) %>%
  mutate(number_of_quarters = n()) %>%
  ungroup()

max_quarters <- max(USPS_data$number_of_quarters)

# Filter out tracts with missing quarters
USPS_data <- USPS_data %>%
  filter(number_of_quarters == max_quarters) 

# After all the cleaning counts are:
USPS_data %>%
  filter(YEAR_MONTH == "2020-03") %>% 
  group_by(`Designation`, `OZ Designation`) %>%
  summarise(count = n()) %>%
  ungroup() 

USPS_data %>%
  filter(YEAR_MONTH == "2020-03") %>% 
  group_by(`Designation_category_detailed`, `OZ Designation`) %>%
  summarise(count = n()) %>%
  ungroup() 

sort(unique((USPS_data$date)))
table(USPS_data$date, USPS_data$Designation)

table(USPS_data$NAME, USPS_data$YEAR_MONTH)

########
# Export
setwd(path_data)
save(USPS_data, file = "USPS_tract_vacancy.RData")

