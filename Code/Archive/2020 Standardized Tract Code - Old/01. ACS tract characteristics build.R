# HUD Aggregated USPS Administrative Data on Vacancies
# Ben Glasner 12/09/2024

rm(list = ls())
options(scipen = 999)
set.seed(42)

###########################
###   Load Packages     ###
###########################
library(tidyr)
library(dplyr)
library(tidycensus)
library(tigris)
library(purrr)
library(stringr)
library(readxl)


#################
### Set paths ###
#################
# Define user-specific project directories
project_directories <- list(
  "name" = "PATH TO GITHUB REPO"
  )

# Setting project path based on current user
current_user <- Sys.info()[["user"]]
if (!current_user %in% names(project_directories)) {
  stop("Root folder for current user is not defined.")
}

path_project <- project_directories[[current_user]]

path_data <- file.path(path_project, "Data")
path_data_crosswalks <- file.path(path_data, "HUD crosswalks")
path_data_tract <- file.path(path_data, "Tract Characteristics")

##################
### Data build ###
##################

# Set Census API key
# Replace 'YOUR_CENSUS_API_KEY' with your actual Census API key
# census_api_key("YOUR_CENSUS_API_KEY", install = TRUE, overwrite = TRUE)

# Reload environment to use the key
readRenviron("~/.Renviron")

variables <- tidycensus::load_variables(dataset = "acs5", year = 2012) %>% filter(geography == "tract")

# Define the years of interest
years <- 2012:2023

# Define ACS variables for each metric
variables_poverty <- c(
  total_poverty = "B17001_001",   # Total population for poverty
  below_poverty = "B17001_002"    # Population below poverty level
)

variables_income <- c(
  median_income = "B19013_001"     # Median household income
)

variables_unemployment <- c(
  labor_force = "B23025_002",      # Total labor force
  unemployed = "B23025_005"        # Number of unemployed
)

variables_prime_age <- c(
  population_25_34 = "B06001_005",   # Population aged 25-34
  population_35_44 = "B06001_006",   # Population aged 35-44
  population_45_54 = "B06001_007",   # Population aged 45-54
  total_population = "B06001_001"     # Total (denominator)
)

variables_housing <- c(
  owner_occupied_housing_unit = "B25032_002",   # Owner-occupied housing units
  renter_occupied_housing_unit = "B25032_013",   # Renter-occupied housing units
  owner_occupied_solo_detached_housing_unit = "B25032_003",   # Owner-occupied housing units!!1, detached
  renter_occupied_solo_detached_housing_unit = "B25032_014"   # Renter-occupied housing units!!1, detached
)

# Combine all variables
all_variables <- c(
  variables_poverty,
  variables_income,
  variables_unemployment,
  variables_prime_age,
  variables_housing
)


    # Retrieve list of states and DC using tigris
    states_df <- tigris::states(cb = TRUE) %>%
      filter(!STUSPS %in% c("AS", "GU", "MP", "PR", "VI")) %>%  # Exclude territories
      select(NAME, STATEFP, STUSPS) %>%
      mutate(state_name = NAME,
             state_fips = STATEFP,
             state_abbr = STUSPS) %>%
      as.data.frame() %>%
      select(state_name, state_fips, state_abbr) %>% 
      arrange(state_fips)


# Function to fetch ACS data and calculate metrics for a given year
  # tidycensus requires users to specify each state for tract-level census api pulls.
  # loop across states and combine tract-level characteristics.

tract_data_list <- list()
tract_year <- list()

for(j in seq_along(states_df$state_fips)) {
  
  for(i in seq_along(years)) {
    
    # Fetch ACS data in wide format
    tract_year[[i]] <- get_acs(
      state = states_df$state_fips[[j]],
      geography = "tract",
      variables = all_variables,
      year = years[[i]],
      survey = "acs5",
      cache_table = TRUE,
      geometry = FALSE,
      output = "wide"
    )
    
    # Rename columns to remove the 'E' suffix (estimate)
    tract_year[[i]] <- tract_year[[i]] %>%
      rename_with(~str_replace_all(., "E$", ""), ends_with("E"))
    
    # Calculate Poverty Rate (%)
    tract_year[[i]] <- tract_year[[i]] %>%
      mutate(
        poverty_rate = (below_poverty / total_poverty) * 100
      )
    
    # Calculate Unemployment Rate (%)
    tract_year[[i]] <- tract_year[[i]] %>%
      mutate(
        unemployment_rate = (unemployed / labor_force) * 100
      )
    
    # Calculate Prime-Age Worker Share (%)
    tract_year[[i]] <- tract_year[[i]] %>%
      mutate(
        prime_age = population_25_34 + population_35_44 + population_45_54,
        prime_age_share = (prime_age / total_population) * 100
      )
    
    # Calculate Solo Detached housing Share (%)
    tract_year[[i]] <- tract_year[[i]] %>%
      mutate(
        total_housing = owner_occupied_housing_unit + renter_occupied_housing_unit,
        solo_detached_housing_share = ((owner_occupied_solo_detached_housing_unit + renter_occupied_solo_detached_housing_unit) / total_housing) * 100
      )
    
    # Select and arrange relevant columns
    tract_year[[i]] <- tract_year[[i]] %>%
      mutate( year = years[[i]]) %>% 
      select(
        year,
        GEOID,
        poverty_rate,
        median_income,
        unemployment_rate,
        prime_age_share,
        solo_detached_housing_share
      )
    
  }
  
  tract_data_list[[j]] <- bind_rows(tract_year)
  
}

# Iterate over all years and compile data
ACS_2012_2023 <- bind_rows(tract_data_list)


# Crosswalk ACS tract data to 2020 definitions for every year
ACS_2012_2019 <- ACS_2012_2023 %>% filter(year <=2019) # years with 2010 tract definitions 
ACS_2020_2023 <- ACS_2012_2023 %>% 
  filter(year >=2020) %>% # years with 2010 tract definitions 
  mutate(geoid = as.numeric(GEOID)) %>%
  select(geoid, year, poverty_rate:solo_detached_housing_share)


# load 2010 to 2020 crosswalk from HUD.
setwd(path_data)
CENSUS_TRACT_CROSSWALK <- read_excel(file.path(path_data_crosswalks, "CENSUS_TRACT_CROSSWALK_2010_to_2020_2020.xlsx"))

# merge tract info to ACS_2012_2019
ACS_2012_2019 <- ACS_2012_2019 %>% 
  rename(GEOID_2010 = GEOID) %>%
  mutate(geoid = as.numeric(GEOID_2010)) %>%
  right_join(CENSUS_TRACT_CROSSWALK, relationship = "many-to-many") %>%
    mutate(poverty_rate = poverty_rate*TOT_RATIO,
           median_income = median_income*TOT_RATIO,
           unemployment_rate = unemployment_rate*TOT_RATIO,
           prime_age_share = prime_age_share*TOT_RATIO,
           solo_detached_housing_share = solo_detached_housing_share*TOT_RATIO
    ) %>%
    group_by(GEOID_2020, year) %>%
    summarise(poverty_rate = sum(poverty_rate, na.rm = TRUE),
              median_income = sum(median_income, na.rm = TRUE),
              unemployment_rate = sum(unemployment_rate, na.rm = TRUE),
              prime_age_share = sum(prime_age_share, na.rm = TRUE),
              solo_detached_housing_share = sum(solo_detached_housing_share, na.rm = TRUE)) %>%
    ungroup() %>%
  mutate(geoid = as.numeric(GEOID_2020)) %>%
  select(geoid, year, poverty_rate:solo_detached_housing_share)


# append the new datasets together
ACS_2012_2023 <- bind_rows(ACS_2012_2019,ACS_2020_2023) %>%
  mutate(poverty_rate = as.numeric(poverty_rate),
         median_income = as.numeric(median_income),
         unemployment_rate = as.numeric(unemployment_rate),
         prime_age_share = as.numeric(prime_age_share),
         solo_detached_housing_share = as.numeric(solo_detached_housing_share)) %>%
  na.omit()

# clean the skew of the data by winzorizing at the 99th level
winsorize_upper <- function(x, p = 0.99) {
  threshold <- quantile(x, p, na.rm = TRUE)
  pmin(x, threshold)
}

ACS_2012_2023 <- ACS_2012_2023 %>%
  group_by(year) %>%
  mutate(across(poverty_rate:solo_detached_housing_share, ~ winsorize_upper(.))) %>%
  ungroup() %>%
  select(geoid, year, poverty_rate:solo_detached_housing_share)
  
ACS_2012_2023 <- ACS_2012_2023 %>%
  distinct(geoid, year, .keep_all = TRUE) 



# Save the compiled data
setwd(path_data_tract)
save(ACS_2012_2023, file = "ACS_2012_2023_2020tracts.RData")
