# HUD Aggregated USPS Administrative Data on Vacancies
# Ben Glasner 1/15/2025
# Testing code

rm(list = ls())
options(scipen = 999)
set.seed(42)
pid <- Sys.getpid()
shell(
  sprintf(
    'powershell.exe "Get-Process -Id %s | ForEach-Object { $_.PriorityClass = \'High\' }"',
    pid
  ),
  intern = FALSE
)
###########################
###   Load Packages     ###
###########################
# devtools::install_github("xuyiqing/fect")
library(fect) # https://yiqingxu.org/packages/fect/01-start.html

library(openxlsx)
library(tidyr)
library(dplyr)

library(broom)

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
path_data <- file.path(path_project, "data")
path_data_USPS <- file.path(path_project, "data/2010 Census Tract Summary Files")
path_data_tract <- file.path(path_project, "data/Tract Characteristics")
path_output <- file.path(path_project, "output")

#################
### Data load ###
#################
setwd(path_data)
load(file = "USPS_tract_vacancy.RData")

##################
### Data clean ###
##################

### Clean up uncertain tracts from the crosswalk
USPS_data <- USPS_data %>%
  # filter(`Designation_category` %in% c("Ineligible","LIC selected","LIC not selected")) %>%
  filter(Sample == "In Clean Sample") %>%
  filter(YEAR >=2015) %>%
  mutate(
    id = as.numeric(geoid), 
    time = dense_rank(date),
    Designation = if_else(`OZ Designation` == 1 & date >= "2018-03-01", 1, 0),
    Total_residential = ACTIVE_RESIDENTIAL_ADDRESSES + STV_RESIDENTIAL_ADDRESSES + LTV_RESIDENTIAL_ADDRESSES, 
  ) %>%
  select(id, geoid, time, YEAR, date, STATE_NAME, STATEFP, COUNTYFP,CBSA_CODE,CBSA_TITLE,
         `Type tract`, Designation, `Designation_category`,LIC_oz_neighbor,
         poverty_rate, median_income, solo_detached_housing_share,
         Total_residential) %>%
  mutate(type_tract = as.factor("Type tract"),
         COUNTYFP = case_when(
           is.na(COUNTYFP) ~ "No County",
           !is.na(COUNTYFP) ~ COUNTYFP,
           TRUE ~ NA
         ),
         CBSA_TITLE = case_when(
           is.na(CBSA_TITLE) ~ "No CBSA",
           !is.na(CBSA_TITLE) ~ CBSA_TITLE,
           TRUE ~ NA
         )
         )

# Ensure panel is balanced after removing missing data periods
USPS_data <- USPS_data %>%
  distinct(id, time, .keep_all = TRUE) %>%
  group_by(id) %>%
  mutate(number_of_quarters_obs = n()) %>%
  ungroup()

max_quarter <- max(USPS_data$number_of_quarters_obs)
USPS_data <- USPS_data %>% filter(number_of_quarters_obs == max_quarter)


USPS_2024 <- USPS_data %>%
  filter(date == "2023-12-01") %>%
  select(id, poverty_rate, median_income,solo_detached_housing_share) %>%
  mutate(YEAR = as.character(2024))

USPS_2025 <- USPS_data %>%
  filter(date == "2023-12-01") %>%
  select(id, poverty_rate, median_income,solo_detached_housing_share) %>%
  mutate(YEAR = as.character(2025))

USPS_data_2015_2023 <- USPS_data %>%
  filter(YEAR>=2015 & YEAR<=2023)
  
USPS_data_2024 <- USPS_data %>%
  filter(YEAR==2024) %>%
  select(-poverty_rate, -median_income, -solo_detached_housing_share) %>%
  left_join(USPS_2024)

USPS_data_2025 <- USPS_data %>%
  filter(YEAR==2025) %>%
  select(-poverty_rate, -median_income, -solo_detached_housing_share) %>%
  left_join(USPS_2025)

USPS_data <- bind_rows(USPS_data_2015_2023,USPS_data_2024,USPS_data_2025) %>%
filter(YEAR %in% c(2015,2016,2017) |  date %in% c(
  "2018-03-01"
  # ,"2019-03-01"
  # ,"2020-03-01"
  # ,"2021-03-01"
  # ,"2022-03-01"
  # ,"2023-03-01"
  # ,"2024-03-01"
  ,"2025-03-01"
  ) 
  ) %>%
  # filter(YEAR >=2015 ) %>%
  mutate(
         `current median income decile` = ntile(median_income, 10),
         `current poverty rate decile` = ntile(poverty_rate, 10),
         `current solo detached decile` = ntile(solo_detached_housing_share, 10)
         )
###############################
### Counterfactual Analysis Function ###
###############################

# Setup formula
outcome_var   <- "Total_residential"
treatment_var <- "Designation"
# Uncomment the next line if you wish to include controls
conditional      <- c("`current median income decile`", "`current poverty rate decile`","`current solo detached decile`")
conditional_allof      <- c("current median income decile", "current poverty rate decile","current solo detached decile")
controls      <- c("poverty_rate","median_income","solo_detached_housing_share")
controls_allof      <- c("poverty_rate","median_income","solo_detached_housing_share")

control_vars  <- paste(controls, collapse = " + ")
# current_formula <- as.formula(paste(outcome_var, "~", treatment_var))
current_formula <- as.formula(paste(outcome_var, "~", treatment_var, "+", control_vars))

#####################################
### Create Treated & Control Sets ###
#####################################

USPS_data <- USPS_data %>%
  select(all_of(outcome_var), 
         all_of(treatment_var), 
         all_of(controls_allof),
         all_of(conditional_allof),
         "id", "date","YEAR", "time", "Designation_category","LIC_oz_neighbor",
         "type_tract","CBSA_TITLE") %>% 
  na.omit() 

balanced_ids <- USPS_data %>%
  group_by(id) %>%
  summarise(n_time = n_distinct(time)) %>%
  filter(n_time == n_distinct(USPS_data$time)) %>%
  pull(id)

USPS_data <- USPS_data %>%
  filter(id %in% balanced_ids)

all_units <- USPS_data %>%
  filter(time == max(time)) %>%
  distinct(id) %>%
  arrange(id) %>% # optional: ensure consistency
  mutate(bin = ntile(id, 5))  # Add a bin number (1 to 5)

setwd(path_data)
save(USPS_data, file = "Counterfactual_ready_data.RData")

for (b in 1:5) {
  bin_ids <- all_units %>% filter(bin == b)
  save(bin_ids, file = file.path(paste0("all_units_bin_", b, ".RData")))
}
