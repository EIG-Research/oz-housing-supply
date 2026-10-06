# HUD Aggregated USPS Administrative Data on Vacancies - 2020 standardized version
# Ben Glasner 01/03/2025

rm(list = ls())
options(scipen = 999)
set.seed(42)

###########################
###   Load Packages     ###
###########################
library(tidyr)
library(dplyr)
library(openxlsx)

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
path_data_USPS <- file.path(path_project, "Data/HUD USPS Data Standardized")
path_data_tract <- file.path(path_project, "Data/Tract Characteristics")
path_output <- file.path(path_project, "Output")



####################
# Read in DID output
did_results <- read.xlsx(file.path(path_output, "CSDID Effect Estimate.xlsx"))

# Additional calculations based on the provided text
avg_effect <- 47.4997 # without neighboring tracts
# avg_effect <- 41.4775 # with neighboring tracts
# oz_tracts <- 8764           # total number of OZ tracts
oz_tracts <- 7580           # total number of OZ tracts in our sample

oz_new_estimated <- avg_effect * oz_tracts  # estimated new addresses due to OZs
cost <- 8200000000
# cost <- 8200000000*0.75 # uncomment for estimate revized down 75%
cost_character <- "8.2 billion"
# cost_character <- "6.15 billion"

####################################
###         Top line stat        ###
####################################

setwd(path_data)
load(file = "USPS_tract_vacancy.RData") 

# Define the time period of interest
target_months <- c("2014-03","2019-03", "2025-03")

# Helper function to calculate residential address change for a given filter
calculate_res_change <- function(data) {
  data %>%
    # Sum up all types of residential addresses into one column
    mutate(Residential = ACTIVE_RESIDENTIAL_ADDRESSES + STV_RESIDENTIAL_ADDRESSES + LTV_RESIDENTIAL_ADDRESSES) %>%
    # Keep only rows for the target months
    filter(YEAR_MONTH %in% target_months) %>%
    # Group by month and calculate total residential addresses
    group_by(YEAR_MONTH) %>%
    summarise(TotalResidential = sum(Residential, na.rm = TRUE), .groups = 'drop') %>%
    # Spread months into separate columns for easier comparison
    pivot_wider(names_from = YEAR_MONTH, values_from = TotalResidential) %>%
    # Calculate the change from 2019-09 to 2024-09
    mutate(change = `2025-03` - `2019-03`,
           pre_change = `2019-03` - `2014-03`)
}

# Calculate national residential change without any category filter
national_res_change <- calculate_res_change(USPS_data)

# Calculate change for LIC-eligible tracts
LIC_res_change <- calculate_res_change(
  subset(USPS_data,Designation %in% c("LIC not selected", "LIC selected"))
)

# Calculate change for designated OZs (assumed under "LIC selected")
OZ_res_change <- calculate_res_change(  
  subset(USPS_data,Designation %in% c("LIC selected"))
)

# Calculate percentages relative to different groups
pct_of_OZ_total    <- round((oz_new_estimated / OZ_res_change$change) * 100, 2)
pct_of_LIC_total   <- round((oz_new_estimated / LIC_res_change$change) * 100, 2)
pct_of_national    <- round((oz_new_estimated / national_res_change$change) * 100, 2)

# Compose the additional summary statement
additional_statement <- paste0(
  "If the average effect of OZs was ", avg_effect, 
  " new active/vacant addresses per tract, and we have ", 
  prettyNum(oz_tracts, big.mark = ","), " OZ tracts, that translates to ", 
  prettyNum(round(oz_new_estimated), big.mark = ","), 
  " new addresses caused by OZs, ", pct_of_OZ_total, 
  "% of all new residential active/vacant addresses since 2019 in OZs, ", 
  pct_of_LIC_total, "% among LICs regardless of designation, and ", pct_of_national, 
  "% of all new active/vacant addresses in the country as a whole."
)

cost_statement <- paste0(
  "At a projected cost of $",
  cost_character,
  ", the average cost per unit caused by OZs is $",
  prettyNum(round(cost/oz_new_estimated), big.mark = ","),
  "."
)

# Print the additional summary
print(additional_statement)
print(cost_statement)
