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
library(ggplot2)

#################
### Set paths ###
#################
# Define user-specific project directories
project_directories <- list(
  "name" = "PATH TO GITHUB REPO",
  "Benjamin Glasner" = "C:/Users/Benjamin Glasner/EIG Dropbox/Benjamin Glasner/GitHub/oz-housing-supply",
  "bngla" = "C:/Users/bngla/EIG Dropbox/Benjamin Glasner/GitHub/oz-housing-supply"
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

#################
### Data load ###
#################

setwd(path_data)
load(file = "USPS_tract_vacancy.RData") # master data set. see 4. final dataset build.R

################
# Clean up uncertain tracts from the crosswalk
# If a tract in 2020 is a mix of any version of:
# LIC, contiguous or inelligible, 
# then drop it from the analysis - conservative first pass

USPS_data <- USPS_data %>%
  filter(Sample == "In Clean Sample")


##########################################
###     Time Series for all tracts     ###
##########################################
annual <- USPS_data %>%
  group_by(date) %>%
  summarise(`Total residential address count` = sum(TOTAL_RESIDENTIAL_ADDRESSES, na.rm = TRUE),
            `Active and Vacant residential address count` = sum(TOTAL_RESIDENTIAL_ADDRESSES, na.rm = TRUE) - sum(NO_STAT_RESIDENTIAL_ADDRESSES, na.rm = TRUE),
            `Active residential address count` = sum(ACTIVE_RESIDENTIAL_ADDRESSES, na.rm = TRUE))
write.csv(annual,file = file.path(path_output,"time series.csv"), row.names = FALSE)

######################################
###     Time Series - Cut by LIC   ###
######################################

annual <- USPS_data %>%
  filter(`Designation_category` %in% c("LIC not selected","LIC selected")) %>%
  group_by(date) %>%
  summarise(`Total residential address count` = sum(TOTAL_RESIDENTIAL_ADDRESSES, na.rm = TRUE),
            `Active and Vacant residential address count` = sum(TOTAL_RESIDENTIAL_ADDRESSES, na.rm = TRUE) - sum(NO_STAT_RESIDENTIAL_ADDRESSES, na.rm = TRUE),
            `Active residential address count` = sum(ACTIVE_RESIDENTIAL_ADDRESSES, na.rm = TRUE))

write.csv(annual,file = file.path(path_output,"time series LIC.csv"), row.names = FALSE)

######################################
###     Descriptive Parallel Trends   ###
######################################

annual <-  USPS_data %>% 
  filter(`Designation_category` %in% c("LIC not selected", "LIC selected", "Ineligible")) %>% 
  group_by(date, `Designation_category`) %>% 
  summarise(
    `Total residential address count` =
      mean(ACTIVE_RESIDENTIAL_ADDRESSES + STV_RESIDENTIAL_ADDRESSES + LTV_RESIDENTIAL_ADDRESSES,
           na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  pivot_wider(
    names_from  = `Designation_category`,
    values_from = `Total residential address count`,
    names_sort  = TRUE   # optional: keeps the columns in alphabetical order
  ) %>%
  mutate(Gap = `LIC not selected` - `LIC selected`)

setwd(path_output)
write.xlsx(annual, file = "Parallel Trends by LIC.xlsx")



########################################
###   Share of addresses over time   ###
########################################

# Generate share of USPS addresses by OZ designation and eligibility
share_designation <- USPS_data %>%
  mutate(`Active and Vacant, Residential` = ACTIVE_RESIDENTIAL_ADDRESSES + STV_RESIDENTIAL_ADDRESSES + LTV_RESIDENTIAL_ADDRESSES) %>%
  group_by(`Designation_category`,date) %>%
  summarise(`Active and Vacant, Residential` = sum(`Active and Vacant, Residential`, na.rm = TRUE), 
            ACTIVE_RESIDENTIAL_ADDRESSES = sum(ACTIVE_RESIDENTIAL_ADDRESSES, na.rm = TRUE), .groups = 'drop') %>%
  group_by(date) %>%
  mutate(share = `Active and Vacant, Residential` / sum(`Active and Vacant, Residential`, na.rm = TRUE)) %>%
  arrange(date, desc(share)) %>%
  mutate(share_percent = round(share * 100, 2)) %>%
  ungroup() %>%
  group_by(Designation_category) %>%
  mutate(share_normalized = round((`Active and Vacant, Residential` / first(`Active and Vacant, Residential`)) * 100, 2)) %>%
  ungroup()

# Plot the share of USPS addresses by OZ designation and eligibility
share_designation %>%
  filter(`Designation_category` %in% c("LIC not selected","LIC selected","Ineligible")) %>%
  ggplot(aes(x = date, 
             y = share_normalized, 
             group = `Designation_category`,
             color = `Designation_category`)) + 
  geom_line() + 
  geom_point()

# Pivot wide for display
share_designation_wide <- share_designation %>%
  select(Designation_category,date,share_percent) %>%
  pivot_wider(
    names_from = Designation_category,
    values_from = share_percent
  )

# Plot for all designation categories
share_designation %>%
  ggplot(aes(x = date, 
             y =share_percent,
             group = Designation_category)) +
  geom_line() + 
  facet_wrap(Designation_category ~ .,scales = "free_y" )

write.xlsx(share_designation_wide, file.path(path_output,"Share of Addresses.xlsx"))
