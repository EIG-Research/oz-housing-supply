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

#################
### Data load ###
#################

setwd(path_data)
load(file = "USPS_tract_vacancy.RData") # master data set. see 4. final dataset build.R


####################################
###         Top line stat        ###
####################################
# Define the time period of interest
target_months <- c("2014-03","2019-03","2020-03", "2025-03")

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
    # Calculate the change from 2019-12 to 2024-12
    mutate(change = `2025-03` - `2020-03`,
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
  subset(USPS_data, Designation %in% c("LIC selected"))
)

# Compose a summary statement comparing OZ growth relative to LIC and national changes
summary_statement <- paste0(
  "OZs added a total of ", prettyNum(OZ_res_change$change, big.mark = ","), 
  " new active and vacant residential addresses from Q1 2020 to Q1 2025. ",
  "This accounted for ", round(OZ_res_change$change / LIC_res_change$change, 3) * 100,
  "% of all new active and vacant addresses among all LIC eligible tracts regardless of OZ designation, which added ",
  prettyNum(LIC_res_change$change, big.mark = ","), 
  ". OZs also accounted for ",
  round(OZ_res_change$change / national_res_change$change, 3) * 100,
  "% of the ", prettyNum(national_res_change$change, big.mark = ","), 
  " new active and vacant residential addresses across the full analysis sample.",
  " From Q1 of 2014 to Q1 of 2019, OZ's accounted for ", 
  round(OZ_res_change$pre_change / national_res_change$pre_change, 3) * 100,
  "% of the ", prettyNum(national_res_change$pre_change, big.mark = ","), 
  " new active and vacant residential addresses across the full analysis sample."
  
)

# Print the first summary
print(summary_statement)

# If you compare designated OZs to overall tracts, did OZs have a stronger pre-trend of address growth?
# Aggregate annual address count changes
Count_add  <- USPS_data %>%
  filter(YEAR_MONTH %in% c("2014-03","2015-03","2016-03","2017-03","2018-03","2019-03","2020-03","2021-03","2022-03","2023-03","2024-03","2025-03"))  %>% 
  mutate(`Active and Vacant, Residential` = ACTIVE_RESIDENTIAL_ADDRESSES + STV_RESIDENTIAL_ADDRESSES + LTV_RESIDENTIAL_ADDRESSES,
         `OZ designation` = case_when(
           Designation %in% c("LIC selected","Contiguous selected") ~ "OZ tracts",
           Designation %in% c("LIC not selected","Contiguous not selected","Ineligible") ~ "non-OZ tracts",
           TRUE ~ NA
         )
  ) %>%
  arrange(YEAR_MONTH) %>%
  group_by(Designation,YEAR_MONTH) %>%
  summarise(`Active and Vacant, Residential` = sum(`Active and Vacant, Residential`, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(names_from = `Designation`, values_from = `Active and Vacant, Residential`) %>%
  rename(`Time Period` = YEAR_MONTH)

# Calculate annual growth in address counts
growth  <- USPS_data %>%
  # filter(YEAR_MONTH %in% c("2014-03","2015-03","2016-03","2017-03","2018-03","2019-03","2020-03","2021-03","2022-03","2023-03","2024-03","2025-03"))  %>% 
  mutate(`Active and Vacant, Residential` = ACTIVE_RESIDENTIAL_ADDRESSES + STV_RESIDENTIAL_ADDRESSES + LTV_RESIDENTIAL_ADDRESSES,
         `OZ designation` = case_when(
           Designation %in% c("LIC selected","Contiguous selected") ~ "OZ tracts",
           Designation %in% c("LIC not selected","Contiguous not selected","Ineligible") ~ "non-OZ tracts",
           TRUE ~ NA
         )) %>%
  arrange(YEAR_MONTH) %>%
  group_by(Designation,YEAR_MONTH,YEAR) %>%
  summarise(`Active and Vacant, Residential` = sum(`Active and Vacant, Residential`, na.rm = TRUE)) %>%
  ungroup() %>% 
  group_by(Designation) %>%
  mutate(`Treatment Period` = case_when(
    YEAR <= 2017 ~ "Pre-OZs Avg. Annual Growth",
    YEAR >=2018 ~ "Post-OZs Avg. Annual Growth",
    TRUE ~ NA
  ),
  growth_rate = ((`Active and Vacant, Residential` - dplyr::lag(`Active and Vacant, Residential`,n = 4))/dplyr::lag(`Active and Vacant, Residential`,n = 4))*100) %>%
  na.omit() %>%
  group_by(Designation, `Treatment Period`) %>%
  summarize(growth_rate = mean(growth_rate, na.omit = TRUE)) %>%
  pivot_wider(names_from = `Designation`, values_from = growth_rate) %>%
  rename(`Time Period` = `Treatment Period`) %>%
  arrange(`Contiguous not selected`)

growth <- bind_rows(Count_add, growth)

setwd(path_output)
write.xlsx(growth, file = "Total Address Growth Rate.xlsx")

########################
# Simplified version
Count_add_simple  <- USPS_data %>%
  filter(YEAR_MONTH %in% c("2014-03","2015-03","2016-03","2017-03","2018-03","2019-03","2020-03","2021-03","2022-03","2023-03","2024-03","2025-03"))  %>% 
  mutate(`Active and Vacant, Residential` = ACTIVE_RESIDENTIAL_ADDRESSES + STV_RESIDENTIAL_ADDRESSES + LTV_RESIDENTIAL_ADDRESSES,
         `OZ designation` = case_when(
           Designation %in% c("LIC selected","Contiguous selected") ~ "OZ tracts",
           Designation %in% c("LIC not selected","Contiguous not selected","Ineligible") ~ "non-OZ tracts",
           TRUE ~ NA
         )
  ) %>%
  arrange(YEAR_MONTH) %>%
  group_by(`OZ designation`,YEAR_MONTH) %>%
  summarise(`Active and Vacant, Residential` = sum(`Active and Vacant, Residential`, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(names_from = `OZ designation`, values_from = `Active and Vacant, Residential`) %>%
  rename(`Time Period` = YEAR_MONTH)

growth_simple  <- USPS_data %>%
  filter(YEAR_MONTH %in% c("2014-03","2015-03","2016-03","2017-03","2018-03","2019-03","2020-03","2021-03","2022-03","2023-03","2024-03","2025-03"))  %>%
  mutate(`Active and Vacant, Residential` = ACTIVE_RESIDENTIAL_ADDRESSES + STV_RESIDENTIAL_ADDRESSES + LTV_RESIDENTIAL_ADDRESSES,
         `OZ designation` = case_when(
           Designation %in% c("LIC selected","Contiguous selected") ~ "OZ tracts",
           Designation %in% c("LIC not selected","Contiguous not selected","Ineligible") ~ "non-OZ tracts",
           TRUE ~ NA
         )) %>%
  arrange(YEAR_MONTH) %>%
  group_by(`OZ designation`,YEAR) %>%
  summarise(`Active and Vacant, Residential` = sum(`Active and Vacant, Residential`, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(`OZ designation`) %>%
  mutate(`Treatment Period` = case_when(
    YEAR <= 2017 ~ "Pre-OZs Avg. Annual Growth",
    YEAR >=2018 ~ "Post-OZs Avg. Annual Growth",
    TRUE ~ NA
  ),
  growth_rate = ((`Active and Vacant, Residential` - dplyr::lag(`Active and Vacant, Residential`))/dplyr::lag(`Active and Vacant, Residential`))*100) %>%
  na.omit() %>%
  group_by(`OZ designation`, `Treatment Period`) %>%
  summarize(growth_rate = mean(growth_rate, na.omit = TRUE)) %>%
  pivot_wider(names_from = `OZ designation`, values_from = growth_rate) %>%
  rename(`Time Period` = `Treatment Period`) %>%
  arrange(-as.numeric(as.factor(`Time Period`)))

growth <- bind_rows(Count_add_simple, growth_simple)

write.xlsx(growth, file = "Total Address Growth Rate, simplified.xlsx")

########################
# Simplified version - growth rate - for generating line plot

growth_rate_simple  <- USPS_data %>%
  # filter(YEAR_MONTH %in% c("2014-03","2015-03","2016-03","2017-03","2018-03","2019-03","2020-03","2021-03","2022-03","2023-03","2024-03","2025-03"))  %>%
  mutate(`Active and Vacant, Residential` = ACTIVE_RESIDENTIAL_ADDRESSES + STV_RESIDENTIAL_ADDRESSES + LTV_RESIDENTIAL_ADDRESSES,
         `OZ designation` = case_when(
           Designation %in% c("LIC selected","Contiguous selected") ~ "OZ tracts",
           Designation %in% c("LIC not selected","Contiguous not selected","Ineligible") ~ "non-OZ tracts",
           TRUE ~ NA
         )) %>%
  arrange(date) %>%
  group_by(`OZ designation`,date) %>%
  summarise(`Active and Vacant, Residential` = sum(`Active and Vacant, Residential`, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(`OZ designation`) %>%
  mutate(
    growth_rate = ((`Active and Vacant, Residential` - dplyr::lag(`Active and Vacant, Residential`,n = 4))/dplyr::lag(`Active and Vacant, Residential`,n = 4))*100
  ) %>%
  ungroup() %>%
  na.omit() %>%
  select(date,`OZ designation`,growth_rate) %>%
  pivot_wider(names_from = `OZ designation`, values_from = growth_rate)

# growth_rate_simple  <- USPS_data %>%
#   filter(YEAR_MONTH %in% c("2014-03","2015-03","2016-03","2017-03","2018-03","2019-03","2020-03","2021-03","2022-03","2023-03","2024-03","2025-03"))  %>%
#   mutate(`Active and Vacant, Residential` = ACTIVE_RESIDENTIAL_ADDRESSES + STV_RESIDENTIAL_ADDRESSES + LTV_RESIDENTIAL_ADDRESSES,
#          `OZ designation` = case_when(
#            Designation %in% c("LIC selected","Contiguous selected") ~ "OZ tracts",
#            Designation %in% c("LIC not selected","Contiguous not selected","Ineligible") ~ "non-OZ tracts",
#            TRUE ~ NA
#          )) %>%
#   arrange(YEAR_MONTH) %>%
#   group_by(`OZ designation`,YEAR_MONTH) %>%
#   summarise(`Active and Vacant, Residential` = sum(`Active and Vacant, Residential`, na.rm = TRUE)) %>%
#   ungroup() %>% 
#   group_by(`OZ designation`) %>%
#   mutate(
#     growth_rate = ((`Active and Vacant, Residential` - dplyr::lag(`Active and Vacant, Residential`))/dplyr::lag(`Active and Vacant, Residential`))*100
#   ) %>%
#   ungroup() %>%
#   na.omit() %>%
#   select(YEAR_MONTH,`OZ designation`,growth_rate) %>%
#   pivot_wider(names_from = `OZ designation`, values_from = growth_rate) 

# growth_rate_simple  <- USPS_data %>%
#   filter(YEAR_MONTH %in% c("2014-03","2015-03","2016-03","2017-03","2018-03","2019-03","2020-03","2021-03","2022-03","2023-03","2024-03","2025-03"))  %>%
#   mutate(`Active and Vacant, Residential` = ACTIVE_RESIDENTIAL_ADDRESSES + STV_RESIDENTIAL_ADDRESSES + LTV_RESIDENTIAL_ADDRESSES,
#          `OZ designation` = case_when(
#            Designation %in% c("LIC selected","Contiguous selected") ~ "OZ tracts",
#            Designation %in% c("LIC not selected","Contiguous not selected","Ineligible") ~ "non-OZ tracts",
#            TRUE ~ NA
#          )) %>%
#   arrange(date) %>%
#   group_by(`OZ designation`,date) %>%
#   summarise(`Active and Vacant, Residential` = sum(`Active and Vacant, Residential`, na.rm = TRUE)) %>%
#   ungroup() %>%
#   group_by(`OZ designation`) %>%
#   mutate(
#     growth_rate = ((`Active and Vacant, Residential` - dplyr::lag(`Active and Vacant, Residential`,n = 1))/dplyr::lag(`Active and Vacant, Residential`,n = 1))*100
#   ) %>%
#   ungroup() %>%
#   na.omit() %>%
#   select(date,`OZ designation`,growth_rate) %>%
#   pivot_wider(names_from = `OZ designation`, values_from = growth_rate)

write.xlsx(growth_rate_simple, file = "Total Address Growth Rate, simplified line plot.xlsx")


########################
# Simplified version - growth rate - for generating line plot

growth_rate_simple_alt  <- USPS_data %>%
  # filter(YEAR_MONTH %in% c("2014-03","2015-03","2016-03","2017-03","2018-03","2019-03","2020-03","2021-03","2022-03","2023-03","2024-03","2025-03"))  %>%
  mutate(`Active and Vacant, Residential` = ACTIVE_RESIDENTIAL_ADDRESSES + STV_RESIDENTIAL_ADDRESSES + LTV_RESIDENTIAL_ADDRESSES,
         `OZ designation` = case_when(
           Designation %in% c("LIC selected","Contiguous selected") ~ "OZ tracts",
           Designation %in% c("LIC not selected") ~ "LIC non-OZ tracts",
           Designation %in% c("Contiguous not selected","Ineligible") ~ "Other non-OZ tracts",
           TRUE ~ NA
         )) %>%
  arrange(date) %>%
  group_by(`OZ designation`,date) %>%
  summarise(`Active and Vacant, Residential` = sum(`Active and Vacant, Residential`, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(`OZ designation`) %>%
  mutate(
    growth_rate = ((`Active and Vacant, Residential` - dplyr::lag(`Active and Vacant, Residential`,n = 4))/dplyr::lag(`Active and Vacant, Residential`,n = 4))*100
  ) %>%
  ungroup() %>%
  na.omit() %>%
  select(date,`OZ designation`,growth_rate) %>%
  pivot_wider(names_from = `OZ designation`, values_from = growth_rate)

write.xlsx(growth_rate_simple_alt, file = "Total Address Growth Rate, alternative simplified line plot.xlsx")


