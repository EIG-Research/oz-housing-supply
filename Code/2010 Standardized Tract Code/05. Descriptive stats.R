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
library(ggplot2)
library(lubridate)
library(panelView)
library(gsynth)
library(plm)
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


#################
### What are the trends in vacancy (counts and share) across designated and undesignated but eligible tracts? 
#################
  
# is the panel balanced?
table(USPS_data$date, USPS_data$`Designation`)

####################################
### Descriptive Statistics Table ###
####################################

# Step 1: trim data to the study period
table_df <- USPS_data %>%
  filter(date == "2017-12-01") %>%
  mutate(`Active and Vacant, Residential` = ACTIVE_RESIDENTIAL_ADDRESSES + STV_RESIDENTIAL_ADDRESSES + LTV_RESIDENTIAL_ADDRESSES)
  
# Step 2: Aggregate USPS address counts
address_counts <- table_df %>%
  filter(!is.na(`Designation`)) %>%
  group_by(`Designation`) %>%
  summarise(
    `Median Active and Vacant Residential Address Count` = median(`Active and Vacant, Residential`, na.rm = TRUE),
    `Median Active Residential Address Count` = median(ACTIVE_RESIDENTIAL_ADDRESSES, na.rm = TRUE),
    `Median Poverty Rate` = round(median(poverty_rate, na.rm = TRUE),2),  
    `Median MFI` = round(median(median_income, na.rm = TRUE),2),  
    `Median Unemployment Rate` = round(median(unemployment_rate, na.rm = TRUE),2),  
    `Median Prime-Age Share` = round(median(prime_age_share, na.rm = TRUE),2),
    `Median Number of Tracts` = n()
  )

# Step 3: Reshape Address Counts to Long Format
address_counts_long <- address_counts %>%
  pivot_longer(
    cols = starts_with("Median"),
    names_to = "Outcome",
    values_to = "Value"
  )

# Step 4: Summarize Share Table
share_table <- table_df %>%
  filter(!is.na(`Designation`)) %>%
  filter(!is.na(`Type tract`)) %>%
  group_by(`Type tract`, `Designation`) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(`Designation`) %>%
  mutate(share = count / sum(count)) %>%
  arrange(`Designation`, desc(share)) %>%
  mutate(share_percent = round(share * 100, 2)) %>%
  ungroup()

# Step 5: Reshape Share Table to Long Format
share_table_long <- share_table %>%
  mutate(Outcome = paste0("Share ", `Type tract`)) %>%  # Create unique outcome names for each Type tract
  select(Outcome, `Designation`, share_percent) %>%
  rename(Value = share_percent)


# Step 6: Combine Address Counts and Share Table
combined_long <- bind_rows(
  address_counts_long,
  share_table_long
)

# Step 7: Pivot Combined Data to Wide Format
final_table <- combined_long %>%
  pivot_wider(
    names_from = `Designation`,
    values_from = Value
  )

# Step 8: Arrange the Final Table for Readability (Optional)

desired_order <- c(
  "Median Active and Vacant Residential Address Count",
  "Median Active Residential Address Count",
  "Median Tract Workers",
  "Median Non-resident Workers",
  "Median Resident Workers",
  "Median MFI",
  "Median Poverty Rate",
  "Median Prime-Age Share",
  "Median Unemployment Rate",
  "Share Large urban",
  "Share Mid-sized urban",
  "Share Small urban",
  "Share Suburban",
  "Share Small town",
  "Share Rural",
  "Number of Tracts"
)

# Step 9: output the final table for datawrapper
final_table <- final_table %>%
  mutate(Outcome = if_else(Outcome == "Median Number of Tracts", "Number of Tracts", Outcome)) %>%
  mutate(Outcome = factor(Outcome, levels = desired_order)) %>%
  arrange(Outcome) %>%
  select("Outcome",
         "LIC selected", "LIC not selected",
         "Contiguous selected", "Contiguous not selected",
         "Ineligible")

setwd(path_output)
write.csv(final_table, file = "Descriptive Table.csv", row.names = FALSE)

################################################
### Output USPS address counts by tract type ###
################################################

# Split the addresses in LIC by whether or not the tract is designated OZ
summarized <- USPS_data  %>%
  filter(`Designation` %in% c("LIC not selected","LIC selected")) %>%
  mutate(`Active and Vacant, Residential` = ACTIVE_RESIDENTIAL_ADDRESSES + STV_RESIDENTIAL_ADDRESSES + LTV_RESIDENTIAL_ADDRESSES) %>% 
  select(
    geoid,
    date,`Type tract`,
     `Active and Vacant, Residential`,
    `OZ Designation`
  ) 

# Summarize address counts in designated / not designated LIC
summarized <- summarized %>%
  group_by(`OZ Designation`,`Type tract`, date) %>%
  summarise(
    `Typical Active and Vacant, Residential` = median(`Active and Vacant, Residential`, na.rm = TRUE)
  )

# Plot the trend in address counts after OZ came into effect
summarized %>%
  filter(!is.na(`Type tract`)) %>% 
  ggplot(aes(x = date,
             group = `Type tract`,
             color = as.factor(`Type tract`))) + 
  geom_vline(xintercept = as.Date("2020/03/01", format = "%Y/%m/%d")) +
  geom_line(aes(y = `Typical Active and Vacant, Residential`)) + 
  geom_point(aes(y = `Typical Active and Vacant, Residential`)) + 
  facet_wrap(`OZ Designation` ~ .)

# Reshape data to better display the difference between OZ/non-OZ
reshaped_data <- summarized %>%
  filter(!is.na(`Type tract`)) %>% 
  pivot_longer(cols = starts_with("Typical") | starts_with("Growth Rate") | starts_with("Share"),
               names_to = "Outcome_Variable", 
               values_to = "Value") %>%
  # Group by the tier, date, and outcome variable
  group_by(`Type tract`, date, Outcome_Variable) %>%
  # Calculate the difference between OZ Designation 1 and 0
  summarize(Difference = Value[`OZ Designation` == 1] - Value[`OZ Designation` == 0], .groups = "drop")

# Reshape the data back to wide format (optional, if needed)
wide_data <- reshaped_data %>%
  pivot_wider(names_from = Outcome_Variable, values_from = Difference)

# View the resulting data
head(wide_data)

wide_data %>%
  filter(date > "2015-12-01") %>% 
  filter(!is.na(`Type tract`)) %>% 
  group_by(`Type tract`) %>%
  mutate(`Typical Active and Vacant, Residential - 2019-12 to zero` = `Typical Active and Vacant, Residential` - 
           `Typical Active and Vacant, Residential`[date == "2017-12-01"]) %>%
  ggplot(aes(x = date,
             group = `Type tract`,
             color = as.factor(`Type tract`))) + 
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = as.Date("2018/03/01", format = "%Y/%m/%d")) +
  geom_line(aes(y = `Typical Active and Vacant, Residential - 2019-12 to zero`)) +
  geom_point(aes(y = `Typical Active and Vacant, Residential - 2019-12 to zero`))


wide_data_res  <- wide_data %>%
  filter(date > "2015-12-01") %>% 
  filter(!is.na(`Type tract`)) %>% 
  group_by(`Type tract`) %>%
  mutate(`Typical Active and Vacant, Residential - 2019-12 to zero` = `Typical Active and Vacant, Residential` - 
           `Typical Active and Vacant, Residential`[date == "2017-12-01"]) %>%
  ungroup() %>%
  select(`Type tract`, date, `Typical Active and Vacant, Residential - 2019-12 to zero`) %>%
  pivot_wider(names_from = `Type tract`, values_from = `Typical Active and Vacant, Residential - 2019-12 to zero`)

setwd(path_output)
write.csv(wide_data_res, "Gap in residential addresses by OZ designation and tract type.csv", row.names = FALSE)


