# HUD Aggregated USPS Administrative Data on Vacancies
# Ben Glasner 10/29/2024

rm(list = ls())
options(scipen = 999)
set.seed(42)

###########################
###   Load Packages     ###
###########################
library(tidyr)
library(dplyr)
library(ggplot2)
library(did)
library(lubridate)
library(openxlsx)

################
# use the participation among those who have access from sipp
# Anyone who increases their contribution, automatically increase to 5% 
# What is the rate of savings among low-income savers?
# If we are just talking about full-time and the eligible population, what is the cost?


#################
### Set paths ###
#################
# Define user-specific project directories
project_directories <- list(
  "name" = "PATH TO GITHUB REPO",
  "Benjamin Glasner" = "C:/Users/Benjamin Glasner/EIG Dropbox/Benjamin Glasner/GitHub/oz-housing-supply",
  "bngla" = "C:/Users/bngla/EIG Dropbox/Benjamin Glasner/GitHub/oz-housing-supply",
  "name" = "PATH TO GITHUB REPO"
)

# Setting project path based on current user
current_user <- Sys.info()[["user"]]
if (!current_user %in% names(project_directories)) {
  stop("Root folder for current user is not defined.")
}

path_project <- project_directories[[current_user]]

path_data <- file.path(path_project, "data")
path_data_USPS <- file.path(path_project, "data/2020 Standardized")
path_data_tract <- file.path(path_data, "Tract Characteristics")

path_output <- file.path(path_project, "output")

#################
### Data load ###
#################
setwd(path_data)
load(file = "USPS_tract_vacancy_2012_2024_2020_definitions.RData")

################
### Clean up uncertain tracts from the crosswalk
### If a tract in 2020 is a mix of any version of the LIC, contiguous or inelligible, then drop it from the analysis - conservative first pass

USPS_data <- USPS_data %>%
  filter(Sample == "In Clean Sample") %>%
  filter(YEAR >=2015) 

#  https://www.huduser.gov/apps/public/usps/download_pdf/2018-USPS-FAQ.pdf 
# There is a phenomenon that HUD has been studying that involves a sudden, sharp
# increase in the number of addresses in the USPS No-Stat category, which also manifests
# itself in an increase in the total number of addresses. The increase in addresses is over 7
# million since 2011, with the bulk of those increases happening in two quarters – one in
# 2011 and the other in 2014. There is no evidence to be found in any of the available
# national administrative or survey data sources that would make that kind of increase in
# the number of residential housing units plausible. 

#################
### What are the trends in vacancy (counts and share) across designated and undesignated but eligible tracts? 
#################

USPS_data <- USPS_data %>% 
  mutate( period = dense_rank(date))

period_value <- min(USPS_data$period[USPS_data$date == "2018-03-01"])
# period_value <- min(USPS_data$period[USPS_data$date == "2020-03-01"])

##### Time Invariant Values
time_invariant <- USPS_data %>% 
  filter(date == "2017-12-01") %>%
  select(geoid,Designation_category,`poverty_rate`:`employed_tract_residents`) %>%
  mutate(`current median income decile` = ntile(median_income, 10),
         `current poverty rate decile` = ntile(poverty_rate, 10),
         `current solo detached decile` = ntile(solo_detached_housing_share, 10)) %>%
  na.omit()

# table(time_invariant$Designation_category, time_invariant$`current median income decile`)
# table(time_invariant$Designation_category, time_invariant$`current poverty rate decile`)

# Continue with other data transformations as needed
USPS_data <- USPS_data %>%
  select(-c(`poverty_rate`:`employed_tract_residents`)) %>%
  filter(!is.na(ACTIVE_RESIDENTIAL_ADDRESSES)) %>%
  # filter(!is.na(ACTIVE_BUSINESS_ADDRESSES)) %>%
  # filter(!is.na(ACTIVE_OTHER_ADDRESSES)) %>%
  mutate(
    G = if_else(`OZ Designation` == 1, period_value, 0),
    geoid_num = as.numeric(geoid),
    
    # Total_active_vacant_exclude_nostat = 
    #   ACTIVE_RESIDENTIAL_ADDRESSES + STV_RESIDENTIAL_ADDRESSES + LTV_RESIDENTIAL_ADDRESSES +
    #   ACTIVE_BUSINESS_ADDRESSES + STV_BUSINESS_ADDRESSES + LTV_BUSINESS_ADDRESSES +
    #   ACTIVE_OTHER_ADDRESSES + STV_OTHER_ADDRESSES + LTV_OTHER_ADDRESSES, 
    
    Total_active_vacant_exclude_nostat_RESIDENTIAL = 
      ACTIVE_RESIDENTIAL_ADDRESSES + STV_RESIDENTIAL_ADDRESSES + LTV_RESIDENTIAL_ADDRESSES,
    
    # VACANCY_RATE_ALL = 100*((STV_RESIDENTIAL_ADDRESSES + LTV_RESIDENTIAL_ADDRESSES + STV_BUSINESS_ADDRESSES + LTV_BUSINESS_ADDRESSES +STV_OTHER_ADDRESSES + LTV_OTHER_ADDRESSES)/(Total_active_vacant_exclude_nostat)),
    # VACANCY_RATE_RESIDENTIAL = 100*((STV_RESIDENTIAL_ADDRESSES + LTV_RESIDENTIAL_ADDRESSES)/(Total_active_vacant_exclude_nostat_RESIDENTIAL)),
    # VACANCY_RATE_BUSINESS = 100*((STV_BUSINESS_ADDRESSES + LTV_BUSINESS_ADDRESSES)/(Total_active_vacant_exclude_nostat_BUSINESS)),
    # VACANCY_RATE_OTHER = 100*((STV_OTHER_ADDRESSES + LTV_OTHER_ADDRESSES)/(Total_active_vacant_exclude_nostat_OTHER))
  ) %>%
  left_join(time_invariant) %>%
  mutate(
    log_res_address = log(Total_active_vacant_exclude_nostat_RESIDENTIAL),
  ) %>%
  mutate(
    log_res_address = if_else(log_res_address<0,NA,log_res_address),
  )

# USPS_data <- USPS_data %>% 
# filter(`Designation_category` %in% c("LIC selected","LIC not selected"))
# filter(`Designation_category` %in% c("LIC selected","LIC not selected") | `current median income decile` <= 6 | `current poverty rate decile` >=5 )

# Calculate the annual rate of growth for total active and vacant residential

panel_USPS <- plm::pdata.frame(USPS_data, index = c("geoid_num","period"), drop.index = FALSE)

panel_USPS$change <- panel_USPS$Total_active_vacant_exclude_nostat_RESIDENTIAL - plm::lag(panel_USPS$Total_active_vacant_exclude_nostat_RESIDENTIAL, k = 4)
panel_USPS$growth_rate <- panel_USPS$change/plm::lag(panel_USPS$Total_active_vacant_exclude_nostat_RESIDENTIAL, k = 4)

panel_USPS <- panel_USPS %>% 
  as.data.frame() %>%
  select("geoid","period","change","growth_rate") %>%
  mutate(geoid = as.numeric(geoid),
         period = as.numeric(period))

USPS_data <- USPS_data %>%
  left_join(panel_USPS) %>%
  mutate(growth_rate = if_else(growth_rate == Inf, NA, growth_rate))

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

#######################
### Build DID Model ###
#######################
# Variable initialization
data_list <- list()
dynamic_results_list <- list()
csdid_treated<- list()
dynamic <- list()
outcome_vars <- list()
plot_data <- list()
plot_data_list <- list()
dates <- sort(unique(USPS_data$date))[2:length(sort(unique(USPS_data$date)))]
plots <- list()
plot_list <- list()

outcome_vars[[1]] <- c(
  "Total_active_vacant_exclude_nostat_RESIDENTIAL"
)

titles <- c(
  "Effect on  Active and Vacant Residential"
)

table_titles <- c(
  "Active and Vacant Residential"
)

# controls      <- c("poverty_rate", "median_income")
controls      <- c("poverty_rate","median_income","solo_detached_housing_share","STATE_NAME")
control_vars  <- paste(controls, collapse = " + ")
current_formula <- as.formula(paste("~", control_vars))

titles_df <- as.data.frame(cbind(table_titles, outcome_vars[[1]]))
names(titles_df) <- c("titles", "outcome_var")

# Get sorted list of geoids for the census tracts of interest
Metro_groupings <- sort(unique(USPS_data$`Type tract`))
geo_groupings <- c("All",Metro_groupings)

# Split data by census tract
data_list[[1]] <- USPS_data
for(i in seq_along(Metro_groupings)){
  j <- i + 1
  data_list[[j]] <- USPS_data %>% filter(`Type tract` == Metro_groupings[[i]])
}

# Transform time variable
dates_df <- USPS_data %>%
  select(period, date) %>%
  distinct() %>%
  arrange(period) %>% 
  mutate(period = period - period_value)

##############################
### Loop over individual states
##############################

# Get the list of states. (Make sure that your dataset has a "state" variable.)
states <- sort(unique(USPS_data$STATE_NAME))

# Create lists to store state-specific results
state_dynamic_results_list <- list()
state_plot_data_list <- list()
state_plot_list <- list()

state_treated_OZ_count <- list()

# Loop over each state
for (s in states) {
  cat("Processing state:", s, "\n")
  
  # For this state, restrict the treated tracts to only those in state s,
  # while keeping all control tracts (OZ Designation == 0) from any state.
  state_USPS_data <- USPS_data %>% 
    filter((`OZ Designation` == 0) | (`OZ Designation` == 1 & STATE_NAME == s)) %>% # Grab controls from anywhere
    # filter((`OZ Designation` == 0 & STATE_NAME == s) | (`OZ Designation` == 1 & STATE_NAME == s)) %>% # Grab controls only from set state
    filter(Designation_category %in% c("Ineligible",
                                       "Contiguous not selected",
                                       "LIC not selected",
                                       "LIC selected"))
  
  state_treated_OZ_count[[s]]  <- state_USPS_data %>% 
    filter(period == period_value) %>%
    filter(Designation_category == "LIC selected") %>%
    select(geoid) %>% 
    distinct() %>%
    summarise(count = n()) %>%
    pull()

  # Initialize lists to store dynamic results and plots for each outcome variable in this state
  state_dynamic <- list()
  state_plot_data <- list()
  state_plots <- list()
  
  for (i in seq_along(outcome_vars[[1]])) {
    outcome <- outcome_vars[[1]][i]
    cat("   Outcome:", outcome, "\n")
    
    # For some outcomes (if needed), subset further (as in your original code)
    if(outcome %in% c("jobs_in_tract","employed_tract_residents")){
      analysis_data <- state_USPS_data %>%
        filter(MONTH == "12") %>% 
        select(geoid_num, period, G, outcome, all_of(controls)) %>%
        na.omit()
    } else {
      analysis_data <- state_USPS_data %>%
        select(geoid_num, period, G, outcome, all_of(controls)) %>%
        na.omit()
    }
    
    # Run the DID estimation using the att_gt function
    csdid_treated_state <- att_gt(
      yname = outcome,
      tname = "period",
      idname = "geoid_num",
      gname = "G",
      xformla = current_formula,
      biters = 1000,
      pl = TRUE,
      data = analysis_data,
      base_period = "universal"
    )
    
    # Aggregate dynamic effects
    state_dynamic[[i]] <- aggte(csdid_treated_state,
                                alp = 0.05,
                                type = "dynamic",
                                na.rm = TRUE)
  }
  
  # Save the state-specific results and plots
  state_dynamic_results_list[[s]] <- state_dynamic
}

##############################
### Build and Export Results Table by State
##############################

# Create a table with one row for each (state, outcome) combination
state_results_export_list <- expand.grid(
  state = states,
  outcome_var = outcome_vars[[1]],
  stringsAsFactors = FALSE
) %>% arrange(state, outcome_var)

# Initialize vectors to store formatted coefficients and SEs
formatted_att_state <- numeric(nrow(state_results_export_list))
formatted_coef_state <- character(nrow(state_results_export_list))
se_values_state <- numeric(nrow(state_results_export_list))
significant_in_aggregate <- character(nrow(state_results_export_list))

# Loop through each row of the export table to populate results
for (k in seq_len(nrow(state_results_export_list))) {
  current_state <- state_results_export_list$state[k]
  current_outcome <- state_results_export_list$outcome_var[k]
  
  # Find the index of the current outcome in outcome_vars
  outcome_index <- which(outcome_vars[[1]] == current_outcome)
  
  # Get the number of estimated periods (using the dynamic object for the current state and outcome)
  time_period_count <- length(state_dynamic_results_list[[current_state]][[outcome_index]]$att.egt)
  
  # Extract the last (i.e. overall) ATT and its standard error
  att <- state_dynamic_results_list[[current_state]][[outcome_index]]$att.egt[time_period_count]
  crit_val <- state_dynamic_results_list[[current_state]][[outcome_index]]$crit.val.egt[1]
  se <- state_dynamic_results_list[[current_state]][[outcome_index]]$se.egt[time_period_count]
  
  # Format the coefficient (and attach an asterisk if significant) based on rounding conventions
  if (!current_outcome %in% c("log_total_active_vacant_exclude_nostat",
                              "growth_rate",
                              "log_bus_address",
                              "log_other_address",
                              "log_res_address")) {
    se_round <- round(se, 3)
    formatted_att_state[k] <- round(att, 4)
    formatted_coef_state[k] <- paste0(round(att, 3), " ", if_else((att / se) > crit_val, "*", ""))
  } else {
    se_round <- round(se, 4)
    formatted_att_state[k] <- round(att, 4)
    formatted_coef_state[k] <- paste0(round(att, 4), " ", if_else((att / se) > crit_val, "*", ""))
  }
  
  se_values_state[k] <- se_round
  significant_in_aggregate[k] <- paste0(if_else((state_dynamic_results_list[[current_state]][[outcome_index]]$overall.att / state_dynamic_results_list[[current_state]][[outcome_index]]$overall.se) > crit_val, "Significant in Aggregate", ""))
}

# Convert the list to a named vector
state_treated_OZ_count_vec <- unlist(state_treated_OZ_count)

# Then, when building your final table:
state_final_table <- state_results_export_list %>%
  mutate(
    ATT  = formatted_att_state,
    Coef = formatted_coef_state,
    SE   = se_values_state,
    Significant = significant_in_aggregate,
    Obs  = state_treated_OZ_count_vec[state]  # lookup the count by state
  ) %>%
  arrange(-ATT) %>%
  mutate(`Total Effect` = ATT*Obs,
         `Total Effect, Significant Agg` = if_else(Significant == "Significant in Aggregate",
                                                     `Total Effect`,
                                                     NA_real_),
         `Total Effect, Significant Final` = if_else(endsWith(Coef, "*"),
                                                        `Total Effect`,
                                                        NA_real_))
# Export the table to an Excel file
setwd(path_output)
write.xlsx(state_final_table, file = "CSDID_Effect_Estimate_by_State.xlsx", overwrite = TRUE)
