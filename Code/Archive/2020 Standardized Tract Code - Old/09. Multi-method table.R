# HUD Aggregated USPS Administrative Data on Vacancies
# Ben Glasner 10/29/2024

rm(list = ls())
options(scipen = 999)
set.seed(42)

###########################
###   Load Packages     ###
###########################
library(tidyverse)

library(tidyr)
library(dplyr)

library(ggplot2)
library(lubridate)
library(openxlsx)

library(lfe)
library(fect)
library(did)  # for att_gt
library(broom)  # for tidy(felm)


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

path_output <- file.path(path_project, "Output")

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
  rename("poverty_rate_pre" = "poverty_rate",
         "median_income_pre" = "median_income",
         "unemployment_rate_pre" = "unemployment_rate",
         "prime_age_share_pre" = "prime_age_share",
         "solo_detached_housing_share_pre" = "solo_detached_housing_share",
         "jobs_in_tract_pre" = "jobs_in_tract",
         "employed_tract_residents_pre" = "employed_tract_residents",
         ) %>%
  na.omit()

# table(time_invariant$Designation_category, time_invariant$`current median income decile`)
# table(time_invariant$Designation_category, time_invariant$`current poverty rate decile`)

# Continue with other data transformations as needed
USPS_data <- USPS_data %>%
  # select(-c(`poverty_rate`:`employed_tract_residents`)) %>%
  filter(!is.na(ACTIVE_RESIDENTIAL_ADDRESSES)) %>%
  # filter(!is.na(ACTIVE_BUSINESS_ADDRESSES)) %>%
  # filter(!is.na(ACTIVE_OTHER_ADDRESSES)) %>%
  mutate(
    G = if_else(`OZ Designation` == 1, period_value, 0),
    Treatment = if_else(`OZ Designation` == 1 & period>=period_value, 1, 0),
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
# Variable 

results_list <- list()
outcome_var <- "Total_active_vacant_exclude_nostat_RESIDENTIAL"
treatment_var <- "Treatment"
max_period <- max(USPS_data$period, na.rm = TRUE)

print(paste0("Analyzing: ", outcome_var))

###############################
### 1. CSDID (Callaway-Sant’Anna)
###############################
csdid_controls <- c("poverty_rate_pre", "median_income_pre", "solo_detached_housing_share_pre")
csdid_control_formula <- as.formula(paste("~", paste(csdid_controls, collapse = " + ")))

# Filter and prep data
analysis_data <- USPS_data %>%
  select("geoid_num", "period", "G", "Treatment", all_of(outcome_var), all_of(csdid_controls)) %>%
  na.omit()

csdid_treated <- att_gt(
  yname = outcome_var,
  tname = "period",
  idname = "geoid_num",
  gname = "G",
  xformla = csdid_control_formula,
  data = analysis_data,
  base_period = "universal",
  pl = TRUE,
  biters = 1000
)

csdid_dynamic <- aggte(csdid_treated, type = "dynamic", alp = 0.05, na.rm = TRUE)

csdid_df <- tibble(
  period = csdid_dynamic$egt
  , estimate = csdid_dynamic$att.egt
  , se = csdid_dynamic$se.egt
  , lower = csdid_dynamic$att.egt - csdid_dynamic$se.egt*csdid_dynamic$crit.val.egt
  , upper = csdid_dynamic$att.egt + csdid_dynamic$se.egt*csdid_dynamic$crit.val.egt
  , crit.val = csdid_dynamic$crit.val.egt
)

csdid_final <- csdid_df %>%
  filter(period == max(csdid_df$period)) %>%
  mutate(method = "CSDID") %>%
  select(
    method 
    , estimate
    , se
    , lower
    , upper
  )

results_list$CSDID <- csdid_final

###############################
### 2. FECT (FE, IFE, CFE, MC)
###############################
# Setup formula

# Uncomment the next line if you wish to include controls
counterfactual_controls <- c("poverty_rate", "median_income", "solo_detached_housing_share")
FECT_formula <- as.formula(paste(outcome_var, "~", treatment_var, "+", paste(counterfactual_controls, collapse = " + ")))

analysis_data <- USPS_data %>%
  select("geoid_num", "period", "G", "Treatment", all_of(outcome_var), all_of(counterfactual_controls)) %>%
  na.omit()

methods <- c("fe")
# methods <- c("fe", "ife", "cfe", "mc")

for (m in methods) {
  fect_model <- fect(
    method = m,
    formula = current_formula,
    data = analysis_data,
    index  = c("geoid_num", "period"),
    force = "two-way",
    se = TRUE,
    vartype = "jackknife",    
    nboots = 100,
    alpha = 0.05,
    r = c(0, 5),
    nlambda = 5,
    CV = TRUE,
    k = 10,
    cv.prop = 0.05,
    cv.treat = FALSE,
    cv.nobs = 3,
    cv.donut = 0,
    criterion = "mspe",
    na.rm = TRUE,
    parallel = TRUE,
    cores = 12,
    seed = 42,
    min.T0 = 5,
    max.missing = 0,
    proportion = 0.3,
    f.threshold = 0.5,
    degree = 2,
    sfe = c("state_fips"),
    cfe = list(c("type_tract", "time"), c("state_fips", "time")),
    fill.missing = FALSE,
    placeboTest = FALSE,
    carryoverTest = FALSE,
    loo = FALSE,
    permute = FALSE,
    m = 2,
    normalize = FALSE
  )
  
  att_df <- tibble(
    period = fect_model$time,
    att = fect_model$att
  )
  
  final_att <- att_df %>%
    filter(period == max_period) %>%
    mutate(method = toupper(m), estimate = att) %>%
    select(method, estimate)
  
  results_list[[toupper(m)]] <- final_att
}

###############################
### Combine & Export
###############################

all_results <- bind_rows(results_list)

write_csv(all_results, "treatment_effect_comparison_final_period.csv")

print(all_results)
    
