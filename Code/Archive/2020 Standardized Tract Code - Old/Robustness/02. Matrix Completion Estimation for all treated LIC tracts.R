# HUD Aggregated USPS Administrative Data on Vacancies
# Ben Glasner 1/15/2025
# Testing code

rm(list = ls())
options(scipen = 999)
set.seed(42)

###########################
###   Load Packages     ###
###########################
# devtools::install_github("xuyiqing/fect")
library(fect) # https://yiqingxu.org/packages/fect/01-start.html

library(openxlsx)
library(tidyr)
library(dplyr)
library(panelView)
library(ggplot2)

library(broom)
library(lmtest)     # For robust standard errors
library(sandwich)   # For clustered standard errors
library(fixest)

library(modelsummary)
library(gt)
library(webshot2)

library(purrr)
library(progress)

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


USPS_data <- USPS_data %>%
  filter(Designation_category_detailed %in% c("LIC selected","LIC not selected, not a border tract","Ineligible, not a border tract")) %>%
  filter(YEAR >=2015)

### Clean up uncertain tracts from the crosswalk
USPS_data <- USPS_data %>%
  mutate(
    id = as.numeric(geoid), 
    time = dense_rank(date),
    Designation = if_else(`OZ Designation` == 1 & date >= "2018-03-01", 1, 0),
    Total_residential = ACTIVE_RESIDENTIAL_ADDRESSES + STV_RESIDENTIAL_ADDRESSES + LTV_RESIDENTIAL_ADDRESSES
  ) %>%
  select(id, time, YEAR,date,STATEFP,
         `Type tract`, Designation, Designation_category_detailed,
         poverty_rate, median_income, unemployment_rate, prime_age_share, solo_detached_housing_share,
         Total_residential) %>%
  mutate(type_tract = as.numeric(as.factor("Type tract")))

# Ensure panel is balanced after removing missing data periods
USPS_data <- USPS_data %>%
  distinct(id, time, .keep_all = TRUE) %>%
  group_by(id) %>%
  mutate(number_of_quarters_obs = n()) %>%
  ungroup()

max_quarter <- max(USPS_data$number_of_quarters_obs)

USPS_data <- USPS_data %>% filter(number_of_quarters_obs == max_quarter)


USPS_data <- USPS_data %>%
  filter(YEAR %in% c(2015,2016,2017) |  date == "2025-03-01") %>%
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

USPS_data <- USPS_data %>%
  # filter(Designation_category %in% c("LIC selected","LIC not selected")) %>%
  select(all_of(outcome_var), 
         all_of(treatment_var), 
         all_of(controls_allof),
         all_of(conditional_allof),
         "id", "date", "time", "Designation",
         "STATEFP","Type tract"
         ) %>% 
  na.omit() 

balanced_ids <- USPS_data %>%
  group_by(id) %>%
  summarise(n_time = n_distinct(time)) %>%
  filter(n_time == n_distinct(USPS_data$time)) %>%
  pull(id)

USPS_data <- USPS_data %>%
  filter(id %in% balanced_ids)



model_output <- fect(
    data = USPS_data
    , method = "mc"
    # , method = "cfe"
    , formula = current_formula
    , na.rm = TRUE
    , index  = c("id", "time")
    , force = "two-way"
    , r = c(0, 5)
    , nlambda = 5
    , CV = TRUE
    , k = 10
    , cv.prop = 0.05
    , cv.treat = FALSE
    , cv.nobs = 3
    , cv.donut = 0
    , criterion = "mspe"
    , se = TRUE
    # , vartype = "jackknife"
    , nboots = 100
    , alpha = 0.05
    , parallel = TRUE
    , seed = 42
    , min.T0 = 5
    , max.missing = 0
    , proportion = 0.3
    , f.threshold = 0.5
    , degree = 2
    # , sfe = c("STATEFP")
    # , cfe = list(c("Type tract", "time"), c("STATEFP", "time"))
    , fill.missing = FALSE
    , placeboTest = FALSE
    , carryoverTest = FALSE
    , loo = FALSE
    , permute = FALSE
    , m = 2
    , normalize = TRUE
    )
  
  # Extract effect estimates for the treated unit
  Effect <- model_output[["eff"]]
  # Transpose and convert to data frame
  Effect <- t(Effect)
  Effect <- as.data.frame(Effect)
  colnames(Effect) <- paste("Period:", seq_len(ncol(Effect)))
  Effect <- cbind(id = rownames(Effect), Effect)
  
  # Reshape into long format and compute confidence bounds
  # effect_long <- Effect %>%
  #   pivot_longer(
  #     cols = starts_with("Period:"),
  #     names_to = "Period",
  #     values_to = "Effect on Total Addresses"
  #   ) 
  
  # Reshape into long format and compute confidence bounds
  effect_long <- Effect %>%
    pivot_longer(
      cols = starts_with("Period:"),
      names_to = "Period",
      values_to = "Effect on Total Addresses"
    ) %>%
    mutate(`Average S.E.` = model_output[["est.avg"]][[2]],
           Upper = `Effect on Total Addresses` + `Average S.E.` * 1.96,
           Lower = `Effect on Total Addresses` - `Average S.E.` * 1.96,
           Significant = case_when(
             (Lower > 0 & Upper > 0) | (Lower < 0 & Upper < 0) ~ 1,
             (pmin(Lower, Upper) <= 0 & pmax(Lower, Upper) >= 0) ~ 0,
             TRUE ~ NA_real_
           ))
  
# Combine all effect estimates into one data frame
# All_estimates <- bind_rows(estimate_list)

###########################################
### (Optional) Further Analysis & Export ###
###########################################

# Save the effect estimates and estimation outputs as needed
setwd(path_output)
save(effect_long, file = "MC_FECT_Effect_Estimates_and_SE_all_tracts.RData")
save(model_output, file = "MC_FECT_Estiamted_Models_all_tracts.RData")



