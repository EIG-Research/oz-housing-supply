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
  "name" = "PATH TO GITHUB REPO"
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
load(file = "USPS_tract_vacancy_2012_2024_2020_definitions.RData")

##################
### Data clean ###
##################

### Clean up uncertain tracts from the crosswalk
USPS_data <- USPS_data %>%
  filter(`Designation_category` %in% c("LIC selected","LIC not selected")) %>%
  filter(Sample == "In Clean Sample") %>%
  # filter(YEAR %in% c(2014,2015,2016,2017) | date == "2023-12-01") %>%
  filter(YEAR %in% c(2014,2015,2016,2017, 2018,2019,2020,2021,2022,2023) ) %>%
  mutate(
    id = as.numeric(geoid), 
    time = dense_rank(date),
    Designation = if_else(`OZ Designation` == 1 & date >= "2018-03-01", 1, 0),
    Total_active = ACTIVE_RESIDENTIAL_ADDRESSES + STV_RESIDENTIAL_ADDRESSES + LTV_RESIDENTIAL_ADDRESSES, 
    log_Total_active = log(Total_active),
    `current median income decile` = ntile(median_income, 10),
    `current poverty rate decile` = ntile(poverty_rate, 10),
    `current solo detached decile` = ntile(solo_detached_housing_share, 10)
  ) %>%
  select(id, time, date, state_fips,
         `Type tract`, Designation, `Designation_category`,
         poverty_rate, median_income, unemployment_rate, prime_age_share, solo_detached_housing_share,
         `current median income decile`, `current poverty rate decile`, `current solo detached decile`,
         Total_active, log_Total_active) %>%
  mutate(type_tract = as.numeric(as.factor("Type tract")))

# Ensure panel is balanced after removing missing data periods
USPS_data <- USPS_data %>%
  distinct(id, time, .keep_all = TRUE) %>%
  group_by(id) %>%
  mutate(number_of_quarters_obs = n()) %>%
  ungroup()

max_quarter <- max(USPS_data$number_of_quarters_obs)
USPS_data <- USPS_data %>% filter(number_of_quarters_obs == max_quarter)

###############################
### Counterfactual Analysis Function ###
###############################

# Setup formula
outcome_var   <- "Total_active"
treatment_var <- "Designation"
# Uncomment the next line if you wish to include controls
# controls      <- c("`current median income decile`", "`current poverty rate decile`")
controls      <- c("solo_detached_housing_share", "median_income", "poverty_rate")
control_vars  <- paste(controls, collapse = " + ")
# current_formula <- as.formula(paste(outcome_var, "~", treatment_var))
current_formula <- as.formula(paste(outcome_var, "~", treatment_var, "+", control_vars))

# Helper function to run fect, save, print, and plot results
run_and_plot <- function(method, selected_data) {
  # Set common fect parameters
  args <- list(
    formula = current_formula,
    data = selected_data,
    na.rm = TRUE,
    index  = c("id", "time"),
    force = "two-way",
    r = c(0, 5),
    lambda = 0.01,
    # nlambda = 5,
    CV = FALSE,
    k = 10,
    cv.prop = 0.05,
    cv.treat = FALSE,
    cv.nobs = 3,
    cv.donut = 0,
    criterion = "mspe",
    method = method,
    se = TRUE,
    # se = FALSE,
    vartype = "jackknife",
    quantile.CI = FALSE,
    nboots = 25,
    alpha = 0.05,
    parallel = TRUE,
    cores = 12,
    max.iteration = 1000,
    seed = 42,
    min.T0 = 5,
    max.missing = 0,
    proportion = 0.3,
    f.threshold = 0.5,
    degree = 2,
    sfe = c("state_fips"),
    cfe = list(c("type_tract", "time"), c("state_fips", "time")),
    fill.missing = FALSE,
    placeboTest = TRUE,
    placebo.period = c(-8, 0),
    carryoverTest = FALSE,
    loo = FALSE,
    permute = FALSE,
    m = 2,
    normalize = FALSE
  )
  
  out <- do.call(fect, args)
  invisible(out)
}

#####################################
### Create Treated & Control Sets ###
#####################################

# Get list of treated unit IDs
treated_units <- USPS_data %>%
  filter(Designation_category == "LIC selected") %>%
  distinct(id)

# # Get full set of control unit IDs (all "LIC not selected")
# control_ids <- USPS_data %>%
#   filter(Designation_category == "LIC not selected") %>%
#   distinct(id) %>%
#   pull(id)

# Initialize list to store effect estimates
estimate_list <- list()
model_output_fe.p <- list()
model_output_ife.p <- list()
model_output_mc.p <- list()

# Set up progress bar over treated units
n_treated <- nrow(treated_units)
pb <- progress_bar$new(
  format = "Treated unit :current/:total [:bar] :elapsed | ETA: :eta",
  total = n_treated,
  width = 60
)

###########################################
### Loop Over Each Treated Unit Individually ###
###########################################

for (i in seq_len(n_treated)) {
  current_treated_id <- treated_units$id[i]
  
  # Get the decile value for the current treated unit
  housing_date_decile <- USPS_data %>%
    filter(id == current_treated_id & date == "2016-09-01") %>%
    pull(`current solo detached decile`)
  
  poverty__date_decile <- USPS_data %>%
    filter(id == current_treated_id & date == "2016-09-01") %>%
    pull(`current poverty rate decile`)
  
  income__date_decile <- USPS_data %>%
    filter(id == current_treated_id & date == "2016-09-01") %>%
    pull(`current median income decile`)
  
  # Get full set of control unit IDs (all "LIC not selected")
  control_ids <- USPS_data %>%
    filter(date == "2016-09-01") %>%
    filter(Designation_category == "LIC not selected") %>%
    filter(abs(`current solo detached decile` - housing_date_decile) <= 1) %>%
    filter(abs(`current poverty rate decile` - poverty__date_decile) <= 1) %>%
    filter(abs(`current median income decile` - income__date_decile) <= 1) %>%
    distinct(id) %>%
    pull(id)
  
  # Create temporary dataset: current treated unit + full control pool
  temp_data <- USPS_data %>%
    filter(id == current_treated_id | id %in% control_ids) %>%
    select(all_of(outcome_var), all_of(treatment_var), all_of(controls), "id", "time" ) %>%
    na.omit()
  
  # Run matrix completion estimation (using method = "mc")
  model_output_fe.p[[i]] <- run_and_plot(method = "fe", selected_data = temp_data)
  model_output_ife.p[[i]] <- run_and_plot(method = "ife", selected_data = temp_data)
  model_output_mc.p[[i]] <- run_and_plot(method = "mc", selected_data = temp_data)
  
  plot(model_output_fe.p[[i]], cex.text = 0.8, stats = c("placebo.p","equiv.p"), 
       main = "Estimated ATT (TWFE)", count = FALSE)
  
  plot(model_output_fe.p[[i]], type = "equiv", 
       cex.legend = 0.6, main = "Testing Pre-Trend (FEct)", cex.text = 0.8, count = FALSE)
  
  plot(model_output_ife.p[[i]], cex.text = 0.8, stats = c("placebo.p","equiv.p"), 
       main = "Estimated ATT (TWFE)")
  
  plot(model_output_ife.p[[i]], type = "equiv", 
       cex.legend = 0.6, main = "Testing Pre-Trend (FEct)", cex.text = 0.8, count = FALSE)
  
  plot(model_output_mc.p[[i]], cex.text = 0.8, stats = c("placebo.p","equiv.p"), 
       main = "Estimated ATT (TWFE)")
  
  plot(model_output_mc.p[[i]], type = "equiv", 
       cex.legend = 0.6, main = "Testing Pre-Trend (FEct)", cex.text = 0.8, count = FALSE)
  
  
  # Extract effect estimates for the treated unit
  Effect <- model_output[[i]][["eff"]]
  # Transpose and convert to data frame
  Effect <- t(Effect)
  Effect <- as.data.frame(Effect)
  colnames(Effect) <- paste("Period:", seq_len(ncol(Effect)))
  Effect <- cbind(id = rownames(Effect), Effect)
  
  # Reshape into long format and compute confidence bounds
  effect_long <- Effect %>%
    pivot_longer(
      cols = starts_with("Period:"),
      names_to = "Period",
      values_to = "Effect on Total Addresses"
    ) %>%
    mutate(`Average S.E.` = model_output[[i]][["est.avg"]][[2]],
           Upper = `Effect on Total Addresses` + `Average S.E.` * 1.96,
           Lower = `Effect on Total Addresses` - `Average S.E.` * 1.96,
           Significant = case_when(
             (Lower > 0 & Upper > 0) | (Lower < 0 & Upper < 0) ~ 1,
             (pmin(Lower, Upper) <= 0 & pmax(Lower, Upper) >= 0) ~ 0,
             TRUE ~ NA_real_
           )) %>%
    filter(id == current_treated_id)
  
  # effect_long <- Effect %>%
  #   pivot_longer(
  #     cols = starts_with("Period:"),
  #     names_to = "Period",
  #     values_to = "Effect on Total Addresses"
  #   ) %>%
  # filter(id == current_treated_id)
  
  estimate_list[[i]] <- effect_long
  
  pb$tick()  # update progress bar
  
  # Remove temporary dataset to free memory
  rm(temp_data)
  gc()
}

# Combine all effect estimates into one data frame
All_estimates <- bind_rows(estimate_list)

###########################################
### (Optional) Further Analysis & Export ###
###########################################

# Save the effect estimates and estimation outputs as needed
setwd(path_output)
save(All_estimates, file = "MC_FECT_Effect_Estimates_and_SE.RData")
save(model_output, file = "MC FECT Estiamted Models.RData")
# Optionally, you might want to also save the individual model outputs if needed.

# You can now proceed with plotting or additional analysis on All_estimates
# For example, plotting the average effect for a given period:

# max_period <- USPS_data %>%
#   filter(time == max(time)) %>%
#   select(Period) %>%
#   distinct() %>%
#   pull(Period)

current_estimate <- All_estimates %>%
  mutate(Period_num = stringr::str_sub(Period ,-2, -1)) %>% 
  filter(Period_num == max(Period_num)) %>%
  summarize(Average_Effect = mean(`Effect on Total Addresses`, na.rm = TRUE)) %>%
  pull(Average_Effect)

print(paste("Average Effect on Total Addresses in Period 10:", round(current_estimate, 4)))


All_estimates %>%
  mutate(Period_num = stringr::str_sub(Period ,-2, -1)) %>% 
  # filter(Period_num == max(Period_num)) %>%
  group_by(Period_num) %>%
  summarize(Average_Effect = mean(`Effect on Total Addresses`, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(aes(x = as.numeric(Period_num), 
             y = Average_Effect)) + 
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 16.5) +
  geom_line() + 
  geom_point() + 
  theme_bw()
