# HUD Aggregated USPS Administrative Data on Vacancies
# Ben Glasner 5/29/2025
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

library(plotly)
library(tigris)
library(purrr)
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
load(file = "Counterfactual_ready_data.RData")

USPS_data <- USPS_data %>%
  filter(Designation_category %in% c("Ineligible", "LIC not selected", "LIC selected")) %>%
  rename(  current_poverty_rate_decile  = `current poverty rate decile`,
           current_median_income_decile   = `current median income decile`,
           current_solo_detached_decile  = `current solo detached decile`)

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
    nlambda = 5,
    CV = TRUE,
    k = 10,
    cv.prop = 0.05,
    cv.treat = FALSE,
    cv.nobs = 3,
    cv.donut = 0,
    criterion = "gmspe", #to alleviate the impact of some outlier prediction errors, we allow the criterion of geometric-mean squared prediction errors
    method = method,
    se = TRUE,
    # se = FALSE,
    vartype = "jackknife",
    # vartype = "bootstrap",
    # quantile.CI = FALSE,
    nboots = 50,
    alpha = 0.05,
    parallel = TRUE,
    # cores = 12,
    # max.iteration = 1000,
    seed = 42,
    min.T0 = 5,
    max.missing = 0,
    proportion = 0.3,
    f.threshold = 0.5,
    degree = 2,
    sfe = c("CBSA_TITLE"),
    cfe = list(c("type_tract", "time"), c("CBSA_TITLE", "time")),
    fill.missing = FALSE,
    placeboTest = FALSE,
    carryoverTest = FALSE,
    loo = FALSE,
    permute = FALSE,
    m = 2,
    normalize = TRUE
  )
  
  out <- do.call(fect, args)
  invisible(out)
}


######################
#### Analysis Prep ###
######################

baseline_date <- "2017-12-01"

# --- 1. Snapshot each tract's baseline decile information ------------
snapshot <- USPS_data %>% 
  filter(date == baseline_date) %>% 
  select(id,
         Designation_category,
         current_poverty_rate_decile,
         current_median_income_decile,
         current_solo_detached_decile)

# --- 2. All unique decile triples that ANY treated tract occupies -----
treated_decile_combos <- snapshot %>% 
  filter(Designation_category == "LIC selected") %>% 
  distinct(current_poverty_rate_decile,
           current_median_income_decile,
           current_solo_detached_decile)

# --- 3. Keep only control tracts that match one of those triples -----
eligible_control_ids <- snapshot %>% 
  filter(Designation_category != "LIC selected") %>%          # untreated pool
  semi_join(treated_decile_combos,                             # exact-match filter
            by = c("current_poverty_rate_decile",
                   "current_median_income_decile",
                   "current_solo_detached_decile")) %>% 
  pull(id)

# --- 4. Build the analysis panel: every row for every matched tract ---
analysis_data <- USPS_data %>% 
  filter(id %in% c(eligible_control_ids,
                   snapshot %>% filter(Designation_category == "LIC selected") %>% pull(id)))

# Optional: quick check
analysis_data %>% 
  filter(time == 1) %>% 
  count(Designation_category)


model_output <- run_and_plot(method = "mc", selected_data = analysis_data)


