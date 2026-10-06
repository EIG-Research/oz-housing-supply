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
    cores = 2,
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


# Get list of treated unit IDs
treated_units <- USPS_data %>%
  filter(time == max(time)) %>%
  filter(Designation_category == "LIC selected") %>%
  distinct(id)

all_units <- USPS_data %>%
  filter(time == max(time)) %>%
  distinct(id)

# Initialize list to store effect estimates
estimate_list <- list()

bin_num <- 1
load(file = file.path(paste0("all_units_bin_", bin_num, ".RData")))

n_treated <- length(bin_ids$id)
estimate_list <- list()

pb <- progress_bar$new(
  format = paste0("Bin ", bin_num, " - :current/:total [:bar] :elapsed | ETA: :eta"),
  total = n_treated,
  width = 60
)

###########################################
### Loop Over Each Treated Unit Individually ###
###########################################

for (i in seq_len(n_treated)) {
  current_treated_id <- bin_ids$id[i]
  
  # Get the decile values for the current treated unit
  base_data <- USPS_data %>%
    filter(id == current_treated_id, date == "2017-12-01")
  
  housing_date_decile <- base_data$`current solo detached decile`
  poverty_date_decile <- base_data$`current poverty rate decile`
  income_date_decile <- base_data$`current median income decile`
  
  # Step 1: Expand boundaries until at least 1000 unique control ids are found
  bound <- 1
  repeat {
    control_ids <- USPS_data %>%
      filter(LIC_oz_neighbor!=1)%>%
      filter(id != current_treated_id,
             date == "2017-12-01",
             Designation_category != "LIC selected",
             abs(`current solo detached decile` - housing_date_decile) <= bound,
             abs(`current poverty rate decile` - poverty_date_decile) <= bound,
             abs(`current median income decile` - income_date_decile) <= bound) %>%
      distinct(id) %>%
      pull(id)
    
    if (length(unique(control_ids)) >= 1000 || bound > 5) break
    bound <- bound + 1
  }
  
  if (length(control_ids) < 100) next  # Skip if still too few controls
  
  control_ids <- sample(control_ids, size = min(500, length(control_ids)))
  
  # Step 2: Check if control variables are time-invariant
  temp_data <- USPS_data %>%
    filter(id == current_treated_id | id %in% control_ids)
  
  
  is_time_invariant <- temp_data %>%
    filter(id %in% current_treated_id) %>%
    select(id, date, all_of(controls)) %>%
    pivot_longer(-c(id, date), names_to = "var", values_to = "value") %>%
    group_by(id, var) %>%
    summarise(n_unique = n_distinct(value), .groups = "drop") %>%
    ungroup() %>%
    group_by(id) %>%
    summarise(is_time_invariant = min(n_unique)) %>%
    pull(is_time_invariant)

  if (is_time_invariant == 1) next  # Skip if control group variables don't vary over time
  
  # Step 3: Prep data and run model
  temp_data <- temp_data %>%
    group_by(id) %>%
    arrange(time) %>%
    mutate(time_org = time,
           time = row_number(),
           Designation = if_else(id == current_treated_id & YEAR >2017, 1, 0)) %>%
    na.omit()
  
  model_output <- run_and_plot(method = "mc", selected_data = temp_data)
  # model_output <- run_and_plot(method = "cfe", selected_data = temp_data)
  
  # Step 4: Extract and format effect estimates
  # Effect <- t(model_output[["eff"]]) %>%
  #   as.data.frame()
  # colnames(Effect) <- paste("Period:", seq_len(ncol(Effect)))
  # Effect <- cbind(id = rownames(Effect), Effect)
  
    Effect <- model_output[["eff"]]
    # Transpose and convert to data frame
    Effect <- t(Effect)
    Effect <- as.data.frame(Effect)
    colnames(Effect) <- paste("Period:", seq_len(ncol(Effect)))
    Effect <- cbind(id = rownames(Effect), Effect)
  
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
    
  estimate_list[[i]] <- effect_long

  pb$tick()  # update progress bar
  rm(temp_data)
}

save(estimate_list, file = file.path(path_output, paste0("effect_estimates_bin_", bin_num, ".RData")))

