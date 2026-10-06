##############################################
# HUD Aggregated USPS Administrative Data on Vacancies
# Propensity-Weighted Donor Selection for fect (Matrix Completion)
# Ben Glasner — Updated: 2025-09-29
#
# This script:
#   1) Loads tract-level USPS vacancy/address data and constructs a balanced panel.
#   2) Defines treatment: OZ designation turning on in the post period (>= 2018-03-01).
#   3) Estimates *baseline* propensity to *ever* be designated using RAW covariates
#      (NOT deciles): poverty_rate, median_income, solo_detached_housing_share, Type tract
#      (you can re-include Zoning_Index and LIC_oz_neighbor if desired).
#   4) NEW (mc-friendly): Remove **pre-existing state-level time trends** by
#      estimating a *pre-period, state-level linear slope* and subtracting it from the outcome
#      (slope-only detrending, centered to retain levels).
#   5) For EACH treated tract, sample up to N never-treated donors with probability
#      proportional to a calipered similarity weight, optionally **favoring same-state donors**.
#   6) Run fect(method="mc") on (treated + sampled donors), extract dynamic effects.
#   7) Aggregate/plot summaries and heterogeneity slices.
#
# Important:
#   • Period parsing fixed via readr::parse_number().
#   • No per-period “Significant” flags from a single average S.E.
#   • Propensity model uses RAW baseline covariates (not deciles).
#   • State-level slope detrending is *pre-period-only* and slope-only (keeps levels).
##############################################

#############################
### 0. Housekeeping & perf ###
#############################

rm(list = ls())
options(scipen = 999)
set.seed(42)

# (Windows-only niceness; safe to ignore on other OSes)
pid <- Sys.getpid()
shell(
  sprintf(
    'powershell.exe "Get-Process -Id %s | ForEach-Object { $_.PriorityClass = \'High\' }"',
    pid
  ),
  intern = FALSE
)

###########################
### 1. Load Packages    ###
###########################
# devtools::install_github("xuyiqing/fect")
library(fect)         # https://yiqingxu.org/packages/fect/01-start.html
library(openxlsx)
library(tidyr)
library(dplyr)
library(panelView)
library(ggplot2)
library(broom)
library(lmtest)
library(sandwich)
library(fixest)
library(modelsummary)
library(gt)
library(webshot2)
library(purrr)
library(progress)
library(plotly)
library(tigris)
library(readr)        # parse_number()

######################
### 2. Set paths   ###
######################

project_directories <- list(
  "name"             = "PATH TO GITHUB REPO",
  "Benjamin Glasner" = "C:/Users/Benjamin Glasner/EIG Dropbox/Benjamin Glasner/GitHub/oz-housing-supply",
  "bngla"            = "C:/Users/bngla/EIG Dropbox/Benjamin Glasner/GitHub/oz-housing-supply",
  "Research"         = "C:/Users/Research/EIG Dropbox/Benjamin Glasner/GitHub/oz-housing-supply"
)

current_user <- Sys.info()[["user"]]
if (!current_user %in% names(project_directories)) {
  stop("Root folder for current user is not defined.")
}

path_project    <- project_directories[[current_user]]
path_data       <- file.path(path_project, "data")
path_output     <- file.path(path_project, "output")

########################
### 3. Load the data ###
########################

setwd(path_data)
# Expects an object named USPS_data
load(file = "USPS_tract_vacancy.RData")

############################################
### 4. Data cleaning & panel construction ###
############################################

# 4.A: Basic filtering and derived fields
USPS_data <- USPS_data %>%
  filter(Designation_category_detailed %in% c("LIC selected",
                                              "LIC not selected, not a border tract",
                                              "Ineligible, not a border tract")) %>%
  filter(YEAR >= 2015) %>%
  mutate(
    id   = as.numeric(geoid),
    time = dense_rank(date),
    Designation = if_else(`OZ Designation` == 1 & date >= as.Date("2018-03-01"), 1L, 0L),
    Total_residential = ACTIVE_RESIDENTIAL_ADDRESSES +
      STV_RESIDENTIAL_ADDRESSES +
      LTV_RESIDENTIAL_ADDRESSES
  ) %>%
  select(id, geoid, time, YEAR, date,
         `Type tract`, Designation, LIC_oz_neighbor,
         poverty_rate, median_income, solo_detached_housing_share, Zoning_Index,
         Total_residential, Designation_category_detailed)

# 4.B: Add state (2-digit FIPS) early; also keep a simple numeric time for detrending
USPS_data <- USPS_data %>%
  mutate(
    GEOID_chr = sprintf("%011.0f", as.numeric(geoid)),
    state     = substr(GEOID_chr, 1, 2),
    t_index   = as.numeric(factor(date))  # strictly increasing time index across the panel
  )

# Balance panel on the current time grid
USPS_data <- USPS_data %>%
  distinct(id, time, .keep_all = TRUE) %>%
  group_by(id) %>%
  mutate(number_of_quarters_obs = n()) %>%
  ungroup()

max_quarter <- max(USPS_data$number_of_quarters_obs)
USPS_data   <- USPS_data %>% filter(number_of_quarters_obs == max_quarter)

# Year-specific deciles (for heterogeneity summaries only)
USPS_data <- USPS_data %>%
  group_by(YEAR) %>%
  mutate(
    `current median income decile` = ntile(median_income, 10),
    `current poverty rate decile`  = ntile(poverty_rate, 10),
    `current solo detached decile` = ntile(solo_detached_housing_share, 10),
    `current zoning index decile`  = ntile(Zoning_Index, 10)
  ) %>%
  ungroup()

# Ever-treated flag
USPS_data <- USPS_data %>%
  group_by(id) %>%
  mutate(EverDesignated = as.integer(max(Designation, na.rm = TRUE))) %>%
  ungroup()

# -------- 4.C: Pre-period, state-level slope detrending (NEW) --------
# Goal: remove pre-existing state trends using only pre period (<=2017).
baseline_cut_year <- 2017
pre_df_state_time <- USPS_data %>%
  filter(YEAR <= baseline_cut_year) %>%
  group_by(state, t_index) %>%
  summarize(ybar = mean(Total_residential, na.rm = TRUE), .groups = "drop")

# For each state, estimate a linear slope of ybar ~ t_index (pre-period only)
state_slopes <- pre_df_state_time %>%
  group_by(state) %>%
  group_modify(~{
    m <- lm(ybar ~ t_index, data = .x)
    tibble(beta_state = coef(m)[["t_index"]])
  }) %>%
  ungroup()

# Center time within pre-period so we remove *slope only* (keep levels):
state_t_center <- pre_df_state_time %>%
  group_by(state) %>%
  summarize(t0 = mean(t_index, na.rm = TRUE), .groups = "drop")

# Join slope and center, then create detrended outcome for *all* periods:
USPS_data <- USPS_data %>%
  left_join(state_slopes,  by = "state") %>%
  left_join(state_t_center, by = "state") %>%
  mutate(
    beta_state = ifelse(is.na(beta_state), 0, beta_state),  # safety if a state lacked pre obs
    t0         = ifelse(is.na(t0),         mean(t_index), t0),
    Total_residential_adj = Total_residential - beta_state * (t_index - t0)
  )

##################################################
### 5. Modeling config & balanced analysis view ###
##################################################

# Outcome and treatment names (strings)
outcome_var        <- "Total_residential_adj"  # <-- use detrended outcome
treatment_var      <- "Designation"
controls_cont      <- c("poverty_rate", "median_income", "solo_detached_housing_share")
controls_cont_allof <- controls_cont

deciles_allof <- c("current median income decile",
                   "current poverty rate decile",
                   "current solo detached decile",
                   "current zoning index decile")

current_formula <- as.formula(
  paste(outcome_var, "~", treatment_var, "+", paste(controls_cont, collapse = " + "))
)

run_fect <- function(method, selected_data) {
  args <- list(
    formula   = current_formula,
    data      = selected_data,
    na.rm     = TRUE,
    index     = c("id", "time"),
    force     = "two-way",
    r         = c(0, 5),
    CV        = TRUE,
    criterion = "gmspe",
    method    = method,
    se        = TRUE,
    vartype   = "jackknife",
    nboots    = 100,
    alpha     = 0.05,
    parallel  = TRUE,
    cores     = 12,
    seed      = 42,
    min.T0    = 5,
    normalize = TRUE
  )
  do.call(fect, args)
}

# Keep only complete ids on the final time grid
USPS_data <- USPS_data %>%
  select(all_of(outcome_var),
         all_of(treatment_var),
         all_of(controls_cont_allof),
         all_of(deciles_allof),
         id, date, YEAR, time, t_index,
         EverDesignated, LIC_oz_neighbor, `Type tract`,
         Zoning_Index, geoid, Designation_category_detailed, state) %>%
  na.omit()

balanced_ids <- USPS_data %>%
  group_by(id) %>%
  summarise(n_time = n_distinct(time), .groups = "drop") %>%
  filter(n_time == n_distinct(USPS_data$time)) %>%
  pull(id)

USPS_data <- USPS_data %>% filter(id %in% balanced_ids)

treated_units <- USPS_data %>%
  group_by(id) %>%
  summarise(EverDesignated = max(EverDesignated), .groups = "drop") %>%
  filter(EverDesignated == 1L)

never_treated_ids <- USPS_data %>%
  group_by(id) %>%
  summarise(EverDesignated = max(EverDesignated), .groups = "drop") %>%
  filter(EverDesignated == 0L) %>%
  pull(id)

#############################################################
### 6. Baseline propensity: RAW covariates (NOT deciles)  ###
#############################################################

baseline_date <- as.Date("2017-12-01")

# You can re-include Zoning_Index and LIC_oz_neighbor by uncommenting below
propensity_covars_raw <- c("poverty_rate",
                           "median_income",
                           "solo_detached_housing_share",
                           "`Type tract`")

baseline_df <- USPS_data %>%
  filter(date == baseline_date) %>%
  select(id,
         EverDesignated,
         poverty_rate, median_income, solo_detached_housing_share, Zoning_Index,
         LIC_oz_neighbor, `Type tract`, state,
         all_of(deciles_allof)) %>%
  mutate(
    `Type tract`    = factor(`Type tract`),
    LIC_oz_neighbor = as.integer(LIC_oz_neighbor)
  ) %>%
  distinct(id, .keep_all = TRUE)

if (length(unique(baseline_df$EverDesignated)) < 2) {
  stop("EverDesignated has no variation at baseline; cannot fit propensity model.")
}

propensity_formula <- as.formula(
  paste("EverDesignated ~", paste(propensity_covars_raw, collapse = " + "))
)

propensity_fit <- glm(propensity_formula,
                      data   = baseline_df,
                      family = binomial(link = "logit"))

baseline_df <- baseline_df %>%
  mutate(p_hat = as.numeric(predict(propensity_fit, type = "response")))

USPS_data <- USPS_data %>%
  left_join(baseline_df %>% select(id, p_hat), by = "id")

############################################################
### 7. Per-treated tract: calipered, weighted donor draw ###
############################################################

# Toggle and strength for same-state preference in donor sampling (optional)
prefer_same_state_donors <- TRUE
same_state_bonus         <- 1.5  # weights multiplied by this if donor state==treated state

initial_caliper   <- 0.10
max_caliper       <- 0.25
control_target_n  <- 500
min_controls_keep <- 100

estimate_list <- list()
n_treated     <- nrow(treated_units)

pb <- progress_bar$new(
  format = "Treated unit :current/:total [:bar] :elapsed | ETA: :eta",
  total  = n_treated,
  width  = 60
)

for (i in seq_len(n_treated)) {
  current_treated_id <- treated_units$id[i]
  
  # Treated tract baseline propensity and state
  pt_row <- baseline_df %>% filter(id == current_treated_id)
  p_t    <- pt_row$p_hat
  st_t   <- pt_row$state
  
  if (length(p_t) != 1 || is.na(p_t)) { pb$tick(); next }
  
  candidates <- baseline_df %>%
    filter(id %in% never_treated_ids, id != current_treated_id) %>%
    mutate(
      absdiff    = abs(p_hat - p_t),
      same_state = as.integer(state == st_t)
    )
  
  cal <- initial_caliper
  repeat {
    candidates_cal <- candidates %>%
      filter(absdiff <= cal) %>%
      mutate(weight = pmax(0, 1 - absdiff / cal)) %>%
      filter(weight > 0)
    
    if (prefer_same_state_donors && nrow(candidates_cal) > 0) {
      candidates_cal <- candidates_cal %>%
        mutate(weight = weight * ifelse(same_state == 1, same_state_bonus, 1))
    }
    
    if (nrow(candidates_cal) >= min_controls_keep || cal >= max_caliper) break
    cal <- cal + 0.05
  }
  
  if (nrow(candidates_cal) < min_controls_keep) { pb$tick(); next }
  
  sample_n_controls <- min(control_target_n, nrow(candidates_cal))
  set.seed(42 + i)
  control_sample_ids <- candidates_cal %>%
    slice_sample(n = sample_n_controls, weight_by = weight, replace = FALSE) %>%
    pull(id)
  
  # Mini-panel with detrended outcome; reindex local time; force treated post-2017
  temp_data <- USPS_data %>%
    filter(id == current_treated_id | id %in% control_sample_ids) %>%
    group_by(id) %>%
    arrange(time) %>%
    mutate(
      time_org    = time,
      time        = row_number(),
      Designation = if_else(id == current_treated_id & YEAR > 2017, 1L, 0L)
    ) %>%
    ungroup() %>%
    select(id, time, YEAR, date,
           all_of(outcome_var), all_of(treatment_var),
           all_of(controls_cont_allof))
  
  temp_pre_counts <- temp_data %>%
    group_by(id) %>%
    summarise(T0 = sum(YEAR <= 2017), .groups = "drop")
  
  if (any(temp_pre_counts$T0 < 3)) { pb$tick(); next }
  
  model_output <- run_fect(method = "mc", selected_data = temp_data)
  
  eff_mat <- model_output[["eff"]]
  if (is.null(eff_mat)) { pb$tick(); next }
  
  Effect <- as.data.frame(t(eff_mat))
  colnames(Effect) <- paste0("Period:", seq_len(ncol(Effect)))
  Effect <- cbind(id = rownames(Effect), Effect)
  
  effect_long <- Effect %>%
    pivot_longer(
      cols      = starts_with("Period:"),
      names_to  = "Period",
      values_to = "Effect on Total Addresses"
    ) %>%
    mutate(
      `Average S.E.` = model_output[["est.avg"]][[2]],
      Upper          = `Effect on Total Addresses` + `Average S.E.` * 1.96,
      Lower          = `Effect on Total Addresses` - `Average S.E.` * 1.96
    ) %>%
    filter(id == as.character(current_treated_id)) %>%
    mutate(
      treated_id     = current_treated_id,
      p_treated      = p_t,
      caliper_used   = cal,
      n_donors_used  = length(control_sample_ids)
    )
  
  estimate_list[[i]] <- effect_long
  pb$tick()
}

All_estimates <- bind_rows(estimate_list)

setwd(path_output)
save(All_estimates,file = "MC_Local_Effect_Individual_Estimates_Detrended.RData")
############################################
### 8. Basic summaries & time profiles   ###
############################################

All_estimates <- All_estimates %>%
  mutate(Period_num = parse_number(Period),
         hypothesis = 0,
         Significant = ifelse(between(hypothesis,Lower,Upper)==TRUE & Period_num>12,"0","1"))

current_estimate <- All_estimates %>%
  group_by(treated_id) %>%
  filter(Period_num == max(Period_num, na.rm = TRUE), Significant == "1") %>%
  ungroup() %>%
  summarize(Average_Effect = mean(`Effect on Total Addresses`, na.rm = TRUE)) %>%
  pull(Average_Effect)

print(paste("Average Effect on Total Addresses (last period):", round(current_estimate, 2)))

All_estimates %>%
  # filter(Significant == "1") %>%
  group_by(Period_num) %>%
  summarize(Average_Effect = mean(`Effect on Total Addresses`, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = Period_num, y = Average_Effect)) +
  geom_hline(yintercept = 0) +
  geom_line() +
  geom_point() +
  theme_bw() +
  labs(x = "Relative Period Index", y = "Average Effect on Total Addresses",
       title = "Average Dynamic Effect Across Treated Units (state-detrended outcome)")

########################################
### 9. Heterogeneity constructions   ###
########################################

baseline_date <- as.Date("2017-12-01")

Heterogeneity <- USPS_data %>%
  filter(date == baseline_date) %>%
  select(id, Total_residential_adj, poverty_rate:`current zoning index decile`, Designation) %>%
  mutate(id = as.character(id))

pctile_99 <- USPS_data %>%
  filter(date %in% c(as.Date("2015-03-01"), baseline_date)) %>%
  group_by(id) %>%
  arrange(date) %>%
  mutate(change = Total_residential_adj - lag(Total_residential_adj),
         percent_change = change / lag(Total_residential_adj)) %>%
  ungroup() %>%
  pull(percent_change) %>%
  quantile(probs = 0.99, na.rm = TRUE)

growth <- USPS_data %>%
  filter(date %in% c(as.Date("2015-03-01"), baseline_date)) %>%
  group_by(id) %>%
  arrange(date) %>%
  mutate(change = Total_residential_adj - lag(Total_residential_adj),
         percent_change = change / lag(Total_residential_adj)) %>%
  ungroup() %>%
  filter(date == baseline_date) %>%
  mutate(
    percent_change = pmin(percent_change, pctile_99),
    growth_decile  = ntile(percent_change, n = 10)
  ) %>%
  drop_na(percent_change) %>%
  select(id, percent_change, growth_decile) %>%
  mutate(id = as.character(id))

All_estimates <- All_estimates %>%
  left_join(Heterogeneity, by = "id") %>%
  left_join(growth,       by = "id")

Current_effect <- All_estimates %>%
  group_by(treated_id) %>%
  filter(Period_num == max(Period_num, na.rm = TRUE), Significant == "1") %>%
  ungroup()

#################################
### 10. State-level summaries ###
#################################

Current_effect <- Current_effect %>%
  mutate(GEOID = sprintf("%011.0f", as.numeric(id)))

state_list <- unique(c(state.abb, "DC"))
tracts_all <- rbind_tigris(lapply(state_list, function(st) {
  tryCatch({ tracts(st, year = 2020) }, error = function(e) NULL)
}) %>% compact())

tract_data <- tracts_all %>%
  select(GEOID, STATEFP, geometry) %>%
  inner_join(Current_effect, by = "GEOID")

states_shp <- tigris::states(year = 2022)

tract_data <- states_shp %>%
  as.data.frame() %>%
  select(REGION, DIVISION, STATEFP, NAME) %>%
  inner_join(tract_data, by = "STATEFP")

# State_impact <- tract_data %>%
#   group_by(NAME) %>%
#   summarize(
#     `Number of Tracts`             = n(),
#     `Average Effect on Tracts`     = mean(`Effect on Total Addresses`, na.rm = TRUE),
#     `Effect on Total Addresses`    = sum(`Effect on Total Addresses`, na.rm = TRUE),
#     .groups = "drop"
#   ) %>%
#   arrange(`Effect on Total Addresses`)
geo_states <- USPS_data %>%
  select(state, id) %>%
  mutate(id = as.character(id)) %>%
  distinct()
  
  
State_impact <- Current_effect %>%
  left_join(geo_states) %>%
  group_by(state) %>%
  summarize(
    `Number of Tracts`             = n(),
    `Average Effect on Tracts`     = mean(`Effect on Total Addresses`, na.rm = TRUE),
    `Effect on Total Addresses`    = sum(`Effect on Total Addresses`, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(`Effect on Total Addresses`)

#########################################
### 11. Summary effect figures (Plotly) ###
#########################################

plot_effect_by_decile <- function(data, group_var, title = "Effect by Decile") {
  group_sym <- rlang::sym(group_var)
  
  summarized_data <- data %>%
    group_by(!!group_sym) %>%
    summarize(
      `Average Effect on Total Addresses` = mean(`Effect on Total Addresses`, na.rm = TRUE),
      n = n(),
      .groups = "drop"
    )
  
  plot_ly(
    summarized_data,
    x = ~get(group_var),
    y = ~`Average Effect on Total Addresses`,
    type = 'scatter',
    mode = 'lines+markers',
    marker = list(
      size = ~sqrt(n) * 2,
      color = '#1f77b4',
      line = list(width = 1, color = 'black')
    ),
    text = ~paste0(
      group_var, ": ", get(group_var), "<br>",
      "Avg Effect: ", round(`Average Effect on Total Addresses`, 1), "<br>",
      "n: ", n, "<br>",
      "Total New Addresses (approx): ", round(`Average Effect on Total Addresses` * n, 0)
    ),
    hoverinfo = "text"
  ) %>%
    layout(
      title  = list(text = title, x = 0.05),
      xaxis  = list(title = group_var),
      yaxis  = list(title = "Average Effect on Total Addresses"),
      shapes = list(list(
        type = "line",
        x0   = min(summarized_data[[group_var]], na.rm = TRUE),
        x1   = max(summarized_data[[group_var]], na.rm = TRUE),
        y0   = 0, y1 = 0,
        line = list(dash = "dash", color = "grey")
      )),
      margin        = list(l = 60, r = 30, b = 50, t = 60),
      plot_bgcolor  = "#f9f9f9",
      paper_bgcolor = "#f9f9f9"
    )
}

plot_effect_by_decile(Current_effect, "current poverty rate decile",  title = "Average Effect by Poverty Rate Decile")
plot_effect_by_decile(Current_effect, "current median income decile", title = "Average Effect by Median Income Decile")
plot_effect_by_decile(Current_effect, "current solo detached decile", title = "Average Effect by Solo Detached Decile")
plot_effect_by_decile(Current_effect, "current zoning index decile",  title = "Average Effect by Zoning Decile")
plot_effect_by_decile(Current_effect, "growth_decile",                title = "Average Effect by (2015→2017) Growth Decile")

#########################################
### 12. Interpretation notes          ###
#########################################
# • We remove *pre-existing state-level linear trends* using pre-period data only,
#   slope-only (centered) so levels remain intact. This mitigates bias from drifting
#   state trajectories without saturating the model with state×time FE.
# • Donor sampling still matches on baseline propensity; optionally upweights
#   same-state donors to better align residual state shocks.
# • fect(method="mc") runs on the detrended outcome; dynamic paths then reflect
#   deviations from those pre trends.

