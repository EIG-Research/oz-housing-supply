# HUD Aggregated USPS Administrative Data on Vacancies
# Ben Glasner 1/15/2025
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

##################
### Data clean ###
##################

### Clean up uncertain tracts from the crosswalk
USPS_data <- USPS_data %>%
  filter(Designation_category_detailed %in% c("LIC selected",
                                              "LIC not selected, not a border tract",
                                              "Ineligible, not a border tract")) %>%
  filter(YEAR >=2015) %>%
  mutate(
    id = as.numeric(geoid), 
    time = dense_rank(date),
    Designation = if_else(`OZ Designation` == 1 & date >= "2018-03-01", 1, 0),
    Total_residential = ACTIVE_RESIDENTIAL_ADDRESSES + STV_RESIDENTIAL_ADDRESSES + LTV_RESIDENTIAL_ADDRESSES, 
  ) %>%
  select(id, geoid, time, YEAR, date,
         `Type tract`, Designation,LIC_oz_neighbor,
         poverty_rate, median_income, solo_detached_housing_share,Zoning_Index,
         Total_residential)

# Ensure panel is balanced after removing missing data periods
USPS_data <- USPS_data %>%
  distinct(id, time, .keep_all = TRUE) %>%
  group_by(id) %>%
  mutate(number_of_quarters_obs = n()) %>%
  ungroup()

max_quarter <- max(USPS_data$number_of_quarters_obs)
USPS_data <- USPS_data %>% filter(number_of_quarters_obs == max_quarter)

USPS_data <- USPS_data %>%
filter(YEAR %in% c(2015,2016,2017) |  date %in% c(
  "2018-03-01"
  ,"2019-03-01"
  ,"2020-03-01"
  ,"2021-03-01"
  ,"2022-03-01"
  ,"2023-03-01"
  ,"2024-03-01"
  ,"2025-03-01"
  ) 
  ) %>%
  group_by(YEAR) %>%
  mutate(
         `current median income decile` = ntile(median_income, 10),
         `current poverty rate decile` = ntile(poverty_rate, 10),
         `current solo detached decile` = ntile(solo_detached_housing_share, 10),
         `current zoning index decile` = ntile(Zoning_Index, 10)
         ) %>%
  ungroup()

###############################
### Counterfactual Analysis Function ###
###############################

# Setup formula
outcome_var   <- "Total_residential"
treatment_var <- "Designation"
# Uncomment the next line if you wish to include controls
conditional      <- c("`current median income decile`", "`current poverty rate decile`","`current solo detached decile`","`current zoning index decile`")
conditional_allof      <- c("current median income decile", "current poverty rate decile","current solo detached decile","current zoning index decile")
controls      <- c("poverty_rate","median_income","solo_detached_housing_share")
controls_allof      <- c("poverty_rate","median_income","solo_detached_housing_share")

control_vars  <- paste(controls, collapse = " + ")
# current_formula <- as.formula(paste(outcome_var, "~", treatment_var))
current_formula <- as.formula(paste(outcome_var, "~", treatment_var, "+", control_vars))

# Helper function to run fect, save, print, and plot results
run_and_plot <- function(method, selected_data) {
  # Set common fect parameters
  args <- list(
    formula = current_formula
    , data = selected_data
    , na.rm = TRUE
    , index  = c("id", "time")
    , force = "two-way"
    , r = c(0, 5)
    , CV = TRUE
    , criterion = "gmspe" #to alleviate the impact of some outlier prediction errors, we allow the criterion of geometric-mean squared prediction errors
    , method = method
    , se = TRUE
    , vartype = "jackknife"
    , nboots = 100
    , alpha = 0.05
    , parallel = TRUE
    , cores = 12
    , seed = 42
    , min.T0 = 5
    , normalize = TRUE
    # nlambda = 5,
    # k = 10,
    # cv.prop = 0.05,
    # cv.treat = FALSE,
    # cv.nobs = 3,
    # cv.donut = 0,
    # se = FALSE,
    # vartype = "bootstrap",
    # quantile.CI = FALSE,
    # max.iteration = 1000,
    # max.missing = 0,
    # proportion = 0.3,
    # f.threshold = 0.5,
    # degree = 2,
    # sfe = c("CBSA_TITLE"),
    # cfe = list(c("type_tract", "time"), c("CBSA_TITLE", "time")),
    # fill.missing = FALSE,
    # placeboTest = FALSE,
    # carryoverTest = FALSE,
    # loo = FALSE,
    # permute = FALSE,
    # m = 2,
  )
  
  out <- do.call(fect, args)
  invisible(out)
}

#####################################
### Create Treated & Control Sets ###
#####################################


USPS_data <- USPS_data %>%
  select(all_of(outcome_var), 
         all_of(treatment_var), 
         all_of(controls_allof),
         all_of(conditional_allof),
         "id", "date","YEAR", "time", "Designation","LIC_oz_neighbor") %>% 
  na.omit() 

balanced_ids <- USPS_data %>%
  group_by(id) %>%
  summarise(n_time = n_distinct(time)) %>%
  filter(n_time == n_distinct(USPS_data$time)) %>%
  pull(id)

USPS_data <- USPS_data %>%
  filter(id %in% balanced_ids)

# Get list of treated unit IDs
treated_units <- USPS_data %>%
  filter(time == max(time)) %>%
  filter(Designation == 1) %>%
  distinct(id)

all_units <- USPS_data %>%
  filter(time == max(time)) %>%
  distinct(id)

# # Get full set of control unit IDs (all "LIC not selected")
control_ids <- USPS_data %>%
  filter(Designation == 0) %>%
  distinct(id) %>%
  pull(id)

# Initialize list to store effect estimates
estimate_list <- list()
model_output <- list()

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

  # Get the decile values for the current treated unit
  base_data <- USPS_data %>%
    filter(id == current_treated_id, date == "2017-12-01")

  housing_date_decile <- base_data$`current solo detached decile`
  poverty_date_decile <- base_data$`current poverty rate decile`
  income_date_decile <- base_data$`current median income decile`
  zoning_date_decile <- base_data$`current zoning index decile`

  # Step 1: Expand boundaries until at least 1000 unique control ids are found
  bound <- 1
  repeat {
    control_id_options <- USPS_data %>%
      filter(id %in% control_ids) %>%
      filter(date == "2017-12-01",
             Designation != "LIC selected",
             abs(`current solo detached decile` - housing_date_decile) <= bound,
             abs(`current poverty rate decile` - poverty_date_decile) <= bound,
             abs(`current median income decile` - income_date_decile) <= bound,
             abs(`current zoning index decile` - zoning_date_decile) <= bound,
             ) %>%
      distinct(id) %>%
      pull(id)

    if (length(unique(control_id_options)) >= 1000 || bound > 5) break
    bound <- bound + 1
  }

  if (length(control_id_options) < 100) next  # Skip if still too few controls

  control_id_options <- sample(control_id_options, size = min(500, length(control_id_options)))

  # Step 2: Check if control variables are time-invariant
  temp_data <- USPS_data %>%
    filter(id == current_treated_id | id %in% control_id_options)


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
             )) %>%
      filter(id == current_treated_id)

    estimate_list[[i]] <- effect_long


  pb$tick()  # update progress bar
  rm(temp_data)
}

# Combine all effect estimates into one data frame
All_estimates <- bind_rows(estimate_list)

###########################################
### (Optional) Further Analysis & Export ###
###########################################

# Save the effect estimates and estimation outputs as needed
setwd(path_output)
# save(All_estimates, file = "MC_FECT_Effect_Estimates_and_SE_TREATED.RData")


load(file = "MC_FECT_Effect_Estimates_and_SE_TREATED.RData")


# Define treated tracts again:
All_estimates <- All_estimates  %>%
  mutate(Treated = if_else(id %in% treated_units$id, 1, 0 ))

# You can now proceed with plotting or additional analysis on All_estimates
# For example, plotting the average effect for a given period:

# max_period <- USPS_data %>%
#   filter(time == max(time)) %>%
#   select(Period) %>%
#   distinct() %>%
#   pull(Period)

current_estimate <- All_estimates %>%
  filter(Treated== 1) %>% 
  mutate(Period_num = stringr::str_sub(Period ,-2, -1)) %>% 
  filter(Period_num == max(Period_num)) %>%
  summarize(Average_Effect = mean(`Effect on Total Addresses`, na.rm = TRUE)) %>%
  pull(Average_Effect)

print(paste("Average Effect on Total Addresses:", round(current_estimate, 4)))


All_estimates %>%
  filter(Treated== 1) %>% 
  mutate(Period_num = stringr::str_sub(Period ,-2, -1)) %>% 
  # filter(Period_num == max(Period_num)) %>%
  group_by(Period_num) %>%
  summarize(Average_Effect = mean(`Effect on Total Addresses`, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(aes(x = as.numeric(Period_num), 
             y = Average_Effect)) + 
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 12.5) +
  geom_line() + 
  geom_point() + 
  theme_bw()

Heterogeneity <- USPS_data %>%
  filter(date == "2017-12-01") %>%
  select(id,Total_residential, poverty_rate:`current zoning index decile`, Designation) %>%
  mutate(id = as.character(id))

# Calculate 99th percentile
pctile_99 <- USPS_data %>% 
  filter(date %in% c("2015-03-01","2017-12-01")) %>%
  group_by(id) %>%
  mutate(change = Total_residential - lag(Total_residential),
         percent_change = change / lag(Total_residential)) %>%
  ungroup() %>%
  pull(percent_change) %>%
  quantile(probs = 0.99, na.rm = TRUE)

# Apply winsorization
growth <- USPS_data %>% 
  filter(date %in% c("2015-03-01","2017-12-01")) %>%
  group_by(id) %>%
  mutate(change = Total_residential - lag(Total_residential),
         percent_change = change / lag(Total_residential)) %>%
  ungroup() %>%
  filter(date %in% c("2017-12-01")) %>%
  mutate(
    percent_change = pmin(percent_change, pctile_99),
    growth_decile = ntile(percent_change, n = 10)
  ) %>%
  na.omit() %>%
  select(id, percent_change, growth_decile) %>%
  mutate(id = as.character(id))

All_estimates <- All_estimates %>%
  left_join(Heterogeneity) %>%
  left_join(growth)

Current_effect <- All_estimates %>%
  mutate(Period_num = stringr::str_sub(Period ,-2, -1)) %>% 
  filter(Period_num == max(Period_num))

# Current_effect <- Current_effect %>%
#   left_join(ai_zoning) %>%
#   mutate(`zoning decile` = ntile(Overall_Index, 10))

# save(Current_effect, file = "MC_FECT_Effect_Estimates__and_SE__TREATED_MAPPING_DATA.RData")


#####################################
### State Summary                 ###
#####################################

# Convert numeric GEOID to character with leading zeros
Current_effect <- Current_effect %>%
  mutate(GEOID = sprintf("%011.0f", as.numeric(id)))  # Adjust column name if needed

# Download all tracts (you can subset to specific states to speed this up)
tracts_all <- rbind_tigris(lapply(state.abb, function(state) {
  tryCatch({
    tracts(state, year = 2020)
  }, error = function(e) NULL)
}) %>% compact())

# Join geometries
tract_data <- tracts_all %>%
  select(GEOID,STATEFP, geometry) %>%
  inner_join(Current_effect, by = "GEOID")

states <- tigris::states(year = 2022) 

tract_data <- states %>%
  as.data.frame() %>%
   select(REGION, DIVISION,STATEFP,NAME) %>%
  inner_join(tract_data)


State_impact <- tract_data   %>% 
  filter(Treated == 1) %>% 
  filter(Significant == 1) %>%
  group_by(NAME) %>%
  summarize(`Number of Tracts` = n(),
            `Average Effect on Tracts` = mean(`Effect on Total Addresses`, na.rm = TRUE),
            `Effect on Total Addresses` = sum(`Effect on Total Addresses`, na.rm = TRUE)) %>%
  arrange(`Effect on Total Addresses`)

write.xlsx(State_impact, file = "MC_Effect_Estimate_by_State.xlsx")

#####################################
### Summary Effect Figures        ###
#####################################

plot_effect_by_decile <- function(data, group_var, title = "Effect by Decile") {
  # Convert group_var to a symbol for tidy evaluation
  group_sym <- rlang::sym(group_var)
  
  # Summarize data
  summarized_data <- data %>%
    filter(Treated == 1) %>% 
    filter(Significant == 1) %>%
    group_by(!!group_sym) %>%
    summarize(
      `Average Effect on Total Addresses` = mean(`Effect on Total Addresses`, na.rm = TRUE),
      n = n(),
      .groups = "drop"
    )
  
  # Create the plot
  plot_ly(
    summarized_data,
    x = ~get(group_var),
    y = ~`Average Effect on Total Addresses`,
    type = 'scatter',
    mode = 'lines+markers',
    marker = list(
      # size = ~sqrt(abs(`Average Effect on Total Addresses`*n)), # Size scaled for visibility
      size = ~sqrt(n) * 2, # Size scaled for visibility
      color = '#1f77b4',
      line = list(width = 1, color = 'black')
    ),
    text = ~paste0(
      group_var, ": ", get(group_var), "<br>",
      "Avg Effect: ", round(`Average Effect on Total Addresses`, 1), "<br>",
      "n: ", n, "<br>",
      "Total New Addresses:", round(`Average Effect on Total Addresses`*n, 0)
    ),
    hoverinfo = "text"
  ) %>%
    layout(
      title = list(text = title, x = 0.05),
      xaxis = list(title = group_var),
      yaxis = list(title = "Average Effect on Total Addresses"),
      shapes = list(list(type = "line", x0 = min(summarized_data[[group_var]]), x1 = max(summarized_data[[group_var]]),
                         y0 = 0, y1 = 0, line = list(dash = "dash", color = "grey"))),
      margin = list(l = 60, r = 30, b = 50, t = 60),
      plot_bgcolor = "#f9f9f9",
      paper_bgcolor = "#f9f9f9"
    ) 
}

plot_effect_by_decile(Current_effect, "current poverty rate decile", title = "Average Effect by Poverty Rate Decile")
plot_effect_by_decile(Current_effect, "current median income decile", title = "Average Effect by Median Income Decile")
plot_effect_by_decile(Current_effect, "current solo detached decile", title = "Average Effect by Solo Detached Decile")
plot_effect_by_decile(Current_effect, "current zoning index decile", title = "Average Effect by Zoning Decile")
plot_effect_by_decile(Current_effect, "growth_decile", title = "Average Effect by Growth Decile")

Current_effect %>% 
  filter(Treated == 1) %>% 
  filter(Significant == 1) %>%
  group_by(growth_decile, `current poverty rate decile`) %>%
  summarise(total = sum(`Effect on Total Addresses`, na.rm = TRUE)) %>%
  pivot_wider(names_from = `current poverty rate decile`, values_from = total) %>%
  filter(!is.na(growth_decile)) %>%
  select(`growth_decile`, `1`,`2`,`3`,`4`,`5`,`6`,`7`,`8`,`9`,`10`) %>% 
  rename(
    `Growth Decile` = `growth_decile`,
    `Poverty Decile 1` = `1`,
    `Poverty Decile 2` = `2`,
    `Poverty Decile 3` = `3`,
    `Poverty Decile 4` = `4`,
    `Poverty Decile 5` = `5`,
    `Poverty Decile 6` = `6`,
    `Poverty Decile 7` = `7`,
    `Poverty Decile 8` = `8`,
    `Poverty Decile 9` = `9`,
    `Poverty Decile 10` = `10`
  ) %>%
  write.xlsx(.,file = "Growth decile and poverty decile.xlsx")


Current_effect %>% 
  filter(Treated == 1) %>% 
  filter(Significant == 1) %>%
  group_by(growth_decile, `current median income decile`) %>%
  summarise(total = sum(`Effect on Total Addresses`, na.rm = TRUE)) %>%
  pivot_wider(names_from = `current median income decile`, values_from = total) %>%
  filter(!is.na(growth_decile)) %>%
  select(`growth_decile`, `1`,`2`,`3`,`4`,`5`,`6`,`7`,`8`,`9`) %>% 
  rename(
    `Growth Decile` = `growth_decile`,
    `Median Income Decile 1` = `1`,
    `Median Income Decile 2` = `2`,
    `Median Income Decile 3` = `3`,
    `Median Income Decile 4` = `4`,
    `Median Income Decile 5` = `5`,
    `Median Income Decile 6` = `6`,
    `Median Income Decile 7` = `7`,
    `Median Income Decile 8` = `8`,
    `Median Income Decile 9` = `9`
  ) %>%
  write.xlsx(.,file = "Growth decile and median income decile.xlsx")


######################################################
### What is linked to the effect estimate?         ###
######################################################

relationships <- Current_effect %>%
  filter(Treated == 1) %>% 
  filter(Significant == 1) %>%
  lm(formula = `Effect on Total Addresses` ~ `current poverty rate decile` + `current median income decile` + `current solo detached decile` + `current zoning index decile` + growth_decile)
summary(relationships)
