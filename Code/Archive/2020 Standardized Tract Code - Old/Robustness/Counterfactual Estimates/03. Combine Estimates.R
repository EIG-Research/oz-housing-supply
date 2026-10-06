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

# Get list of treated unit IDs
treated_units <- USPS_data %>%
  filter(time == max(time)) %>%
  filter(Designation_category == "LIC selected") %>%
  distinct(id)

setwd(path_output)
all_results <- list()

load("effect_estimates_bin_1.RData")
all_results[[1]] <- estimate_list

load("effect_estimates_bin_2.RData")
all_results[[2]] <- estimate_list

load("effect_estimates_bin_3.RData")
all_results[[3]] <- estimate_list

load("effect_estimates_bin_4.RData")
all_results[[4]] <- estimate_list

load("effect_estimates_bin_5.RData")
all_results[[5]] <- estimate_list

All_estimates <- bind_rows(all_results)

##########################
### Combine and Export ###
##########################

# Save the effect estimates and estimation outputs as needed
setwd(path_output)
save(All_estimates, file = "MC_FECT_Effect_Estimates_and_SE_ALL.RData")
# load(file = "MC_FECT_Effect_Estimates_and_SE_ALL.RData")


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
  select(id,Total_residential, poverty_rate:`current solo detached decile`, Designation_category) %>%
  mutate(id = as.character(id))

All_estimates <- All_estimates %>%
  left_join(Heterogeneity)

Current_effect <- All_estimates %>%
  mutate(Period_num = stringr::str_sub(Period ,-2, -1)) %>% 
  filter(Period_num == max(Period_num))

# Current_effect <- Current_effect %>%
#   left_join(ai_zoning) %>%
#   mutate(`zoning decile` = ntile(Overall_Index, 10))

save(Current_effect, file = "MC_FECT_Effect_Estimates__and_NO_SE_ALL_MAPPING_DATA.RData")
# save(Current_effect, file = "CFE_FECT_Effect_Estimates__and_NO_SE_ALL_MAPPING_DATA.RData")


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
  # filter(Significant == 1) %>%
  group_by(NAME) %>%
  summarize(`Effect on Total Addresses` = sum(`Effect on Total Addresses`, na.rm = TRUE)) %>%
  arrange(`Effect on Total Addresses`)

#####################################
### Summary Effect Figures        ###
#####################################

plot_effect_by_decile <- function(data, group_var, title = "Effect by Decile") {
  # Convert group_var to a symbol for tidy evaluation
  group_sym <- rlang::sym(group_var)
  
  # Summarize data
  summarized_data <- data %>%
    filter(Treated == 1) %>% 
    # filter(Significant == 1) %>%
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
# plot_effect_by_decile(Current_effect, "zoning decile", title = "Average Effect by Zoning Decile")


