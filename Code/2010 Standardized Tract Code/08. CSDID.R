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

library(purrr)
library(stringr)

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
path_data_USPS <- file.path(path_project, "data/2020 Standardized")
path_data_tract <- file.path(path_data, "Tract Characteristics")

path_output <- file.path(path_project, "Output")

#################
### Data load ###
#################
setwd(path_data)
load(file = "USPS_tract_vacancy.RData")


USPS_data <- USPS_data %>%
  filter(Designation_category_detailed %in% c("LIC selected","LIC not selected, not a border tract","Ineligible, not a border tract")) %>%
  filter(YEAR >=2015) 

#################
### What are the trends in vacancy (counts and share) across designated and undesignated but eligible tracts? 
#################

USPS_data <- USPS_data %>% 
  mutate( period = dense_rank(date))

period_value <- min(USPS_data$period[USPS_data$date == "2018-03-01"])

# Continue with other data transformations as needed
USPS_data <- USPS_data %>%
  mutate(
    G = if_else(`OZ Designation` == 1, period_value, 0),
    geoid_num = as.numeric(geoid),
    Total_active_vacant_exclude_nostat_RESIDENTIAL = 
      ACTIVE_RESIDENTIAL_ADDRESSES + STV_RESIDENTIAL_ADDRESSES + LTV_RESIDENTIAL_ADDRESSES,
    log_level = log(Total_active_vacant_exclude_nostat_RESIDENTIAL)
  ) %>%
  filter(!is.na(Total_active_vacant_exclude_nostat_RESIDENTIAL)) %>%
  mutate(log_level = if_else(is.finite(log_level), log_level,NA))

# Calculate the annual rate of growth for total active and vacant residential

panel_USPS <- plm::pdata.frame(USPS_data, index = c("geoid_num","period"), drop.index = FALSE)

panel_USPS$change <- panel_USPS$Total_active_vacant_exclude_nostat_RESIDENTIAL - plm::lag(panel_USPS$Total_active_vacant_exclude_nostat_RESIDENTIAL, k = 4)
panel_USPS$growth_rate <- panel_USPS$change/plm::lag(panel_USPS$Total_active_vacant_exclude_nostat_RESIDENTIAL, k = 4)

# 1. Compute the 1st and 99th percentile cut-offs
percentiles <- panel_USPS %>%
  summarise(
    lower = quantile(growth_rate, probs = 0.01, na.rm = TRUE),
    upper = quantile(growth_rate, probs = 0.99, na.rm = TRUE)
  )

p1  <- as.numeric(percentiles$lower)
p99 <- as.numeric(percentiles$upper)

# 2. Windsorize (create a new variable; or overwrite growth_rate if you prefer)
panel_USPS <- panel_USPS %>%
  mutate(
    growth_rate_winz = case_when(
      as.numeric(growth_rate) <  as.numeric(p1)  ~ p1,
      as.numeric(growth_rate) >  as.numeric(p99) ~ p99,
      TRUE               ~ as.numeric(growth_rate)
    )
  )

panel_USPS <- panel_USPS %>%
  as.data.frame() %>%
  select("geoid","period","change","growth_rate","growth_rate_winz") %>%
  mutate(geoid = as.numeric(geoid),
         period = as.numeric(period))

USPS_data <- USPS_data %>%
  left_join(panel_USPS) %>%
  mutate(growth_rate = if_else(growth_rate == Inf, NA, growth_rate)) %>%
  filter(!is.na(growth_rate))

# # Count the number of quarters covered by the data set for each tract
# USPS_data <- USPS_data %>%
#   filter(!is.na(growth_rate)) %>%
#   distinct(geoid, date, .keep_all = TRUE) %>%
#   group_by(geoid) %>%
#   mutate(number_of_quarters = n()) %>%
#   ungroup()
# 
# max_quarters <- max(USPS_data$number_of_quarters)
# 
# # Filter out tracts with missing quarters
# USPS_data <- USPS_data %>%
#   filter(number_of_quarters == max_quarters) 

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
  , "growth_rate_winz"
  , "log_level"
)

titles <- c(
  "Effect on Active and Vacant Residential"
  , "Effect on Address Growth Rate"
  , "Effect on log(Active and Vacant Residential)"
)

table_titles <- c(
  "Active and Vacant Residential"
  , "Address Growth Rate"
  , "log(Active and Vacant Residential)"
)

controls      <- c("poverty_rate","median_income","solo_detached_housing_share","Zoning_Index")
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

# Create the DID model and plot the pre-post treatment comparison
for (j in seq_along(geo_groupings)){
  print(geo_groupings[[j]])
  
  for(i in seq_along(outcome_vars[[1]])){
    print(outcome_vars[[1]][[i]])
    
    # filter job and employment data by year

    analysis_data <- data_list[[j]] %>%
        select("geoid_num","period","date","G", outcome_vars[[1]][[i]],
               all_of(controls)) %>%
        na.omit()
    
    
    # create linear model for the treated group
    csdid_treated[[i]] <- att_gt(
      yname = outcome_vars[[1]][[i]],
      tname = "period",
      idname = "geoid_num",
      gname = "G",
      xformla = current_formula,
      # est_method = "ipw",
      biters = 1000,
      pl = TRUE,
      data = analysis_data, 
      base_period = "universal"
    )
    ggdid(csdid_treated[[i]])
    # compute the overall effect by averaging the effect of the treatment across
    # all positive lengths of exposure
    dynamic[[i]] <- aggte(csdid_treated[[i]], 
                          # alp = 0.01,
                          # alp = 0.02,
                          alp = 0.05,
                          type = "dynamic",
                          na.rm = TRUE) 
    
    using <- data.frame(cbind(dynamic[[i]][["egt"]],
                              dynamic[[i]][["att.egt"]],
                              dynamic[[i]][["se.egt"]])) 
    names(using) <- c("Period", "ATT", "SE")
    
    using_dates <- dates_df %>% filter(period %in% c(using$Period))
    
    # Export data for treatment and experiment lines to graph
    plot_data[[i]] <- using %>% 
      mutate(
        Row = row_number(),
        date = using_dates$date,
        Lower = ATT - dynamic[[i]]$crit.val.egt*SE,
        Upper = ATT + dynamic[[i]]$crit.val.egt*SE,
        `Pre/Post` = case_when(
          Period<0 ~ "Pre-Treatment",
          Period>=0 ~ "Post-Treatment",
          TRUE ~ NA
        ),
        `Pre/Post` = factor(`Pre/Post`, levels = c("Pre-Treatment", "Post-Treatment"))
      )
    
    
    # Create a data frame for the vline information
    vline_data <- data.frame(
      date = as.Date(c("2017-12-20", "2018-06-14", "2019-12-19")),
      label_date = as.Date(c("2017-09-20", "2018-09-14", "2020-03-19")),
      label = c("OZs Enacted", "OZ Map Certified", "Regulations Finalized"),  # Labels for the legend
      linetype = c("Dashed Line 1", "Dashed Line 2", "Solid Line"),
      # color = c("#254f85", "#79c5fd", "#008080") # Colors for the lines
      color = c("black", "black", "black") # Colors for the lines
    )
    
    plots[[i]] <- plot_data[[i]] %>%
      ggplot(aes(x = date,
                 color = `Pre/Post`)) +   
      ###################################################
      # Add geom_vline with legend information
      # geom_vline(data = vline_data, aes(xintercept = date, color = label, linetype = linetype),
      #            size = 1, alpha = 0.3) +
      ###################################################
      
      # Add text labels and arrows
      geom_segment(data = vline_data, 
                   aes(x = date, xend = date, 
                       y = min(plot_data[[i]]$Lower, na.rm = TRUE), 
                       yend = max(plot_data[[i]]$Upper, na.rm = TRUE)),
                   # arrow = arrow(length = unit(0.02, "npc")), 
                   color = vline_data$color) +
      geom_text(data = vline_data, 
                aes(x = label_date, 
                    y = max(plot_data[[i]]$Upper, na.rm = TRUE) * 0.65, 
                    label = label),
                angle = 90, hjust = -0.1, vjust = 0.5, 
                color = vline_data$color, size = 3) +
      
      # Horizontal line at zero
      geom_hline(yintercept = 0, color = "grey20") +
      geom_vline(xintercept = as.Date("2012-01-01"), color = "grey20") +
      
      # Data points and error bars
      geom_point(aes(y = ATT)) + 
      geom_errorbar(aes(ymin = Lower, ymax = Upper)) + 
      
      # Theme and axis labels
      theme_minimal() +
      ylab("Average Treatment on the Treated") + 
      xlab("Date") + 
      
      # Custom colors for Pre/Post Treatment
      scale_color_manual(
        values = c("Pre-Treatment" = "grey60", "Post-Treatment" = "grey10"), 
        guide = guide_legend(title = "Treatment Period: ")
      ) +
      
      # Remove linetype legend
      guides(linetype = "none") +
      
      # Adjust legend position
      theme(legend.position = "bottom",
            panel.grid = element_blank())
    
  }
  
  dynamic_results_list[[j]] <- dynamic
  plot_data_list[[j]] <- plot_data
  plot_list[[j]] <- plots
  
}
#############################################
### Create a table for the average effect ###
#############################################

# Initialize the results_export_list with all combinations
results_export_list <- expand.grid(
  geo_grouping = geo_groupings,
  outcome_var = outcome_vars[[1]],
  stringsAsFactors = FALSE
) %>% 
  arrange(geo_grouping, outcome_var)

# Initialize vectors to store formatted coefficients and SEs
formatted_coef <- character(nrow(results_export_list))
se_values <- numeric(nrow(results_export_list))

# Loop through each row to populate the coefficients and SEs
for (k in seq_len(nrow(results_export_list))) {
  # Extract current geo_grouping and outcome_var
  current_geo <- results_export_list$geo_grouping[k]
  current_outcome <- results_export_list$outcome_var[k]
  
  # Find the indices for geo_grouping and outcome_var
  geo_index <- which(geo_groupings == current_geo)
  outcome_index <- which(outcome_vars[[1]] == current_outcome)
  
  # Extract ATT, SE, and critical value
  att <- dynamic_results_list[[geo_index]][[outcome_index]]$overall.att
  crit_val <- dynamic_results_list[[geo_index]][[outcome_index]]$crit.val.egt[1]
  
  # Format the coefficient with an asterisk if significant
  if(!current_outcome %in% c( "log_total_active_vacant_exclude_nostat"
                              , "growth_rate_winz"
                              ,"log_bus_address"
                              ,"log_other_address"
                              ,"log_res_address")){
    se <- round(dynamic_results_list[[geo_index]][[outcome_index]]$overall.se,3)
    
    formatted_coef[k] <- paste0(
      round(att, 3), " ",
      if_else((dynamic_results_list[[geo_index]][[outcome_index]]$overall.att / dynamic_results_list[[geo_index]][[outcome_index]]$overall.se) > crit_val, "*", ""))
  }else{
    se <- round(dynamic_results_list[[geo_index]][[outcome_index]]$overall.se*100,4)
    
    
    formatted_coef[k] <- paste0(
      round(att*100, 4), " ",
      if_else((dynamic_results_list[[geo_index]][[outcome_index]]$overall.att / dynamic_results_list[[geo_index]][[outcome_index]]$overall.se) > crit_val, "*", ""))
  }
  
  
  # Store the standard error
  se_values[k] <- se
}

# Add the formatted coefficients and SEs to the results_export_list
results_export_list <- results_export_list %>%
  mutate(
    Coef = formatted_coef,
    SE = se_values
  )

# Pivot coefficients to wide format
coef_table <- results_export_list %>%
  select(geo_grouping, outcome_var, Coef) %>%
  pivot_wider(names_from = geo_grouping, values_from = Coef, names_prefix = "Coef_")

# Pivot SEs to wide format
se_table <- results_export_list %>%
  select(geo_grouping, outcome_var, SE) %>%
  pivot_wider(names_from = geo_grouping, values_from = SE, names_prefix = "SE_")

# Combine the coefficients and SE tables
final_table <- coef_table %>%
  left_join(se_table, by = "outcome_var") %>%
  arrange(outcome_var) %>%
  mutate(
    `All` = paste0(`Coef_All`," (",`SE_All`,")"),
    `Large urban` = paste0(`Coef_Large urban`," (",`SE_Large urban`,")"),
    `Mid-sized urban` = paste0(`Coef_Mid-sized urban`," (",`SE_Mid-sized urban`,")"),
    `Small urban` = paste0(`Coef_Small urban`," (",`SE_Small urban`,")"),
    `Suburban` = paste0(`Coef_Suburban`," (",`SE_Suburban`,")"),
    `Small town` = paste0(`Coef_Small town`," (",`SE_Small town`,")"),
    `Rural` = paste0(`Coef_Rural`," (",`SE_Rural`,")"),
  ) %>%
  left_join(titles_df) %>%
  select(
    titles,
    `All`,`Large urban`,`Mid-sized urban`,`Small urban`,`Suburban`,`Small town`,`Rural`
  )

# Pivot table to horizontal for datawrapper display
reshaped_table_averages <- final_table %>%
  pivot_longer(
    cols = c(`All`, `Large urban`, `Mid-sized urban`, `Small urban`, `Suburban`, `Small town`, `Rural`),
    names_to = "geo_grouping",
    values_to = "Coef_SE"
  ) %>%
  pivot_wider(
    names_from = titles,
    values_from = Coef_SE
  ) %>%
  arrange(match(geo_grouping, c("All", "Large urban", "Mid-sized urban", "Small urban", "Suburban", "Small town", "Rural"))) %>%
  select(geo_grouping,all_of(table_titles)) %>%
  rename(`Tract Geography` = geo_grouping)

setwd(path_output)
# write.xlsx(reshaped_table_averages, file = "CSDID Effect Estimate.xlsx", overwrite = TRUE)

##############################################################
### Create a table for the final period in the data effect ###
##############################################################

# Initialize the results_export_list with all combinations
results_export_list <- expand.grid(
  geo_grouping = geo_groupings,
  outcome_var = outcome_vars[[1]],
  stringsAsFactors = FALSE
) %>% 
  arrange(geo_grouping, outcome_var)

# Initialize vectors to store formatted coefficients and SEs
formatted_coef <- character(nrow(results_export_list))
se_values <- numeric(nrow(results_export_list))

# Loop through each row to populate the coefficients and SEs
for (k in seq_len(nrow(results_export_list))) {
  # Extract current geo_grouping and outcome_var
  current_geo <- results_export_list$geo_grouping[k]
  current_outcome <- results_export_list$outcome_var[k]
  
  # Find the indices for geo_grouping and outcome_var
  geo_index <- which(geo_groupings == current_geo)
  outcome_index <- which(outcome_vars[[1]] == current_outcome)
  
  # Number of periods estimated 
  time_period_count <- length(dynamic_results_list[[geo_index]][[outcome_index]]$att.egt)
  # Extract ATT, SE, and critical value
  att <- dynamic_results_list[[geo_index]][[outcome_index]]$att.egt[time_period_count]
  crit_val <- dynamic_results_list[[geo_index]][[outcome_index]]$crit.val.egt[1]
  
  # Format the coefficient with an asterisk if significant
  if(!current_outcome %in% c( "log_total_active_vacant_exclude_nostat"
                              , "growth_rate_winz"
                              ,"log_bus_address"
                              ,"log_other_address"
                              ,"log_res_address")){
    se <- round(dynamic_results_list[[geo_index]][[outcome_index]]$se.egt[time_period_count],3)
    
    formatted_coef[k] <- paste0(
      round(att, 3), " ",
      if_else((dynamic_results_list[[geo_index]][[outcome_index]]$att.egt[time_period_count] / dynamic_results_list[[geo_index]][[outcome_index]]$se.egt[time_period_count]) > crit_val, "*", ""))
  }else{
    se <- round(dynamic_results_list[[geo_index]][[outcome_index]]$se.egt[time_period_count]*100,4)
    
    
    formatted_coef[k] <- paste0(
      round(att*100, 4), " ",
      if_else((dynamic_results_list[[geo_index]][[outcome_index]]$att.egt[time_period_count] / dynamic_results_list[[geo_index]][[outcome_index]]$se.egt[time_period_count]) > crit_val, "*", ""))
  }
  
  
  # Store the standard error
  se_values[k] <- se
}

# Add the formatted coefficients and SEs to the results_export_list
results_export_list <- results_export_list %>%
  mutate(
    Coef = formatted_coef,
    SE = se_values
  )

# Pivot coefficients to wide format
coef_table <- results_export_list %>%
  select(geo_grouping, outcome_var, Coef) %>%
  pivot_wider(names_from = geo_grouping, values_from = Coef, names_prefix = "Coef_")

# Pivot SEs to wide format
se_table <- results_export_list %>%
  select(geo_grouping, outcome_var, SE) %>%
  pivot_wider(names_from = geo_grouping, values_from = SE, names_prefix = "SE_")

# Combine the coefficients and SE tables
final_table <- coef_table %>%
  left_join(se_table, by = "outcome_var") %>%
  arrange(outcome_var) %>%
  mutate(
    `All` = paste0(`Coef_All`," (",`SE_All`,")"),
    `Large urban` = paste0(`Coef_Large urban`," (",`SE_Large urban`,")"),
    `Mid-sized urban` = paste0(`Coef_Mid-sized urban`," (",`SE_Mid-sized urban`,")"),
    `Small urban` = paste0(`Coef_Small urban`," (",`SE_Small urban`,")"),
    `Suburban` = paste0(`Coef_Suburban`," (",`SE_Suburban`,")"),
    `Small town` = paste0(`Coef_Small town`," (",`SE_Small town`,")"),
    `Rural` = paste0(`Coef_Rural`," (",`SE_Rural`,")"),
  ) %>%
  left_join(titles_df) %>%
  select(
    titles,
    `All`,`Large urban`,`Mid-sized urban`,`Small urban`,`Suburban`,`Small town`,`Rural`
  )

# Pivot table to horizontal for datawrapper display
reshaped_table <- final_table %>%
  pivot_longer(
    cols = c(`All`, `Large urban`, `Mid-sized urban`, `Small urban`, `Suburban`, `Small town`, `Rural`),
    names_to = "geo_grouping",
    values_to = "Coef_SE"
  ) %>%
  pivot_wider(
    names_from = titles,
    values_from = Coef_SE
  ) %>%
  arrange(match(geo_grouping, c("All", "Large urban", "Mid-sized urban", "Small urban", "Suburban", "Small town", "Rural"))) %>%
  select(geo_grouping,all_of(table_titles)) %>%
  rename(`Tract Geography` = geo_grouping)

setwd(path_output)
# write.xlsx(reshaped_table, file = "CSDID Last Period Effect Estimate.xlsx", overwrite = TRUE)

# png(file = "CSDID Event Study All Active and Vacant.png",width = 800, height = 533)
# plot_list[[1]][[1]]
# dev.off()
# 
# png(file = "CSDID Event Study Logged All Active and Vacant.png",width = 800, height = 533)
# plot_list[[1]][[2]]
# dev.off()
# 
# pdf(file = "CSDID Event Studies.pdf",width = 12, height = 8)
# plot_list
# dev.off()

# Selected list of significant impacts
export_plot_list <- list()
export_plot_list[[1]] <- plot_data_list[[1]][[1]] # All tracts, Active and Vacant Residential 
export_plot_list[[2]] <- plot_data_list[[1]][[2]] # All tracts, growth rate 

export_plot_list[[3]] <- plot_data_list[[2]][[1]] # Large Urban tracts, Active and Vacant Residential 
export_plot_list[[4]] <- plot_data_list[[2]][[2]] # Large Urban tracts, growth rate 

export_plot_list[[5]] <- plot_data_list[[3]][[1]] # Mid-size Urban tracts, Active and Vacant Residential
export_plot_list[[6]] <- plot_data_list[[4]][[1]] # rural tracts, Active and Vacant Residential
export_plot_list[[7]] <- plot_data_list[[5]][[1]] # small town tracts, Active and Vacant Residential
export_plot_list[[8]] <- plot_data_list[[6]][[1]] # small Urban tracts, Active and Vacant Residential
export_plot_list[[9]] <- plot_data_list[[7]][[1]] # suburban tracts, Active and Vacant Residential

sheet_names <- c(
  "All, Act and Vac Res"
  , "All, Log(Act and Vac Res)"
  
  , "Large, Act and Vac Res"
  , "Large, Log(Act and Vac Res)"
  
  , "Mid-size, Act and Vac Res"
  , "rural, Act and Vac Res"
  , "Small town, Act and Vac Res"
  , "Small Urban, Act and Vac Res"
  , "Suburban, Act and Vac Res"
)

# Create a new workbook
wb <- createWorkbook()

# Loop through each model result
for(i in seq_along(export_plot_list)){

  # Define sheet name using geo_groupings
  sheet_name <- sheet_names[[i]]

  # Add worksheet to the workbook
  addWorksheet(wb, sheet_name)

  # Write data to the worksheet
  writeData(wb, sheet = sheet_name, x = export_plot_list[[i]])
}

# Define the file path where you want to save the workbook
file_path <- "CSDID_model_results.xlsx"

# Save the workbook
saveWorkbook(wb, file = file_path, overwrite = TRUE)


##################################################
### Create academic ready event study figures  ###
##################################################
# 1) Build df_plot with explicit Outcome labels + stable facet order
df_plot <- bind_rows(
  plot_data_list[[1]][[1]] %>% mutate(Outcome = "Active and Vacant Addresses"),
  plot_data_list[[1]][[2]] %>% mutate(Outcome = "Address Growth Rate"),
  plot_data_list[[1]][[3]] %>% mutate(Outcome = "log(Active and Vacant Addresses)")
) %>%
  mutate(
    date  = as.Date(date),
    Row   = factor(Row),
    phase = factor(`Pre/Post`, levels = c("Pre-Treatment", "Post-Treatment")),
    Outcome = factor(
      Outcome,
      levels = c("Active and Vacant Addresses",
                 "Address Growth Rate",
                 "log(Active and Vacant Addresses)")
    )
  )

# 2) Events (as you wrote)
events_df <- tibble::tibble(
  date  = as.Date(c("2017-12-22", "2018-06-14", "2019-12-19")),
  event = factor(c("TCJA Passed", "OZ Map Certified", "OZ Regulations Finalized"),
                 levels = c("TCJA Passed", "OZ Map Certified", "OZ Regulations Finalized"))
)

# 3) Plot (legend order + nicer date breaks + facet fix)
p_bw <- ggplot(df_plot, aes(x = date, y = ATT)) +
  # Policy verticals with legend
  geom_vline(
    data = events_df,
    aes(xintercept = date, linetype = event),
    color = "grey40", linewidth = 0.5, show.legend = TRUE
  ) +
  # Zero line
  geom_hline(yintercept = 0, color = "grey60", linewidth = 0.4) +
  # Error bars & points
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0, linewidth = 0.6, color = "black") +
  geom_point(aes(fill = phase), shape = 21, size = 2.5, stroke = 0.7, color = "black") +
  scale_fill_manual(values = c("Pre-Treatment" = "white", "Post-Treatment" = "black"), name = NULL) +
  scale_linetype_manual(
    name   = NULL,
    values = c("TCJA Passed" = "dashed",
               "OZ Map Certified" = "dotted",
               "OZ Regulations Finalized" = "dotdash")
  ) +
  # Optional: readable time axis (tweak to taste)
  scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m") +
  labs(
    title = "Dynamic treatment effects, all tract types",
    subtitle = "Points show ATT; error bars show 95% confidence intervals",
    x = "Date",
    y = "Average treatment effect"
  ) +
  facet_grid(Outcome ~ ., scales = "free_y") +   # <- 'scales' (plural)
  theme_bw(base_size = 12) +
  theme(
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    strip.background   = element_blank(),
    strip.placement    = "outside",
    strip.text.y.left  = element_text(angle = 0, hjust = 0.5, vjust = 0.5, face = "bold"),
    legend.position    = "bottom",
    legend.title       = element_blank(),
    plot.title         = element_text(face = "bold"),
    panel.border       = element_blank()
  ) +
  guides(
    fill = guide_legend(order = 1),
    linetype = guide_legend(order = 2)
  )

print(p_bw)

# Export unchanged (PDF + PNG)
ggplot2::ggsave(
  filename = file.path(path_output, "event_study_by_outcome.pdf"),
  plot     = p_bw,
  device   = grDevices::cairo_pdf,
  width    = 8, height = 10, units = "in"
)
ggplot2::ggsave(
  filename = file.path(path_output, "event_study_by_outcome.png"),
  plot     = p_bw,
  dpi      = 600,
  width    = 8, height = 10, units = "in",
  bg       = "transparent"
)




# ---------- Helper to build one figure for a given geography (with post-period ATT lines) ----------
make_event_study_plot <- function(plot_data_list_j, geo_label, out_dir = path_output) {
  
  # Assemble outcomes for this geography (expects up to 3 elements)
  df_plot <- list(
    "Active and Vacant Addresses"            = 1L,
    "Address Growth Rate"                    = 2L,
    "log(Active and Vacant Addresses)"       = 3L
  ) %>%
    imap_dfr(~ {
      idx <- .x
      lab <- .y
      if (length(plot_data_list_j) >= idx && !is.null(plot_data_list_j[[idx]])) {
        plot_data_list_j[[idx]] %>% mutate(Outcome = lab)
      } else {
        NULL
      }
    }) %>%
    mutate(
      date  = as.Date(date),
      Row   = factor(Row),
      phase = factor(`Pre/Post`, levels = c("Pre-Treatment", "Post-Treatment")),
      Outcome = factor(
        Outcome,
        levels = c("Active and Vacant Addresses",
                   "Address Growth Rate",
                   "log(Active and Vacant Addresses)")
      )
    )
  
  # If this geo has no data (shouldn't happen), bail out
  if (nrow(df_plot) == 0) return(invisible(NULL))
  
  # ---- Build per-outcome post-period range (x start/end) ----
  post_range <- df_plot %>%
    dplyr::filter(phase == "Post-Treatment") %>%
    dplyr::group_by(Outcome) %>%
    dplyr::summarise(
      xstart = min(date, na.rm = TRUE),
      xend   = max(date, na.rm = TRUE),
      .groups = "drop"
    )
  
  # ---- Pull average ATT by outcome from reshaped_table_averages ----
  # Helper to parse numeric coefficient from strings like "0.123 * (0.045)"
  parse_coef_num <- function(x) {
    # take the first numeric (handles negatives/decimals), ignore stars and SE
    as.numeric(stringr::str_extract(x, "-?\\d+\\.?\\d*"))
  }
  
  # Map table column names to the facet labels you use
  table_to_facet <- c(
    "Active and Vacant Residential"       = "Active and Vacant Addresses",
    "Address Growth Rate"                 = "Address Growth Rate",
    "log(Active and Vacant Residential)"  = "log(Active and Vacant Addresses)"
  )
  
  # Scaling back to plotting units:
  # (your table multiplied growth/log by 100 when formatting)
  scale_back <- c(
    "Active and Vacant Addresses"       = 1,
    "Address Growth Rate"               = 1/100,
    "log(Active and Vacant Addresses)"  = 1
  )
  
  # Extract the row for this geography and reshape long
  ave_tbl <- reshaped_table_averages %>%
    dplyr::filter(.data[["Tract Geography"]] == geo_label) %>%
    tidyr::pivot_longer(
      cols = names(table_to_facet),
      names_to = "Title",
      values_to = "coef_se_str"
    ) %>%
    dplyr::mutate(
      Outcome = factor(unname(table_to_facet[Title]),
                       levels = levels(df_plot$Outcome)),
      avg_att_table_units = parse_coef_num(coef_se_str)
    ) %>%
    dplyr::filter(!is.na(Outcome))
  
  # Convert to plotting units & attach post x-range
  hline_df <- ave_tbl %>%
    dplyr::mutate(
      y = avg_att_table_units * unname(scale_back[as.character(Outcome)])
    ) %>%
    dplyr::inner_join(post_range, by = "Outcome") %>%
    dplyr::select(Outcome, y, xstart, xend)
  
  # Events/legend (dates & labels you specified)
  events_df <- tibble::tibble(
    date  = as.Date(c("2017-12-22", "2018-06-14", "2019-12-19")),
    event = factor(c("TCJA Passed", "OZ Map Certified", "OZ Regulations Finalized"),
                   levels = c("TCJA Passed", "OZ Map Certified", "OZ Regulations Finalized"))
  )
  
  # Build plot (B/W, with post-period horizontal average lines)
  p_bw <- ggplot(df_plot, aes(x = date, y = ATT)) +
    # Policy verticals with legend
    geom_vline(
      data = events_df,
      aes(xintercept = date, linetype = event),
      color = "grey40", linewidth = 0.5, show.legend = TRUE
    ) +
    # Zero line
    geom_hline(yintercept = 0, color = "grey60", linewidth = 0.4) +
    # NEW: post-period horizontal line at the average ATT for each outcome
    geom_segment(
      data = hline_df,
      aes(x = xstart, xend = xend, y = y, yend = y),
      inherit.aes = FALSE,
      linewidth = 0.6,
      color = "grey30",
      linetype = "solid"
    ) +
    # Error bars & points
    geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0, linewidth = 0.6, color = "black") +
    geom_point(aes(fill = phase), shape = 21, size = 2.5, stroke = 0.7, color = "black") +
    # Scales/legends
    scale_fill_manual(values = c("Pre-Treatment" = "white", "Post-Treatment" = "black"), name = NULL) +
    scale_linetype_manual(
      name   = NULL,
      values = c("TCJA Passed" = "dashed",
                 "OZ Map Certified" = "dotted",
                 "OZ Regulations Finalized" = "dotdash")
    ) +
    scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m") +
    labs(
      # title    = paste0("Dynamic treatment effects by outcome — ", geo_label),
      # subtitle = "Points show ATT; error bars show 95% confidence intervals; solid line = post-period average ATT",
      x = "Date",
      y = "Average treatment effect"
    ) +
    facet_grid(Outcome ~ ., scales = "free_y") +
    theme_bw(base_size = 12) +
    theme(
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_blank(),
      strip.background   = element_blank(),
      strip.placement    = "outside",
      strip.text.y.left  = element_text(angle = 0, hjust = 0.5, vjust = 0.5, face = "bold"),
      axis.text.x  = element_text(angle = 90, hjust = 0.5, vjust = 0.5),
      legend.position    = "bottom",
      legend.title       = element_blank(),
      plot.title         = element_text(face = "bold"),
      panel.border       = element_blank()
    ) +
    guides(
      fill = guide_legend(order = 1),
      linetype = guide_legend(order = 2)
    )
  
  # Export (PDF + PNG)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  file_stub <- stringr::str_replace_all(tolower(geo_label), "[^a-z0-9]+", "_") %>%
    stringr::str_replace_all("^_|_$", "")
  
  ggsave(
    filename = file.path(out_dir, paste0("event_study_by_outcome_", file_stub, ".pdf")),
    plot     = p_bw,
    device   = grDevices::cairo_pdf,
    width    = 8, height = 11, units = "in"
  )
  ggsave(
    filename = file.path(out_dir, paste0("event_study_by_outcome_", file_stub, ".png")),
    plot     = p_bw,
    dpi      = 600,
    width    = 8, height = 11, units = "in",
    bg       = "transparent"
  )
  
  invisible(p_bw)
}


# ---------- Helper to build one figure for a given geography (with post-period ATT lines) ----------
# requires: dplyr, ggplot2, stringr, tibble, ggh4x
library(ggh4x)

# ---------- Helper to build one figure for a given geography (with post-period ATT lines) ----------
make_event_study_plot_for_slides <- function(plot_data_list_j, geo_label, out_dir = path_output, f_zero = 0.2) {
  
  # Assemble outcomes for this geography (expects up to 3 elements)
  df_plot <- list(
    "Active and Vacant Addresses"            = 1L,
    "Address Growth Rate"                    = 2L,
    "log(Active and Vacant Addresses)"       = 3L
  ) %>%
    purrr::imap_dfr(~ {
      idx <- .x
      lab <- .y
      if (length(plot_data_list_j) >= idx && !is.null(plot_data_list_j[[idx]])) {
        plot_data_list_j[[idx]] %>% dplyr::mutate(Outcome = lab)
      } else {
        NULL
      }
    }) %>%
    dplyr::mutate(
      date  = as.Date(date),
      Row   = factor(Row),
      phase = factor(`Pre/Post`, levels = c("Pre-Treatment", "Post-Treatment")),
      Outcome = factor(
        Outcome,
        levels = c("Active and Vacant Addresses",
                   "Address Growth Rate",
                   "log(Active and Vacant Addresses)")
      )
    )
  
  if (nrow(df_plot) == 0) return(invisible(NULL))
  
  # ---- Build per-outcome post-period range (x start/end) ----
  post_range <- df_plot %>%
    dplyr::filter(phase == "Post-Treatment") %>%
    dplyr::group_by(Outcome) %>%
    dplyr::summarise(
      xstart = min(as.Date(date), na.rm = TRUE),
      xend   = max(as.Date(date), na.rm = TRUE),
      .groups = "drop"
    )
  
  # ---- Pull average ATT by outcome from reshaped_table_averages ----
  parse_coef_num <- function(x) as.numeric(stringr::str_extract(x, "-?\\d+\\.?\\d*"))
  
  table_to_facet <- c(
    "Active and Vacant Residential"       = "Active and Vacant Addresses",
    "Address Growth Rate"                 = "Address Growth Rate",
    "log(Active and Vacant Residential)"  = "log(Active and Vacant Addresses)"
  )
  
  scale_back <- c(
    "Active and Vacant Addresses"       = 1,
    "Address Growth Rate"               = 1/100,  # table formats percent
    "log(Active and Vacant Addresses)"  = 1
  )
  
  ave_tbl <- reshaped_table_averages %>%
    dplyr::filter(.data[["Tract Geography"]] == geo_label) %>%
    tidyr::pivot_longer(
      cols = names(table_to_facet),
      names_to = "Title",
      values_to = "coef_se_str"
    ) %>%
    dplyr::mutate(
      Outcome = factor(unname(table_to_facet[Title]),
                       levels = levels(df_plot$Outcome)),
      avg_att_table_units = parse_coef_num(coef_se_str)
    ) %>%
    dplyr::filter(!is.na(Outcome))
  
  hline_df <- ave_tbl %>%
    dplyr::mutate(
      y = avg_att_table_units * unname(scale_back[as.character(Outcome)])
    ) %>%
    dplyr::inner_join(post_range, by = "Outcome") %>%
    dplyr::select(Outcome, y, xstart, xend)
  
  # Policy event verticals
  events_df <- tibble::tibble(
    date  = as.Date(c("2017-12-22", "2018-06-14", "2019-12-19")),
    event = factor(c("TCJA Passed", "OZ Map Certified", "OZ Regulations Finalized"),
                   levels = c("TCJA Passed", "OZ Map Certified", "OZ Regulations Finalized"))
  )
  
  # ---- NEW: Per-facet y-limits that anchor zero at the same fraction f_zero ----
  # Use CI envelope (Lower/Upper) to ensure bars fit inside limits
  limits_df <- df_plot %>%
    dplyr::group_by(Outcome) %>%
    dplyr::summarise(
      ymin = min(Lower, ATT, na.rm = TRUE),
      ymax = max(Upper, ATT, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    # keep facet order consistent with levels(df_plot$Outcome)
    dplyr::mutate(Outcome = factor(Outcome, levels = levels(df_plot$Outcome))) %>%
    dplyr::arrange(Outcome)
  
  anchor_zero_limits <- function(ymin, ymax, f = 0.2) {
    # f in (0,1): pixel fraction from bottom where 0 should sit (0.5 = center)
    R <- max(
      if (is.finite(ymax) && ymax > 0) ymax / (1 - f) else 0,
      if (is.finite(ymin) && ymin < 0) abs(ymin) / f else 0,
      1e-12
    )
    c(-f * R, (1 - f) * R)
  }
  
  scales_list <- purrr::pmap(limits_df[, c("ymin", "ymax")], ~ {
    lims <- anchor_zero_limits(..1, ..2, f = f_zero)
    ggplot2::scale_y_continuous(limits = lims)
  })
  
  # ---- Plot (B/W) ----
  p_bw <- ggplot(df_plot, aes(x = date, y = ATT)) +
    geom_vline(
      data = events_df,
      aes(xintercept = date, linetype = event),
      color = "grey40", linewidth = 0.5, show.legend = TRUE
    ) +
    geom_hline(yintercept = 0, color = "grey60", linewidth = 0.4) +
    geom_segment(
      data = hline_df,
      aes(x = xstart, xend = xend, y = y, yend = y),
      inherit.aes = FALSE,
      linewidth = 0.6, color = "grey30", linetype = "solid"
    ) +
    geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0, linewidth = 0.6, color = "black") +
    geom_point(aes(fill = phase), shape = 21, size = 2.5, stroke = 0.7, color = "black") +
    scale_fill_manual(values = c("Pre-Treatment" = "white", "Post-Treatment" = "black"), name = NULL) +
    scale_linetype_manual(
      name   = NULL,
      values = c("TCJA Passed" = "dashed",
                 "OZ Map Certified" = "dotted",
                 "OZ Regulations Finalized" = "dotdash")
    ) +
    scale_x_date(date_breaks = "12 months", date_labels = "%Y-%m") +
    labs(x = "Date", y = "Average treatment effect") +
    ggh4x::facet_wrap2(~ Outcome, scales = "free_y") +
    ggh4x::facetted_pos_scales(y = scales_list) +   # <-- aligns zero across facets
    theme_bw(base_size = 12) +
    theme(
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_blank(),
      strip.background   = element_blank(),
      strip.placement    = "outside",
      axis.text.x        = element_text(angle = 90, hjust = 0.5, vjust = 0.5),
      legend.position    = "bottom",
      legend.title       = element_blank(),
      plot.title         = element_text(face = "bold"),
      panel.border       = element_blank()
    ) +
    guides(
      fill = guide_legend(order = 1),
      linetype = guide_legend(order = 2)
    )
  
  # ---- Export (PDF + PNG) ----
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  file_stub <- stringr::str_replace_all(tolower(geo_label), "[^a-z0-9]+", "_") %>%
    stringr::str_replace_all("^_|_$", "")
  
  ggsave(
    filename = file.path(out_dir, paste0("event_study_by_outcome_", file_stub, "_for_slides.pdf")),
    plot     = p_bw,
    device   = grDevices::cairo_pdf,
    width    = 10.66667,    # inches
    height   = 6,     # inches
    units = "in"
  )
  ggsave(
    filename = file.path(out_dir, paste0("event_study_by_outcome_", file_stub, "_for_slides.png")),
    plot     = p_bw,
    dpi      = 600,
    width    = 10.66667, height = 6, units = "in",
    bg       = "transparent"
  )
  
  invisible(p_bw)
}


# ---------- Loop over geographies and build/export plots ----------
# Assumes: geo_groupings is in the same order as plot_data_list (j = 1..length)
imap(plot_data_list, ~ make_event_study_plot(.x, geo_label = geo_groupings[[.y]], out_dir = path_output))
imap(plot_data_list, ~ make_event_study_plot_for_slides(.x, geo_label = geo_groupings[[.y]], out_dir = path_output))
