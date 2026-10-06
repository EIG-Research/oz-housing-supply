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
load(file = "USPS_tract_vacancy.RData")

################
### Clean up uncertain tracts from the crosswalk
### If a tract in 2020 is a mix of any version of the LIC, contiguous or inelligible, then drop it from the analysis - conservative first pass

USPS_data <- USPS_data %>%
  filter(Sample == "In Clean Sample") %>%
  filter(Designation_category %in% c("LIC selected","LIC not selected","Ineligible")) %>%
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

##### Time Invariant Values
time_invariant <- USPS_data %>% 
  filter(date == "2017-12-01") %>%
  select(geoid,Designation_category,`poverty_rate`:`employed_tract_residents`) %>%
  mutate(`current median income decile` = ntile(median_income, 10),
         `current poverty rate decile` = ntile(poverty_rate, 10),
         `current solo detached decile` = ntile(solo_detached_housing_share, 10)) %>%
  na.omit()

# Continue with other data transformations as needed
USPS_data <- USPS_data %>%
  select(-c(`poverty_rate`:`employed_tract_residents`)) %>%
  mutate(
    G = if_else(`OZ Designation` == 1, period_value, 0),
    geoid_num = as.numeric(geoid),
    
    
    Total_active_vacant_exclude_nostat_RESIDENTIAL = 
      ACTIVE_RESIDENTIAL_ADDRESSES + STV_RESIDENTIAL_ADDRESSES + LTV_RESIDENTIAL_ADDRESSES,
    
    # VACANCY_RATE_ALL = 100*((STV_RESIDENTIAL_ADDRESSES + LTV_RESIDENTIAL_ADDRESSES + STV_BUSINESS_ADDRESSES + LTV_BUSINESS_ADDRESSES +STV_OTHER_ADDRESSES + LTV_OTHER_ADDRESSES)/(Total_active_vacant_exclude_nostat)),
    # VACANCY_RATE_RESIDENTIAL = 100*((STV_RESIDENTIAL_ADDRESSES + LTV_RESIDENTIAL_ADDRESSES)/(Total_active_vacant_exclude_nostat_RESIDENTIAL)),
    # VACANCY_RATE_BUSINESS = 100*((STV_BUSINESS_ADDRESSES + LTV_BUSINESS_ADDRESSES)/(Total_active_vacant_exclude_nostat_BUSINESS)),
    # VACANCY_RATE_OTHER = 100*((STV_OTHER_ADDRESSES + LTV_OTHER_ADDRESSES)/(Total_active_vacant_exclude_nostat_OTHER))
  ) %>%
  left_join(time_invariant) %>%
  filter(!is.na(Total_active_vacant_exclude_nostat_RESIDENTIAL))

# Calculate the annual rate of growth for total active and vacant residential

panel_USPS <- plm::pdata.frame(USPS_data, index = c("geoid_num","period"), drop.index = FALSE)

panel_USPS$change <- panel_USPS$Total_active_vacant_exclude_nostat_RESIDENTIAL - plm::lag(panel_USPS$Total_active_vacant_exclude_nostat_RESIDENTIAL, k = 1)
panel_USPS$growth_rate <- panel_USPS$change/plm::lag(panel_USPS$Total_active_vacant_exclude_nostat_RESIDENTIAL, k = 1)

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

# Count the number of quarters covered by the data set for each tract
USPS_data <- USPS_data %>%
  filter(!is.na(growth_rate)) %>%
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
  , "growth_rate_winz"
)

titles <- c(
  "Effect on  Active and Vacant Residential"
  , "Effect on Address Growth Rate"
)

table_titles <- c(
  "Active and Vacant Residential"
  , "Address Growth Rate"
)

# controls      <- c("poverty_rate", "median_income")
controls      <- c("poverty_rate","median_income","solo_detached_housing_share")
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

for (j in seq_along(geo_groupings)){
  print(geo_groupings[[j]])
  
  for(i in seq_along(outcome_vars[[1]])){
    print(outcome_vars[[1]][[i]])
    
    # filter job and employment data by year
    if(outcome_vars[[1]][[i]] %in% c("jobs_in_tract","employed_tract_residents")){
      analysis_data <- data_list[[j]] %>%
        filter(MONTH =="12") %>% 
        select("geoid_num","period","G", outcome_vars[[1]][[i]],
               all_of(controls)) %>%
        na.omit()
    }else{
      analysis_data <- data_list[[j]] %>%
        select("geoid_num","period","G", outcome_vars[[1]][[i]],
               all_of(controls)) %>%
        na.omit()
    }
    
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
    
    # compute the overall effect by averaging the effect of the treatment across
    # all positive lengths of exposure
    dynamic[[i]] <- aggte(csdid_treated[[i]],
                          # alp = 0.01,
                          # alp = 0.02,
                          # alp = 0.05,
                          alp = 0.1,
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
        )
      )

    # Create a data frame for the vline information
    vline_data <- data.frame(
      date = as.Date(c("2017-12-20", "2018-06-14", "2019-12-19")),
      label = c("OZs Enacted", "OZ Map Certified", "Regulations Finalized"),  # Labels for the legend
      linetype = c("Dashed Line 1", "Dashed Line 2", "Solid Line"),
      color = c("#254f85", "#79c5fd", "#008080") # Colors for the lines
    )
    
    plots[[i]] <- plot_data[[i]] %>%
      ggplot(aes(x = date,
                 color = `Pre/Post`)) +   
      ###################################################
    # Add geom_vline with legend information
    geom_vline(data = vline_data, aes(xintercept = date, color = label, linetype = linetype),
               size = 1, alpha = 0.3) +
      ###################################################
    geom_hline(yintercept = 0, color = "grey20") +
      geom_point(aes(y = ATT)) + 
      geom_errorbar(aes(ymin = Lower, ymax = Upper)) + 
      ggtitle(label = titles[[i]],
              subtitle = geo_groupings[[j]]) + 
      theme_minimal() +
      scale_color_manual(
        values = c("#254f85", "#79c5fd", "grey20","grey80","#008080"),  # Custom colors
        guide = guide_legend(title = "Policy Events") # Add legend title
      ) +
      scale_linetype_manual(
        values = c("Dashed Line 1" = "dashed", "Dashed Line 2" = "dashed", "Solid Line" = "solid"),
        # guide = guide_legend(title = "Line Types")   # Add legend title for linetype
        guide = guide_legend(element_blank())
      ) +
      guides(linetype = "none") + 
      theme(legend.position = "right")
  }
  
  dynamic_results_list[[j]] <- dynamic
  plot_data_list[[j]] <- plot_data
  plot_list[[j]] <- plots
}

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

  
  # Format the coefficient with an asterisk if significant
  if(!current_outcome %in% c("growth_rate_winz")){
    # Extract ATT, SE, and critical value
    att <- dynamic_results_list[[geo_index]][[outcome_index]]$att.egt[time_period_count]
    se <- round(dynamic_results_list[[geo_index]][[outcome_index]]$se.egt[time_period_count],1)
    crit_val <- dynamic_results_list[[geo_index]][[outcome_index]]$crit.val.egt[1]
    
    formatted_coef[k] <- paste0(
      round(att, 1), " ",
      if_else((dynamic_results_list[[geo_index]][[outcome_index]]$att.egt[time_period_count] / dynamic_results_list[[geo_index]][[outcome_index]]$se.egt[time_period_count]) > crit_val, "*", ""))
  }else{
    att <- dynamic_results_list[[geo_index]][[outcome_index]][["overall.att"]]
    se <- round(dynamic_results_list[[geo_index]][[outcome_index]][["overall.se"]]*100,4)
    crit_val <- dynamic_results_list[[geo_index]][[outcome_index]]$crit.val.egt[1]
    
    
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
write.xlsx(reshaped_table, file = "CSDID Effect Estimate with neighbors.xlsx", overwrite = TRUE)
