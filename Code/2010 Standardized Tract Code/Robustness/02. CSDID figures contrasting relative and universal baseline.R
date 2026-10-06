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
library(gridExtra)
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

#######################
### Build DID Model ###
#######################
# Variable initialization
data_list <- list()
csdid_treated_universal <- list()
csdid_treated_varying <- list()
dynamic_universal <- list()
dynamic_varying <- list()
outcome_vars <- list()
dates <- sort(unique(USPS_data$date))[2:length(sort(unique(USPS_data$date)))]

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

# Transform time variable
dates_df <- USPS_data %>%
  select(period, date) %>%
  distinct() %>%
  arrange(period) %>% 
  mutate(period = period - period_value)

  
for(i in seq_along(outcome_vars[[1]])){
    print(outcome_vars[[1]][[i]])
    
    # filter job and employment data by year

    analysis_data <- USPS_data %>%
        select("geoid_num","period","date","G", outcome_vars[[1]][[i]],
               all_of(controls)) %>%
        na.omit()
    
    
    # create linear model for the treated group
    csdid_treated_universal[[i]] <- att_gt(
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
    ggdid(csdid_treated_universal[[i]])
    # compute the overall effect by averaging the effect of the treatment across
    # all positive lengths of exposure
    dynamic_universal[[i]] <- aggte(csdid_treated_universal[[i]], 
                          # alp = 0.01,
                          # alp = 0.02,
                          alp = 0.05,
                          type = "dynamic",
                          na.rm = TRUE) 
    
    # create linear model for the treated group
    csdid_treated_varying[[i]] <- att_gt(
      yname = outcome_vars[[1]][[i]],
      tname = "period",
      idname = "geoid_num",
      gname = "G",
      xformla = current_formula,
      # est_method = "ipw",
      biters = 1000,
      pl = TRUE,
      data = analysis_data,
      base_period = "varying"
    )
    ggdid(csdid_treated_varying[[i]])
    # compute the overall effect by averaging the effect of the treatment across
    # all positive lengths of exposure
    dynamic_varying[[i]] <- aggte(csdid_treated_varying[[i]], 
                                    # alp = 0.01,
                                    # alp = 0.02,
                                    alp = 0.05,
                                    type = "dynamic",
                                    na.rm = TRUE) 
}

vline_data <- data.frame(
  date       = as.Date(c("2017-12-20", "2018-06-14", "2019-12-19")),
  label_date = as.Date(c("2017-09-20", "2018-09-14", "2020-03-19")),
  label      = c("OZs Enacted", "OZ Map Certified", "Regulations Finalized"),
  color      = c("black", "black", "black")
)

styles   <- c("universal", "varying")
plot_map <- list()

for(i in seq_along(outcome_vars[[1]])) {
  # ─────────────────────────────────────────────────────────────
  # 1) Build both data frames to grab a common y‐range
  # ─────────────────────────────────────────────────────────────
  all_dfs <- list()
  for(style in styles) {
    dyn_list   <- if(style=="universal") dynamic_universal else dynamic_varying
    using <- data.frame(
      Period = dyn_list[[i]]$egt,
      ATT    = dyn_list[[i]]$att.egt,
      SE     = dyn_list[[i]]$se.egt
    )
    using_dates <- dates_df %>% filter(period %in% using$Period)
    plot_df <- using %>%
      mutate(
        date  = using_dates$date[match(Period, using_dates$period)],
        Lower = ATT - dyn_list[[i]]$crit.val.egt * SE,
        Upper = ATT + dyn_list[[i]]$crit.val.egt * SE
      )
    all_dfs[[style]] <- plot_df
  }
  
  # common y‐axis limits
  y_low  <- min(sapply(all_dfs, function(d) min(d$Lower, na.rm=TRUE)))
  y_high <- max(sapply(all_dfs, function(d) max(d$Upper, na.rm=TRUE)))
  y_lim  <- c(y_low, y_high)
  
  # ─────────────────────────────────────────────────────────────
  # 2) Build each panel with fixed y‐axis and vlines
  # ─────────────────────────────────────────────────────────────
  for(style in styles) {
    plot_df <- all_dfs[[style]]
    cs_list <- if(style=="universal") csdid_treated_universal else csdid_treated_varying
    
    # overall ATT line
    agg_simple <- aggte(cs_list[[i]], type="simple")
    agg_val    <- if(!is.null(agg_simple$overall.att)) agg_simple$overall.att else agg_simple$att
    
    post_dates <- plot_df$date[plot_df$Period >= 0]
    post_start <- min(post_dates, na.rm=TRUE)
    post_end   <- max(post_dates, na.rm=TRUE)
    
    p <- ggplot(plot_df, aes(x = date, y = ATT)) +
      # dynamic estimates
      geom_point(aes(color = Period < 0)) +
      geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0) +
      
      # event‐annotation vlines & labels
      geom_segment(data=vline_data,
                   aes(x = date, xend = date),
                   y = y_lim[1], yend = y_lim[2],
                   color = vline_data$color) +
      geom_text(data=vline_data,
                aes(x = label_date, label = label),
                y = y_lim[2] * 0.65,
                angle = 90, hjust = -0.1, vjust = 0.5,
                color = vline_data$color, size = 3) +
      
      # zero and pre‐policy marker
      geom_hline(yintercept = 0, color = "grey20") +
      geom_vline(xintercept = as.Date("2012-01-01"), color = "grey20") +
      
      # horizontal line for overall ATT
      annotate("segment",
               x    = post_start, xend = post_end,
               y    = agg_val,     yend = agg_val,
               color    = "firebrick",
               linetype = "dashed",
               size     = 0.8) +
      
      # fixed y‐axis
      scale_y_continuous(limits = y_lim) +
      
      # styling
      scale_color_manual(
        values = c("TRUE" = "grey60", "FALSE" = "grey10"),
        labels = c("Post-Treatment","Pre-Treatment")
      ) +
      labs(
        title = paste0(titles[i], " (", style, " base)"),
        x     = "Date", 
        y     = "ATT",
        color = NULL
      ) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "bottom",panel.grid = element_blank())
    
    plot_map[[paste0(style, "_", i)]] <- p
  }
}

# ───────────────────────────────────────────────────────────────
# 3) Render the 2×2 grid
# ───────────────────────────────────────────────────────────────
grid.arrange(
  plot_map[["universal_1"]], plot_map[["varying_1"]],
  plot_map[["universal_2"]], plot_map[["varying_2"]],
  plot_map[["universal_3"]], plot_map[["varying_3"]],
  ncol = 2
)



# Assemble into a single grob
combined_grob <- arrangeGrob(
  plot_map[["universal_1"]], plot_map[["varying_1"]],
  plot_map[["universal_2"]], plot_map[["varying_2"]],
  plot_map[["universal_3"]], plot_map[["varying_3"]],
  ncol = 2
)

setwd(path_output)
# Export to PNG at 300 dpi, 10"×8"
ggsave(
  filename = "fig_event_studies.png",
  plot     = combined_grob,
  width    = 10,    # inches
  height   = 8,     # inches
  units    = "in",
  dpi      = 300
)
