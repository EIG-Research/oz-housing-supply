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
library(forcats)

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

############################################################
# CSDID: Control-set sensitivity sweep
############################################################

##----------------------------------------------------------
## Outcomes, titles, geo groupings, dates, and splits
##----------------------------------------------------------
outcome_vars <- c(
  "Total_active_vacant_exclude_nostat_RESIDENTIAL",
  "growth_rate_winz",
  "log_level"
)

table_titles <- c(
  "Active and Vacant Residential",
  "Address Growth Rate",
  "log(Active and Vacant Residential)"
)



titles_df <- tibble(titles = table_titles, outcome_var = outcome_vars)

Metro_groupings <- sort(unique(USPS_data$`Type tract`))
geo_groupings   <- c("All", Metro_groupings)

# Data splits: All + per-tract-type
data_list <- list()
data_list[[1]] <- USPS_data
for (i in seq_along(Metro_groupings)) {
  data_list[[i + 1]] <- USPS_data %>% filter(`Type tract` == Metro_groupings[[i]])
}

# Event-time index for plotting and final-period alignment
dates_df <- USPS_data %>%
  select(period, date) %>%
  distinct() %>%
  arrange(period) %>%
  mutate(period = period - period_value)

##----------------------------------------------------------
## Define control sets
##----------------------------------------------------------
control_sets <- list(
  "none"                               = character(0),
  "poverty_income"                     = c("poverty_rate","median_income"),
  "poverty_income_detached"            = c("poverty_rate","median_income","solo_detached_housing_share"),
  "poverty_income_detached_zoning"     = c("poverty_rate","median_income","solo_detached_housing_share","Zoning_Index")
)

##----------------------------------------------------------
## Run CSDID over all control sets, geographies, and outcomes
##----------------------------------------------------------
results_long <- tibble()
results_dynamic_long <- tibble()

for (ctrl_name in names(control_sets)) {
  ctrl_vec <- control_sets[[ctrl_name]]
  current_formula <- if (length(ctrl_vec) == 0) {
    as.formula("~ 1")
  } else {
    as.formula(paste("~", paste(ctrl_vec, collapse = " + ")))
  }
  
  # store dynamic objects if you want plots later
  dynamic_results_list <- vector("list", length(geo_groupings))
  
  for (g in seq_along(geo_groupings)) {
    dynamic <- vector("list", length(outcome_vars))
    
    for (i in seq_along(outcome_vars)) {
      outcome_i <- outcome_vars[[i]]
      
      analysis_data <- data_list[[g]] %>%
        select(geoid_num, period, date, G, all_of(outcome_i), all_of(ctrl_vec)) %>%
        na.omit()
      
      # CSDID estimation
      est <- att_gt(
        yname   = outcome_i,
        tname   = "period",
        idname  = "geoid_num",
        gname   = "G",
        xformla = current_formula,
        biters  = 1000,
        pl      = TRUE,
        data    = analysis_data,
        base_period = "universal"
      )
      
      dyn <- aggte(
        est,
        type = "dynamic",
        alp  = 0.01,
        na.rm = TRUE
      )
      
      if (g == 1) {
        dyn_df <- tibble(
          control_spec = ctrl_name,
          outcome_var  = outcome_i,
          event_time   = dyn$egt,
          att          = dyn$att.egt,
          se           = dyn$se.egt,
          crit         = dyn$crit.val.egt[1]
        ) %>%
          mutate(
            lower = att - crit * se,
            upper = att + crit * se
          ) %>%
          # dates_df already centers period at period_value
          left_join(dates_df %>% rename(event_time = period), by = "event_time")
        
        results_dynamic_long <- bind_rows(results_dynamic_long, dyn_df)
      }
      
      dynamic[[i]] <- dyn
    }
    
    dynamic_results_list[[g]] <- dynamic
  }
  
  ##--------------------------------------
  ## Collect OVERALL effects
  ##--------------------------------------
  for (g in seq_along(geo_groupings)) {
    for (i in seq_along(outcome_vars)) {
      dyn <- dynamic_results_list[[g]][[i]]
      overall_att <- dyn$overall.att
      overall_se  <- dyn$overall.se
      crit_val    <- dyn$crit.val.egt[1]
      tstat       <- overall_att / overall_se
      star        <- ifelse(abs(tstat) > crit_val, "*", "")
      
      results_long <- bind_rows(
        results_long,
        tibble(
          control_spec = ctrl_name,
          geo_grouping = geo_groupings[[g]],
          outcome_var  = outcome_vars[[i]],
          metric       = "overall",
          att          = overall_att,
          se           = overall_se,
          crit_val     = crit_val,
          star         = star
        )
      )
    }
  }
  
  ##--------------------------------------
  ## Collect FINAL-PERIOD effects
  ##--------------------------------------
  for (g in seq_along(geo_groupings)) {
    for (i in seq_along(outcome_vars)) {
      dyn <- dynamic_results_list[[g]][[i]]
      Tn  <- length(dyn$att.egt)
      final_att <- dyn$att.egt[Tn]
      final_se  <- dyn$se.egt[Tn]
      crit_val  <- dyn$crit.val.egt[1]
      tstat     <- final_att / final_se
      star      <- ifelse(abs(tstat) > crit_val, "*", "")
      
      results_long <- bind_rows(
        results_long,
        tibble(
          control_spec = ctrl_name,
          geo_grouping = geo_groupings[[g]],
          outcome_var  = outcome_vars[[i]],
          metric       = "final",
          att          = final_att,
          se           = final_se,
          crit_val     = crit_val,
          star         = star
        )
      )
    }
  }
}

##----------------------------------------------------------
## Format results into wide tables per metric
##   - keeps your title mapping and geography ordering
##   - separate tables for overall and final metrics
##----------------------------------------------------------
fmt_block <- function(df) {
  # Special percent scaling rules if desired
  df %>%
    mutate(
      att_disp = case_when(
        outcome_var %in% c("growth_rate_winz") ~ round(att, 4),
        TRUE                                   ~ round(att, 3)
      ),
      se_disp = case_when(
        outcome_var %in% c("growth_rate_winz") ~ round(se, 4),
        TRUE                                   ~ round(se, 3)
      ),
      coef_se = paste0(att_disp, " ", star, " (", se_disp, ")")
    ) %>%
    select(control_spec, geo_grouping, outcome_var, coef_se) %>%
    left_join(titles_df, by = "outcome_var") %>%
    select(control_spec, geo_grouping, titles, coef_se) %>%
    pivot_wider(names_from = geo_grouping, values_from = coef_se) %>%
    arrange(match(control_spec, names(control_sets))) %>%
    select(control_spec,
           All, `Large urban`, `Mid-sized urban`, `Small urban`,
           Suburban, `Small town`, Rural,
           titles) %>%
    pivot_wider(names_from = titles, values_from = c(All, `Large urban`, `Mid-sized urban`,
                                                     `Small urban`, Suburban, `Small town`, Rural))
}

results_overall_tbl <- results_long %>%
  filter(metric == "overall") %>%
  fmt_block()

results_final_tbl <- results_long %>%
  filter(metric == "final") %>%
  fmt_block()

## Optional: more compact presentation by stacking titles as columns and geographies as rows
compact_table <- function(df_metric) {
  # Return one row per control_spec x geo_grouping, columns per outcome title
  results_long %>%
    filter(metric == df_metric) %>%
    mutate(
      att_disp = case_when(
        outcome_var %in% c("growth_rate_winz") ~ round(att, 4),
        TRUE                                   ~ round(att, 3)
      ),
      se_disp = case_when(
        outcome_var %in% c("growth_rate_winz") ~ round(se, 4),
        TRUE                                   ~ round(se, 3)
      ),
      coef_se = paste0(att_disp, " ", star, " (", se_disp, ")")
    ) %>%
    left_join(titles_df, by = "outcome_var") %>%
    select(control_spec, geo_grouping, titles, coef_se) %>%
    pivot_wider(names_from = titles, values_from = coef_se) %>%
    arrange(match(geo_grouping, c("All","Large urban","Mid-sized urban",
                                  "Small urban","Suburban","Small town","Rural")),
            match(control_spec, names(control_sets)))
}

results_overall_compact <- compact_table("overall")
results_final_compact   <- compact_table("final")

#----------------------------------------------------------
# (Optional) Write to Excel with four sheets
#----------------------------------------------------------
openxlsx::write.xlsx(
  list(
    overall_wide_by_geo_per_title = results_overall_tbl,
    final_wide_by_geo_per_title   = results_final_tbl,
    overall_compact               = results_overall_compact,
    final_compact                 = results_final_compact
  ),
  file = file.path(path_output, "CSDID_control_sensitivity.xlsx"),
  overwrite = TRUE
)

############################################################
# Event-study: All geography, 4 control sets, 3 outcomes
# Style mirrors your draft figures
############################################################

library(tibble)

# Labels
outcome_labels <- c(
  "Total_active_vacant_exclude_nostat_RESIDENTIAL" = "Active and Vacant Addresses",
  "growth_rate_winz"                                = "Address Growth Rate",
  "log_level"                                       = "log(Active and Vacant Addresses)"
)

# Control-spec ordering & labels
results_dynamic_long_edit <- results_dynamic_long %>%
  mutate(
    control_spec = factor(
      control_spec,
      levels = c("none","poverty_income","poverty_income_detached","poverty_income_detached_zoning"),
      labels = c("None","Poverty+Income","+Single-Family Share","+Zoning Index")
    ),
    phase = if_else(event_time < 0, "Pre-Treatment", "Post-Treatment"),
    outcome_lab = outcome_labels[outcome_var]
  )

# Preferred overall ATT for grey dashed reference (one per outcome facet)
ref_lines <- results_long %>%
  filter(metric == "overall",
         geo_grouping == "All",
         control_spec == "poverty_income_detached_zoning") %>%
  transmute(outcome_var, yref = att)

plot_df <- results_dynamic_long_edit %>%
  left_join(ref_lines, by = "outcome_var")

# Slight x-offset by control set to reduce overlap (days)
offset_map <- tibble::tibble(
  control_spec = factor(c("None","Poverty+Income","+Single-Family Share","+Zoning Index"),
                        levels = c("None","Poverty+Income","+Single-Family Share","+Zoning Index")),
  offset_days  = c(-9, -3, 3, 9)
)

plot_df <- plot_df %>%
  left_join(offset_map, by = "control_spec") %>%
  mutate(date_j = date + lubridate::days(offset_days))

# Policy markers (vertical lines)
vline_data <- tibble::tibble(
  date       = as.Date(c("2017-12-20","2018-06-14","2019-12-19")),
  label      = c("OZs Enacted","OZ Map Certified","Regulations Finalized")
)

# Shapes (21–24 support fill/outline; good in grayscale)
shape_vals <- c("None"=21,"Poverty+Income"=22,"+Single-Family Share"=23,"+Zoning Index"=24)

# Build plot (BW)
p_bw <- ggplot(plot_df, aes(x = date_j, y = att)) +
  # Zero line
  geom_hline(yintercept = 0, color = "grey60", linewidth = 0.4) +
  # Preferred overall ATT (one per facet)
  # geom_hline(aes(yintercept = yref), color = "grey40", linetype = "dashed", linewidth = 0.4) +
  # Policy verticals
  geom_vline(data = vline_data, aes(xintercept = date), color = "grey50", linewidth = 0.4) +
  # Error bars (linetype encodes Pre/Post)
  geom_errorbar(
    aes(ymin = lower, ymax = upper, linetype = phase),
    width = 0, linewidth = 0.6, color = "black"
  ) +
  # Points (shape = controls; fill encodes Pre/Post; outline always black)
  geom_point(
    aes(shape = control_spec, fill = phase),
    size = 1.5, stroke = 0.7, color = "black"
  ) +
  scale_shape_manual(values = shape_vals, name = "Controls") +
  scale_linetype_manual(values = c("Pre-Treatment" = "dotted", "Post-Treatment" = "solid"), name = NULL) +
  scale_fill_manual(values = c("Pre-Treatment" = "white", "Post-Treatment" = "black"), name = NULL) +
  facet_wrap(~ outcome_lab, ncol = 1, scales = "free_y") +
  labs(
    title = "Dynamic treatment effects (CSDID), All tracts by control specification",
    subtitle = "Points show period-specific ATT; error bars show 95% confidence intervals",
    x = "Date",
    y = "Average treatment effect"
  ) +
  theme_bw(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    strip.background = element_blank(),
    strip.placement = "outside",
    # Center strip text on y for 3x1 layout
    strip.text = element_text(face = "bold"),
    legend.position = "bottom",
    legend.title = element_blank(),
    plot.title = element_text(face = "bold")
  ) + 
  facet_grid(outcome_lab ~ control_spec  , scale = "free_y" )

# Preview
print(p_bw)

# --- High-quality export for LaTeX (vector) -----------------

ggplot2::ggsave(
  filename = file.path(path_output, "event_study_All_by_controls.pdf"),
  plot     = p_bw,
  device   = grDevices::cairo_pdf,   # embeds fonts, avoids Type 3
  width    = 12, height = 9, units = "in"
)

# Optional high-res PNG alongside PDF
ggplot2::ggsave(
  filename = file.path(path_output, "event_study_All_by_controls.png"),
  plot     = p_bw,
  dpi      = 600,
  width    = 12, height = 9, units = "in",
  bg       = "transparent"
)
