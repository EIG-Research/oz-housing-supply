################################################################
## 0.  Set-up & libraries  -------------------------------------
################################################################
rm(list = ls())
options(scipen = 999)
set.seed(42)

library(tidyverse)
library(lubridate)
library(glue)
library(fixest)      # TWFE
library(did)         # Callaway-Sant’Anna DID
library(plm)         # panel lags
library(broom)
library(modelsummary)
library(writexl)
library(fect)

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
################################################################
## 1.  Load raw USPS data  -------------------------------------
################################################################
setwd(path_data)

load("USPS_tract_vacancy.RData")          # object: USPS_data
usps_raw <- USPS_data %>%
  filter(YEAR   >= 2014)

################################################################
## 2.  Broad vs. narrow analytic samples -----------------------
################################################################
broad_data <- usps_raw %>%
  filter(Designation %in% c("LIC selected",
                                     "LIC not selected",
                                     "Ineligible"))

narrow_data <- usps_raw %>%
  filter(Designation_category_detailed %in% c("LIC selected",
                                              "LIC not selected, not a border tract",
                                              "Ineligible, not a border tract"))

################################################################
## 3.  Shared wrangling function (adds growth + **log outcome**)
################################################################
prep_panel <- function(df) {
  
  df <- df %>%
    mutate(
      period              = dense_rank(date),
      period_treat_start  = min(period[date == "2018-03-01"]),
      type_tract          = `Type tract`,
      treat               = as.integer(`OZ Designation` == 1 & period >= period_treat_start),
      g                   = if_else(`OZ Designation` == 1, period_treat_start, 0L),
      geoid_num           = as.numeric(geoid),
      Total_active_vacant_exclude_nostat_RESIDENTIAL =
        ACTIVE_RESIDENTIAL_ADDRESSES + STV_RESIDENTIAL_ADDRESSES + LTV_RESIDENTIAL_ADDRESSES,
      `current median income decile` = ntile(median_income, 10),
      `current poverty rate decile` = ntile(poverty_rate, 10),
      `current solo detached decile` = ntile(solo_detached_housing_share, 10),
      `current zoning index decile` = ntile(Zoning_Index, 10)
    ) %>%
    filter(!is.na(Total_active_vacant_exclude_nostat_RESIDENTIAL))
  
  # -------- quarterly growth (winsorised) -------------------
  pdata <- plm::pdata.frame(df, index = c("geoid_num", "period"), drop.index = FALSE)
  pdata$change <- pdata$Total_active_vacant_exclude_nostat_RESIDENTIAL -
    lag(pdata$Total_active_vacant_exclude_nostat_RESIDENTIAL, 4)
  pdata$growth <- pdata$change /
    lag(pdata$Total_active_vacant_exclude_nostat_RESIDENTIAL, 4)
  
  cut <- pdata %>%
    summarise(lo = quantile(growth, 0.01, na.rm = TRUE),
              hi = quantile(growth, 0.99, na.rm = TRUE))
  
  pdata$growth_winz <- pmin(pmax(pdata$growth, cut$lo), cut$hi)
  
  # -------- merge growth + **log level** --------------------
  df <- df %>%
    mutate(log_level = log(Total_active_vacant_exclude_nostat_RESIDENTIAL)) %>% 
    filter(is.finite(log_level))                                                # drop -Inf/Inf
  df <- df %>%
    left_join(
      pdata %>%
        as.data.frame() %>%
        select(geoid, period, growth_winz) %>%
        mutate(geoid = as.numeric(geoid),
               period = as.integer(period)),
      by = c("geoid", "period")
    ) %>%
    filter(!is.na(growth_winz)) %>%
    group_by(geoid) %>% mutate(n_quarters = n()) %>% ungroup()
  
  max_q <- max(df$n_quarters)
  df %>% filter(n_quarters == max_q)
}

broad_data  <- prep_panel(broad_data)
narrow_data <- prep_panel(narrow_data)


dec_vars <- c(
  "current median income decile",
  "current poverty rate decile",
  "current solo detached decile",
  "current zoning index decile"
)

baseline_combos <- broad_data %>% 
  filter(date == as.Date("2017-12-01")) %>% 
  filter(Designation == "LIC selected") %>% 
  select(all_of(dec_vars)) %>%
  distinct(across(all_of(dec_vars)))      # one row per unique combination


ID_list <- broad_data %>% 
  filter(Designation == "LIC selected") %>% 
  bind_rows(
    broad_data %>% 
      filter(date == as.Date("2017-12-01")) %>%
      semi_join(baseline_combos, by = dec_vars)
  ) %>% 
  select(geoid) %>%
  distinct() %>%
  pull()


broad_data_mc  <- broad_data %>%
  # filter(YEAR %in% c(2015, 2016,2017,2025), geoid %in% ID_list) %>%
  filter(geoid %in% ID_list)
  
narrow_data_mc <- narrow_data %>%
  # filter(YEAR %in% c(2015, 2016,2017,2025), geoid %in% ID_list) %>%
  filter(geoid %in% ID_list)


################################################################
## 4.  Estimate models – Broad sample -------------------------
################################################################

setwd(path_output)

# ## ---- TWFE --------------------------------------------------
# twfe_broad_vac <- feols(
#   Total_active_vacant_exclude_nostat_RESIDENTIAL ~ treat +
#     poverty_rate + median_income + solo_detached_housing_share + Zoning_Index |
#     geoid + period,
#   data    = subset(broad_data, Designation != "Ineligible"),
#   cluster = ~ geoid
# )
# 
# twfe_broad_log <- feols(
#   log_level ~ treat +
#     poverty_rate + median_income + solo_detached_housing_share + Zoning_Index |
#     geoid + period,
#   data    = subset(broad_data, Designation != "Ineligible"),
#   cluster = ~ geoid
# )
# 
# twfe_broad_gro <- feols(
#   growth_winz ~ treat +
#     poverty_rate + median_income + solo_detached_housing_share + Zoning_Index |
#     geoid + period,
#   data    = subset(broad_data, Designation != "Ineligible"),
#   cluster = ~ geoid
# )
# 
# ## ---- CSDID -------------------------------------------------
# cs_broad_vac <- att_gt(
#   yname      = "Total_active_vacant_exclude_nostat_RESIDENTIAL",
#   tname      = "period",
#   idname     = "geoid",
#   gname      = "g",
#   xformla    = ~ poverty_rate + median_income + solo_detached_housing_share + Zoning_Index,
#   data       = broad_data,
#   est_method = "dr"
# ) |> aggte(type = "simple")
# 
# cs_broad_log <- att_gt(
#   yname      = "log_level",
#   tname      = "period",
#   idname     = "geoid",
#   gname      = "g",
#   xformla    = ~ poverty_rate + median_income + solo_detached_housing_share + Zoning_Index,
#   data       = broad_data,
#   est_method = "dr"
# ) |> aggte(type = "simple")
# 
# cs_broad_gro <- att_gt(
#   yname      = "growth_winz",
#   tname      = "period",
#   idname     = "geoid",
#   gname      = "g",
#   xformla    = ~ poverty_rate + median_income + solo_detached_housing_share + Zoning_Index,
#   data       = broad_data,
#   est_method = "dr"
# ) |> aggte(type = "simple")
# 
# 
# ## ---- Matrix Completion  -------------------------------------------------
# 
# controls      <- c("poverty_rate","median_income","solo_detached_housing_share")
# control_vars  <- paste(controls, collapse = " + ")
# 
# mc_broad_vac <- fect(
#   formula = as.formula(paste("Total_active_vacant_exclude_nostat_RESIDENTIAL", "~", "treat", "+", control_vars)),
#   data = broad_data_mc,
#   na.rm = TRUE,
#   index  = c("geoid", "period"),
#   force = "two-way",
#   CV = TRUE,
#   criterion = "gmspe", #to alleviate the impact of some outlier prediction errors, we allow the criterion of geometric-mean squared prediction errors
#   method = "mc",
#   se = TRUE,
#   nboots = 100,
#   alpha = 0.05,
#   parallel = TRUE,
#   seed = 42,
#   min.T0 = 5
# )
# 
# mc_broad_log <- fect(
#   formula = as.formula(paste("log_level", "~", "treat", "+", control_vars)),
#   data = broad_data_mc,
#   na.rm = TRUE,
#   index  = c("geoid", "period"),
#   force = "two-way",
#   CV = TRUE,
#   criterion = "gmspe", #to alleviate the impact of some outlier prediction errors, we allow the criterion of geometric-mean squared prediction errors
#   method = "mc",
#   se = TRUE,
#   nboots = 100,
#   alpha = 0.05,
#   parallel = TRUE,
#   seed = 42,
#   min.T0 = 5
# )
# 
# mc_broad_gro <- fect(
#   formula = as.formula(paste("growth_winz", "~", "treat", "+", control_vars)),
#   data = broad_data_mc,
#   na.rm = TRUE,
#   index  = c("geoid", "period"),
#   force = "two-way",
#   CV = TRUE,
#   criterion = "gmspe", #to alleviate the impact of some outlier prediction errors, we allow the criterion of geometric-mean squared prediction errors
#   method = "mc",
#   se = TRUE,
#   nboots = 100,
#   alpha = 0.05,
#   parallel = TRUE,
#   seed = 42,
#   min.T0 = 5
# )
# 
# ################################################################
# ## 5.  Estimate models – Narrow sample ------------------------
# ################################################################
# ## ---- TWFE --------------------------------------------------
# twfe_narr_vac <- feols(
#   Total_active_vacant_exclude_nostat_RESIDENTIAL ~ treat +
#     poverty_rate + median_income + solo_detached_housing_share + Zoning_Index |
#     geoid + period,
#   data    = subset(narrow_data, Designation != "Ineligible"),
#   cluster = ~ geoid
# )
# 
# twfe_narr_log <- feols(
#   log_level ~ treat +
#     poverty_rate + median_income + solo_detached_housing_share + Zoning_Index |
#     geoid + period,
#   data    = subset(narrow_data, Designation != "Ineligible"),
#   cluster = ~ geoid
# )
# 
# twfe_narr_gro <- feols(
#   growth_winz ~ treat +
#     poverty_rate + median_income + solo_detached_housing_share + Zoning_Index |
#     geoid + period,
#   data    = subset(narrow_data, Designation != "Ineligible"),
#   cluster = ~ geoid
# )
# 
# ## ---- CSDID -------------------------------------------------
# cs_narr_vac <- att_gt(
#   yname      = "Total_active_vacant_exclude_nostat_RESIDENTIAL",
#   tname      = "period",
#   idname     = "geoid",
#   gname      = "g",
#   xformla    = ~ poverty_rate + median_income + solo_detached_housing_share + Zoning_Index,
#   data       = narrow_data,
#   est_method = "dr"
# ) |> aggte(type = "simple")
# 
# cs_narr_log <- att_gt(
#   yname      = "log_level",
#   tname      = "period",
#   idname     = "geoid",
#   gname      = "g",
#   xformla    = ~ poverty_rate + median_income + solo_detached_housing_share + Zoning_Index,
#   data       = narrow_data,
#   est_method = "dr"
# ) |> aggte(type = "simple")
# 
# cs_narr_gro <- att_gt(
#   yname      = "growth_winz",
#   tname      = "period",
#   idname     = "geoid",
#   gname      = "g",
#   xformla    = ~ poverty_rate + median_income + solo_detached_housing_share + Zoning_Index,
#   data       = narrow_data,
#   est_method = "dr"
# ) |> aggte(type = "simple")
# 
# ## ---- Matrix Completion  -------------------------------------------------
# 
# controls      <- c("poverty_rate","median_income","solo_detached_housing_share")
# control_vars  <- paste(controls, collapse = " + ")
# 
# mc_narr_vac <- fect(
#   formula = as.formula(paste("Total_active_vacant_exclude_nostat_RESIDENTIAL", "~", "treat", "+", control_vars)),
#   data = narrow_data_mc,
#   na.rm = TRUE,
#   index  = c("geoid", "period"),
#   force = "two-way",
#   CV = TRUE,
#   criterion = "gmspe", #to alleviate the impact of some outlier prediction errors, we allow the criterion of geometric-mean squared prediction errors
#   method = "mc",
#   se = TRUE,
#   nboots = 100,
#   alpha = 0.05,
#   parallel = TRUE,
#   seed = 42,
#   min.T0 = 5
# )
# 
# mc_narr_log <- fect(
#   formula = as.formula(paste("log_level", "~", "treat", "+", control_vars)),
#   data = narrow_data_mc,
#   na.rm = TRUE,
#   index  = c("geoid", "period"),
#   force = "two-way",
#   CV = TRUE,
#   criterion = "gmspe", #to alleviate the impact of some outlier prediction errors, we allow the criterion of geometric-mean squared prediction errors
#   method = "mc",
#   se = TRUE,
#   nboots = 100,
#   alpha = 0.05,
#   parallel = TRUE,
#   seed = 42,
#   min.T0 = 5
# )
# 
# mc_narr_gro <- fect(
#   formula = as.formula(paste("growth_winz", "~", "treat", "+", control_vars)),
#   data = narrow_data_mc,
#   na.rm = TRUE,
#   index  = c("geoid", "period"),
#   force = "two-way",
#   CV = TRUE,
#   criterion = "gmspe", #to alleviate the impact of some outlier prediction errors, we allow the criterion of geometric-mean squared prediction errors
#   method = "mc",
#   se = TRUE,
#   nboots = 100,
#   alpha = 0.05,
#   parallel = TRUE,
#   seed = 42,
#   min.T0 = 5
# )
# 
# 
# 
# # Broad Sample Models
# broad_models <- list(
#   twfe_broad_vac = twfe_broad_vac,
#   twfe_broad_log = twfe_broad_log,
#   twfe_broad_gro = twfe_broad_gro,
#   cs_broad_vac   = cs_broad_vac,
#   cs_broad_log   = cs_broad_log,
#   cs_broad_gro   = cs_broad_gro,
#   mc_broad_vac   = mc_broad_vac,
#   mc_broad_log   = mc_broad_log,
#   mc_broad_gro   = mc_broad_gro
# )
# 
# # Narrow Sample Models
# narr_models <- list(
#   twfe_narr_vac = twfe_narr_vac,
#   twfe_narr_log = twfe_narr_log,
#   twfe_narr_gro = twfe_narr_gro,
#   cs_narr_vac   = cs_narr_vac,
#   cs_narr_log   = cs_narr_log,
#   cs_narr_gro   = cs_narr_gro,
#   mc_narr_vac   = mc_narr_vac,
#   mc_narr_log   = mc_narr_log,
#   mc_narr_gro   = mc_narr_gro
# )
# 
# # Combine all models into a single list
# all_models <- list(
#   broad = broad_models,
#   narrow = narr_models
# )
# 
# save(all_models, file = "foundational_models.RData")
load(file = "foundational_models.RData")

################################################################
## 6.  Assemble results table ---------------------------------
################################################################
pull_twfe <- function(obj,
                        outcome,
                        sample,
                        term   = "treat",
                        method = "TWFE",
                        level  = 0.95,
                        vcov   = NULL) {
  
  # Respect optional vcov (e.g., ~cluster_id or a vcov matrix/function)
  smry <- if (is.null(vcov)) summary(obj) else summary(obj, vcov = vcov)
  
  td <- tidy(smry) %>% filter(.data$term == term)
  if (nrow(td) == 0L) stop(sprintf("Term '%s' not found in model.", term))
  
  z <- stats::qnorm(1 - (1 - level)/2)
  
  tibble(
    Sample    = sample,
    Outcome   = outcome,
    Method    = method,
    estimate  = td$estimate[1],
    std.error = td$std.error[1],
    conf.low  = td$estimate[1] - z * td$std.error[1],
    conf.high = td$estimate[1] + z * td$std.error[1]
  )
}
pull_csdid <- function(obj, outcome, sample) {
  tibble(
    Sample   = sample,
    Outcome  = outcome,
    Method   = "CSDID",
    estimate = obj$overall.att,
    std.error = obj$overall.se,
    conf.low  = obj$overall.att - 1.96 * obj$overall.se,
    conf.high = obj$overall.att + 1.96 * obj$overall.se
  )
}

pull_mc <- function(obj, outcome, sample) {
  tibble(
    Sample   = sample,
    Outcome  = outcome,
    Method   = "MC",
    estimate = obj$est.avg[[1]],
    std.error = obj$est.avg[[2]],
    conf.low  = obj$est.avg[[3]],
    conf.high = obj$est.avg[[4]]
  )
}

results_tbl <- bind_rows(
  # ── Broad sample ─────────────────────────────────────────
  pull_twfe(all_models[["broad"]][["twfe_broad_vac"]],  "Avg. New Addresses per Tract",      "All Tracts"),
  pull_twfe(all_models[["broad"]][["twfe_broad_log"]],  "Log Avg. New Addresses",            "All Tracts"),   
  pull_twfe(all_models[["broad"]][["twfe_broad_gro"]],  "Annual Growth rate (winz)",         "All Tracts"),
  pull_csdid(all_models[["broad"]][["cs_broad_vac"]],   "Avg. New Addresses per Tract",      "All Tracts"),
  pull_csdid(all_models[["broad"]][["cs_broad_log"]],   "Log Avg. New Addresses",            "All Tracts"),   
  pull_csdid(all_models[["broad"]][["cs_broad_gro"]],   "Annual Growth rate (winz)",         "All Tracts"),
  pull_mc(all_models[["broad"]][["mc_broad_vac"]],   "Avg. New Addresses per Tract",      "All Tracts"),
  pull_mc(all_models[["broad"]][["mc_broad_log"]],   "Log Avg. New Addresses",            "All Tracts"),   
  pull_mc(all_models[["broad"]][["mc_broad_gro"]],   "Annual Growth rate (winz)",         "All Tracts"),
  
  # ── Narrow sample ────────────────────────────────────────
  pull_twfe(all_models[["narrow"]][["twfe_narr_vac"]],   "Avg. New Addresses per Tract",      "Excluding Neighboring"),
  pull_twfe(all_models[["narrow"]][["twfe_narr_log"]],   "Log Avg. New Addresses",            "Excluding Neighboring"), 
  pull_twfe(all_models[["narrow"]][["twfe_narr_gro"]],   "Annual Growth rate (winz)",         "Excluding Neighboring"),
  pull_csdid(all_models[["narrow"]][["cs_narr_vac"]],    "Avg. New Addresses per Tract",      "Excluding Neighboring"),
  pull_csdid(all_models[["narrow"]][["cs_narr_log"]],    "Log Avg. New Addresses",            "Excluding Neighboring"), 
  pull_csdid(all_models[["narrow"]][["cs_narr_gro"]],    "Annual Growth rate (winz)",         "Excluding Neighboring"),
  pull_mc(all_models[["narrow"]][["mc_narr_vac"]],    "Avg. New Addresses per Tract",      "Excluding Neighboring"),
  pull_mc(all_models[["narrow"]][["mc_narr_log"]],    "Log Avg. New Addresses",            "Excluding Neighboring"), 
  pull_mc(all_models[["narrow"]][["mc_narr_gro"]],    "Annual Growth rate (winz)",         "Excluding Neighboring")
)

################################################################
## 7.  Display table & plot ------------------------------------
################################################################
results_tbl %>%
  arrange(Outcome)

# Order & clean labels specifically for plotting
plot_tbl <- results_tbl %>% 
  mutate(
    Method  = factor(Method, levels = c("TWFE","CSDID","MC")),
    Method  = fct_recode(Method,
                         "TWFE"  = "TWFE",
                         "CSDID" = "CSDID",
                         "MC"    = "MC"),
    Outcome = factor(Outcome,
                     levels = c("Avg. New Addresses per Tract",
                                "Log Avg. New Addresses",
                                "Annual Growth rate (winz)"),
                     labels = c("New Addresses",
                                "Log(New Addresses)",
                                "Growth Rate")
                     # labels = c("Average New Addresses per Tract",
                     #            "Log Average New Addresses",
                     #            "Annual Growth Rate (winsorized)")
    ),
    Sample  = factor(Sample,
                     levels = c("All Tracts","Excluding Neighboring"),
                     labels = c("All Tracts","Excluding Neighboring Tracts")
    )
  )

# Black-and-white, shape/linetype-only encoding
gg_est <- ggplot(
  plot_tbl,
  aes(x = Method,
      y = estimate,
      ymin = conf.low,
      ymax = conf.high,
      # linetype = Method,
      shape = Sample)  # distinguish estimators with line pattern
) +
  geom_hline(yintercept = 0, linewidth = 0.4, linetype = "dashed") +
  geom_vline(xintercept = c(1.5, 2.5),
             color = "grey60", linetype = "solid", linewidth = 0.3) +
  geom_errorbar(
    position = position_dodge(width = 0.6),
    width = 0.1,
    linewidth = 0.4,
    color = "black"
  ) +
  geom_point(
    position = position_dodge(width = 0.6),
    size = 1.5,
    stroke = 0.7,
    color = "black",
    fill = "white"
  ) +
  facet_grid(Outcome ~ ., switch = "y", scales = "free_y") +
  scale_x_discrete(labels = c(
    TWFE  = "Two-Way Fixed Effects",
    CSDID = "Callaway–Sant’Anna DID",
    MC    = "Matrix Completion"
  )) +
  scale_shape_manual(values = c(16, 17)) +          # solid circle, solid triangle
  # scale_linetype_manual(values = c("solid","dotted","dotdash")) +
  # labs(
  #   title = "Estimated Average Treatment Effects in the Post-treatment Period",
  #   subtitle = "Points show estimates; bars show 95% confidence intervals",
  #   x = "Estimator",
  #   y = "Estimate (95% Confidence Interval)"
  # ) +
  labs(
    title = "",
    subtitle = "",
    x = "Estimator",
    y = "Estimate (95% Confidence Interval)"
  ) +
  theme_bw(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    strip.background = element_blank(),
    strip.text.y = element_text(angle = 90,      # keep horizontal (use 90 if you want vertical text)
                                hjust = 0.5,    # center horizontally within the strip
                                vjust = 0.5,     # center vertically within the strip
                                , face = "bold"),
    legend.position = "bottom",
    legend.title = element_blank(),
    plot.title = element_text(face = "bold")
  )

gg_est

# ---- Paths and size (adjust as needed) ----

w_in <- 9; h_in <- 6  # target figure size in inches

# ---- A) Vector PDF (recommended) ----
# Uses Cairo to embed fonts properly and avoid Type 3 fonts.
ggplot2::ggsave(
  filename = file.path(path_output, "estimator_sample_outcome_comparison.pdf"),
  plot     = gg_est,
  device   = grDevices::cairo_pdf,
  width    = w_in, height = h_in, units = "in"
)

ggplot2::ggsave(
  filename = file.path(path_output, "estimator_sample_outcome_comparison.png"),
  plot     = gg_est,
  dpi      = 600,
  width    = w_in, height = h_in, units = "in",
  bg       = "transparent"
)
#####################

setwd(path_output)
results_tbl <- results_tbl %>%
  mutate(estimate = if_else(Outcome == "Avg. New Addresses per Tract", round(estimate,2), round(estimate,5)),
         std.error = if_else(Outcome == "Avg. New Addresses per Tract", round(std.error,2), round(std.error,5)),
         conf.low = if_else(Outcome == "Avg. New Addresses per Tract", round(conf.low,2), round(conf.low,5)),
         conf.high = if_else(Outcome == "Avg. New Addresses per Tract", round(conf.high,2), round(conf.high,5))) %>%
  arrange(Outcome, Sample) %>%
  select(Outcome, Sample, Method, estimate, std.error)

write_xlsx(results_tbl, path = "Alternative Estimator table.xlsx")

