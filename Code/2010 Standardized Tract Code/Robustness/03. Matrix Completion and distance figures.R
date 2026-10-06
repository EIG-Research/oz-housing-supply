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

library(tidyr)
library(dplyr)
library(ggplot2)
library(scales)

library(tigris)
library(purrr)

library(openxlsx)
library(readr)
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
setwd(path_output)
load(file = "foundational_models.RData")

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
         `Type tract`, Designation,Designation_category_detailed,dist_km_to_treated,LIC_oz_neighbor,,
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
  mutate(
         `current median income decile` = ntile(median_income, 10),
         `current poverty rate decile` = ntile(poverty_rate, 10),
         `current solo detached decile` = ntile(solo_detached_housing_share, 10),
         `current zoning index decile` = ntile(Zoning_Index, 10)
         )

pre_treatment_ch<- USPS_data %>% filter(date == "2017-12-01")

###############################
### MC results in long form ###
###############################
# MC effect on addresses
# mc_effect <- all_models[["broad"]][["mc_narr_vac"]]
mc_effect <- all_models[["narrow"]][["mc_narr_vac"]]

# Step 4: Extract and format effect estimates
Effect <- mc_effect[["eff"]]
# Transpose and convert to data frame
Effect <- t(Effect)
Effect <- as.data.frame(Effect)
colnames(Effect) <- paste("Period:", seq_len(ncol(Effect)))
Effect <- cbind(id = rownames(Effect), Effect)

Effect <- Effect %>%
  pivot_longer(
    cols = starts_with("Period:"),
    names_to = "Period",
    values_to = "Effect on Total Addresses"
  ) %>%
  mutate(`Average S.E.` = mc_effect[["est.avg"]][[2]],
         Upper = `Effect on Total Addresses` + `Average S.E.` * 1.96,
         Lower = `Effect on Total Addresses` - `Average S.E.` * 1.96,
         Significant = case_when(
           (Lower > 0 & Upper > 0) | (Lower < 0 & Upper < 0) ~ 1,
           (pmin(Lower, Upper) <= 0 & pmax(Lower, Upper) >= 0) ~ 0,
           TRUE ~ NA_real_
         ))  %>% 
  mutate(Period_num = stringr::str_sub(Period ,-2, -1)) %>% 
  filter(Period_num == max(Period_num))


######################
# Join pre-treatment data to Effect data

Effect <- Effect %>%
  mutate(id = as.double(id)) %>% 
  left_join(pre_treatment_ch)  %>% 
  mutate(Treated = if_else(Designation_category_detailed == "LIC selected", 1, 0))

#############################
### Distance plot (sig only)
#############################

km_target <- 15          # max radius to display
step <- 0.2              # 0.2-km bins

# --- Denominator: treated total from MC (SIGNIFICANT ONLY) ---
treated_total_mc_sig <- Effect %>%
  filter(Designation_category_detailed == "LIC selected",
         Significant == 1) %>%
  summarise(Total = sum(`Effect on Total Addresses`, na.rm = TRUE)) %>%
  pull()

# --- Bin helper (nearest 0.2 km) ---
bin_fun <- function(x) round(x * (1/step)) / (1/step)

# --- Bars/bins for SIGNIFICANT-ONLY universe ---
bars_df <- Effect %>%
  filter(Significant == 1,
         dist_km_to_treated <= km_target) %>%
  mutate(
    dist_bin_km = bin_fun(dist_km_to_treated),
    dist_bin_km = round(dist_bin_km, 1)
  ) %>%
  group_by(Designation_category_detailed, dist_bin_km) %>%
  summarise(Effect = sum(`Effect on Total Addresses`, na.rm = TRUE), .groups = "drop")

# --- Ensure the 0.0-km bin exists and equals the treated significant total ---
has_zero <- any(abs(bars_df$dist_bin_km - 0) < 1e-9)
if (!has_zero) {
  bars_df <- bind_rows(
    tibble(Designation_category_detailed = "LIC selected",
           dist_bin_km = 0,
           Effect = treated_total_mc_sig),
    bars_df
  )
} else {
  # If 0 bin exists, force its treated value to match the denominator exactly
  bars_df <- bars_df %>%
    mutate(Effect = ifelse(Designation_category_detailed == "LIC selected" &
                             abs(dist_bin_km - 0) < 1e-9,
                           treated_total_mc_sig, Effect))
}

# --- Inclusive cumulative totals (treated at 0 km + neighbors), sig-only ---
cum_all_sig <- bars_df %>%
  group_by(dist_bin_km) %>%
  summarise(total_Effect = sum(Effect), .groups = "drop") %>%
  arrange(dist_bin_km) %>%
  mutate(
    cum_inclusive = cumsum(total_Effect),
    Index = 100 * cum_inclusive / treated_total_mc_sig   # baseline = 100 at 0 km
  )

cum_lic_sig <- bars_df %>%
  filter(Designation_category_detailed %in% c(
    "LIC selected","LIC not selected, not a border tract","LIC not selected, border tract"
  )) %>%
  group_by(dist_bin_km) %>%
  summarise(total_Effect = sum(Effect), .groups = "drop") %>%
  arrange(dist_bin_km) %>%
  mutate(
    cum_inclusive = cumsum(total_Effect),
    Index = 100 * cum_inclusive / treated_total_mc_sig
  )

# --- Sanity check at 5 km (or nearest bin to km_target_check) ---
km_target_check <- 5
bin_check <- cum_all_sig$dist_bin_km[which.min(abs(cum_all_sig$dist_bin_km - km_target_check))]
idx_km <- cum_all_sig %>% filter(dist_bin_km == bin_check) %>% pull(Index)
print(idx_km)

# --- Plot (sig-only universe, indexed) ---
line_cols <- c("Undesignated LIC + ineligible (inclusive)" = "#194F8B",
               "LIC only (inclusive)"                       = "#39274F")

p <- ggplot() +
  geom_hline(yintercept = 100, color = "grey40") +
  geom_line(data = cum_all_sig,
            aes(x = dist_bin_km, y = Index, color = "Undesignated LIC + ineligible (inclusive)"),
            linewidth = 1) +
  geom_point(data = cum_all_sig,
             aes(x = dist_bin_km, y = Index, color = "Undesignated LIC + ineligible (inclusive)"),
             size = 1.5) +
  geom_line(data = cum_lic_sig,
            aes(x = dist_bin_km, y = Index, color = "LIC only (inclusive)"),
            linewidth = 1) +
  geom_point(data = cum_lic_sig,
             aes(x = dist_bin_km, y = Index, color = "LIC only (inclusive)"),
             size = 1.5) +
  scale_x_continuous(breaks = scales::pretty_breaks(),
                     name = sprintf("Distance to nearest Opportunity Zone (km, %.1f-km bins)", step)) +
  scale_y_continuous(labels = scales::label_number(accuracy = 1),
                     name = "Indexed cumulative total (treated MC significant = 100 at 0 km)") +
  scale_color_manual(values = line_cols, name = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        panel.grid.major.x = element_blank())

print(p)

# ---- Datawrapper exports (sig-only, indexed + absolute) ----
plot_wide_idx <- cum_all_sig %>%
  select(dist_bin_km,
         `Undesignated LIC and ineligible` = Index) %>%
  full_join(
    cum_lic_sig %>% select(dist_bin_km, `Undesignated LIC` = Index),
    by = "dist_bin_km"
  ) %>%
  arrange(dist_bin_km)

plot_wide_abs <- cum_all_sig %>%
  select(dist_bin_km,
         `Undesignated LIC and ineligible` = cum_inclusive) %>%
  full_join(
    cum_lic_sig %>% select(dist_bin_km, `Undesignated LIC` = cum_inclusive),
    by = "dist_bin_km"
  ) %>%
  arrange(dist_bin_km)

readr::write_csv(plot_wide_idx, file.path(path_output, "distance_profile_indexed_wide_sig.csv"))
readr::write_csv(plot_wide_abs, file.path(path_output, "distance_profile_cumulative_wide_sig.csv"))

openxlsx::write.xlsx(
  list(indexed_sig = plot_wide_idx, cumulative_absolute_sig = plot_wide_abs),
  file.path(path_output, "distance_profile_wide_sig.xlsx"),
  overwrite = TRUE
)

# ---- Ratio and message (sig-only numerator & denominator) ----
# Denominator: treated significant total
Total_effect_among_treated_MC_sig <- treated_total_mc_sig

# Numerator: inclusive cumulative (sig-only) at target radius
bin_target <- cum_all_sig$dist_bin_km[which.min(abs(cum_all_sig$dist_bin_km - km_target))]
Total_effect_within_target_sig <- cum_all_sig %>%
  filter(dist_bin_km == bin_target) %>%
  pull(cum_inclusive)

ratio_sig <- Total_effect_within_target_sig / Total_effect_among_treated_MC_sig

# Your CSDID treated-only total (kept as-is; change if you also want a sig-only CSDID)
Total_effect_among_treated_CSDID <- 416287
# -------- Multi-radius summary (2 km, 5 km, 15 km) --------
radii <- c(2, 5, 15)

summ <- purrr::map_dfr(radii, function(r) {
  bin_r <- cum_all_sig$dist_bin_km[which.min(abs(cum_all_sig$dist_bin_km - r))]
  row_r <- cum_all_sig %>% dplyr::filter(dist_bin_km == bin_r)
  cum_r <- row_r$cum_inclusive
  ratio_r <- cum_r / Total_effect_among_treated_MC_sig
  scaled_r <- ratio_r * Total_effect_among_treated_CSDID
  tibble::tibble(
    radius_km = r,
    bin_km    = bin_r,
    ratio_pct = round(100 * ratio_r, 1),
    scaled    = round(scaled_r),
    index     = round(row_r$Index, 1)
  )
})

# Print a clean summary
cat(
  "Inclusive additionality summary (significant-only universe)\n",
  "Baseline treated CSDID total:", prettyNum(Total_effect_among_treated_CSDID, big.mark=","), "\n",
  "Baseline treated MC total (sig-only):", prettyNum(round(Total_effect_among_treated_MC_sig), big.mark=","), "\n\n",
  paste0(
    "- Within ", sprintf("%.1f", summ$radius_km), " km (bin ", sprintf("%.1f", summ$bin_km), "): ",
    summ$ratio_pct, "% ratio, indexed ", summ$index,
    ", scaled total = ", prettyNum(summ$scaled, big.mark=",")),
  sep = "\n"
)
cat(message, "\n")



#############################
### Heterogeneity estimates
#############################

Effect_heterogeneity_zoning <- Effect %>%
  group_by(`current zoning index decile`, Treated) %>%
  summarize(`Total Effect on Total Addresses` = sum(Significant*`Effect on Total Addresses`),
            `avg Effect on Total Addresses` = mean(Significant*`Effect on Total Addresses`)) %>%
  na.omit() %>%
  arrange(Treated, `current zoning index decile`) %>%
  filter(Treated == 1)
  

Effect_heterogeneity_income <- Effect %>%
  group_by(`current median income decile`, Treated) %>%
  summarize(`Total Effect on Total Addresses` = sum(Significant*`Effect on Total Addresses`),
            `avg Effect on Total Addresses` = mean(Significant*`Effect on Total Addresses`)) %>%
  na.omit() %>%
  arrange(Treated, `current median income decile`) %>%
  filter(Treated == 1)


Effect_heterogeneity_poverty <- Effect %>%
  group_by(`current poverty rate decile`, Treated) %>%
  summarize(`Total Effect on Total Addresses` = sum(Significant*`Effect on Total Addresses`),
            `avg Effect on Total Addresses` = mean(Significant*`Effect on Total Addresses`)) %>%
  na.omit() %>%
  arrange(Treated, `current poverty rate decile`) %>%
  filter(Treated == 1)


Effect_heterogeneity_solo_detached <- Effect %>%
  group_by(`current solo detached decile`, Treated) %>%
  summarize(`Total Effect on Total Addresses` = sum(Significant*`Effect on Total Addresses`),
            `avg Effect on Total Addresses` = mean(Significant*`Effect on Total Addresses`)) %>%
  na.omit() %>%
  arrange(Treated, `current solo detached decile`) %>%
  filter(Treated == 1)

