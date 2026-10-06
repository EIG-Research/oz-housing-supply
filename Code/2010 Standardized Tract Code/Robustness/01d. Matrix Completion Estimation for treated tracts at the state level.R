# HUD Aggregated USPS Administrative Data on Vacancies
# Parallel shards (RStudio sessions) + merge
# Ben Glasner — adapted for sharding

rm(list = ls())
options(scipen = 999)
set.seed(42)

# (Windows) Raise process priority (ok to keep)
pid <- Sys.getpid()
try({
  shell(
    sprintf('powershell.exe "Get-Process -Id %s | ForEach-Object { $_.PriorityClass = \'High\' }"', pid),
    intern = FALSE
  )
}, silent = TRUE)

###########################
###   Load Packages     ###
###########################
# devtools::install_github("xuyiqing/fect")
library(fect)          # https://yiqingxu.org/packages/fect/01-start.html
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
library(stringr)
# Optional: fast IO (uncomment if you want feather/parquet)
# library(arrow)

############################
### Shard configuration  ###
############################
# Configure via environment variables so each RStudio session can be set differently
SHARD_ID   <- as.integer(Sys.getenv("SHARD_ID",   unset = "1"))     # 1..N_SHARDS
N_SHARDS   <- as.integer(Sys.getenv("N_SHARDS",   unset = "5"))
FECT_CORES <- as.integer(Sys.getenv("FECT_CORES", unset = "2"))     # cores per session
# MODE       <- Sys.getenv("MODE", unset = "compute")                 # "compute" or "merge"
MODE       <- Sys.getenv("MODE", unset = "merge")                 # "compute" or "merge"

if (is.na(SHARD_ID) || is.na(N_SHARDS) || SHARD_ID < 1 || SHARD_ID > N_SHARDS) {
  stop("Invalid SHARD_ID / N_SHARDS. Set Sys.setenv(SHARD_ID='1..N', N_SHARDS='N').")
}

#################
### Set paths ###
#################
project_directories <- list(
  "name" = "PATH TO GITHUB REPO",
  "Benjamin Glasner" = "C:/Users/Benjamin Glasner/EIG Dropbox/Benjamin Glasner/GitHub/oz-housing-supply",
  "bngla" = "C:/Users/bngla/EIG Dropbox/Benjamin Glasner/GitHub/oz-housing-supply",
  "Research" = "C:/Users/Research/EIG Dropbox/Benjamin Glasner/GitHub/oz-housing-supply"
)

current_user <- Sys.info()[["user"]]
if (!current_user %in% names(project_directories)) {
  stop("Root folder for current user is not defined.")
}

path_project      <- project_directories[[current_user]]
path_data         <- file.path(path_project, "data")
path_data_USPS    <- file.path(path_project, "data/2010 Census Tract Summary Files")
path_data_tract   <- file.path(path_project, "data/Tract Characteristics")
path_output       <- file.path(path_project, "output")
path_state_chk    <- file.path(path_output, "fect_state_results")  # per-state checkpoints
path_shard_rollup <- file.path(path_output, "fect_shards")         # per-shard rollups
path_logs         <- file.path(path_output, "fect_logs")

dir.create(path_output,      showWarnings = FALSE, recursive = TRUE)
dir.create(path_state_chk,   showWarnings = FALSE, recursive = TRUE)
dir.create(path_shard_rollup,showWarnings = FALSE, recursive = TRUE)
dir.create(path_logs,        showWarnings = FALSE, recursive = TRUE)

#################
### Data load ###
#################
setwd(path_data)
load(file = "USPS_tract_vacancy.RData")

##################
### Data clean ###
##################
USPS_data <- USPS_data %>%
  filter(Designation_category_detailed %in% c(
    "LIC selected",
    "LIC not selected, not a border tract",
    "Ineligible, not a border tract"
  )) %>%
  filter(YEAR >= 2015) %>%
  mutate(
    id = as.numeric(geoid),
    time = dense_rank(date),
    Designation = if_else(`OZ Designation` == 1 & date >= "2018-03-01", 1L, 0L),
    Total_residential = ACTIVE_RESIDENTIAL_ADDRESSES + STV_RESIDENTIAL_ADDRESSES + LTV_RESIDENTIAL_ADDRESSES
  ) %>%
  select(
    id, geoid, time, YEAR, date, STATEFP, `Type tract`, Designation, LIC_oz_neighbor,
    poverty_rate, median_income, solo_detached_housing_share, Zoning_Index, Total_residential
  ) %>%
  distinct(id, time, .keep_all = TRUE) %>%
  group_by(id) %>% mutate(number_of_quarters_obs = n()) %>% ungroup()

max_quarter <- max(USPS_data$number_of_quarters_obs)
USPS_data   <- USPS_data %>% filter(number_of_quarters_obs == max_quarter)

USPS_data <- USPS_data %>%
  group_by(YEAR) %>%
  mutate(
    `current median income decile`   = ntile(median_income, 10),
    `current poverty rate decile`    = ntile(poverty_rate, 10),
    `current solo detached decile`   = ntile(solo_detached_housing_share, 10),
    `current zoning index decile`    = ntile(Zoning_Index, 10)
  ) %>%
  ungroup()

###############################
### Setup model components  ###
###############################
outcome_var        <- "Total_residential"
treatment_var      <- "Designation"
controls_allof     <- c("poverty_rate","median_income","solo_detached_housing_share")
conditional_allof  <- c("current median income decile", "current poverty rate decile",
                        "current solo detached decile","current zoning index decile")
control_vars       <- paste(controls_allof, collapse = " + ")
current_formula    <- as.formula(paste(outcome_var, "~", treatment_var, "+", control_vars))

USPS_data <- USPS_data %>%
  select(
    id, geoid, STATEFP, time, YEAR, date,
    all_of(outcome_var), all_of(treatment_var),
    all_of(controls_allof), all_of(conditional_allof)
  ) %>%
  na.omit()

# Balanced panel only
balanced_ids <- USPS_data %>%
  group_by(id) %>% summarise(n_time = n_distinct(time), .groups = "drop") %>%
  filter(n_time == n_distinct(USPS_data$time)) %>% pull(id)
USPS_data <- USPS_data %>% filter(id %in% balanced_ids)

# Never-treated ids as global controls
never_treated_ids <- USPS_data %>%
  group_by(id) %>% summarise(ever_treated = max(.data[[treatment_var]]), .groups = "drop") %>%
  filter(ever_treated == 0) %>% pull(id)

# States with at least one treated tract at last period (global set)
treated_states_all <- USPS_data %>%
  filter(time == max(time), .data[[treatment_var]] == 1) %>%
  distinct(STATEFP) %>% arrange(STATEFP) %>% pull(STATEFP)

# Deterministic sharding over states
state_index <- seq_along(treated_states_all)
assigned_shard <- ((state_index - 1) %% N_SHARDS) + 1L
states_this_shard <- treated_states_all[assigned_shard == SHARD_ID]

#########################
### fect runner fn    ###
#########################
run_and_collect <- function(method, selected_data, st, fect_cores = 2L) {
  args <- list(
    formula   = current_formula,
    data      = selected_data,
    na.rm     = TRUE,
    index     = c("id", "time"),
    force     = "two-way",
    r         = c(0, 5),
    CV        = TRUE,
    method    = method,
    se        = TRUE,
    nboots    = 100,
    alpha     = 0.05,
    parallel  = TRUE,
    cores     = fect_cores,
    seed      = 42,
    min.T0    = 5,
    normalize = TRUE
  )
  out <- do.call(fect, args)
  
  # eff: rows=periods, cols=treated units -> transpose to rows=units
  eff_df <- as.data.frame(t(out[["eff"]]))
  eff_df <- tibble::rownames_to_column(eff_df, var = "id")
  colnames(eff_df)[-1] <- paste0("Period: ", seq_len(ncol(eff_df) - 1))
  
  effect_long <- eff_df %>%
    mutate(id = as.numeric(id)) %>%
    pivot_longer(starts_with("Period: "),
                 names_to = "Period", values_to = "Effect on Total Addresses") %>%
    filter(id %in% unique(selected_data$id[selected_data$STATEFP == st &
                                             selected_data$YEAR > 2017 &
                                             selected_data[[treatment_var]] == 1])) %>%
    mutate(
      `Average S.E.` = out[["est.avg"]][[2]],
      Upper          = `Effect on Total Addresses` + `Average S.E.` * 1.96,
      Lower          = `Effect on Total Addresses` - `Average S.E.` * 1.96,
      Significant    = case_when(
        (Lower > 0 & Upper > 0) | (Lower < 0 & Upper < 0) ~ 1,
        (pmin(Lower, Upper) <= 0 & pmax(Lower, Upper) >= 0) ~ 0,
        TRUE ~ NA_real_
      ),
      STATEFP = st
    )
  
  effect_long
}

########################################
### COMPUTE mode (run per shard)     ###
########################################
if (MODE == "compute") {
  
  if (length(states_this_shard) == 0L) {
    message(sprintf("Shard %d/%d has no assigned states. Nothing to do.", SHARD_ID, N_SHARDS))
    quit(save = "no")
  }
  
  pb <- progress_bar$new(
    format = sprintf("Shard %d/%d :state [:bar] :elapsed | ETA: :eta", SHARD_ID, N_SHARDS),
    total  = length(states_this_shard),
    width  = 70
  )
  
  shard_results <- vector("list", length(states_this_shard))
  names(shard_results) <- states_this_shard
  log_file <- file.path(path_logs, sprintf("shard_%02d.log", SHARD_ID))
  
  for (i in seq_along(states_this_shard)) {
    st <- states_this_shard[i]
    pb$tick(tokens = list(state = st))
    
    state_chk_file <- file.path(path_state_chk, sprintf("fect_state_%s.rds", st))
    if (file.exists(state_chk_file)) {
      # Already computed; keep for shard rollup later
      shard_results[[i]] <- tryCatch(readRDS(state_chk_file), error = function(e) NULL)
      next
    }
    
    # Build state dataset: all treated in st + all never-treated everywhere
    treated_ids <- USPS_data %>%
      filter(STATEFP == st, time == max(time), .data[[treatment_var]] == 1) %>%
      pull(id)
    
    if (length(treated_ids) == 0L) {
      # Defensive: skip if no treated ids remain after filters
      write(paste(Sys.time(), "SKIP (no treated):", st), file = log_file, append = TRUE)
      next
    }
    
    temp_data <- USPS_data %>%
      filter(id %in% c(treated_ids, never_treated_ids)) %>%
      group_by(id) %>% arrange(time, .by_group = TRUE) %>%
      mutate(
        time_org = time,
        time     = row_number(),
        !!treatment_var := if_else(id %in% treated_ids & YEAR > 2017, 1L, 0L)
      ) %>% ungroup()
    
    # Run fect and collect
    res <- tryCatch({
      out_df <- run_and_collect(method = "mc", selected_data = temp_data, st = st, fect_cores = FECT_CORES)
      
      # Quick running print 
      suppressWarnings({
        avg.effect <- out_df %>%
          filter(Period == "Period: 41", Significant == 1) %>%
          summarise(`Average Effect` = mean(`Effect on Total Addresses`, na.rm = TRUE))
        print(avg.effect)
      })
      
      # Save checkpoint per state (atomic write)
      tmp_file <- paste0(state_chk_file, ".tmp")
      saveRDS(out_df, tmp_file)
      file.rename(tmp_file, state_chk_file)
      
      out_df
    }, error = function(e) {
      write(paste(Sys.time(), "ERROR:", st, "->", conditionMessage(e)), file = log_file, append = TRUE)
      NULL
    })
    
    shard_results[[i]] <- res
    rm(temp_data, res); gc()
  }
  
  # Save per-shard rollup (bind all available)
  shard_bind <- shard_results %>%
    compact() %>%
    bind_rows()
  
  shard_file <- file.path(path_shard_rollup, sprintf("fect_shard_%02d.rds", SHARD_ID))
  saveRDS(shard_bind, shard_file)
  
  # Optional CSV for sanity checks
  csv_file <- file.path(path_shard_rollup, sprintf("fect_shard_%02d.csv", SHARD_ID))
  suppressWarnings(try(readr::write_csv(shard_bind, csv_file), silent = TRUE))
  
  message(sprintf("Shard %d complete. Wrote: %s and %s", SHARD_ID, shard_file, csv_file))
  quit(save = "no")
}

########################################
### MERGE mode (run after all shards) ###
########################################
if (MODE == "merge") {
  # Prefer state-level checkpoints (most granular + restart safe)
  state_files <- list.files(path_state_chk, pattern = "^fect_state_.*\\.rds$", full.names = TRUE)
  if (length(state_files) == 0L) {
    # Fallback to shard rollups
    state_files <- list.files(path_shard_rollup, pattern = "^fect_shard_.*\\.rds$", full.names = TRUE)
    if (length(state_files) == 0L) stop("No shard or state checkpoint files found to merge.")
  }
  
  All_estimates <- state_files %>% map_dfr(readRDS)
  
  # Final outputs
  final_rds  <- file.path(path_output, "MC_FECT_Effect_Estimates_merged.rds")
  final_csv  <- file.path(path_output, "MC_FECT_Effect_Estimates_merged.csv")
  
  saveRDS(All_estimates, final_rds)
  suppressWarnings(try(readr::write_csv(All_estimates, final_csv), silent = TRUE))
  
  message("Merged results saved to:")
  message(" - ", final_rds)
  message(" - ", final_csv)
  
  # Quick running print 
  suppressWarnings({
    avg.effect <- All_estimates %>%
      filter(Period == "Period: 41", Significant == 1) %>%
      summarise(`Average Effect` = mean(`Effect on Total Addresses`, na.rm = TRUE))
    print(avg.effect)
  })
  
  # If you want parquet/feather:
  # arrow::write_parquet(All_estimates, file.path(path_output, "MC_FECT_Effect_Estimates_merged.parquet"))
  # arrow::write_feather(All_estimates,  file.path(path_output, "MC_FECT_Effect_Estimates_merged.feather"))
}
states <- tigris::states(year = 2019, cb = FALSE) %>%
  as.data.frame() %>% select(-geometry)

State_effects <- All_estimates %>%
  group_by(STATEFP) %>%
  filter(Period == "Period: 41") %>%
  summarise(`Average Effect` = mean(`Effect on Total Addresses`, na.rm = TRUE)) %>%
  left_join(states)
