# HUD Aggregated USPS Administrative Data on Vacancies
# Sharded per-tract FEct (single-unit runs) + merge
# Ben Glasner — per-tract ATT/SE

rm(list = ls())
options(scipen = 999)
set.seed(42)

###########################
### Packages ##############
###########################
options(repos = c(CRAN = "https://cloud.r-project.org"))
pkg_needed <- c("fect","openxlsx","tidyr","dplyr","panelView","ggplot2","broom","lmtest",
                "sandwich","fixest","modelsummary","gt","webshot2","purrr","progress",
                "plotly","tigris","stringr","readr","sf")
to_get <- setdiff(pkg_needed, rownames(installed.packages()))
if (length(to_get)) install.packages(to_get, Ncpus = max(1L, parallel::detectCores() - 1L))
invisible(lapply(pkg_needed, function(p) suppressPackageStartupMessages(library(p, character.only = TRUE))))
options(tigris_use_cache = TRUE)

############################
### Shard configuration  ###
############################
SHARD_ID   <- as.integer(Sys.getenv("SHARD_ID",   unset = "1"))  # 1..N_SHARDS
N_SHARDS   <- as.integer(Sys.getenv("N_SHARDS",   unset = "5"))
FECT_CORES <- as.integer(Sys.getenv("FECT_CORES", unset = "2"))  # cores per session
MODE       <- Sys.getenv("MODE", unset = "compute")              # "compute" or "merge"
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
if (!current_user %in% names(project_directories)) stop("Root folder for current user is not defined.")
path_project       <- project_directories[[current_user]]
path_data          <- file.path(path_project, "data")
path_output        <- file.path(path_project, "output")
path_logs          <- file.path(path_output, "fect_logs")
path_tract_chk     <- file.path(path_output, "fect_tract_results")      # per-tract checkpoints
path_shard_rollup  <- file.path(path_output, "fect_tract_shards")       # per-shard rollups
dir.create(path_output,       showWarnings = FALSE, recursive = TRUE)
dir.create(path_logs,         showWarnings = FALSE, recursive = TRUE)
dir.create(path_tract_chk,    showWarnings = FALSE, recursive = TRUE)
dir.create(path_shard_rollup, showWarnings = FALSE, recursive = TRUE)

#################
### Data load ###
#################
setwd(path_data)
load("USPS_tract_vacancy.RData")

##################
### Data clean ###
##################
USPS_data <- USPS_data %>%
  dplyr::filter(Designation_category_detailed %in% c(
    "LIC selected","LIC not selected, not a border tract","Ineligible, not a border tract"
  )) %>%
  dplyr::filter(YEAR >= 2015) %>%
  dplyr::mutate(
    id   = as.numeric(geoid),
    time = dplyr::dense_rank(date),
    Designation = dplyr::if_else(`OZ Designation` == 1 & date >= "2018-03-01", 1L, 0L),
    Total_residential = ACTIVE_RESIDENTIAL_ADDRESSES + STV_RESIDENTIAL_ADDRESSES + LTV_RESIDENTIAL_ADDRESSES
  ) %>%
  dplyr::select(id, geoid, STATEFP, `Type tract`, time, YEAR, date, Designation,
                poverty_rate, median_income, solo_detached_housing_share, Zoning_Index,
                Total_residential) %>%
  dplyr::distinct(id, time, .keep_all = TRUE) %>%
  dplyr::group_by(id) %>% dplyr::mutate(number_of_quarters_obs = dplyr::n()) %>% dplyr::ungroup()

max_quarter <- max(USPS_data$number_of_quarters_obs)
USPS_data   <- USPS_data %>% dplyr::filter(number_of_quarters_obs == max_quarter)

USPS_data <- USPS_data %>%
  dplyr::group_by(YEAR) %>%
  dplyr::mutate(
    `current median income decile` = dplyr::ntile(median_income, 10),
    `current poverty rate decile`  = dplyr::ntile(poverty_rate, 10),
    `current solo detached decile` = dplyr::ntile(solo_detached_housing_share, 10),
    `current zoning index decile`  = dplyr::ntile(Zoning_Index, 10)
  ) %>% dplyr::ungroup()

###############################
### Model components ##########
###############################
outcome_var       <- "Total_residential"
treatment_var     <- "Designation"
controls_allof    <- c("poverty_rate","median_income","solo_detached_housing_share")
conditional_allof <- c("current median income decile","current poverty rate decile",
                       "current solo detached decile","current zoning index decile")
control_vars      <- paste(controls_allof, collapse = " + ")
current_formula   <- as.formula(paste(outcome_var, "~", treatment_var, "+", control_vars))

USPS_data <- USPS_data %>%
  dplyr::select(id, geoid, STATEFP, time, YEAR, date,
                dplyr::all_of(outcome_var), dplyr::all_of(treatment_var),
                dplyr::all_of(controls_allof), dplyr::all_of(conditional_allof)) %>%
  tidyr::drop_na()

# Balanced panel only
balanced_ids <- USPS_data %>%
  dplyr::group_by(id) %>% dplyr::summarise(n_time = dplyr::n_distinct(time), .groups = "drop") %>%
  dplyr::filter(n_time == dplyr::n_distinct(USPS_data$time)) %>% dplyr::pull(id)
USPS_data <- USPS_data %>% dplyr::filter(id %in% balanced_ids)

# Never-treated controls (global)
never_treated_ids <- USPS_data %>%
  dplyr::group_by(id) %>% dplyr::summarise(ever_treated = max(.data[[treatment_var]]), .groups = "drop") %>%
  dplyr::filter(ever_treated == 0) %>% dplyr::pull(id)

# Treated tracts at the last observed period (targets for per-tract runs)
treated_ids_all <- USPS_data %>%
  dplyr::filter(time == max(time), .data[[treatment_var]] == 1) %>%
  dplyr::distinct(id, STATEFP) %>% dplyr::arrange(id)

# Deterministic sharding over treated tracts
tract_index     <- seq_len(nrow(treated_ids_all))
assigned_shard  <- ((tract_index - 1L) %% N_SHARDS) + 1L
ids_this_shard  <- treated_ids_all$id[assigned_shard == SHARD_ID]

# Helper: null-coalesce
`%||%` <- function(a, b) if (!is.null(a)) a else b

#########################
### fect runner (1 id) ##
#########################
run_one_tract <- function(tid, fect_cores = 2L) {
  st <- USPS_data %>% dplyr::filter(id == tid) %>% dplyr::distinct(STATEFP) %>% dplyr::pull(STATEFP) %>% .[1]
  # data: this treated tract + all never-treated
  temp_data <- USPS_data %>%
    dplyr::filter(id %in% c(tid, never_treated_ids)) %>%
    dplyr::group_by(id) %>% dplyr::arrange(time, .by_group = TRUE) %>%
    dplyr::mutate(
      time_org = time,
      time     = dplyr::row_number(),
      !!treatment_var := dplyr::if_else(id == tid & YEAR > 2017, 1L, 0L)
    ) %>% dplyr::ungroup()
  
  n_pre  <- temp_data %>% dplyr::filter(id == tid, .data[[treatment_var]] == 0L) %>% nrow()
  n_post <- temp_data %>% dplyr::filter(id == tid, .data[[treatment_var]] == 1L) %>% nrow()
  
  args <- list(
    formula   = current_formula,
    data      = temp_data,
    na.rm     = TRUE,
    index     = c("id", "time"),
    force     = "two-way",
    r         = c(0, 5),
    CV        = TRUE,
    method    = "mc",
    se        = TRUE,
    vartype   = "bootstrap",
    nboots    = 100,
    alpha     = 0.05,
    parallel  = TRUE,
    cores     = fect_cores,
    seed      = 42,
    min.T0    = 5,
    normalize = TRUE,
    keep.sims = TRUE   # keep bootstrap sims for robust effect calculations
  )
  
  out <- tryCatch(do.call(fect::fect, args), error = function(e) e)
  if (inherits(out, "error")) return(NULL)
  
  # Robustly parse est.avg (single-tract run => overall ATT for this tract's post periods)
  est_avg <- out[["est.avg"]]
  if (is.null(est_avg)) return(NULL)
  
  # Handle possible shapes/names
  if (is.data.frame(est_avg)) {
    ATT <- as.numeric(est_avg$ATT %||% est_avg$att %||% est_avg[[1]][1])
    SE  <- as.numeric(est_avg$S.E. %||% est_avg$SE %||% est_avg$se %||% est_avg[[2]][1])
    L   <- as.numeric(est_avg$CI.lower %||% est_avg$lower %||% est_avg[[3]][1])
    U   <- as.numeric(est_avg$CI.upper %||% est_avg$upper %||% est_avg[[4]][1])
    P   <- as.numeric(est_avg$p.value %||% est_avg$p %||% est_avg[[5]][1])
  } else if (is.atomic(est_avg)) {
    ATT <- as.numeric(est_avg[[1]]); SE <- as.numeric(est_avg[[2]])
    L   <- as.numeric(est_avg[[3]]); U  <- as.numeric(est_avg[[4]])
    P   <- as.numeric(est_avg[[5]] %||% NA_real_)
  } else {
    return(NULL)
  }
  
  tibble::tibble(
    id = tid,
    STATEFP = st,
    ATT = ATT,
    SE  = SE,
    CI.lower = L,
    CI.upper = U,
    p.value  = P,
    n_pre = n_pre,
    n_post = n_post
  )
}

########################################
### COMPUTE mode (per-shard) ###########
########################################
if (MODE == "compute") {
  if (length(ids_this_shard) == 0L) {
    message(sprintf("Shard %d/%d has no assigned tracts. Nothing to do.", SHARD_ID, N_SHARDS))
    quit(save = "no")
  }
  
  pb <- progress::progress_bar$new(
    format = sprintf("Shard %d/%d :tract [:bar] :elapsed | ETA: :eta", SHARD_ID, N_SHARDS),
    total  = length(ids_this_shard),
    width  = 70
  )
  
  log_file <- file.path(path_logs, sprintf("tract_shard_%02d.log", SHARD_ID))
  shard_rows <- vector("list", length(ids_this_shard))
  
  for (i in seq_along(ids_this_shard)) {
    tid <- ids_this_shard[i]
    pb$tick(tokens = list(tract = tid))
    
    tract_chk <- file.path(path_tract_chk, sprintf("fect_tract_%s.rds", tid))
    if (file.exists(tract_chk)) {
      shard_rows[[i]] <- tryCatch(readRDS(tract_chk), error = function(e) NULL)
      next
    }
    
    res <- tryCatch(run_one_tract(tid, fect_cores = FECT_CORES), error = function(e) {
      write(paste(Sys.time(), "ERROR tract", tid, "->", conditionMessage(e)), file = log_file, append = TRUE)
      NULL
    })
    
    if (!is.null(res)) {
      tmp <- paste0(tract_chk, ".tmp")
      saveRDS(res, tmp); file.rename(tmp, tract_chk)
    } else {
      write(paste(Sys.time(), "SKIP/NULL result tract", tid), file = log_file, append = TRUE)
    }
    shard_rows[[i]] <- res
    rm(res); gc()
  }
  
  shard_bind <- shard_rows %>% purrr::compact() %>% dplyr::bind_rows()
  shard_file <- file.path(path_shard_rollup, sprintf("fect_tract_shard_%02d.rds", SHARD_ID))
  saveRDS(shard_bind, shard_file)
  csv_file   <- file.path(path_shard_rollup, sprintf("fect_tract_shard_%02d.csv", SHARD_ID))
  suppressWarnings(try(readr::write_csv(shard_bind, csv_file), silent = TRUE))
  
  message(sprintf("Shard %d complete. Wrote: %s and %s", SHARD_ID, shard_file, csv_file))
  quit(save = "no")
}

########################################
### MERGE mode #########################
########################################
if (MODE == "merge") {
  tract_files <- list.files(path_tract_chk, pattern = "^fect_tract_.*\\.rds$", full.names = TRUE)
  if (length(tract_files) == 0L) {
    tract_files <- list.files(path_shard_rollup, pattern = "^fect_tract_shard_.*\\.rds$", full.names = TRUE)
    if (length(tract_files) == 0L) stop("No tract or shard checkpoint files found to merge.")
  }
  
  Tract_estimates <- tract_files %>% purrr::map_dfr(readRDS)
  
  final_rds <- file.path(path_output, "MC_FECT_Tract_ATT_merged.rds")
  final_csv <- file.path(path_output, "MC_FECT_Tract_ATT_merged.csv")
  saveRDS(Tract_estimates, final_rds)
  suppressWarnings(try(readr::write_csv(Tract_estimates, final_csv), silent = TRUE))
  
  message("Merged per-tract results saved to:")
  message(" - ", final_rds)
  message(" - ", final_csv)
  
  # Optional: state summary (mean ATT of member tracts)
  states_df <- tigris::states(year = 2019, cb = FALSE) |> sf::st_drop_geometry() |>
    dplyr::mutate(STATEFP = sprintf("%02d", as.integer(STATEFP)))
  Tract_estimates <- Tract_estimates %>%
    dplyr::mutate(STATEFP = sprintf("%02d", as.integer(STATEFP)))
  State_effects <- Tract_estimates %>%
    dplyr::group_by(STATEFP) %>%
    dplyr::summarise(Mean_ATT = mean(ATT, na.rm = TRUE),
                     Mean_SE  = mean(SE,  na.rm = TRUE),
                     n_tracts = dplyr::n(), .groups = "drop") %>%
    dplyr::left_join(states_df, by = "STATEFP")
}
