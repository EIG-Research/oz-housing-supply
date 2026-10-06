# HUD Aggregated USPS Administrative Data on Vacancies
# Ben Glasner 10/29/2024

rm(list = ls())
options(scipen = 999)
set.seed(42)

###########################
###   Load Packages     ###
###########################
library(tigris)      # tract geometries
library(sf)          # spatial ops
library(dplyr)
library(purrr)       # functional mapping
library(scales)
library(ggplot2)
options(tigris_use_cache = TRUE)  # keep shapefiles on disk

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

path_output <- file.path(path_project, "output")

#################
### Data load ###
#################
setwd(path_data_tract)
using <- read.csv("2010_tracts_with_2010_qualifications.csv") %>%
  select(-X) %>%
  rename(geoid = GEOID )

#############################################
## Provide your vector of 2020 tract GEOIDs
#############################################
target_tracts <- using %>% 
  filter(Designation == "LIC selected") %>%
  select(geoid) %>%
  distinct() %>%
  pull()

#############################################
## 1.  Pull the tract shapes we’ll need
#############################################
# ---- Helper: all states represented in the input  ----------------------------
state_fips <- tigris::states() %>% as.data.frame()%>% select(GEOID)  %>% mutate(GEOID = as.numeric(GEOID)) %>% pull()

tract_list <- list()

for(i in seq_along(state_fips)){
  tract_list[[i]] <- tracts(state = state_fips[[i]], year = 2017, class = "sf", cb = TRUE)
}

tract_sf <- bind_rows(tract_list)

#############################################
## 2.  Split target vs. candidate tracts
#############################################
targets_sf <- filter(tract_sf, GEOID %in% target_tracts)               # focal tracts
cands_sf   <- filter(tract_sf, !GEOID %in% target_tracts)              # everything else

#############################################
## 3.  Identify touching geometries
#############################################
# st_touches returns an index list: i → rows of cands that touch row i of targets
touch_idx  <- st_touches(targets_sf, cands_sf)

# Un-nest, de-duplicate, and pull neighboring GEOIDs
neighbor_geoids <- unique(cands_sf$GEOID[unlist(touch_idx)])

#############################################
## 4.  Return neighbors as sf or plain table
#############################################
neighbor_sf  <- filter(cands_sf, GEOID %in% neighbor_geoids)           # sf object
neighbor_tbl <- neighbor_sf |> st_drop_geometry()                      # data-frame only

neighbor_tbl <- neighbor_tbl %>%
  mutate(geoid = as.numeric(GEOID),
         LIC_oz_neighbor = 1) %>%
  select(geoid,LIC_oz_neighbor)
#############################################
## 5.  Centroids + distance-to-treated
#############################################

# ---- 5a.  Choose a projection suitable for distance ----
# EPSG:5070 = NAD83 / Conus Albers Equal-Area – good for lower-48.
# If you have AK/HI/territories in the mix, see note below.
aeac_crs <- 5070

# ---- 5b.  Compute centroids in that CRS ----------------
tract_centroids <- tract_sf |>
  st_transform(aeac_crs) |>
  st_centroid()

targets_centroids <- tract_centroids |>
  filter(GEOID %in% target_tracts)                 # treated points only

# ---- 5c.  For every tract, find the index of the nearest treated centroid ----
nearest_idx <- st_nearest_feature(tract_centroids, targets_centroids)

# ---- 5d.  Great-circle (here: planar) distance, tract-by-tract --------------
dist_vec_m <- st_distance(
  tract_centroids,
  targets_centroids[nearest_idx, ],
  by_element = TRUE
) |>
  as.numeric()                 # strip units → plain numeric meters

# ---- 5e.  Assemble the final sf with distance field -------------------------
tract_centroids <- tract_centroids |>
  mutate(
    dist_km_to_treated = if_else(
      GEOID %in% target_tracts,
      0,                        # treated tracts get distance = 0
      dist_vec_m / 1000         # others: m → km
    )
  )

# Optional: drop geometry for a regular tibble
tract_distance_tbl <- tract_centroids |> st_drop_geometry() 

tract_distance_tbl <- tract_distance_tbl %>%
  mutate(geoid = as.numeric(GEOID)) %>%
  select(STATEFP,COUNTYFP,TRACTCE,
         geoid,dist_km_to_treated) %>%
  filter(as.numeric(STATEFP)<=56)

#############################################
## 6.  Create USPS data list of tracts that neighbor an OZ and distance to OZs
#############################################

neighbor_data <- tract_distance_tbl %>%
  left_join(neighbor_tbl) %>%
  mutate(LIC_oz_neighbor = if_else(is.na(LIC_oz_neighbor),0,LIC_oz_neighbor))

setwd(path_data_tract)
save(neighbor_data, file = "LIC_OZ_neighbor_data_2010.RData")
