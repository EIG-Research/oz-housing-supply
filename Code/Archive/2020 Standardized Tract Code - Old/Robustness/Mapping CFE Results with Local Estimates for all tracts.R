# HUD Aggregated USPS Administrative Data on Vacancies
# Ben Glasner 1/15/2025
# Testing code

rm(list = ls())
options(scipen = 999)
set.seed(42)

###########################
###   Load Packages     ###
###########################
library(dplyr)
library(sf)
library(leaflet)
library(tigris)
library(purrr)
library(RColorBrewer)
options(tigris_use_cache = TRUE, tigris_class = "sf")

library(htmlwidgets)

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

path_project <- project_directories[[current_user]]
path_data <- file.path(path_project, "data")
path_data_USPS <- file.path(path_project, "data/2010 Census Tract Summary Files")
path_data_tract <- file.path(path_project, "data/Tract Characteristics")
path_output <- file.path(path_project, "output")

#################
### Data load ###
#################
setwd(path_output)
load("CFE_FECT_Effect_Estimates__and_NO_SE_ALL_MAPPING_DATA.RData")  # Loads `Current_effect`

#########################
### Prepare Geometry  ###
#########################

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
  select(GEOID, STATEFP, NAME, geometry) %>%
  inner_join(Current_effect, by = "GEOID") 

#########################
### State Effects     ###
#########################

state <- tigris::states(year = 2022) %>%
  as.data.frame() %>%
  select(STATEFP,NAME,REGION,DIVISION)

State_effect <- tract_data %>% 
  as.data.frame() %>%
  select(STATEFP,`Effect on Total Addresses`) %>%
  group_by(STATEFP) %>%
  summarise(`Effect on Total Addresses` = sum(`Effect on Total Addresses`, na.rm = TRUE)) %>%
  left_join(state)

##########################
### Create Leaflet Map ###
##########################
# Custom diverging palette centered at zero
custom_palette <- colorRampPalette(c(
  "#d73027", "#fdae61", "#fee090",    # deep red → orange → light yellow
  "#e0f3f8",                          # light grey for near-zero
  "#e0f3f8", "#91bfdb", "#4575b4",    # light blue → sky blue → rich blue
  "#74c476", "#006d2c"     # light green → forest → dark green
))

# Generate bins symmetric around 0
effect_max <- max(tract_data$`Effect on Total Addresses`, na.rm = TRUE)
effect_min <- min(tract_data$`Effect on Total Addresses`, na.rm = TRUE)

# breaks <- seq(effect_min, effect_max, length.out = 9)
breaks <- c(effect_min,-300, -50, -25, 0 , 25, 50, 300, 1000, 3000, 5000, effect_max)

pal <- colorBin(
  palette = custom_palette(6),  # 6 transitions for 7 bins
  domain = tract_data$`Effect on Total Addresses`,
  bins = breaks,
  na.color = "transparent"
)

# Preserve the original tract polygons
tract_polygons <- tract_data  # full geometry intact

# Calculate centroids for each tract
tract_points <- tract_data %>%
  st_centroid() %>%
  st_transform(crs = 4326) %>%
  mutate(
    lng = st_coordinates(.)[, 1],
    lat = st_coordinates(.)[, 2]
  )

treated_points <- tract_data %>%
  filter(Treated == 1) %>%
  st_centroid() %>%
  st_transform(crs = 4326) %>%
  mutate(
    lng = st_coordinates(.)[, 1],
    lat = st_coordinates(.)[, 2]
  )

# Create leaflet map with both layers
my_map <- leaflet() %>%
  setView(lng = -98.5, lat = 39.8, zoom = 4) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  
  # Add all tracts layer
  addCircleMarkers(data = tract_points,
                   lng = ~lng,
                   lat = ~lat,
                   radius = ~sqrt(abs(`Effect on Total Addresses`)) * 0.3,
                   color = ~pal(`Effect on Total Addresses`),
                   stroke = TRUE,
                   weight = 0.5,
                   fillOpacity = 0.8,
                   popup = ~paste0(
                     "<strong>Tract: </strong>", NAME, "<br/>",
                     "<strong>GEOID: </strong>", GEOID, "<br/>",
                     "<strong>Effect: </strong>", round(`Effect on Total Addresses`, 1)
                   ),
                   group = "All Tracts") %>%
  
  # Add treated tracts layer
  addCircleMarkers(data = treated_points,
                   lng = ~lng,
                   lat = ~lat,
                   radius = ~sqrt(abs(`Effect on Total Addresses`)) * 0.3,
                   color = ~pal(`Effect on Total Addresses`),
                   stroke = TRUE,
                   weight = 0.5,
                   fillOpacity = 0.8,
                   popup = ~paste0(
                     "<strong>Tract: </strong>", NAME, "<br/>",
                     "<strong>GEOID: </strong>", GEOID, "<br/>",
                     "<strong>Effect: </strong>", round(`Effect on Total Addresses`, 1)
                   ),
                   group = "Treated Tracts") %>%
  
  # Add legend once
  addLegend(pal = pal,
            values = tract_points$`Effect on Total Addresses`,
            title = "Effect on Total Addresses",
            position = "bottomright") %>%
  
  # Add toggle controls
  addLayersControl(
    overlayGroups = c("All Tracts", "Treated Tracts"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  
  # Optional: show only one layer initially
  hideGroup("Treated Tracts")


saveWidget(my_map, file = file.path(path_output, "tract_effect_map_CFE_ALL.html"), selfcontained = TRUE)
