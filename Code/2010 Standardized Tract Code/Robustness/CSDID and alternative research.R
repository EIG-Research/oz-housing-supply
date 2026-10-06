# Opportunity‑Zone Housing Study ‑ code completely rewritten
# to (i) estimate ONLY the effect on
#     Total_active_vacant_exclude_nostat_RESIDENTIAL
#   with a *universal* base period, and
# (ii) plot that event‑study *together* with the
#     research‑paper timeline on ONE common x‑axis.
# Ben Glasner • last edited 2025‑07‑14

rm(list = ls())
options(scipen = 999)
set.seed(42)

###########################
###   Load Packages     ###
###########################
library(tidyverse)
library(lubridate)
library(openxlsx)
library(readxl)
library(did)           # Callaway‑Sant’Anna
library(plm)
library(ggplot2)
library(scales)

library(dplyr)
library(ggrepel)
library(lubridate)

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
load("USPS_tract_vacancy.RData")   # creates USPS_data
OZ_Timeline <- read_xlsx("OZ literature summary.xlsx") |>
  mutate(across(c(`Publication Date`,
                  `Data Start Date`,
                  `Data End Date`), as.Date)) |>
  arrange(desc(`Data End Date`)) |>
  mutate(Author = factor(Author, levels = unique(Author), ordered = TRUE))

# ── USPS tract sample restrictions & variables ───────────────
USPS_data <- USPS_data |>
  filter(Designation_category_detailed %in%
           c("LIC selected",
             "LIC not selected, not a border tract",
             "Ineligible, not a border tract"),
         YEAR >= 2015) |>
  mutate(period = dense_rank(date),
         period_OZ_start = min(period[date == "2018-03-01"]),
         G      = if_else(`OZ Designation` == 1, period_OZ_start, 0),
         geoid_num = as.numeric(geoid),
         Total_active_vacant_exclude_nostat_RESIDENTIAL =
           ACTIVE_RESIDENTIAL_ADDRESSES +
           STV_RESIDENTIAL_ADDRESSES   +
           LTV_RESIDENTIAL_ADDRESSES) |>
  filter(!is.na(Total_active_vacant_exclude_nostat_RESIDENTIAL))

# ── dynamic ATT (universal base) ─────────────────────────────
controls <- c("poverty_rate", "median_income",
              "solo_detached_housing_share", "Zoning_Index")
xform    <- reformulate(controls)

analysis_data <- USPS_data |>
  select(geoid_num,
         period,
         date,
         G,
         Total_active_vacant_exclude_nostat_RESIDENTIAL,
         all_of(controls)) |>
  drop_na()

att_mod <- att_gt(yname      = "Total_active_vacant_exclude_nostat_RESIDENTIAL",
                  tname      = "period",
                  idname     = "geoid_num",
                  gname      = "G",
                  xformla    = xform,
                  base_period = "universal",
                  biters     = 1000,
                  pl         = TRUE,
                  data       = analysis_data)

dyn_att <- aggte(att_mod, type = "dynamic", alp = 0.05)

# helper df for plotting the ATT
plot_df <- tibble(
  Period = dyn_att$egt,
  date   = USPS_data |> distinct(period, date) |>
    arrange(period) |>
    mutate(period_rel = period - att_mod$group) |>
    filter(period_rel %in% dyn_att$egt) |>
    pull(date),
  ATT    = dyn_att$att.egt,
  SE     = dyn_att$se.egt,
  Lower  = ATT - dyn_att$crit.val.egt * SE,
  Upper  = ATT + dyn_att$crit.val.egt * SE)

# ── common y‑limits & timeline y‑offsets ─────────────────────
y_lim  <- range(c(plot_df$Lower, plot_df$Upper), na.rm = TRUE)
pad    <- diff(y_lim) * 0.05                 # visual gap
timeline_step <- diff(y_lim) * 0.08          # vertical spacing between authors

OZ_Timeline <- OZ_Timeline |>
  mutate(tl_y = y_lim[1] - pad - (as.numeric(Author) - 1) * timeline_step)

# ── policy event data for vlines ─────────────────────────────
events <- tibble(
  date       = as.Date(c("2017-12-20", "2018-06-14", "2019-12-19")),
  label_date = as.Date(c("2017-09-20", "2018-09-14", "2020-03-19")),
  label      = c("OZs Enacted", "OZ Map Certified", "Regs Finalized")
)

# overall ATT (simple)
overall_att <- aggte(att_mod, type = "simple")$overall.att
post_dates  <- plot_df$date[plot_df$Period >= 0]
post_start  <- min(post_dates); post_end <- max(post_dates)

# ── FINAL COMBINED GRAPH ────────────────────────────────────
minLower <- min(plot_df$Lower, na.rm = TRUE)   # helper for axis labels




# --- helpers driven by the data, not magic numbers ---
y_min_tl   <- min(OZ_Timeline$tl_y, na.rm = TRUE)
y_max_data <- max(plot_df$Upper, na.rm = TRUE)
y_min_ci   <- min(plot_df$Lower, na.rm = TRUE)
y_pad_low  <- 0.04 * (y_max_data - y_min_ci)
y_top_room <- 0.08 * (y_max_data - y_min_ci)     # space above points for labels
event_y    <- y_min_ci + 0.35 * (y_max_data - y_min_ci)  # where event labels sit

# Legend-friendly aesthetic: factor, not logical
plot_df <- plot_df %>%
  mutate(PeriodFlag = factor(ifelse(Period < 0, "Pre-Treat.", "Post-Treat."),
                             levels = c("Pre-Treat.", "Post-Treat.")))

# Position event labels slightly to the right of the line to avoid overlap
events_lbl <- events %>%
  mutate(label_x = label_date, label_y = event_y)

# A11y palette (Okabe–Ito greys for points)
pal <- c("Pre-Treat." = "grey40", "Post-Treat." = "grey10")

export_plot <-
  ggplot() +
  # 1) dynamic ATTs with CIs
  geom_linerange(
    data = plot_df,
    aes(x = date, ymin = Lower, ymax = Upper),
    linewidth = 0.5, alpha = 0.7
  ) +
  geom_point(
    data = plot_df,
    aes(x = date, y = ATT, colour = PeriodFlag, shape = PeriodFlag),
    size = 2
  ) +
  # 2) reference at y = 0
  geom_hline(yintercept = 0, colour = "black", linewidth = 0.4) +
  # 3) overall ATT (dashed) + inline label
  annotate(
    "segment",
    x = post_start, xend = post_end,
    y = overall_att, yend = overall_att,
    linetype = "22", colour = "#b22222", linewidth = 0.9
  ) +
  annotate(
    "text",
    x = post_end, y = overall_att,
    label = " Post-TCJA ATT", hjust = -0.05, vjust = 0.5,
    colour = "#b22222", size = 3.2
  ) +
  # 4) policy v-lines & labels
  geom_vline(
    data = events, aes(xintercept = date),
    colour = "black", linewidth = 0.5, linetype = "dotted"
  ) +
  geom_text(
    data = events_lbl,
    aes(x = label_x, y = label_y, label = label),
    angle = 90, hjust = 0, size = 3
  ) +
  # 5) research-paper timeline (cleaner bar + start/end dots)
  geom_segment(
    data = OZ_Timeline,
    aes(x = `Data Start Date`, xend = `Data End Date`, y = tl_y, yend = tl_y),
    linewidth = 2.2, colour = "#b0b7bf", alpha = 0.55, lineend = "round"
  ) +
  geom_point(
    data = OZ_Timeline, aes(x = `Data Start Date`, y = tl_y),
    shape = 21, fill = "#254f85", size = 2.6, stroke = 0.2, colour = "grey20"
  ) +
  geom_point(
    data = OZ_Timeline, aes(x = `Data End Date`, y = tl_y),
    shape = 21, fill = "#254f85", size = 2.6, stroke = 0.2, colour = "grey20"
  ) +
  ggrepel::geom_text_repel(
    data = OZ_Timeline,
    aes(x = `Data End Date`, y = tl_y, label = Author),
    nudge_x = 20, min.segment.length = 0, direction = "y", size = 2.7,
    segment.size = 0.2, segment.alpha = 0.5, box.padding = 0.3, point.padding = 0.3,
    colour = "#4a4e4d", seed = 42
  ) +
  # 6) scales, labels, theme
  scale_colour_manual(values = pal, guide = guide_legend(title = NULL)) +
  scale_shape_manual(values = c(16, 17), guide = guide_legend(title = NULL)) +
  scale_y_continuous(
    breaks = pretty_breaks(n = 8),
    expand = expansion(mult = c(0, 0.02))
  ) +
  scale_x_date(
    breaks = pretty_breaks(n = 12),
    labels = label_date("%b %Y")
  ) +
  labs(
    title = "Opportunity Zone Housing Effect: Event Study and Research Timeline",
    subtitle = "Points show ATT by event time with 95% CIs; dashed line marks overall ATT across the post period.",
    # caption = "Notes: Pre/post markers use a11y-friendly colors and shapes; research timeline summarises study windows."
    y = "ATT on Total Active + Vacant Residential", x = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.text.x        = element_text(angle = 45, hjust = 1),
    legend.position    = "bottom",
    legend.box.margin  = margin(t = -6),
    plot.title.position = "plot",
    plot.margin        = margin(10, 60, 10, 10)  # room for right-side labels
  ) +
  coord_cartesian(
    ylim = c(y_min_tl - y_pad_low, y_max_data + y_top_room),
    xlim = c(ymd("2009-12-01"), ymd("2025-12-30")),
    clip = "off"
  )


export_plot
setwd(path_output)

ggsave(
  filename = "housing_effect_vs_research_papers.png",
  plot     = export_plot,
  width    = 12,    # inches
  height   = 9,     # inches
  units    = "in",
  dpi      = 300,
  # bg       = "white"   # ← white background
  bg       = NULL   # ← white background
  
)

ggsave(
  filename = "housing_effect_vs_research_papers.pdf",
  plot     = export_plot,
  device   = cairo_pdf,                 # or: device = "pdf"
  width    = 12, height = 6.75, units = "in",  # 16:9 (12 : 6.75)
  bg       = "transparent"
)

ggsave(
  filename = "housing_effect_vs_research_papers_16_9.png",
  plot     = export_plot,
  width    = 10.66667,    # inches
  height   = 6,     # inches
  units    = "in",
  dpi      = 300,
  # bg       = "white"   # ← white background
  bg       = NULL   # ← white background
  
)

# ggsave(
#   filename = "housing_effect_vs_research_papers_1920x1080.png",
#   plot     = export_plot,
#   device   = ragg::agg_png,            # crisper text/lines than default
#   width    = 1920/300, height = 1080/300, units = "in",
#   dpi      = 300,
#   bg       = "transparent"
# )
