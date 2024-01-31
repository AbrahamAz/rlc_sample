rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## sourcing the tool
source("src/init.R")


## loading the data
area_data <- read_xlsx("data/inputs/kobo_export/sampling_data.xlsx", col_types = "text", sheet = 1)
sampling_data <- read_xlsx("data/inputs/kobo_export/sampling_data.xlsx",col_types = "text", sheet = "select_final_set")
raw_main <- read_xlsx("data/inputs/kobo_export/raw_data.xlsx",col_types = "text", sheet = 1) 
raw_households <- read_xlsx("data/inputs/kobo_export/raw_data.xlsx", col_types = "text", sheet = "households")


rlc_data <- raw_households %>% 
  left_join(select(raw_main, c(`_uuid`,cluster_ID,
                               `_cluster_geopoint_latitude`,
                               `_cluster_geopoint_longitude`)), by = c("_submission__uuid" = "_uuid"))

source("src/rlc_quality_checks.R")
## CHECK the clusters and the waypoints ID matches
check_waypoints_to_cluster_id(sampling_data = sampling_data, rlc_data = rlc_data)

## CHECK the waypoints are inside the polygon (50 m threshold)
check_waypoints_in_area(area_data = area_data, rlc_data = rlc_data)

## CHECK the wayhpoints are close to their generated cluster points (50 m threshold)
check_waypoints_to_cluster_distance(sampling_data = sampling_data, rlc_data = rlc_data)

## CHECK the hh points are inside the polygon (50 m threshold)
check_hh_in_area(area_data = area_data, rlc_data = rlc_data)

## CHECK the hh points are close to their generated waypoints (50 m threshold)
check_hh_to_waypoint_distance(rlc_data = rlc_data)

## CHECK the hh points are close to their generated waypoints (50 m threshold)
check_hh_unique_to_waypoint(rlc_data=rlc_data)
