rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## sourcing the tool
source("src/init.R")
source("src/rlc_quality_checks.R")

## loading the data
area_data <- read_xlsx("data/inputs/kobo_export/sampling_data.xlsx", col_types = "text", sheet = 1)
sampling_data <- read_xlsx("data/inputs/kobo_export/sampling_data.xlsx",col_types = "text", sheet = "select_final_set")
raw_main <- read_xlsx("data/inputs/kobo_export/raw_data.xlsx",col_types = "text", sheet = 1) 
raw_households <- read_xlsx("data/inputs/kobo_export/raw_data.xlsx", col_types = "text", sheet = "households")

rlc_data <- raw_households %>% 
  left_join(select(raw_main, c(`_uuid`,cluster_ID,
                               `_cluster_geopoint_latitude`,
                               `_cluster_geopoint_longitude`)), by = c("_submission__uuid" = "_uuid"))

## CHECK the cluster_IDs
check_rlc_clusters(sampling_data = sampling_data, rlc_data = rlc_data)






