rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## sourcing the tool
source("src/init.R")


## loading the data
sampling_data <- read_xlsx("data/inputs/kobo_export/sampling_data.xlsx", sheet = "select_final_set")
raw_main <- read_xlsx("data/inputs/kobo_export/raw_data.xlsx",col_types = "text", sheet = 1)
raw_households <- read_xlsx("data/inputs/kobo_export/raw_data.xlsx", col_types = "text", sheet = "households")

## Check if cluster_IDs match with sample

check_cluster_ID <- function(sampling_data = sampling_data,
                             rlc_data = raw_main,
                             column_cluster_id_sampling = "show_cluster_id",
                             column_cluster_id_rlc = "cluster_ID") {
  ## Make sure to include data
  if(is.null(sampling_data) | is.null(rlc_data)) warning("Make sure that both the sampling and the rlc data exits.")
  
  ## list of cluster_IDs from sampling and rlc
  list_cluster_id_sampling <- sampling_data[[column_cluster_id_sampling]]
  list_cluster_id_rlc <- rlc_data[[column_cluster_id_rlc]]
  
  ## check for duplicate
  if(any(duplicated(list_cluster_id_sampling))) warning("You have duplicated cluster IDs in the list from the sampling data")
  if(any(duplicated(list_cluster_id_rlc))) warning("You have duplicated cluster IDs in the list from the rlc data.")
  
  ## check both lists match
  difference_rlc_to_sample <- setdiff(list_cluster_id_sampling,list_cluster_id_rlc)
  difference_sample_to_rlc <- setdiff(list_cluster_id_rlc,list_cluster_id_sampling)
  
  if(!identical(difference_rlc_to_sample, character(0))){
    warning(paste("The following cluster IDs are missing from the rlc list", difference_rlc_to_sample, sep = " "))
  } 
  
  
}

check_cluster_ID(sampling_data = sampling_data,
                 rlc_data = raw_main)
