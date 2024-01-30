## Check if cluster_IDs match with sample
check_rlc_clusters <- function(sampling_data = sampling_data,
                               rlc_data = rlc_data,
                               column_cluster_id_sampling = "show_cluster_id",
                               column_cluster_id_rlc = "cluster_ID") {
  ## Make sure to include data
  if(is.null(sampling_data) | is.null(rlc_data)) stop("Make sure that both the sampling and the rlc data exits.")
  
  ## list of cluster_IDs from sampling and rlc
  list_cluster_id_sampling <- sampling_data[[column_cluster_id_sampling]]
  list_cluster_id_rlc <- rlc_data[[column_cluster_id_rlc]]
  
  ## check both lists match and no cluster_ID is missing
  difference_rlc_to_sample <- setdiff(list_cluster_id_sampling,list_cluster_id_rlc)
  difference_sample_to_rlc <- setdiff(list_cluster_id_rlc,list_cluster_id_sampling)
  
  if(!identical(difference_rlc_to_sample, character(0))){
    cat(paste(paste("Attention: \nThe following cluster IDs are missing from the rlc list", difference_rlc_to_sample, sep = " "),
              paste("The following cluster IDs might be matching from the sampling", difference_sample_to_rlc, sep = " "), sep = '\n'))
  } else {
    cat("All you clusters from the sampling frame are identical to the ones in the rlc data!!!\n\n")
  }
  
  ## check if every cluster_ID have 3 HH
  check_3_hh <- rlc_data %>% 
    group_by(cluster_ID) %>% 
    summarise(HHs = n()) %>% 
    mutate(flag = HHs != 3)
  
  if(any(check_3_hh$flag)) {
    cat(paste("However, not all your clusters have a total of 3 HHs."))
    kable(check_3_hh %>% filter(flag) %>% select(-flag))
  } else {
    cat(paste("All your clusters have a total of 3 HHs."))
  }
}