setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list = ls())

## sourcing the tool
source("src/init.R")

## SET FILENAMES AND OTHER STRINGS  --------------------------------------------
strings <- c(
  dataset.name.short = gsub(" ", "_",dlgInput("Please provide name of assessmend (please dont use special characters)", "COUNTRY_ASSESSMENT_CODE_DATE")$res),   # provide a short name for filenames of output documents (e.g. "POL_PDM")      # this string is only used for creating titles for output documents
  out_date = stringr::str_sub(stringr::str_remove_all(Sys.Date(), '-'), 3),      # this one is appended to the end of filenames
  sampling_data = choose.files(caption = "Please select the sampling data (Raw Data from the ReachGeoRand)", multi = F), 
  rlc_data = choose.files(caption = "Please select the rld data \n(Should include the household data with GPS locations)", multi = F)  # the filename of your data for 
)

# <- additional indicators and grouping variables are added here 
## TABULAR  -------------------------------------------------------------------
rmarkdown::render('rlc_sample_template_fast_track.Rmd',
output_file = paste0("output/", strings['dataset.name.short'], "_Quality_Check_and_weighting_calculation_", strings['out_date'],".html"))
cat("\n> Quality Check completed! You can check your output folder.")

# -----------------------------------------------------------------------------`