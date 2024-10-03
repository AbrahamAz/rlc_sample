# load a single raw Kobo data export:

# and loads the data into kobo.raw.main, kobo.raw.loop1...
# also included are the standard steps of renaming uuid, and adding the loop_index

raw_data_filename <- list.files("data/inputs/kobo_export/", filename_path, full.names = T)

if(length(raw_data_filename) > 1) { stop("Found multiple files containing raw Kobo data! Please clean up the kobo_export folder.")
}else if(length(raw_data_filename) == 0){ 
  warning("Raw Kobo data not found!")
  kobo.raw.main <- data.frame()
  kobo.raw.loop1 <- data.frame()
  dataset_creation_time <- NA
  dctime_short <- ""
}else if(length(raw_data_filename) == 1){
  kobo.raw.main <- read_xlsx(raw_data_filename, col_types = "text", sheet = 1) %>%
    rename(uuid = "_uuid")
  kobo.raw.births <- read_xlsx(raw_data_filename, col_types = "text", sheet = "birth") %>%
    rename(uuid = "_submission__uuid",
           enum = "birth_enum") %>% 
    mutate(loop_index = paste0("loopbirths_",`_index`))
  kobo.raw.awd <- read_xlsx(raw_data_filename, col_types = "text", sheet = "awd") %>%
    rename(uuid = "_submission__uuid",
           enum = "awd_enum") %>% 
    mutate(loop_index = paste0("loopawd_",`_index`))
  kobo.raw.measles <- read_xlsx(raw_data_filename, col_types = "text", sheet = "measles") %>%
    rename(uuid = "_submission__uuid",
           enum = "measles_enum") %>% 
    mutate(loop_index = paste0("loopmeasles_",`_index`))
  kobo.raw.deaths <- read_xlsx(raw_data_filename, col_types = "text", sheet = "deaths") %>%
    rename(uuid = "_submission__uuid",
           enum = "death_enum") %>% 
    mutate(loop_index = paste0("loopdeaths_",`_index`))
  cat("\nLoaded raw Kobo data from file", raw_data_filename, "\n")
  dataset_creation_time <- as.Date(file.info(raw_data_filename)$ctime)
  dctime_short <- str_extract(gsub('-', '', str_sub(dataset_creation_time, 3)), "\\d+") 
}

rm(filename_path)