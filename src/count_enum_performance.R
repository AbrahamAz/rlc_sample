source("src/init.R")

filename.dataset <- "data/inputs/kobo_export/raw_data.xlsx"

kobo.raw <- read_excel(filename.dataset, col_types = "text") %>%
  rename(uuid ="_uuid", index = "_index")

deletion.log <- read_excel(paste0("output/deletion_log/Informant_method_deletion_log.xlsx"), col_types = "text")
cleaning.log <- read_excel(paste0("output/cleaning_log/Informant_method_cleaning_log.xlsx"), col_types = "text")

create.count_collected_enu(kobo.raw,     "enum")
create.count_deleted_enu(deletion.log, "enum")
create.count_enu_cleaning(cleaning.log, "enum")

cat("\n> Done. Created 3 files in output/enum_performance.")
