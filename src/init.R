# loading all packages, function and list of variables.

# loading all packages, functions and the Kobo tool
if (!require("pacman")) install.packages("pacman")
pacman::p_load(docstring, tidyverse, readxl, writexl, openxlsx, stringr, 
               sf, geosphere, qdapRegex, cluster, randomcoloR, svDialogs, scales, knitr)

source("src/utils/misc_utils.R")
source("src/utils/check_kobo.R")
source("src/utils/kobo_utils.R")
source("src/utils/regional_detect_data_falsification.R")
source("src/utils/utils_audit.R")
source("src/utils/utils_cleaning.R")
source("src/utils/utils_cleaning_loops.R")

options(scipen = 999)
options(dplyr.summarise.inform = FALSE)
enum_colname <- "enum"

dir.audits <- "data/inputs/audits/reach/"
dir.requests <- "output/checking/requests/"
dir.responses <- "output/checking/responses/"

###############################################################################

# input filnenames
filename.tool <- "resources/tool.xlsx" ### CHANGE the tool name to whatever fits properly
filename_path <- "raw_data.xlsx"
###############################################################################

# load TOOL Refuggess
cat("\n- LOADING tool ...\n")

cat("\nLoading Kobo tool from file", filename.tool, "...\n")
label_colname <- load.label_colname(filename.tool)
tool.survey  <- load.tool.survey(filename.tool)
tool.choices <- load.tool.choices(filename.tool)
cat("..OK\n")