# loading all packages, function and list of variables.

# loading all packages, functions and the Kobo tool
if (!require("pacman")) install.packages("pacman")
pacman::p_load(docstring, tidyverse, readxl, writexl, openxlsx, stringr, 
               sf, geosphere, qdapRegex, cluster, randomcoloR, svDialogs, scales, knitr,
               sfheaders, raster,leaflet.extras,data.table,KernSmooth,RColorBrewer)


options(scipen = 999)

