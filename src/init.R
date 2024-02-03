# loading all packages, function and list of variables.

# loading all packages, functions and the Kobo tool
if (!require("pacman")) install.packages("pacman")
pacman::p_load(docstring, tidyverse, readxl, writexl, openxlsx, stringr, 
               sf, geosphere, qdapRegex, cluster, randomcoloR, scales, knitr,svDialogs,
               sfheaders, raster,leaflet.extras,data.table,KernSmooth,RColorBrewer,spatstat,
               Metrics,berryFunctions)
install.packages('resources/rgdal_1.6-7.tar.gz')
options(scipen = 999)

