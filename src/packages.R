
# remotes::install_github("r-spatial/rgee")
# remove.packages('rgee')
# library(rgee)
# ee_install(py_env = "rgee")   # run only once # C:/Users/5600944/AppData/Local/r-miniconda/envs/rgee
# ee_Initialize() # initialize rgee

# Always that you want to use rgee you will need to run as follow:
library(rgee)
# Sys.setenv("RETICULATE_PYTHON" = "C:/Users/5600944/AppData/Local/r-miniconda/envs/rgee/python.exe")
# ee_Initialize()

# To save the virtual environment "EARTHENGINE_PYTHON", run: 
# rgee::ee_install_set_pyenv(py_path = "C:/Users/5600944/AppData/Local/r-miniconda/envs/rgee/python.exe")

library(ggplot2)
library(readr)
library(mapview)
library(tidyverse)
library(dplyr)      # for mutate, such as adding cols
library(stringr)
library(sp)
library(rgdal)  # for reading/creating shapefiles
library(raster) # for stack & reading raster
library(qdapRegex)
library(sf)
library(jsonlite) # required for mapView
library(zoo)
library(EnvStats) # for rosnerTest
library(lubridate) # for year/month etc.
library(zoo)      # for na.locf
library(car)      # for outlierTest
library(testit)   # for has_error
library(geosphere) # for bearing
library(scales)   # for squish in ggplot
library(pointdensityP) # for density estimate

# library(cowplot)
# library(ggspatial) # for north arrow annotation ggRGB
# # library(ggtern)
library(RStoolbox) # for ggRGB
# library(data.table) # for fread
# library(reshape2) # data melt
# library(maptools) # elide spatial line (rotate)
# library(patchwork)

# library(plyr)
# library(egg) # for tagging facets
# library( rasterVis ) # for gPlot
# library( gridExtra )
library(viridis) # for visualization
