# # library(plyr)
# # library(ggridges)
# remotes::install_github("r-spatial/rgee")
# remove.packages('Rcpp')
library(rgee)
# ee_install()    # run only once
# ee_Initialize() # initialize rgee
library(ggplot2)
library(readr)
library(mapview)
# library(leafpop)
# library(tidyverse)
library(dplyr)      # for mutate, such as adding cols
# library(reshape2)
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
# library(RStoolbox) # for ggRGB
# library(data.table) # for fread
# library(reshape2) # data melt
# library(maptools) # elide spatial line (rotate)
# library(patchwork)

# library(plyr)
# library(egg) # for tagging facets
# library( rasterVis ) # for gPlot
# library( gridExtra )
library(viridis) # for visualization
