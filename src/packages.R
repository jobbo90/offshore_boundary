library(rgee)
library(ggplot2)
library(patchwork)
library(readr)
library(mapview)
library(leaflet)
library(tidyverse)
library(plyr)
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


# library(egg) # for tagging facets
# library( rasterVis ) # for gPlot
# library( gridExtra )
library(viridis) # for visualization
