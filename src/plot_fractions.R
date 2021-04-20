## ---------------------------
#'
#' Script name: Plot offshore boundary results
#'
#' Short Description: 
#' 
#'
#' Author: Job de Vries
#'
#' Date Created: 2020-11-16
#'
#' Copyright (c) Job de Vries, 2020
#' Email: j.devries4@uu.nl
#'
## ---------------------------
#'
#' Description
#' 
#'
#'
## ---------------------------

rm(list = ls())
#' set working directory for Mac and PC
wd<-getwd()
# setwd("D:/BackUp_D_mangroMud_202001/Research/Software/Projects/offshore_boundary")


## ---------------------------

options(scipen = 6, digits = 4) # I prefer to view outputs in non-scientific notation
memory.limit(30000000)     # this is needed on some PCs to increase memory allowance, but has no impact on macs.

#  Map view options:
# https://r-spatial.github.io/mapview/articles/articles/mapview_02-advanced.html

## ---------------------------

#' load up the packages 
source("./src/packages.R")       # loads up all the packages we need
library(strex)                  # only required here

## ---------------------------
source("./src/functions.R")

## ---------------------------

mapviewOptions(basemaps = c( "Esri.WorldImagery","Esri.WorldShadedRelief", "OpenStreetMap.DE"))
dataFolder <- './data/'
# raw\GEE_exports\allArrayExport_test
aoi <- c('229_56')

posToExclude <- c(seq(138000,147000,1000),
                  seq(241000, 255000, 1000))  

# select folders
folderSelect <- as.matrix(list.files(paste0(dataFolder, 'raw/GEE_exports/allArrayExport_test'), 
                                     full.names = T))
df <- rewrite(folderSelect);

# only csv's
df <- df[grep('.csv', folderSelect, ignore.case = T),]
filtered <- vector('list', 100)
for (x in seq_along(aoi)){
      # q <- 1
      region = aoi[x]
      
      filters = c(region)
      
      filtered = rbind(filtered, df %>% 
                         dplyr::filter(
                           filters %>%
                             # apply the filter of all the text rows for each pattern
                             # you'll get one list of logical by pattern ignored_string
                             purrr::map(~ to_keep(.x, text = text)) %>%
                             # get a logical vector of rows to keep
                             purrr::pmap_lgl(all)
                         ))
}
filtered <- unique(filtered)
allFiles <- unique(do.call(rbind, lapply(as.matrix(filtered)[,1], 
                                         function(x) read.csv(x, stringsAsFactors = FALSE,
                                                              sep = ',', 
                                                              na.strings=c("","NA")
                                                              ))))


allfiles2 <- read.csv(as.matrix(filtered)[1,1], header=TRUE, sep=',', dec= '.', stringsAsFactors=FALSE)

unique(allFiles$DATE_ACQUIRED)

# subset on date & pos
dateSubset <- subset(allFiles, DATE_ACQUIRED == "2019-03-16" & pos == 138000)

# [1] "system.index"          "CLOUD_COVER"           "DATE_ACQUIRED"         "areaName"              "coastline"             "inflict_SuperSmoothed"
# [7] "lat"                   "lon"                   "mud_bilat"             "ndwi_threshold"        "peaks_SuperSmoothed"   "peaks_bilatSmoothed"  
# [13] "pos"                   "superSmooth_mud"       "valleys_SmoothedBilat" "valleys_superSmoothed" "x_axis"                ".geo" 


bilatMud <- unlist(str_extract_numbers(
  dateSubset$mud_bilat,
  decimals = T,
  leading_decimals = T,
  negs = T,
  sci = T,
  commas = FALSE,
  leave_as_string = FALSE
))
coast <- as.numeric(unlist(regmatches( dateSubset$coastline,
                                       gregexpr("[[:digit:]]+\\.*[[:digit:]]*", 
                                                dateSubset$coastline))))

bilatPeaks <- bilatMud <- unlist(str_extract_numbers(
  dateSubset$peaks_bilatSmoothed, decimals = T, leading_decimals = T,
  negs = T, sci = T, commas = FALSE, leave_as_string = FALSE))


x_ax <- as.numeric(unlist(regmatches( dateSubset$x_axis,
                                      gregexpr("[[:digit:]]+\\.*[[:digit:]]*", 
                                               dateSubset$x_axis))))
superSmoot <- as.numeric(unlist(regmatches( dateSubset$superSmooth_mud,
                                            gregexpr("[[:digit:]]+\\.*[[:digit:]]*", 
                                                     dateSubset$superSmooth_mud))))



plot(x_ax, bilatMud, col = rgb(red = 0, green = 0, blue = 0, alpha = 0.3))
points(x_ax[which(bilatPeaks > 0)], bilatPeaks[which(bilatPeaks > 0)], 
       col = rgb(red = 1, green = 0, blue = 0, alpha = 1), pch = 16)
points(x_ax, superSmoot, 
       col = rgb(red = 1, green = 1, blue = 0, alpha = 0.5), pch = 1)


# extent
abline(v = x_ax[which(bilatPeaks > 0)], col = 'red')







