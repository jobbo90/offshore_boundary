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
# setwd("I:/BackUp_D_mangroMud_202001/Research/Software/Projects/offshore_boundary")


## ---------------------------

options(scipen = 6, digits = 4) # I prefer to view outputs in non-scientific notation
memory.limit(30000000)     # this is needed on some PCs to increase memory allowance, but has no impact on macs.

#  Map view options:
# https://r-spatial.github.io/mapview/articles/articles/mapview_02-advanced.html

## ---------------------------

#' load up the packages 
source("./src/packages.R")       # loads up all the packages we need
ee_Initialize()
## ---------------------------
source("./src/functions.R")

## ---------------------------

dataFolder <- './data/raw'

# transect  <- readOGR(paste0(dataFolder, '/transects'), '2009_WnZ_transect')

# select folders
folderSelect <- as.matrix(list.files(paste0(dataFolder, '/GEE_exports/testImages'), full.names = T))
# metaMatrix <- as.matrix(list.files(folderSelect, pattern=".csv", full.names = T))
mapviewOptions(basemaps = c( "Esri.WorldImagery","Esri.WorldShadedRelief", "OpenStreetMap.DE"))

df <- rewrite(folderSelect);

# only csv's
df <- df[grep('.csv', folderSelect, ignore.case = T),]



# csv <- data.frame(matrix(NA, 0, 18),
#            stringsAsFactors=F)
# 
# for (q in nrow(df)){
#   # q = 2
#   csv <- rbind(csv, as.matrix(read.csv2(as.character(df[q,1]), 
#                       header = T, sep = ',', na.strings=c("","NA"))))
#   
# }

# strings to compare
# set1 <- 'Suriname_229_56_2009_till_2009_testImage2009115'
# set2 <- 'Suriname_229_56_2009_till_2009_testImage2009115_extraMask'
# set1 <- 'Suriname_229_56_2009_till_2009_testImage20091115_extraMask_V20201207'
# set2 <- 'Suriname_229_56_2009_till_2009_testImage20091115_extraMask_largestDrop'

# largest, relative and slope drop for 20091115 image
set2 <- 'Suriname_229_56_2009_till_2009_testImage20091115_relativeDrop_20201217'
# set3 <- 'Suriname_229_56_2009_till_2009_testImage20091115_relativeDrop_20201210'
# set3 <- 'Suriname_229_56_2009_till_2009_testImage20091115_slopeDrop_20201210'
# set3 <- 'Suriname_229_56_2009_till_2009_testImage20091115_relativeDrop_20201210'


# largest, relative and slope drop for 20090912 image
# set1 <- 'Suriname_229_56_2009_till_2009_testImage20090912_largestDrop_20201210'
# set3 <- 'Suriname_229_56_2009_till_2009_testImage20090912_relativeDrop_20201210'
set1 <- 'Suriname_229_56_2009_till_2009_testImage20090912_relativeDrop_20201217'

# set3 <- 'Suriname_229_56_2009_till_2009_testImage20090912_extraMask_largestDrop'
# set2 <- 'Suriname_229_56_2009_till_2009_testImage20090912_extraMask_V20201207'


csv1 = as.matrix(read.csv2(as.character(df[grep(set1, folderSelect, ignore.case = T),1]),
                          header = T, sep = ',', na.strings=c("","NA"))) # rewrite as matrix to read columns as numeric values
csv2 = as.matrix(read.csv2(as.character(df[grep(set2, folderSelect, ignore.case = T),1]),
                           header = T, sep = ',', na.strings=c("","NA"))) # rewrite as matrix to read columns as numeric values
# csv3 = as.matrix(read.csv2(as.character(df[grep(set3, folderSelect, ignore.case = T),1]),
#                            header = T, sep = ',', na.strings=c("","NA"))) # rewrite as matrix to read columns as numeric values
# 



# csv1[order(csv1[,'pos']),]

# plot(csv1[,'pos'],csv1[,'mudFract'])


points1<- reshape_csvPoints(csv1, 'peakCoordX', 'peakCoordY')

# # plot test
points1_df <- points1 %>% st_drop_geometry()
# points1_df$pos <- as.numeric(points1_df$pos)
# points1_df[order(points1_df$pos),]
# plot(points1_df_order$pos, points1_df_order$mudFract)




pointsLand1 <- reshape_csvPoints(csv1, 'coastX', 'coastY')
points2<- reshape_csvPoints(csv2, 'peakCoordX', 'peakCoordY')
pointsLand2 <- reshape_csvPoints(csv2, 'coastX', 'coastY')
# points3<- reshape_csvPoints(csv3, 'peakCoordX', 'peakCoordY')
# pointsLand3 <- reshape_csvPoints(csv3, 'coastX', 'coastY')

# filter -1?



lines1 <- reshape_csvLines(csv1)

# change format of lines
lines_sf <- st_as_sf(lines1)

# ee_clean_pyenv()
# ee_install()
# ee_install_upgrade(version = "0.1.224")
# Sys.getenv('EARTHENGINE_PYTHON')
# "C:\\Users\\5600944\\AppData\\Local\\r-miniconda\\envs\\rgee\\python.exe"



# Load an image.
image20090912 <- ee$Image("LANDSAT/LT05/C01/T1_TOA/LT05_229056_20090912")
image20091115 <- ee$Image("LANDSAT/LT05/C01/T1_TOA/LT05_229056_20091115")


vizParams <- list(
  bands = c("B5", "B4", "B3"),
  min = 0.05, max = 0.4, gamma = 1.4
)
test1 <- Map$addLayer(image20090912, vizParams, "Landsat 8 20090912")
test2 <- Map$addLayer(image20091115, vizParams, "Landsat 8 20091115")

test1 + test2 +
  # mapview(pointsLand2,col.regions = c("blue")) +
  mapview(points1, col.regions = c("red"), layer.name = c("set1")) + 
  mapview(points2, col.regions = c("green"), layer.name = c("set2")) + 
  # mapview(points3, col.regions = c("yellow"), layer.name = c("set3")) +
  mapview(lines_sf,xcol = "x", ycol = "y") 



# attempt to filter points
# 1) each point compared with neighbours:
  # on comparable index value
  # comparable distance value (large jumps are not sensible)
# check for linear features (e.g. do multiple points form a linear feature)


# after filtering create convext hulls
# convex of all points
test<- as(points1, Class = 'Spatial')
library(rgeos)
convex<- gConvexHull(test)
# geom <- points1$geometry
# https://babichmorrowc.github.io/post/2019-03-18-alpha-hull/



# df_coastDist <- df_coastDist[,order(names(df_coastDist))]
# 
# plot(as.Date(colnames(df_coastDist)[1:ncol(df_coastDist)-1]), c(1:length(2:ncol(df_coastDist))))
# 
# plot(as.Date(colnames(df_coastDist)[1:ncol(df_coastDist)-1]), df_coastDist[1,1:ncol(df_coastDist)-1])
# clicked <- identify(as.Date(colnames(df_coastDist)[1:ncol(df_coastDist)-1]),
#                     df_coastDist[1,1:ncol(df_coastDist)-1], 
#                     n=1, labels=colnames(df_coastDist)[1:ncol(df_coastDist)-1])





