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
set1 <- 'Suriname_229_56_2009_till_2009_testImage20091115_extraMask_V20201207'
set2 <- '/Suriname_229_56_2009_till_2009_testImage20091115_extraMask_largestDrop'

# set1 <- '/Suriname_229_56_2009_till_2009_testImage20090912_extraMask_largestDrop'
# set2 <- 'Suriname_229_56_2009_till_2009_testImage20090912_extraMask_V20201207'


csv1 = as.matrix(read.csv2(as.character(df[grep(set1, folderSelect, ignore.case = T),1]),
                          header = T, sep = ',', na.strings=c("","NA"))) # rewrite as matrix to read columns as numeric values
csv2 = as.matrix(read.csv2(as.character(df[grep(set2, folderSelect, ignore.case = T),1]),
                           header = T, sep = ',', na.strings=c("","NA"))) # rewrite as matrix to read columns as numeric values

# mydata <- sapply(list.files(paste0(dataFolder, '/GEE_exports/testImages'), full.names = T), read.csv)


points1<- reshape_csvPoints(csv1, 'peakCoordX', 'peakCoordY')
pointsLand1 <- reshape_csvPoints(csv1, 'coastX', 'coastY')
points2<- reshape_csvPoints(csv2, 'peakCoordX', 'peakCoordY')
pointsLand2 <- reshape_csvPoints(csv2, 'coastX', 'coastY')

lines1 <- reshape_csvLines(csv1)

# change format of lines
lines_sf <- st_as_sf(lines1)


mapview(pointsLand2,col.regions = c("blue")) + mapview(points1, col.regions = c("red")) + mapview(points2, col.regions = c("green")) + 
  mapview(lines_sf,xcol = "x", ycol = "y") 



# overlay with the image of interest?
# remotes::install_github("r-spatial/rgee") 

#  cannot remove jsonlite package (remove.packages("jsonlite"))

#2D plotting
# order on column names
df_coastDist <- df_coastDist[,order(names(df_coastDist))]

plot(as.Date(colnames(df_coastDist)[1:ncol(df_coastDist)-1]), c(1:length(2:ncol(df_coastDist))))

plot(as.Date(colnames(df_coastDist)[1:ncol(df_coastDist)-1]), df_coastDist[1,1:ncol(df_coastDist)-1])
clicked <- identify(as.Date(colnames(df_coastDist)[1:ncol(df_coastDist)-1]),
                    df_coastDist[1,1:ncol(df_coastDist)-1], 
                    n=1, labels=colnames(df_coastDist)[1:ncol(df_coastDist)-1])





