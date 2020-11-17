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

setwd("I:/BackUp_D_mangroMud_202001/Research/Software/Projects/offshore_boundary")


## ---------------------------

options(scipen = 6, digits = 4) # I prefer to view outputs in non-scientific notation
memory.limit(30000000)     # this is needed on some PCs to increase memory allowance, but has no impact on macs.

## ---------------------------

#' load up the packages 
source("./src/packages.R")       # loads up all the packages we need

## ---------------------------
source("./src/functions.R")

## ---------------------------

dataFolder <- './data/raw'

# transect  <- readOGR(paste0(dataFolder, '/transects'), '2009_WnZ_transect')

# select folders
folderSelect <- as.matrix(list.files(paste0(dataFolder, '/GEE_exports'), full.names = T))
# metaMatrix <- as.matrix(list.files(folderSelect, pattern=".csv", full.names = T))

df <- rewrite(folderSelect)

csv = as.matrix(read.csv2(as.character(df[3,1]), 
                          header = T, sep = ',', na.strings=c("","NA"))) # rewrite as matrix to read columns as numeric values
dates <- col_of_interest(csv, 'DATE_ACQUIRED$')
coastDist <- col_of_interest(csv, 'coastDist$')

# all unique dates
uniqueDates <- unique(csv[,dates])

# all unique transect (id's)
uniqueX<- unique(csv[, col_of_interest(csv, 'originX$')])
uniqueY<- unique(csv[, col_of_interest(csv, 'originY$')])
geo<- unique(csv[, col_of_interest(csv, '.geo')])

lines <- vector('list', length(uniqueX))

allPoints <- vector('list', length(csv))

for (n in 1:length(uniqueX)){
  #n<-1
  coords <- qdapRegex::ex_between(as.character(geo[n]), ":[", "]}")[[1]]
  all_digits <- regmatches(coords, gregexpr("[-[:digit:].]+", coords))[[1]]
  
  begin_coords <- data.frame(lon = as.numeric(all_digits[1]),
                             lat = as.numeric(all_digits[2]))
  
  end_coords <- data.frame(lon = as.numeric(all_digits[3]),
                           lat = as.numeric(all_digits[4]))  
  x <- as.matrix(rbind(begin_coords, end_coords))
  
  lines[[n]] <- Lines(list(Line(x)), ID = n)
  
  test1transect <- subset(csv,csv[,col_of_interest(csv, 'originX$')]== uniqueX[n] 
                          & csv[,col_of_interest(csv, 'coastDist$')] >= 0 )
  # lines_sf <- SpatialLines(lines)
  coords <- data.frame(x = as.numeric(test1transect[,col_of_interest(csv, 'coastX$')]),
                       y = as.numeric(test1transect[,col_of_interest(csv, 'coastY$')]))
  allPoints <- rbind(allPoints, coords)
  

  
}

points <- SpatialPoints(allPoints, proj4string=CRS(as.character(NA)))
lines_sf <- SpatialLines(lines)
plot(lines_sf)

plot(points, add = T, col = 'red')

# str_match(as.character(geo[1]), ":[\\s*(.*?)\\s*]}")
# coords <- qdapRegex::ex_between(as.character(geo[1]), ":[", "]}")[[1]]
# all_digits <- regmatches(coords, gregexpr("[-[:digit:].]+", coords))[[1]]
# 
# 
# begin_coords <- data.frame(lon = as.numeric(all_digits[1]),
#                            lat = as.numeric(all_digits[2]))
# 
# # Coordinates on the original line
# end_coords <- data.frame(lon = as.numeric(all_digits[3]),
#                          lat = as.numeric(all_digits[4]))  
# 
# x <- as.matrix(rbind(begin_coords, end_coords))
# 
# n = 1
# lines <- vector('list', n) #length(uniqueX)
# 
# lines[[n]] <- Lines(list(Line(x)), ID = n)
# lines_sf <- SpatialLines(lines) #, proj4string = CRS(as.character(line@proj4string))
# plot(lines_sf)
# 
# test1transect <- subset(csv,csv[,col_of_interest(csv, 'originX$')]== uniqueX[1] 
#                         & csv[,col_of_interest(csv, 'coastDist$')] >= 0 )
# 
# # maxCoastDist <- test1transect[max(test1transect[,col_of_interest(csv, 'coastDist$')]),]
# # setting coastDist > 0 also excludes observations when transect is not sufficient landward
# 
# testMax <- test1transect[which.max(test1transect[,6]),]
# 
# # plot(as.Date(test1transect[,3]), test1transect[,6])
# 
# coords <- data.frame(x = as.numeric(test1transect[,col_of_interest(csv, 'coastX$')]),
#                     y = as.numeric(test1transect[,col_of_interest(csv, 'coastY$')]))
# points <- SpatialPoints(coords, proj4string=CRS(as.character(NA)))
# plot(points, add = T, col = 'red')
