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

csv = as.matrix(read.csv2(as.character(df[2,1]), 
                          header = T, sep = ',', na.strings=c("","NA"))) # rewrite as matrix to read columns as numeric values
dates <- col_of_interest(csv, 'DATE_ACQUIRED$')
coastDist <- col_of_interest(csv, 'coastDist$')

# all unique dates
uniqueDates <- unique(csv[,dates])

# all unique transect (id's)
pos <- unique(csv[, col_of_interest(csv, 'pos$')])
uniqueX<- unique(csv[, col_of_interest(csv, 'originX$')])
uniqueY<- unique(csv[, col_of_interest(csv, 'originY$')])
geo<- unique(csv[, col_of_interest(csv, '.geo')])

lines <- vector('list', length(uniqueX))

allPoints <- vector('list', length(csv))


df_coastDist = data.frame(matrix(NA, length(uniqueX), length(uniqueDates)),
                stringsAsFactors=F)

colnames(df_coastDist) <- c(uniqueDates)

for (n in 1:length(uniqueX)){
  #n<-102
  
  
  # Coordinates of transects
  coords <- qdapRegex::ex_between(as.character(geo[n]), ":[", "]}")[[1]]
  all_digits <- regmatches(coords, gregexpr("[-[:digit:].]+", coords))[[1]]
  
  begin_coords <- data.frame(lon = as.numeric(all_digits[1]),
                             lat = as.numeric(all_digits[2]))
  
  end_coords <- data.frame(lon = as.numeric(all_digits[3]),
                           lat = as.numeric(all_digits[4]))  
  x <- as.matrix(rbind(begin_coords, end_coords))
  lines[[n]] <- Lines(list(Line(x)), ID = n)  # create line feature
  
  # only return observations if coastDist >= 0 and if coordinates
  test1transect <- subset(csv,csv[,col_of_interest(csv, 'originX$')]== uniqueX[n] 
                          & csv[,col_of_interest(csv, 'coastDist$')] >= 0 )
  
  
  subset <- test1transect[,sort(c(col_of_interest(csv, 'coastDist$'), col_of_interest(csv, 'DATE_ACQUIRED$')))]
  
  if(is.null(nrow(subset))){
    df_coastDist[n,] <- rep(NA, length(uniqueDates))
    
  } else {
    mathcingDates <- match(colnames(df_coastDist), subset[ ,col_of_interest(subset, 'DATE_ACQUIRED')]) 
    
    df_coastDist[n,mathcingDates[!is.na(c(mathcingDates))]] <- subset[,col_of_interest(subset, 'coastDist$')]
    }
  
  # 
  # 
  # coordiates of coastline points
  coords <- data.frame(x = as.numeric(test1transect[,col_of_interest(csv, 'coastX$')]),
                       y = as.numeric(test1transect[,col_of_interest(csv, 'coastY$')]))
  
  allPoints <- rbind(allPoints, coords)
  
}

points <- SpatialPoints(allPoints, proj4string=CRS("+proj=longlat +datum=WGS84"))
AllLines <- SpatialLines(lines, proj4string=CRS("+proj=longlat +datum=WGS84"))
df <- SpatialLinesDataFrame(AllLines,data.frame(df_coastDist))

# change format of lines
lines_sf <- st_as_sf(df)
points_sf <- st_as_sf(points)

# mapview(breweries, popup = popupTable(breweries, zcol = c('founded', 'number.of.types'))) 

mapview(lines_sf,xcol = "x", ycol = "y", popup = popupTable(lines_sf)) + mapview(points_sf)

spplot(map, zcol = 1, zoom = 6, 
       colorkey = FALSE, scales = list(draw = TRUE))

# plot(lines_sf)
# 
# plot(points, add = T, col = 'red')


offShorePoints <- vector('list', length(csv))

# off-shore points for selected images
for (q in 1:length(uniqueDates)){
  q <- 6
  
  # select all rows that have an acquisition date 
  date <- uniqueDates[q]
  csv_subset <- subset(csv,csv[,col_of_interest(csv, 'DATE_ACQUIRED$')]== date) 
  
  coordsOffShore <- data.frame(x = as.numeric(csv_subset[,col_of_interest(csv_subset, 'peakCoordX$')]),
                       y = as.numeric(csv_subset[,col_of_interest(csv_subset, 'peakCoordY$')]), 
                       pos = as.numeric(csv_subset[,col_of_interest(csv_subset, 'pos$')]))
  
  offShorePoints <- rbind(offShorePoints, coordsOffShore)
  
}


mapviewOptions(basemaps = c( "Esri.WorldImagery","Esri.WorldShadedRelief", "OpenStreetMap.DE")) 
SpatialOffShore <- SpatialPoints(offShorePoints, proj4string= CRS("+proj=longlat +datum=WGS84"))

mapview(SpatialOffShore,xcol = "x", ycol = "y", crs = 4326) + lines_sf



