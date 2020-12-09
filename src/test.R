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
folderSelect <- as.matrix(list.files(paste0(dataFolder, '/GEE_exports'), full.names = T))
# metaMatrix <- as.matrix(list.files(folderSelect, pattern=".csv", full.names = T))
mapviewOptions(basemaps = c( "Esri.WorldImagery","Esri.WorldShadedRelief", "OpenStreetMap.DE"))

df <- rewrite(folderSelect);

# only csv's
df <- df[grep('.csv', folderSelect, ignore.case = T),][1:2,]

# csv <- data.frame(matrix(NA, 0, 18),
#            stringsAsFactors=F)
# 
# for (q in nrow(df)){
#   # q = 2
#   csv <- rbind(csv, as.matrix(read.csv2(as.character(df[q,1]), 
#                       header = T, sep = ',', na.strings=c("","NA"))))
#   
# }

csv = as.matrix(read.csv2(as.character(df[2,1]),
                          header = T, sep = ',', na.strings=c("","NA"))) # rewrite as matrix to read columns as numeric values
dates <- col_of_interest(csv, 'DATE_ACQUIRED$')
coastDist <- col_of_interest(csv, 'coastDist$')

# all unique dates
uniqueDates <- unique(csv[,dates]);

# all unique transect (id's)
pos <- unique(csv[, col_of_interest(csv, 'pos$')]);
uniqueX<- unique(csv[, col_of_interest(csv, 'originX$')]);
uniqueY<- unique(csv[, col_of_interest(csv, 'originY$')]);
geo<- unique(csv[, col_of_interest(csv, '.geo')]);

# define output matrices
lines <- vector('list', length(uniqueX));
allPoints <- vector('list', length(csv));
df_coastDist = data.frame(matrix(NA, length(uniqueX), length(uniqueDates)+1),
                stringsAsFactors=F) # Empty data frame for coastal distance
colnames(df_coastDist) <- c('pos', uniqueDates)

for (n in 1:length(uniqueX)){
  #n<-103
  
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
  
  subset <- test1transect[,sort(c(col_of_interest(csv, 'coastDist$'), col_of_interest(csv, 'DATE_ACQUIRED$'),
                                  col_of_interest(csv, 'pos$')))]
  
  # construct a data frame that matches the lines
  if(is.null(nrow(subset))){
    df_coastDist[n,] <- c(as.numeric(subset[3]), rep(NA, length(uniqueDates)))

    # THIS THROWS ERROR LATER ON WHEN THERE IS NOTHING IN SUBSET ALSO NO POS IS ASSIGNED TO THE TRANSECT DATABASE!
    
  } else {
    # at the correct location in df_coastDist append the offshore location of mudbank boundary
    mathcingDates <- match(subset[ ,col_of_interest(subset, 'DATE_ACQUIRED')], colnames(df_coastDist)) 
    
    # append the pos number of the transect to the first column
    df_coastDist[n,col_of_interest(df_coastDist, 'pos$')] <-  as.numeric(subset[,col_of_interest(subset, 'pos$')])[1]
    
    # fill for each date column the retrieved value
    df_coastDist[n,mathcingDates[!is.na(c(mathcingDates))]] <- subset[,col_of_interest(subset, 'coastDist$')]

    }

  # coordiates of coastline points
  coords <- data.frame(x = as.numeric(test1transect[,col_of_interest(csv, 'coastX$')]),
                       y = as.numeric(test1transect[,col_of_interest(csv, 'coastY$')]),
                       DATE_ACQUIRED = as.character(test1transect[,col_of_interest(csv, 'DATE_ACQUIRED$')]),
                       pos = as.character(test1transect[,col_of_interest(csv, 'pos$')]))
  
  allPoints <- rbind(allPoints, coords)
  
}

points <- SpatialPointsDataFrame(data.frame(allPoints[,'x'], allPoints[,'y'] ), 
                                 data = data.frame(DATE_ACQUIRED = allPoints[,'DATE_ACQUIRED'],
                                                   pos = allPoints[,'pos']),
                                 proj4string=CRS("+proj=longlat +datum=WGS84"))
# colnames(points@data) <- c("DATE_ACQUIRED")
AllLines <- SpatialLines(lines, proj4string=CRS("+proj=longlat +datum=WGS84"))
sp_Lines_df <- SpatialLinesDataFrame(AllLines,data.frame(df_coastDist))

# change format of lines
lines_sf <- st_as_sf(sp_Lines_df)
points_sf <- st_as_sf(points)

mapview(lines_sf,xcol = "x", ycol = "y") + mapview(points_sf, zcol = c("DATE_ACQUIRED"))

# order on column names
df_coastDist <- df_coastDist[,order(names(df_coastDist))]

plot(as.Date(colnames(df_coastDist)[1:ncol(df_coastDist)-1]), c(1:length(2:ncol(df_coastDist))))

plot(as.Date(colnames(df_coastDist)[1:ncol(df_coastDist)-1]), df_coastDist[1,1:ncol(df_coastDist)-1])
clicked <- identify(as.Date(colnames(df_coastDist)[1:ncol(df_coastDist)-1]),
                    df_coastDist[1,1:ncol(df_coastDist)-1], 
                    n=1, labels=colnames(df_coastDist)[1:ncol(df_coastDist)-1])


offShorePoints <- vector('list', length(csv))
# off-shore points for selected images
for (q in 1:length(uniqueDates)){
  # q <- 6
  
  # select all rows that have an acquisition date 
  date <- uniqueDates[q]
  csv_subset <- subset(csv,csv[,col_of_interest(csv, 'DATE_ACQUIRED$')]== date) 
  
  coordsOffShore <- data.frame(x = as.numeric(csv_subset[,col_of_interest(csv_subset, 'peakCoordX$')]),
                       y = as.numeric(csv_subset[,col_of_interest(csv_subset, 'peakCoordY$')]), 
                       pos = as.numeric(csv_subset[,col_of_interest(csv_subset, 'pos$')]),
                       DATE_ACQUIRED = as.character(date),
                       axisDist = as.numeric(csv_subset[,col_of_interest(csv_subset, 'axisDist$')]))
  
  offShorePoints <- rbind(offShorePoints, coordsOffShore)
  
}


# filter off-shore points -1
offShorePoints <- subset(offShorePoints, offShorePoints[,col_of_interest(offShorePoints, 'x')] != -1)

# Example per time step
date_to_test <- c('2019-09-16')
testYearly <- subset(offShorePoints, offShorePoints[,col_of_interest(offShorePoints, 'DATE_ACQUIRED')] == date_to_test)

testYearlyCoastline <- subset(allPoints, allPoints[,col_of_interest(allPoints, 'DATE_ACQUIRED')] == date_to_test)

# transform to spatialPointsDataFrame
SpatialtestYearly <- SpatialPointsDataFrame(data.frame(testYearly[,'x'], testYearly[,'y'] ), 
                                          data = data.frame(pos = testYearly[,'pos'], 
                                                            DATE_ACQUIRED= testYearly[,'DATE_ACQUIRED'],
                                                            axisDist = testYearly[,'axisDist']),
                                          proj4string=CRS("+proj=longlat +datum=WGS84"))

Spatial_testYearlyCoastline <- SpatialPointsDataFrame(data.frame(testYearlyCoastline[,'x'], testYearlyCoastline[,'y'] ), 
                                                      data = data.frame(pos = testYearlyCoastline[,'pos'], 
                                                                        DATE_ACQUIRED= testYearlyCoastline[,'DATE_ACQUIRED']),
                                                      proj4string=CRS("+proj=longlat +datum=WGS84"))

mapview(SpatialtestYearly, xcol = "x", ycol = "y")  + 
        mapview(Spatial_testYearlyCoastline, xcol = "x", ycol = "y", col.regions = c("red"))


# create a spatialData Frame (for interactive plotting)
SpatialOffShore <- SpatialPointsDataFrame(data.frame(offShorePoints[,'x'], offShorePoints[,'y'] ), 
                                          data = data.frame(offShorePoints[,'pos'], offShorePoints[,'DATE_ACQUIRED']),
                                          proj4string=CRS("+proj=longlat +datum=WGS84"))
colnames(SpatialOffShore@data) <- c("pos","DATE_ACQUIRED")
SpatialOffShore_sf <- st_as_sf(SpatialOffShore)
# SpatialOffShore <- SpatialPoints(offShorePoints, proj4string= CRS("+proj=longlat +datum=WGS84"))

mapview(SpatialOffShore_sf,xcol = "x", ycol = "y",  zcol = c("DATE_ACQUIRED")) + mapview(lines_sf)

# average position per tansect
allAveragePoints <- vector('list', length(SpatialtestYearly@data$pos));
for (x in 1:length(SpatialtestYearly@data$pos)){
  
  # x<-113
  alongshorePosition <- SpatialtestYearly@data$pos[x]
  
  transectObs = subset(offShorePoints, offShorePoints[,col_of_interest(offShorePoints, 'pos')] == alongshorePosition)
  
  meanAxisDist = mean(transectObs$axisDist)
  
  # translate distances along transect
  # get corresponding transect (matching pos)
  selectedTransect <- sp_Lines_df[sp_Lines_df$pos == alongshorePosition, ]
  # mapview(st_as_sf(selectedTransect))
  
  # get coordinates at meanDistance away from origin
  # change to meters
  selectedTransect <- spTransform(selectedTransect, CRS(as.character("+proj=utm +zone=21 +ellps=intl +towgs84=-265,120,-358,0,0,0,0 +units=m +no_defs")))
  lineCoords <- coordinates(selectedTransect)[[1]][[1]]
  
  # theta <- atan2(pos$ynext-pos$yprev, pos$xnext-pos$xprev)
  # # Angle between points on the line in radians (y1- y0, x1-x0)
  theta <- atan2(lineCoords[1]-lineCoords[2], lineCoords[3]-lineCoords[4])   #?
  thetaT <- theta+pi/2
  
  dx_poi <- meanAxisDist*cos(thetaT)      # coordinates of point of interest as defined by position length (sep)
  dy_poi <- meanAxisDist*sin(thetaT)
  
  xnew = lineCoords[1] - dx_poi
  ynew = lineCoords[3] - dy_poi
  
  newPoint <- spTransform(SpatialPoints(data.frame(x = xnew, y = ynew),CRS(as.character(selectedTransect@proj4string))),
                          CRS("+proj=longlat +datum=WGS84"))
  
  allAveragePoints <- rbind(allAveragePoints, data.frame(x=newPoint@coords[1], y = newPoint@coords[2],
                            pos = alongshorePosition,
                            meanAxisDist = meanAxisDist))
  
  # mapview(st_as_sf(selectedTransect)) +mapview(newPoint)
  
}

meaOffShore <- SpatialPointsDataFrame(data.frame(allAveragePoints[,'x'], allAveragePoints[,'y'] ), 
                                          data = data.frame(allAveragePoints[,'pos'], allAveragePoints[,'meanAxisDist']),
                                          proj4string=CRS("+proj=longlat +datum=WGS84"))
st_meaOffShore <- st_as_sf(meaOffShore)
mapview(st_meaOffShore,xcol = "x", ycol = "y", col.regions = c("red")) + mapview(lines_sf) + mapview(SpatialOffShore_sf,xcol = "x", ycol = "y",  zcol = c("DATE_ACQUIRED"))
# apply filters:
# remove points of which neighbouring points (or two neighboring points?) are to far away (cross + along shore)
# remove points that are on-shore


