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
# library(rgee)
# Sys.setenv("RETICULATE_PYTHON" = "C:/Users/5600944/AppData/Local/r-miniconda/envs/rgee/python.exe")
ee_Initialize()
## ---------------------------
source("./src/functions.R")

## ---------------------------

mapviewOptions(basemaps = c( "Esri.WorldImagery","Esri.WorldShadedRelief", "OpenStreetMap.DE"))
dataFolder <- './data/processed'
# years <- c('2005', '2006','2007', '2008','2009')
years <- seq(from = 2000, to = 2020, by = 1)
aoi <- c('Suriname')

# select folders
folderSelect <- as.matrix(list.files(paste0(dataFolder, '/offshore_points'), full.names = T))
df <- rewrite(folderSelect);
# only csv's
df <- df[grep('.csv', folderSelect, ignore.case = T),]


filtered <- vector('list', 100)
for (q in seq_along(years)) {
  for (x in seq_along(aoi)){
      # q <- 1
      year = years[q]
      region = aoi[x]
      
      filters = c(year, region)
      
      filtered = rbind(filtered, df %>% 
                         dplyr::filter(
                           filters %>%
                             # apply the filter of all the text rows for each pattern
                             # you'll get one list of logical by pattern ignored_string
                             purrr::map(~ to_keep(.x, text = text)) %>%
                             # get a logical vector of rows to keep
                             purrr::pmap_lgl(all)
                         ))
}}
filtered <- unique(filtered)
allFiles <- unique(do.call(rbind, lapply(as.matrix(filtered)[,1], 
                                         function(x) read.csv(x, stringsAsFactors = FALSE,
                                                              sep = ',', 
                                                              na.strings=c("","NA")
                                                              ))))
# testReada <- read.csv(as.matrix(filtered)[1,1],
         # sep = ',', na.strings=c("","NA"))

# somehow all the dates lost 1 hour (due to timezone def?)
# allFiles$year_col <- as.POSIXct(paste(as.POSIXct(allFiles$year_col), "23:00:00")) + 60*60
# allFiles$date_col <- as.POSIXct(paste(as.POSIXct(allFiles$date_col), "23:00:00")) + 60*60
# allFiles$quarterly_col <- as.Date(as.POSIXct(paste(as.POSIXct(allFiles$quarterly_col), "23:00:00")) + 360*60)

allFiles <- allFiles %>% dplyr::mutate(year = year(DATE_ACQUIRED),
                                           month = month(DATE_ACQUIRED, label=TRUE),
                                           day = day(DATE_ACQUIRED))

# x <- as.matrix(filtered)[2,1]
# testRead <- read.csv(x, stringsAsFactors = FALSE,
#          sep = ',', na.strings=c("","NA"))
# 
# unique_all <- unique(allFiles)

# all unique transect (id's)
allPos <- unique(allFiles[, col_of_interest(allFiles, 'pos$')]);
uniqueX<- unique(allFiles[, col_of_interest(allFiles, 'originX$')]);
uniqueY<- unique(allFiles[, col_of_interest(allFiles, 'originY$')]);
# geo<- unique(allFiles[, col_of_interest(allFiles, '.geo')]);

keep_columns <- colnames(allFiles)#c('axisDist', 'dist_locf', 'distance', 'outlier', 'mudFract',
                 # 'originX', 'originY')  # necessary for mudbank output
# mudbanks <- reshape_csvPoints(allFiles, 'x', 'y', keep_columns)
mudbanks <- st_as_sf(SpatialPointsDataFrame(data.frame(
  allFiles$x, allFiles$y),
  proj4string=CRS("+proj=longlat +datum=WGS84"),
  data = data.frame(allFiles)))

collectionL4 <- ee$ImageCollection("LANDSAT/LT04/C01/T1_TOA")$
  filterBounds(ee$Geometry$Point(-55.54, 5.94))

collectionL5 <- ee$ImageCollection("LANDSAT/LT05/C01/T1_TOA")$
  filterBounds(ee$Geometry$Point(-55.54, 5.94))

collectionL7 <- ee$ImageCollection("LANDSAT/LE07/C01/T1_TOA")$
  filterBounds(ee$Geometry$Point(-55.54, 5.94))
  # merge(collection)

collectionL8 <- ee$ImageCollection("LANDSAT/LC08/C01/T1_TOA")$
  filterBounds(ee$Geometry$Point(-55.54, 5.94))

collection <- collectionL8$merge(collectionL5)$#merge(collectionL7)$
  merge(collectionL4)

visParams = list(
    bands = c("B5", "B4", "B3"),
    min = 0.05, max = 0.4, gamma = 1.4
  )

#' implement workflow
#' 1) filter outliers
#'     - Filter on neighborhood (previous 2, current and next two points)
#'     Use distance to see if the point is deviating
#'     Consider using fraction..
#' 2) Apply douglas pecker algorithm
#'      - Requires to define subsections (see https://www.tandfonline.com/doi/pdf/10.1559/152304099782424901?casa_token=9wn9uSUp3zYAAAAA:XYDB0pKcZcH69STl6eOAlKoMPEwIbvxtlwUwzZ00q4V-z8yOfAREUCePnd4fiZbS9H2A-woJqt0mIg  )
#'      to ensure separate mudbanks are recognized
#'      
# all unique dates
uniqueDates <- unique(allFiles[,'DATE_ACQUIRED']);

# # for testing: set to cloudfree
# cloudFree <- ee$Image(collection$filter(ee$Filter$lt("CLOUD_COVER", 30))$
#                         filterDate(as.character(as.Date(min(uniqueDates))-1), as.character(as.Date(max(uniqueDates))+1))$
#                         sort("CLOUD_COVER")$first()) #
# 
# 
# filtCollect <- collection$filterDate(as.character(reference_date-20), as.character(reference_date+20))$
#   sort("CLOUD_COVER")$first()
# 
# id <- as.Date(eedate_to_rdate(filtCollect$get("system:time_start")))

# or get around a reference date

# test <- which.min(abs(as.Date(uniqueDates) - reference_date))
# uniqueDates <- uniqueDates[1:length(uniqueDates) == test]

# good dates: 2017-09-02, "2018-02-27", 2018-08-28, 2018-09-27


# plot one example
reference_date <- as.Date("2009-11-15")
nearestDate <- uniqueDates[1:length(uniqueDates) == 
                             which.min(abs(as.Date(uniqueDates) - reference_date))]



# collection for testing
filtCollect <- collection$filterDate(as.character(as.Date(nearestDate)-1), 
                                     as.character(as.Date(nearestDate)+1))
dates <- ee_get_date_ic(filtCollect, time_end = FALSE)

first <- Map$addLayer(filtCollect$first(), visParams, paste0('landsat: ',nearestDate))

# or on multiple entries 
pos_to_test <- seq(from = 165000, to = 248000, by = 1000)
years_to_test <- 2005
subset_for_testPlot <- subset(allFiles, pos %in% pos_to_test &
                                year == years_to_test)

mudbanks_selection <-subset(subset_for_testPlot, mudbank_outlier != 1 &
                              mudbank_extent > 0)

mudbank_selection_Outlier <- subset(subset_for_testPlot,
                                    mudbank_outlier >= 1 |
                                    mudbank_extent < 0)


plot(rev(subset_for_testPlot$pos), rev(subset_for_testPlot$mudbank_extent+
                                         subset_for_testPlot$coast_median),
     xlim= rev(range(rev(subset_for_testPlot$pos))),
     xlab="alongshore position", ylab="along transect distance [m]",
     main = paste0('position: ',min(pos_to_test),' - ', 
                   max(pos_to_test), ' in ', years_to_test))

points(rev(subset(subset_for_testPlot, mudbank_outlier == 1)$pos),
       rev(subset(subset_for_testPlot, mudbank_outlier == 1)$mudbank_extent + 
             + subset(subset_for_testPlot, mudbank_outlier == 1)$coast_median), 
       col='red')
points(rev(subset_for_testPlot$pos), rev(subset_for_testPlot$coast_median),
       col = 'blue')

legend("topleft", 
       legend = c("boundary", "outliers", 'median coastal position'),
       col = c('black', 'red', 'blue'),
       pt.cex = 2,pch = c(1,1))

mudbankPos <- sp_pnt_ee(mudbanks_selection$x,
                        mudbanks_selection$y,  
                        'non outlier', "#ece7f2")

outlierPos <- sp_pnt_ee(mudbank_selection_Outlier$x,
                        mudbank_selection_Outlier$y,  'outlier',
                        "orange")


first + mudbankPos +outlierPos



plot(as.numeric(as.character(mudbanks_selection$pos)), 
     mudbanks_selection$mudbank_extent)
points(as.numeric(as.character(mudbank_selection_Outlier$pos)),
       mudbank_selection_Outlier$mudbank_extent, col = 'red')


all_years <- as.Date(as.POSIXlt(unique(allFiles$year_col)))
group_pos <- unique(allFiles$pos)

# get median position for each year
for(y in 1:length(all_years)){
  # y <- 4
  selected_year <- year(all_years[y])

  for (p in 1:length(group_pos)){
    # p = 230#113
    position = group_pos[p]
    # position = 202000
    
    subsets <- subset(allFiles, year == selected_year &
                      pos == position &
                        mudbank_extent > 0 &
                        mudbank_outlier == 0) # no outlier
    
    # ajoining points from same year
    # ajoining_points <- subset(allFiles, 
    #                           year_col == all_years[y] &
    #                           as.character(pos) <=  position+2000 &
    #                             as.character(pos) >= position-2000 &
    #                             as.character(pos) != position)
    
    
    
    # plot(as.Date(as.character(subsets$DATE_ACQUIRED)), subsets$axisDist,
    #      main = paste0('mudbank position position: ',position, ' [m]'),
    #      ylim = c(min(c(subsets$axisDist, subsets$coastDist), na.rm = T),
    #           max(c(subsets$axisDist, subsets$coastDist), na.rm = T)),
    #      xlab = paste0(format(as.Date(selected_year), "%Y")),
    #      ylab = 'distance from transect origin')
    # points(as.Date(as.character(subsets$DATE_ACQUIRED)), subsets$coastDist,col = 'red')

    
    # abline(h=mean(subsets$coastDist, na.rm = T), cex=2)
    # abline(h=median(subsets$coastDist, na.rm = T), col = 'red', cex = 2, lty = 2)
    # abline(h=quantile(subsets$coastDist, probs = c(0.25), na.rm= T), col ='blue', cex =2, lty = 3)
    # abline(h=quantile(subsets$coastDist, probs = c(0.75), na.rm = T), col ='blue', cex =2, lty = 3)

    # allFiles$q75 <- quantile(subsets$coastDist, probs = c(0.75))
    # allFiles$q25 <- quantile(subsets$coastDist, probs = c(0.25))


    if (nrow(subsets) > 1){
      
      # get the coastline distance;
      meanCoast <- mean(subsets$coastDist, na.rm = T)
      modalCoast <- modal(subsets$coast_median)
      
      medianOffshore <- median(subsets$axisDist, na.rm=T)
      
      bearing <- median(subsets$bearing)
      originX <- median(subsets$originX)
      originY <- median(subsets$originY)
      
      # set origin at correct location or add coastline dist to dist of interest?
      destPoint <- destPoint(SpatialPoints(data.frame(x = originX, y = originY),
                                           CRS("+proj=longlat +datum=WGS84")), 
                             bearing, medianOffshore)
      
      # spatialDest <- SpatialPoints(data.frame(x = destPoint[1], y = destPoint[2]),
      #                              CRS("+proj=longlat +datum=WGS84"))
      # 
      
      # apply density function
      # pdens <- pointdensity(df = subsets, lat_col = "x", lon_col = "y",
      #                       date_col = "DATE_ACQUIRED", grid_size = 0.1, 
      #                       radius = 1)
      # 
      # # or on combination data set( neighbouring transects)
      # combined <- rbind(subsets, ajoining_points)
      # pdens_comb <- pointdensity(df = combined, lat_col = "x", lon_col = "y",
      #                       date_col = NULL, grid_size = 0.5, 
      #                       radius = 1)
      # 
      # # now it is the trick to find the max count for the selected transect
      # Tempcount <- merge(subsets, pdens_comb, by.x=c('x', 'y'), 
      #                        by.y=c('lat', 'lon'))
      # 
      # 
      # maxCount <- Tempcount[Tempcount$count == max(Tempcount$count),]
      # maxCount_spatial <- SpatialPoints(data.frame(x = maxCount$x, 
      #                          y = maxCount$y),
      #               CRS("+proj=longlat +datum=WGS84"))
      
      
      # spatial_pdens <- SpatialPoints(data.frame(x = pdens$lat[which.max(pdens$count)], 
      #                                           y = pdens$lon[which.max(pdens$count)]),
      #                                CRS("+proj=longlat +datum=WGS84"))
      
      
      allFiles[which(row.names(allFiles) %in% row.names(subsets)), 'distX'] <-
        # pdens$lat[which.max(pdens$count)]
        destPoint[1]
      
      allFiles[which(row.names(allFiles) %in% row.names(subsets)), 'distY'] <-
        # pdens$lon[which.max(pdens$count)]
        destPoint[2]
      # 
      # allObs_year <- SpatialPoints(data.frame(x = subsets$x, y = subsets$y),
      #                         CRS("+proj=longlat +datum=WGS84"))
      # all_obs_ajoining <- SpatialPoints(data.frame(x = ajoining_points$x, 
      #                                              y = ajoining_points$y),
      #                                   CRS("+proj=longlat +datum=WGS84"))

      # mapView(originPoint, col.regions = c("blue")) + mapView(spatialDest, col.regions = c('yellow')) +
      #   mapView(allObs_year) + mapView(spatial_pdens, col.regions = c('orange')) +
      #   mapView(all_obs_ajoining) + 
      #   mapView(maxCount_spatial, col.regions = c("red"))
      #   
      #   
    }
    
   
    
  }
}



# filter points in river mouths:
# 139000 - 147000 (suriname Rivier)
# 242000 -252000  (saramacca rivier / coppename)
image <- collection$filterDate(as.character(as.Date('2000-01-01')-1), 
                                     as.character(as.Date('2000-12-31')+1))$
                  sort("CLOUD_COVER")$first()


# group_pos <- seq(95000, 120000,1000)
# group_pos <- seq(190000, 240000,1000)
# group_pos <- seq(280000, 340000,1000)
subsets <- subset(allFiles,  year_col == all_years[1] &
                    # as.numeric(as.character(pos)) %in% pos_to_test &
                    mudbank_outlier == 0)

# subsets2 <- subset(allFiles,  year_col == all_years[7] &
#                     # as.numeric(as.character(pos)) %in% pos_to_test &
#                     mudbank_outlier == 0)
# subsets3 <- subset(allFiles,  year_col == all_years[12] &
#                      # as.numeric(as.character(pos)) %in% pos_to_test &
#                      mudbank_outlier == 0)

allObs <- sp_pnt_ee(subsets$x,
          subsets$y,  'allObs',
          "orange")


# remove NA
meanPos <- SpatialPoints(data.frame(x = subsets$distX[complete.cases(subsets$distX)],
                                    y =  subsets$distY[complete.cases(subsets$distY)]),
                         CRS("+proj=longlat +datum=WGS84"))

meanPos_sp <- sp_pnt_ee(meanPos$x,
          meanPos$y,  'meanPos',
          "red")


# meanPos2 <- SpatialPoints(data.frame(x = subsets2$distX[complete.cases(subsets2$distX)],
#                                     y =  subsets2$distY[complete.cases(subsets2$distY)]),
#                          CRS("+proj=longlat +datum=WGS84"))
# meanPos3 <- SpatialPoints(data.frame(x = subsets3$distX[complete.cases(subsets3$distX)],
#                                      y =  subsets3$distY[complete.cases(subsets3$distY)]),
#                           CRS("+proj=longlat +datum=WGS84"))
# 
# mapView(meanPos, col.regions = c("red"), layer.name = as.character(all_years[4])) +
#   mapView(meanPos2, col.regions = c("green"), layer.name = as.character(all_years[7])) +
#   mapView(meanPos3, col.regions = c("blue"), layer.name = as.character(all_years[10]))
#   


# or on combination data set( neighbouring transects)
# combined <- rbind(subsets, ajoining_points)
selection_density <- pointdensity(df = subsets, lat_col = "x", lon_col = "y",
                                  date_col = NULL, grid_size = 0.1, 
                                  radius = 1)

# now it is the trick to find the max count for the selected transect
Tempcount <- merge(subsets, selection_density, by.x=c('x', 'y'), 
                   by.y=c('lat', 'lon'))


# find max for each column
test <- Tempcount %>% 
  dplyr::group_by(pos) %>%
  top_n(1, count)

# ggplot idea: https://www.earthdatascience.org/tutorials/visualize-2d-point-density-ggmap/



# maxCount <- Tempcount[Tempcount$count == max(Tempcount$count),]
maxCount <- SpatialPoints(data.frame(x = test$x, 
                                             y = test$y),
                                  CRS("+proj=longlat +datum=WGS84"))


maxCount_spatial <- sp_pnt_ee(maxCount$x,maxCount$y,
                              'density', "yellow")


image_date<-eedate_to_rdate(image$get("system:time_start"))
first <- Map$addLayer(image, visParams, paste0('landsat: ',image_date))

Map$centerObject(image, 14)
# transects <- build_csvLines(allFiles)

first + allObs  + meanPos_sp + maxCount_spatial


# test mudbank estimate on 1 image
# ref_date <- c('2009-09-12')
# subset_1_image <- subset(allFiles, DATE_ACQUIRED == as.Date(ref_date) &
#                      pos %in% pos_to_test &
#                     mudbank_extent > 0 &
#                     mudbank_outlier == 0)
# 
# 
# spatial_test<- SpatialPoints(data.frame(x = subset_1_image$x, y = subset_1_image$y),
#               CRS("+proj=longlat +datum=WGS84"))
# 
# test_image <- collection$filterDate(as.character(as.Date(ref_date)-1), 
#                       as.character(as.Date(ref_date)+1))$
#   sort("CLOUD_COVER")$first()
# first <- Map$addLayer(test_image, visParams, paste0('landsat: ',image_date))
# first + mapView(spatial_test)


mas_folder <- 'D:/BackUp_D_mangroMud_202001/Research/External_Data/Spatial/Kustmetingen_MAS/surveys'
# relevant files: 
# 4-6-2002 (near Suriname river)
# 31-12-2008 (entire coast of Suriname)

mas_folder2 <- 'D:/BackUp_D_mangroMud_202001/Research/External_Data/Source/KustmetingMAS_source/shp'
# relevant file: 2016_west 
# to lesser extent 2014_oost

folderSelect <- as.matrix(list.files(paste0(mas_folder2), full.names = T))
df <- rewrite(folderSelect);
df <- df[grep('.shp', folderSelect, ignore.case = T),]



# yearsMas <- c('2008')
# filteredMas <- vector('list', 100)
# for (q in seq_along(yearsMas)) {
#   # q <- 1
#   year = yearsMas[q]
#   
#   filters = c(year)
#   
#   filteredMas = rbind(filteredMas, df %>% 
#                      dplyr::filter(
#                        filters %>%
#                          # apply the filter of all the text rows for each pattern
#                          # you'll get one list of logical by pattern ignored_string
#                          purrr::map(~ to_keep(.x, text = text)) %>%
#                          # get a logical vector of rows to keep
#                          purrr::pmap_lgl(all)
#                      ))
# }
# filteredMas <- unique(filteredMas)
# allMasPoints <- do.call(rbind, lapply(as.matrix(filteredMas)[,1], function(x) shapefile(x)))

# or a single file
allMasPoints <- shapefile(paste0(df[2,]))
first + mapView(allMasPoints, zcol = "z") + mapView(maxCount_spatial,
                                                    col.regions = c("yellow")) +
  mapView(meanPos, col.regions = c("red"), layer.name = 'meanPos' ) +
  mapView(allObs)

e <- extent(bbox(allMasPoints))
# e[4] <- 6.2
# e[1] <- -56
# e[2] <- -55
# p <- as(e, 'SpatialPolygons')  

# crs(p) <- crs(allMasPoints)
# mapView(p) + mapView(overPoints)

# overPoints <- over(allMasPoints,p )
# overPoints <- point.in.poly(allMasPoints, p)


# library(geosphere)
library(rgeos)
# library(spatialEco)
library(sp)

# get nearest observation for 2005
dist_with_markers <- gDistance(maxCount_spatial[50],allMasPoints, byid=T)
nearest <- apply(dist_with_markers,2, which.min)
nearest_dist <- apply(dist_with_markers, 2, min) 


mapView(allMasPoints[nearest,])+mapView(maxCount_spatial[50])

# or?
# dist from average to other points (in m) that make up the emlid measurement
# average_dist[[q]] <- distGeo(cbind(l[2], l[1]), coords, a=6378137, f=1/298.257223563)
# proj$stdev[q] <- sd(average_dist[[q]], na.rm = T)
# proj$mean_dist[q] <- mean(average_dist[[q]], na.rm = T)

# crashes..
# mapView(allPoints)

# 
# 
# 
# # douglasPeucker
# # requires
# functionD <- DouglasPeuckerEpsilon(trajx = subset_1_image$x,
#                                    trajy = subset_1_image$y, epsilon = 0.01, 
#                                    spar = NA)
# 
# first + mapView(spatial_test) +  mapView(SpatialPoints(data.frame(x = functionD$x, y = functionD$y),
#                                                CRS("+proj=longlat +datum=WGS84")))
# 
# plot(subset_1_image$x, subset_1_image$y)
# # testD <- DouglasPeuckerNbPoints( geom_ordered[range,1], trajy = geom_ordered[range,2], 2, spar=NA)
# # points( testD[,1], testD[,2],type="p", col = 'red')
# points( functionD[,1], functionD[,2],type="p", col = 'red')
# 
# # convex hull
# # only appliccable per mudbank estimate 
# convexHull <- chull(subset_1_image$x, subset_1_image$y)
# points(subset_1_image$x[convexHull], subset_1_image$y[convexHull],
#        col = 'blue')
# 
# first + mapView(spatial_test) + 
#   mapView(SpatialPoints(data.frame(x = functionD$x, y = functionD$y),
#                        CRS("+proj=longlat +datum=WGS84"))) +
#   mapView(SpatialPoints(data.frame(x = subset_1_image$x[convexHull], 
#                                    y = subset_1_image$y[convexHull]),
#                         CRS("+proj=longlat +datum=WGS84")))
# 


# https://www.statsandr.com/blog/outliers-detection-in-r/#:~:text=An%20outlier%20is%20a%20value,significantly%20from%20other%20data%20points.
# K <- length(testPos$coastDist)-2
# if(K > 10){
#   K <- 9
# }
# Rtest <- rosnerTest(testPos$coastDist,
#                    k = length(testPos$coastDist) - 2
#                    # k = 10 or if length < 10 it should be 
#                    # this value makes a difference.... apply to blocks of 3 years ==> so outlier size depends on amount of images in those 3 years?
# )
# Rtest2 <- rosner(testPos$coastDist)
# 
# indices <- Rtest$all.stats$Obs.Num[which(Rtest$all.stats$Outlier)]
# 
# # if outlier give 1 in a new column
# testPos$outlier <- 0 # TRUE
# testPos$outlier[indices] <- 1 # FALSE
# # plot outliers
# points(as.Date(testPos$DATE_ACQUIRED[testPos$outlier == 0]),testPos$coastDist[testPos$outlier == 0], col = 'red')



# positions<- as.numeric(as.character(combined_ordered$pos))
# distances <-combined_ordered$axisDist
# x<-seq(0,50,1)
# y<-((runif(1,10,20)*x)/(runif(1,0,10)+x))+rnorm(51,0,1)

# starts2 <- gausslin.start(positions, distances)
# ystart2 <-with(starts2, As*exp(-((x-Bs)/Cs)^2)+Ds*x+Es)
# xo <- order(positions)
# plot(positions, distances)
# lines(positions[xo],ystart2[xo],col=2)
# 
# 
# 
# geom <- st_coordinates(ajoining_points)
# test_spline<-smooth.spline(geom[,1] ~ geom[,2], spar=0.50)
# 
# SpatialPoints <- SpatialPointsDataFrame(data.frame(test_spline$y, test_spline$x ), 
#                                         data = data.frame(DATE_ACQUIRED = ajoining_points$DATE_ACQUIRED,
#                                                           mudFract = ajoining_points$mudFract),
#                                         proj4string=CRS("+proj=longlat +datum=WGS84"))

# points_sf <- st_as_sf(SpatialPoints)
# https://gis.stackexchange.com/questions/68359/creating-average-polygon


# if dist < threshold (e.g. 0.01?) then keep
# else check for index value, if sufficient: keep it 
# http://www.scielo.br/scielo.php?script=sci_arttext&pid=S0104-65002004000100006
# http://www.geoinfo.info/proceedings_geoinfo2006.split/paper1.pdf
# https://www.tandfonline.com/doi/pdf/10.1559/152304099782424901?casa_token=9wn9uSUp3zYAAAAA:XYDB0pKcZcH69STl6eOAlKoMPEwIbvxtlwUwzZ00q4V-z8yOfAREUCePnd4fiZbS9H2A-woJqt0mIg 

# test douglasPeuckerEpsilon
# library(kmlShape)




# douglas pecker algorithm looks at the points that are furthest away from line, if large enough: included as vertext.
# this assumes outliers are filtered sufficiently
# e.g. first filter (e.g. by using a spline function (spar in Douglas function))
# or manual filter that looks at a combination of distance on a line, and the fraction (within a range)

# Alternatively; inverse of douglas filtering. 
# Look at point, if it's distance is to far away exclude it
# But that will only work if whe apply it on sub polylines that are on a imaginary line that corresponds to a boundary
# http://www.scielo.br/scielo.php?script=sci_arttext&pid=S0104-65002004000100006

#   findClosestPoint_manual <- function(trajx,trajy){
# 
#   trajx <- geom_ordered[range,1]
#   trajy <- geom_ordered[range,2]
# 
#   dmax <- 1
#   index <- 1
#   end <- length(trajx)
#   
#   if(end==2){
#     index <- 1
#     dmax <- 0
#   }else{
#     for(i in 2:(end-1)){ # for each point but the first and last
#       i <- 2
#       # calculate the distance
#       d <- shortestDistanceToLines(Mx=trajx[i],My=trajy[i], Ax=trajx[1],Ay=trajy[1], Bx=trajx[end],By=trajy[end])
#       if ( d < dmax ) {
#         # update dmax & index with the distance
#         # in the end only the max distance is included (due to the if(d>dmax))
#         index <- i
#         dmax <- d
#       }else{} # don't do anything
#     }
#   }
#   
#   output <- c(index, dmax)
#   names(output) <- c('index', 'dmax')
#   
#   return(output)
#   # return(c(index=index,dmax=dmax))
# }

# nearestPoint <- findClosestPoint_manual(geom_ordered[range,1],geom_ordered[range,2])
# points( geom_ordered[nearestPoint['index'],1], geom_ordered[nearestPoint['index'],2],type="p", col = 'green')
# segments(trajx[1],trajy[1],  trajx[end], trajy[end])


# farestPoint <- findFarestPoint_manual(geom_ordered[range,1], geom_ordered[range,2])

# points(geom_ordered[farestPoint['index'],1], geom_ordered[farestPoint['index'],2],type="p", col = 'green')


