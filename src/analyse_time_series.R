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
ee_Initialize()
## ---------------------------
source("./src/functions.R")

## ---------------------------

mapviewOptions(basemaps = c( "Esri.WorldImagery","Esri.WorldShadedRelief", "OpenStreetMap.DE"))
dataFolder <- './data/processed'
years <- c('2005', '2006','2007', '2008','2009') 

# select folders
folderSelect <- as.matrix(list.files(paste0(dataFolder, '/offshore_points'), full.names = T))
df <- rewrite(folderSelect);
# only csv's
df <- df[grep('.csv', folderSelect, ignore.case = T),]


filtered <- vector('list', 100)
for (q in seq_along(years)) {
      # q <- 1
      year = years[q]
      
      filters = c(year)
      
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
allFiles <- unique(do.call(rbind, lapply(as.matrix(filtered)[,1], function(x) read.csv(x, stringsAsFactors = FALSE,
                                                                                sep = ',', na.strings=c("","NA")
                                                                                ))))

# somehow all the dates lost 1 hour (due to timezone def?)
allFiles$year_col <- as.POSIXct(paste(as.POSIXct(allFiles$year_col), "23:00:00")) + 60*60
allFiles$date_col <- as.POSIXct(paste(as.POSIXct(allFiles$date_col), "23:00:00")) + 60*60


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

collection <- collectionL8$merge(collectionL7)$merge(collectionL5)$
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

mudbanks_selection <-subset(mudbanks, 
                            as.Date(mudbanks$DATE_ACQUIRED) == nearestDate) 

mudbank_selection_Outlier <- subset(mudbanks, 
                                    as.Date(DATE_ACQUIRED) == nearestDate &
                                    (mudbanks$mudbank_outlier >= 1 |
                                    mudbanks$mudbank_distance < 0))

# collection for testing
filtCollect <- collection$filterDate(as.character(as.Date(nearestDate)-1), 
                                     as.character(as.Date(nearestDate)+1))
dates <- ee_get_date_ic(filtCollect, time_end = FALSE)

first <- Map$addLayer(filtCollect$first(), visParams, paste0('landsat: ',nearestDate))



first + 
  # mapview(coastlines_selection, col.regions = c("red"), layer.name = paste0('coastlines ',nearestDate)) +
  mapview(mudbanks_selection, col.regions = c("green"), layer.name = 'non outlier' ) +
  mapview(mudbank_selection_Outlier, col.regions = c("orange"), layer.name = 'outlier' )
  # mapview(mudbanks_selection2, col.regions = c("blue"), layer.name = '2009-11-15' )

plot(as.numeric(as.character(mudbanks_selection$pos)), 
     mudbanks_selection$mudbank_distance)
points(as.numeric(as.character(mudbank_selection_Outlier$pos)),
       mudbank_selection_Outlier$mudbank_distance, col = 'red')


all_years <- as.Date(as.POSIXlt(unique(allFiles$year_col)))


# 
for(y in 1:length(all_years)){
  # y <- 5
  year <- as.Date(all_years[y])

  for (p in 1:length(group_pos)){
    # p = 113
    # position = group_pos[p]
    position = 200000
    
    subsets <- subset(allFiles, year_col == year &
                      pos == position &
                        mudbank_distance > 0 &
                        mudbank_outlier == 0) # no outlier
    
    plot(as.Date(as.character(subsets$DATE_ACQUIRED)), subsets$axisDist,
         main = paste0('coastline position: ',position, ' [m]'), 
         ylim = c(min(c(subsets$axisDist, subsets$coastDist), na.rm = T),
              max(c(subsets$axisDist, subsets$coastDist), na.rm = T)),
         xlab = paste0(format(as.Date(year), "%Y")),
         ylab = 'distance from transect origin')
    points(as.Date(as.character(subsets$DATE_ACQUIRED)), subsets$coastDist,col = 'red')
    
    
    
    # abline(h=mean(subsets$coastDist, na.rm = T), cex=2)
    # abline(h=median(subsets$coastDist, na.rm = T), col = 'red', cex = 2, lty = 2)
    # abline(h=quantile(subsets$coastDist, probs = c(0.25), na.rm= T), col ='blue', cex =2, lty = 3)
    # abline(h=quantile(subsets$coastDist, probs = c(0.75), na.rm = T), col ='blue', cex =2, lty = 3)

    # allFiles$q75 <- quantile(subsets$coastDist, probs = c(0.75))
    # allFiles$q25 <- quantile(subsets$coastDist, probs = c(0.25))


    if (nrow(subsets) > 1){
      dist_of_interest <- mean(subsets$coastDist)
      
      # should be 1 origin of the line.
      originX <- unique(subsets$originX)
      originY <- unique(subsets$originY)
      
      # take any one x and y coordinate (angle is allways the same)
      endX <- subsets$x[1]
      endY <- subsets$y[1]
      
      # transform to meters
      # http://www.movable-type.co.uk/scripts/latlong.html?from=48.86,-122.0992&to=48.8599,-122.1449
      originPoint <- spTransform(SpatialPoints(data.frame(x = originX, y = originY),
                                               CRS("+proj=longlat +datum=WGS84")),
                                 CRS(as.character("+proj=utm +zone=21 +ellps=intl +towgs84=-265,120,-358,0,0,0,0 +units=m +no_defs")))
      
      # originX_m <- coordinates(originPoint)[1,1]
      # originY_m <- coordinates(originPoint)[1,2]
      
      # endPoint <- spTransform(SpatialPoints(data.frame(x = endX, y = endY),
      #                                          CRS("+proj=longlat +datum=WGS84")),
      #                            CRS(as.character("+proj=utm +zone=21 +ellps=intl +towgs84=-265,120,-358,0,0,0,0 +units=m +no_defs")))
      # 
      # endPointX_m <- coordinates(endPoint)[1,1]
      # endPointY_m <- coordinates(endPoint)[1,2]
      # mapView(originPoint) + mapView(endPoint)
      
      # y1 - y0, x1 - x0
      # theta <- atan2(lineCoords[1]-lineCoords[2], lineCoords[3]-lineCoords[4])   #?
      # theta <- atan2(endPointY_m - originY_m, endPointY_m - originX_m)
      # thetaT <-theta * 180 / pi # bearing in degrees #  theta+pi/2 
      # 
      
      bearing <- bearing(SpatialPoints(data.frame(x = originX, y = originY),
                                       CRS("+proj=longlat +datum=WGS84")),
                         SpatialPoints(data.frame(x = endX, y = endY),
                                       CRS("+proj=longlat +datum=WGS84"))
      )
      
      # origin X needs to be corrected by coastline pos, just like
      # mudbank locations
      # set origin at correct location or add coastline dist to dist of interest?
      destPoint <- destPoint(SpatialPoints(data.frame(x = originX, y = originY),
                                           CRS("+proj=longlat +datum=WGS84")), bearing, dist_of_interest)
      
      # spatialDest <- SpatialPoints(data.frame(x = destPoint[1], y = destPoint[2]),
                                   # CRS("+proj=longlat +datum=WGS84"))
      
      allFiles_gt0[which(row.names(allFiles_gt0) %in% row.names(subsets)), 'distX'] <-
        destPoint[1]
      
      allFiles_gt0[which(row.names(allFiles_gt0) %in% row.names(subsets)), 'distY'] <-
        destPoint[2]
      
      # dx_poi <- dist_of_interest*cos(thetaT)
      # dy_poi <- dist_of_interest*sin(thetaT)
      # 
      # # xnew = lineCoords[1] - dx_poi
      # # ynew = lineCoords[3] - dy_poi
      # 
      # xnew = originX_m - dx_poi
      # ynew = originY_m - dy_poi
      # 
      # # xnew = lineCoords[1] - dx_poi
      # ynew = lineCoords[3] - dy_poi
      # distPoint <- spTransform(SpatialPoints(data.frame(x = xnew, y = ynew),
      #                                       CRS(as.character("+proj=utm +zone=21 +ellps=intl +towgs84=-265,120,-358,0,0,0,0 +units=m +no_defs"))),
      #                                       CRS("+proj=longlat +datum=WGS84"))
      
      mapView(originPoint, col.regions = c("red")) + mapView(spatialDest) # + mapView(endPoint)
      
      
    }
    
   
    
  }
}

# filter points in river mouths:
# 139000 - 147000 (suriname Rivier)
# 242000 -252000  (saramacca rivier / coppename)


image <- collection$filterDate(as.character(as.Date(year)-1), 
                                     as.character(as.Date(year)+365))$
                  sort("CLOUD_COVER")$first()


# group_pos <- seq(95000, 120000,1000)
# group_pos <- seq(190000, 240000,1000)
# group_pos <- seq(280000, 340000,1000)
subsets <- subset(allFiles_gt0, as.Date(as.POSIXlt(year_col)) == year &
                    as.numeric(as.character(pos)) %in% 
                    group_pos &
                    outlier == 0)

meanPos <- SpatialPoints(data.frame(x = subsets$distX, y =  subsets$distY),
                         CRS("+proj=longlat +datum=WGS84"))

# spatialDest <- SpatialPoints(data.frame(x = destPoint[1], y = destPoint[2]), 
#                              CRS("+proj=longlat +datum=WGS84"))

image_date<-eedate_to_rdate(image$get("system:time_start"))
first <- Map$addLayer(image, visParams, paste0('landsat: ',image_date))

Map$centerObject(image, 12)

first + mapview(subsets) + mapView(meanPos, col.regions = c("red"), layer.name = 'meanPos' )


# for (x in 1:length(SpatialtestYearly@data$pos)){
#   
#   # x<-113
#   alongshorePosition <- SpatialtestYearly@data$pos[x]
#   
#   transectObs = subset(offShorePoints, offShorePoints[,col_of_interest(offShorePoints, 'pos')] == alongshorePosition)
#   
#   meanAxisDist = mean(transectObs$axisDist)
#   
#   # translate distances along transect
#   # get corresponding transect (matching pos)
#   selectedTransect <- sp_Lines_df[sp_Lines_df$pos == alongshorePosition, ]
#   # mapview(st_as_sf(selectedTransect))
#   
#   # get coordinates at meanDistance away from origin
#   # change to meters
#   selectedTransect <- spTransform(selectedTransect, CRS(as.character("+proj=utm +zone=21 +ellps=intl +towgs84=-265,120,-358,0,0,0,0 +units=m +no_defs")))
#   lineCoords <- coordinates(selectedTransect)[[1]][[1]]
#   
#   # theta <- atan2(pos$ynext-pos$yprev, pos$xnext-pos$xprev)
#   # # Angle between points on the line in radians (y1- y0, x1-x0)
#   theta <- atan2(lineCoords[1]-lineCoords[2], lineCoords[3]-lineCoords[4])   #?
#   thetaT <- theta+pi/2
#   
#   dx_poi <- meanAxisDist*cos(thetaT)      # coordinates of point of interest as defined by position length (sep)
#   dy_poi <- meanAxisDist*sin(thetaT)
#   
#   xnew = lineCoords[1] - dx_poi
#   ynew = lineCoords[3] - dy_poi
#   
#   newPoint <- spTransform(SpatialPoints(data.frame(x = xnew, y = ynew),CRS(as.character(selectedTransect@proj4string))),
#                           CRS("+proj=longlat +datum=WGS84"))
#   
#   allAveragePoints <- rbind(allAveragePoints, data.frame(x=newPoint@coords[1], y = newPoint@coords[2],
#                                                          pos = alongshorePosition,
#                                                          meanAxisDist = meanAxisDist))
#   
# }


# 
# meaOffShore <- SpatialPointsDataFrame(data.frame(allAveragePoints[,'x'], allAveragePoints[,'y'] ), 
#                                       data = data.frame(allAveragePoints[,'pos'], allAveragePoints[,'meanAxisDist']),
#                                       proj4string=CRS("+proj=longlat +datum=WGS84"))
# st_meaOffShore <- st_as_sf(meaOffShore)








# median position?
# # library(ICSNP)
# cov.matrix <- matrix(c(3,2,1,2,4,-0.5,1,-0.5,2), ncol=3)
# X <- rmvnorm(100, c(0,0,0), cov.matrix)
# spatial.median(X)
# 
# # library(Gmedian)
# n <- 1e4
# d <- 500
# x <- matrix(rnorm(n*d,sd=1/sqrt(d)), n, d)
# x <- t(apply(x,1,cumsum))
# 
# plot(x)
# median.est = Gmedian(x)



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

# 
# x  <- runif(50,1,30)
# y  <- dnorm(x,10,2)*10+rnorm(50,0,.2)
# y1 <- y+5+x*.09 # This is the data
# xo <- order(x)
# starts <- gausslin.start(x,y1)
# ystart <- with(starts, As*exp(-((x-Bs)/Cs)^2)+Ds*x+Es)
# plot(x,y1)
# lines(x[xo],ystart[xo],col=2)
# 



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

# plot(combined_ordered$x, combined_ordered$y)
# functionD <- DouglasPeuckerEpsilon(trajx = combined_ordered$x,trajy = combined_ordered$y, epsilon = 0.0001, spar = NA)
# testD <- DouglasPeuckerNbPoints( geom_ordered[range,1], trajy = geom_ordered[range,2], 2, spar=NA)
# points( testD[,1], testD[,2],type="p", col = 'red')
# points( functionD[,1], functionD[,2],type="p", col = 'red')


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


