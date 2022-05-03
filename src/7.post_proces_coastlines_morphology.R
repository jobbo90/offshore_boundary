## ---------------------------
#'
#' Script name: process coastline morphology descriptors and combine that with 
#' coastline shape and mudbank details.
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
# Sys.setenv("RETICULATE_PYTHON" = "C:/Users/5600944/AppData/Local/r-miniconda/envs/rgee/python.exe")
ee_Initialize()
# library(fields)
library(smoothr)
library(rgeos)

## ---------------------------
source("./src/functions.R")


## ---------------------------

# mapviewOptions(basemaps = c( "Esri.WorldImagery","Esri.WorldShadedRelief", "OpenStreetMap.DE"))


# https://stackoverflow.com/questions/48692150/calculate-derivatives-curvature-of-a-polynomial?rq=1
# https://stackoverflow.com/questions/50083392/calculate-total-absolute-curvature-from-coordinates-in-r
# https://rdrr.io/cran/geomorph/src/R/digit.curves.r


# mapviewOptions(basemaps = c( "Esri.WorldImagery","Esri.WorldShadedRelief", "OpenStreetMap.DE"))
# leaflet() %>%
#   addProviderTiles("Esri.WorldImagery")
exportFunction <- TRUE
years <- seq(from = 2021, to = 2021, by = 1)

# near river mouths estimates for coastlines in old version of GEE script are 
# pos to exlcude for mudbank boundary estimates / outlier detection
posToExcludeSUR <- c(
  seq(130000,137000,1000), # coppename
  seq(234000, 243000, 1000)) # Suriname River

posToExcludeFG <- c(
  seq(261000,270000,1000), # approuage River
  seq(315000,334000,1000),# baia oiapoque 
  seq(223000,225000,1000), # orapu
  seq(205000,207000,1000), # cayenne
  seq(335000,403000,1000) # Brazil
) 
posToExcludeGUY <- c(
  seq(0,39000,1000), # Venezuela
  seq(527000,532000,1000), # Courantyne River
  seq(460000, 462000,1000),# berbice River
  seq(364000,365000,1000), # demerara River
  seq(294000,345000,1000), # Essequibo River delta
  seq(72000,74000,1000) # waini River
) 

allPos <- list('Suriname' = posToExcludeSUR,
               "FrenchGuiana" = posToExcludeFG,
               "Guyana" = posToExcludeGUY )

posToExclude <- c(-1)#allPos[[aoi]]

reference_date <- as.Date("1986-01-01")
pixelsize <- 30                        # amount of meters (used as max distance between points)
prefOrient <- 95                       # degrees, search angle 
# is this dependend on buffer size??
smoothnessFactor <- 2

aoi <- c('FrenchGuiana') #'FrenchGuiana', 'Suriname', 'Guyana'
buffer <- paste0("buffer",c(100,250)) # c(100, 250, 500)

# select folders
dataFolder <- './data/raw/GEE_exports/coastline_morphology'
folderSelect <- as.matrix(list.files(paste0(dataFolder), full.names = T))
df <- rewrite(folderSelect);
# only csv's
df <- df[grep('.csv', folderSelect, ignore.case = T),]

utm21N <- as.character("+proj=utm +zone=21 +ellps=intl +towgs84=-265,120,-358,0,0,0,0 +units=m +no_defs +datum=WGS84")

filtered <- vector('list', 100)
for (q in seq_along(years)) {
  for (x in seq_along(aoi)){
    for (b in seq_along(buffer)){
      year = years[q]
      region = aoi[x]
      buff = buffer[b]
      
      filters = c(year, region, buff)
      
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
  }
}
filtered <- unique(filtered)

allFiles <- do.call(bind_rows, lapply(as.matrix(filtered)[,1], 
                                      function(x) read.csv(x, 
                                                           stringsAsFactors = FALSE,
                                                           sep = ',', na.strings=c("","NA"),
                                                           colClasses = "character"
                                      )))

# estimate (!) class for each column
allFiles <- type_convert(allFiles)



# allFiles_dropPOS <- allFiles %>%
#   dplyr::mutate(toFilter = 0) %>%
#   dplyr::mutate(toFilter = ifelse((roi == "Suriname" & 
#                                      pos %in% posToExcludeSUR),1,toFilter),
#                 toFilter = ifelse((roi == "French Guiana" & 
#                                      pos %in% posToExcludeFG),1,toFilter),
#                 toFilter = ifelse((roi == "Guyana" & 
#                                      pos %in% posToExcludeGUY),1,toFilter)) %>%
#   filter(toFilter == 0) %>%
#   dplyr::select(-c(toFilter))

allFiles_mutate <- allFiles %>%
  dplyr::mutate(year_col = as.Date(cut(lubridate::date(date),
                                       "1 year")))  %>%
  rowwise() %>%
  dplyr::mutate(xOriginal = charToVecArray(xOriginal, ','),
                yOriginal = charToVecArray(yOriginal, ',')) 

# to numeric
allFiles_mutate$meanCurvature <- as.numeric(allFiles_mutate$meanCurvature)
allFiles_mutate$minCurvature <- as.numeric(-1)
allFiles_mutate$medianCurvature <- as.numeric(-1)
allFiles_mutate$maxCurvature <- as.numeric(-1)
allFiles_mutate$sinuositySmoothed <- as.numeric(-1)

# from coastline coordinates re-compute:
# - smooth/spline
# - orientation
# - concavity
# - sinuosity

# compute curvature for al positions
outPutTable <- list() # output table to store all used points 
outputCounter <- 1
# put this in a dplyr:: pipeline. 

# hist(allFiles_mutate$transectBearing)
# hist(allFiles_mutate$transectBearing, breaks = seq(-180, 180, 22.5))


for (buf in buffer){
  # buf <- buffer[1]
  bufferNumeric <- as.numeric(gsub("buffer", "", buf))
  
  subset<-allFiles_mutate %>%
    filter(as.numeric(transectBuffer) == as.numeric(gsub("buffer", "", buf)))
  
  for(nr in 1:nrow(subset)){
    # nr <- 1401 # which(subset$dist != 	-1) & subset$date == as.Date("2019-06-01 UTC"))
    # examples: 
    # POS 282000 DATE 2006 ==> complex shape
    # POS 0 DATE 2006 ==> Orientation flipped due to coastline bearing (norht-south)
    # POS 139000 DATE 2003 
    # POS 103000 DATE 2003 ==> wrong coasltine detected (should be resolved with larger buffer)
    # POS 210000 DATE 2007 ==> island but correctly detected
    # POS 154000 DATE 1993 ==> complex shape relatively good corrected (islands)
    # POS 211000 DATE 2008 ==> 
    # POS 141000 DATE 2007 ==> complex shape
    # pos 146000 date 2005 ==> starting point makes a difference...
    
    # require(svMisc)
    svMisc::progress(nr, max.value = nrow(subset))
    coastDistance <- subset$dist[nr]
    
    id <- subset$pos[nr]
    acquisition_date <- subset$date[nr]
    ROI <- gsub(' ', '', subset$roi[nr])
    
    transectBuffer <- subset$transectBuffer[nr]
    transectOrient <- subset$transectBearing[nr] #[180-0-180]
    transectOrient2 <- subset$transectBearingDegree[nr] # [0-360]
    # transectOrient <- subset$transectBearingDegree
    # subset$coastlineBearingTransformed[nr]
    
    
    # from the Gee script we get the following: 
    # See if smoothing & ordering coordinates make a difference
    # bearingTransform <- subset$centeredOrientation[nr]
    sinuosity <- subset$sinuosity[nr]
    coastlineLengthOrig <- subset$coastlineLength[nr]
    
    coord <- data.frame(x= subset$xOriginal[[nr]],
                        y = subset$yOriginal[[nr]],
                        id = 'a')
    
    if(nrow(coord) > 3 ){ 
      
      # determine coastline position
      coast<-SpatialPoints(data.frame(x = subset$coastX[nr], 
                                      y = subset$coastY[nr]), 
                           CRS("+init=epsg:4326 +datum=WGS84"))
      
      # for easterly orientating transects this is tricky, because the point may 
      # be to far south, therefore not selecting the last point correctly

      # startOrientation <- transectOrient + 115
      startOrientation <- case_when(
        # add a case when transectorientation is -1, set to 135?
        # < 270 & > 180: set to 135
        # 
        is_angle_between(transectOrient2, 180, 270) ~ 115,
        is_angle_between(transectOrient2, 270, 292) ~ transectOrient2 + 200,
        is_angle_between(transectOrient2, 292, 315) ~ transectOrient2 + 177,
        is_angle_between(transectOrient2, 315, 337.5) ~ transectOrient2 + 155,
        is_angle_between(transectOrient2, 337.5, 10) ~ transectOrient2 + 135,
        is_angle_between(transectOrient2, 10, 22.5) ~ transectOrient2 + 110,
        is_angle_between(transectOrient2, 22.5, 45) ~ transectOrient2 + 95,
        is_angle_between(transectOrient2, 45, 67.5) ~ transectOrient2 + 80,
        is_angle_between(transectOrient2, 67.5, 90) ~ transectOrient2 + 55,
        is_angle_between(transectOrient2, 90, 112.5) ~ transectOrient2 + 35,
        is_angle_between(transectOrient2, 112.5, 135) ~ transectOrient2 + 10,
        is_angle_between(transectOrient2, 135, 180) ~ transectOrient2,
        )
      
      startOrientation <- replace(startOrientation, startOrientation > 360,
                                  startOrientation - 360)
      
      
      # move it buffersize*2 meters, depending on transect orientation to south - east
      startPoint <- SpatialPoints(destPoint(coast, startOrientation, transectBuffer*2),
                                  CRS("+init=epsg:4326 +datum=WGS84"))
      
      coastlinePoints <- SpatialPoints(
        data.frame(x = coord$x, y = coord$y), CRS(utm21N))
      
      ids <- 1:nrow(coastlinePoints@coords)
      
      # convexHull <- gConvexHull(coastlinePoints)
      
      # nearest will be the first in line for sorting
      neareast <- apply(gDistance(coastlinePoints,
                                  spTransform(startPoint,CRS(utm21N)), byid = T),
                        1, which.min)
      last <- apply(gDistance(coastlinePoints,
                              spTransform(startPoint,CRS(utm21N)), byid = T), # or use coastlinePoints[neareast]?
                    1, which.max)
      
      # distance between initial estimate of first & last point
      initialDist <- gDistance(coastlinePoints[neareast], coastlinePoints[last])
      
      idsSorted <- c(as.numeric(neareast)) 
      distances <- pointDistance(p1 = coastlinePoints@coords, lonlat = F)
      colnames(distances) <- ids
      rownames(distances) <- ids
      
      for (p in ids[!ids %in% neareast]){
        # print(p)
        # get last index
        lastI <- idsSorted[length(idsSorted)]
        nextID <- distances[lastI,]
        potential <- nextID[!names(nextID) %in% unique(c(idsSorted))]
        
        # make them spatial points
        ind <- as.numeric(names(potential))
        thePoints <- coord[ind,]
        
        # if(nrow(thePoints) < 1  & length(idsSorted) != length(ids)){ # | nextIndex == as.numeric(last)
        #   distancesToFarAway <- as.numeric(which(nextID > pixelsize*2))
        #   potential <- nextID[!names(nextID) %in% unique(c(idsSorted, distancesToFarAway))]
        #   
        #   # make them spatial points
        #   ind <- as.numeric(names(potential))
        #   thePoints <- coord[ind,]
        #   
        # } 
        
        if(nrow(thePoints) < 1 ){
          break
        }
        
        orientations <- sapply(1:(nrow(thePoints) ), function(i){
          # i <- 1
          bear <- geosphere::bearing(spTransform(SpatialPoints(
            data.frame(x = coord[lastI,1], y = coord[lastI,2]), 
            CRS(utm21N)), CRS("+init=epsg:4326 +datum=WGS84")),
            
            spTransform(SpatialPoints(data.frame(x = thePoints[i,1], 
                                                 y = thePoints[i,2]), 
                                      CRS(utm21N)), CRS("+init=epsg:4326 +datum=WGS84"))
          )
          return(bear) #ifelse(bear<0, bear + 360, bear))
        })
        
        thePoints <- cbind(thePoints,
                           idnr = ind,
                           orientations, 
                           flipOrient = replace(orientations, orientations<0, 
                                             orientations[orientations<0] +360))
        
        
        # orientation to last point 
        preferedDirection <- geosphere::bearing(spTransform(SpatialPoints(
          data.frame(x = coord[lastI,1], y = coord[lastI,2]), 
          CRS(utm21N)), CRS("+init=epsg:4326 +datum=WGS84")),
          
          spTransform(SpatialPoints(data.frame(x = coord[last,1], 
                                               y = coord[last,2]), 
                                    CRS(utm21N)), CRS("+init=epsg:4326 +datum=WGS84"))
        )
        
        preferedDirection<- ifelse(as.numeric(last)%in% idsSorted,-70,  
                                   preferedDirection)
        
        bound1 <- #preferedDirection+(prefOrient/2)
        ifelse(preferedDirection+(prefOrient/2) < 0,
               preferedDirection+(prefOrient/2) + 360,
               preferedDirection+(prefOrient/2))
        bound2 <- #preferedDirection-(prefOrient/2)
        ifelse(preferedDirection-(prefOrient/2) < 0,
                         preferedDirection-(prefOrient/2) + 360,
                         preferedDirection-(prefOrient/2))
        
        angleFilterID1 <- NULL
        angleFilterID2 <- NULL
        angleFilterID3 <- NULL
        angleFilterID4 <- NULL
        
        thePoints <- thePoints %>%
          rowwise() %>%
          dplyr::mutate(inRange = is_angle_between(flipOrient, bound1, bound2))
        
        # if there is indices in the preferred direction
        if(length(which(thePoints$inRange)) > 0
          # length(which(orientations < bound1 &
                        # orientations > bound2 )) > 0
          ){
          
          # distance less than 1 pixel(incusive to account for rounding)
          distancesNearby <- as.numeric(which(nextID < pixelsize+1 &
                                              nextID > 0 &
                          !(as.numeric(names(nextID)) %in% idsSorted)  ))
          
          
          
          # angle within range?
          prefIDs <-  as.numeric(thePoints$idnr)[which(thePoints$inRange)]
          
          idNearby <- prefIDs[which(prefIDs %in% distancesNearby)]
          
          angleFilterID1 <- as.numeric(row.names(thePoints))[
            which(as.numeric(row.names(thePoints)) %in% c(idNearby))]
        } 
        
        # if nothing within 30 meter and in the angle range
        # look further away
        if(length(angleFilterID1) < 1  & length(idsSorted) != length(ids)){ 
          
          # within search distance 
          toFar2 <- as.numeric(which(nextID > pixelsize*2.1))
          potential2 <- as.numeric(names(nextID[!names(nextID) %in% 
                                                  unique(c(idsSorted, toFar2))]))
          
          # angle within range
          prefIDs2 <-  as.numeric(thePoints$idnr)[
            which(thePoints$inRange)]
          
          id2 <- prefIDs2[which(prefIDs2 %in% potential2)]
          
          angleFilterID2 <- as.numeric(thePoints$idnr)[
            which(as.numeric(thePoints$idnr) %in% c(id2))]
        } 
        
        if(length(angleFilterID2) < 1 & length(angleFilterID1) < 1){
          
          toFar3 <- as.numeric(which(nextID > pixelsize*2.1))
          potential3 <- as.numeric(names(nextID[!names(nextID) %in% 
                                                  unique(c(idsSorted, toFar3))]))
          # based on angle filter; prefered ID's 
          prefIDs3 <-  as.numeric(thePoints$idnr)[which(
            thePoints$orientations < 1 & 
              thePoints$orientations > -181 )]
          
          id3 <- prefIDs3[which(prefIDs3 %in% potential3)]
          
          angleFilterID3 <- as.numeric(thePoints$idnr)[
            which(as.numeric(thePoints$idnr) %in% c(id3))]
          
          
        }
        
        if(length(angleFilterID1) < 1 & length(angleFilterID2) < 1 & 
           length(angleFilterID3) < 1){
          
          toFar4 <- as.numeric(which(nextID > pixelsize*2.1))
          potential4 <- as.numeric(names(nextID[!names(nextID) %in% 
                                                  unique(c(idsSorted, toFar4))]))
          # # based on angle filter; prefered ID's 
          # prefIDs3 <-  as.numeric(thePoints$idnr)[which(
          #   thePoints$orientations < 1 & 
          #     thePoints$orientations > -181 )]
          
          # id3 <- potential4
          
          angleFilterID4 <- as.numeric(thePoints$idnr)[
            which(as.numeric(thePoints$idnr) %in% c(potential4))]
          
          
        }
        
        test <- cbind(as.numeric(angleFilterID1), as.numeric(angleFilterID2),
                      as.numeric(angleFilterID3), as.numeric(angleFilterID4))
        
        # select nearest point if multiple are selected
        nextIndex <- as.numeric(names(which.min(
          potential[as.numeric(names(potential)) %in% as.numeric(test)])))
        
        # suggested improvement:
        # if next index is empty, check how many are still available and if they
        # are potential candidates (e.g. within distance or withing angle?) 
        # and rerun previous steps?
        
        idsSorted <- c(idsSorted, nextIndex)
        # print(idsSorted)
        remove(test,potential)
        
        if(length(nextIndex) < 1 ){ # | nextIndex == as.numeric(last)
          break}
      }
      
      # if(length(idsSorted) <= 1 ){
      #   break # doest this result in skipping the loop but including the else?
      # }
      
      # if only 1 idsSorted is returned (stops at start point)
      # should include that the loop breaks?
      orderd_coorindates <- coord[idsSorted[!is.na(idsSorted)],]
      
      # if minimum distance optimization for smoothing is applied this should not be necessar?
      coordinates(orderd_coorindates) <- ~x+ y
      xOrdered <- lapply(split(orderd_coorindates, orderd_coorindates$id), 
                         function(x) Lines(list(Line(coordinates(x))), x$id[1L]))
      
      linesOrdered <- SpatialLines(xOrdered)
      # if 1 idsSorted: error subscript out of bounds
      linelengthOrdered <- geodesicLine(linesOrdered) 
      
      # fit ksmooth function through ordered points
      # this also affects the sinuosity value!
      originalPoints_OrderedSmooth<- smoothr::smooth(linesOrdered, method = "ksmooth", 
                                                     smoothness=smoothnessFactor, 
                                                     max_distance = pixelsize)
      
      # plot(coord$x, coord$y, ylim=c(min(coord$y)-150,max(coord$y)+150),
      #      xlim=c(min(coord$x)-150,max(coord$x)+150))
      # points(coordinates(spTransform(startPoint,CRS(utm21N)))[,1],
      #        coordinates(spTransform(startPoint,CRS(utm21N)))[,2],
      #        col = 'red' )
      # points(coordinates(spTransform(coast,CRS(utm21N)))[,1], # coastline point
      #        coordinates(spTransform(coast,CRS(utm21N)))[,2],
      #        col = 'green' )
      # lines(originalPoints_OrderedSmooth)
      # points(coord$x[neareast],coord$y[neareast], col = 'blue' ) # startingPoint
      # points(coord$x[last],coord$y[last], col = 'orange' ) # end
      # text(coord$x+10, coord$y, labels=rownames(coord), cex=0.9, font=2)

      smoothed <- data.frame(coordinates(originalPoints_OrderedSmooth)$a[[1]])
      smoothedX <- smoothed[,1]
      smoothedY <- smoothed[,2]
      
      # compute sinusosity:
      # straight Line distance between start & end point of line
      # total linelength ( in units of original line input)
      sinSmoothed <- lineLinegth(smoothedX, smoothedY)/linelengthOrdered
      bearingFirstLast <- first_last_dir(smoothed) # bearing between first and last point
      
      # recompute orientation from smoothed points
      coordinates(smoothed) <- ~X1 + X2        # coorindates
      proj4string(smoothed) <- CRS(utm21N)     # add projection
      smoothed_tr <- spTransform(smoothed, CRS("+init=epsg:4326 +datum=WGS84")) # transform to WGS84
      wgsCoords <- data.frame(coordinates(smoothed_tr))            # get coordinates in degrees
      
      whichseg <- seq(1:(nrow(wgsCoords)-1))   # excluce last point
      
      # collect 
      pos=data.frame(                        
        whichseg=whichseg,           # segment number the position is corresponding to
        x0=wgsCoords[whichseg,1],     # x-coordinate of the point
        y0=wgsCoords[whichseg,2],     # y-coordinate of the point
        
        x1=wgsCoords[whichseg+1,1],   # Get X coordinate next on line 
        y1=wgsCoords[whichseg+1,2]    # Get Y coordinate next on line
      )
      
      pos <- pos %>%
        dplyr::mutate(bearing = geosphere::bearing(
          SpatialPoints(data.frame(x = x0, y =y0), CRS("+init=epsg:4326 +datum=WGS84")), 
          SpatialPoints(data.frame(x = x1, y =y1), CRS("+init=epsg:4326 +datum=WGS84"))))
      
      bearing <- median(pos$bearing)      # or median bearing between all points
      
      # compute orientation centered around north (0-360 range)
      centeredOrientation <- bearingToOrientation(bearing)
      # compute orientation between last and first coastline point
      centeredOrientation2 <- bearingToOrientation(bearingFirstLast)
      
      # degree of curvature between coordinates along line in units of: 
      #  curve direction changes over a small distance travelled (angle in rad/m)
      # defined as the inclination per arc length
      curvature <- getCurvature(smoothedX, smoothedY) # getCurvature(numericX, numericY)
      
      # concave (inward curve), signs are arbitrary (?) but here concave = positive
      # convex (outward curve), signs are arbitrary (?) but here convex = negative
      maxC <- ifelse(length(curvature) > 0,  
                     curvature[which.max(abs(curvature))], 0)
      minC <- ifelse(length(curvature) > 0, 
                     curvature[which.min(curvature)],0)
      meanC <- ifelse(length(curvature) > 0, 
                      mean(curvature),0)
      medianC <- ifelse(length(curvature) > 0, 
                        median(curvature, na.rm = T),0)
    } else{
      curvature <- -1
      maxC <- -1
      minC <- -1
      meanC <- -1
      medianC <- -1
      sinSmoothed <- -1
      orderd_coorindates <- data.frame(x= c(-1,-1),
                                       y = c(-1,-1))
      bearing <- -1
      centeredOrientation <- -1
      centeredOrientation2 <- -1
      coord <- data.frame(x= c(-1,-1),
                          y = c(-1,-1))
      initialDist <- -1
      linelengthOrdered <- -1
      
    }
    
    subset[nr,'meanCurvature'] <- meanC
    subset[nr,'minCurvature'] <- minC
    subset[nr,'medianCurvature'] <- medianC
    subset[nr,'maxCurvature'] <- maxC
    subset[nr,'sinuositySmoothed'] <- sinSmoothed
    
    lengthCoords <- length(orderd_coorindates)
    
    outPutTable[[outputCounter]] <- data.frame(
      numericY = coordinates(orderd_coorindates)[,2], # ordered
      numericX = coordinates(orderd_coorindates)[,1], # orderd_coorindates
      pos = rep(id, lengthCoords),
      transectBuffer = rep(bufferNumeric, lengthCoords),
      meanCurvature = rep(meanC, lengthCoords),
      minC = rep(minC, lengthCoords),
      maxC = rep(maxC, lengthCoords),
      medianC = rep(medianC, lengthCoords),
      date = rep(acquisition_date, lengthCoords),
      roi = rep(ROI, lengthCoords),
      bearing = rep(bearing, lengthCoords),
      
      centeredOrientation =  rep(centeredOrientation,lengthCoords),
      centeredOrientation2 =  rep(centeredOrientation2,lengthCoords),
      coastlineLengthOrig =  rep(coastlineLengthOrig, lengthCoords),    # original (GEE) estimate coastline length
      coastlineLength =  rep(initialDist, lengthCoords),                # distance between start and end point
      coastlineLengthOrdered =  rep(linelengthOrdered, lengthCoords),   # After ordering points, distance between first and last
      sinuosity =  rep(sinuosity, lengthCoords),                 # original sin
      sinuositySmoothed =  rep(sinSmoothed, lengthCoords)        # ordered & smoothed 
    )
    
    # spatial representation
    # curvSpatial <- SpatialPointsDataFrame(data.frame(
    #   numericX, numericY),
    #   proj4string=CRS("+proj=utm +zone=21 +datum=WGS84 +units=m +no_defs "),
    #   data = data.frame( outPutTable[[nr]]))
    # mapview(curvSpatial, zcol = 'curvature')
    outputCounter = outputCounter +1
  }
  
  }
  

# row bind all subslist int a data table.
res = do.call(rbind, outPutTable)


# coastlines <- st_as_sf(SpatialPointsDataFrame(data.frame(
#   allFiles$coastX, allFiles$coastY),
#   proj4string=CRS("+proj=longlat +datum=WGS84"),
#   data = data.frame(allFiles)))
# mapview(coastlines, zcol = "coastlineBearingTransformed") # meanCurvatureUpdated / coastlineBearing / dist

# define bins for coastline changes
n_directions = 8
dir_bin_width = 360 / n_directions
dir_bin_cuts = seq(dir_bin_width / 2, 360 - dir_bin_width / 2, dir_bin_width)
dir_bin_cuts_ordered = c(tail(dir_bin_cuts, 1), head(dir_bin_cuts, -1))

factor_labs = paste(dir_bin_cuts_ordered,
                    dir_bin_cuts, sep = ", ")

coastlineMorphology <- res %>% 
  # add bins & labels for coastline orientation
  dplyr::mutate(cut_bins = cut(centeredOrientation,
                               breaks = dir_bin_cuts )) %>%
  dplyr::mutate(x_bins = findInterval(x=centeredOrientation, 
                                      vec = dir_bin_cuts)) %>%
  dplyr::mutate(x_bins = ifelse(x_bins == n_directions,
                                0, x_bins)) %>%
  rowwise() %>%
  dplyr::mutate(intervalLabel = list(factor_labs[x_bins+1]))

# get per year and position stats
annualTable <- coastlineMorphology %>%
  dplyr::group_by(date, pos, transectBuffer) %>% # quarterly_col
  filter(as.numeric(meanCurvature) != -1.0) %>%
  dplyr::summarize(meanCurvature = mean(meanCurvature, na.rm = T),
                   minC = mean(minC, na.rm = T),
                   maxC = mean(maxC, na.rm = T),
                   medianC = mean(medianC, na.rm = T),
                   bearing = mean(bearing, na.rm = T),
                   centeredOrientation2 = mean(centeredOrientation2, na.rm = T),
                   coastlineLength = mean(coastlineLength, na.rm = T),
                   centeredOrientation = mean(centeredOrientation, na.rm = T),
                   coastlineLengthOrdered = mean(coastlineLengthOrdered, na.rm = T),
                   sinuositySmoothed = mean(sinuositySmoothed, na.rm = T),
                   x_bins = mean(x_bins, na.rm = T),
                   roi =  paste(unique(roi)), #  paste(unique(species), collapse = ', ')
                   transectBuffer = mean(transectBuffer, na.rm = T)
  ) %>%
  
  ungroup() %>%
  rowwise() %>%
  # intervallabel causes problem in export so leave / change? 
  # dplyr::mutate(intervalLabel = list(factor_labs[x_bins+1])) %>%
  dplyr::mutate(year_col = as.Date(paste0(year(date), '-01-01')))

# long to wide format for each transectbuffer value that was used as input
# colnames(annualTableWide)
annualTableWide <- annualTable %>% # 
  tidyr::pivot_wider(names_from = c(transectBuffer), 
                     values_from = c(
      meanCurvature, minC, maxC, medianC, bearing,centeredOrientation, 
      centeredOrientation2,coastlineLength,coastlineLengthOrdered, 
      sinuositySmoothed, x_bins, transectBuffer))

if(exportFunction){
  
  # for (buf in buffer){
  uniqueDates <- unique(annualTableWide$year_col)
  
    for (year in unique(format(as.Date(uniqueDates), '%Y'))){
      # year <- unique(format(as.Date(uniqueDates), '%Y'))[1]
      # print(year)
      start_year <- as.Date(ISOdate(year, 1, 1))
      end_year <- as.Date(ISOdate(year, 12, 31)) 
      
      per_year <-subset(annualTableWide,
                        as.Date(date) >= start_year &
                          as.Date(date) <= end_year )
                        # & transectBuffer == buf)
      
      # df <- as.data.frame(do.call(cbind ,per_year))
      # write_csv(per_year, 
                # paste0(wd,"/data/processed/coastlines/", aoi,
                       # '_', year,'_' ,buf, '_coastlineMorphology.csv'))
      write_csv(per_year, 
                paste0(wd,"/data/processed/coastlines/", aoi,
                       '_', year, '_coastlineMorphology.csv'))
      
      print( paste0(wd,"/data/processed/coastlines/", aoi,
                    '_', year, '_coastlineMorphology.csv'))
      remove(per_year,start_year, end_year)}
  # }
}

#################################
#' 
#' annual coastline change for coastline orientation
#' 
#################################

# if exists read the processed coastline morphology data and merge with original
coastlinesFolder <- './data/processed/coastlines'
folderSelect <- as.matrix(list.files(paste0(coastlinesFolder), 
                                     full.names = T))
df <- rewrite(folderSelect);
# only csv's
df <- df[grep('coastlineMorphology.csv', folderSelect, ignore.case = T),]
years <- seq(from = 1985, to = 2021, by = 1)

filtered <- vector('list', 100)
for (q in seq_along(years)) {
  for (x in seq_along(aoi)){
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
  }
  # q <- 1
  
}


# # bind_rows!!
morphologyObs <- do.call(bind_rows, lapply(as.matrix(filtered)[,1], 
                                    function(x) read.csv(x, 
                                                         stringsAsFactors = FALSE,
                                                         sep = ',', na.strings=c("","NA"),
                                                         colClasses = "character")))

morphologyObs <- type_convert(morphologyObs)


# # now for the 500m buffer (different naming)
# df <- rewrite(folderSelect);
# # only csv's
# df <- df[grep('500.csv', folderSelect, ignore.case = T),]
# filtered <- vector('list', 100)
# for (q in seq_along(years)) {
#   for (x in seq_along(aoi)){
#     year = years[q]
#     region = aoi[x]
#     
#     filtered = rbind(filtered, df %>% 
#                        dplyr::filter(
#                          filters %>%
#                            # apply the filter of all the text rows for each pattern
#                            # you'll get one list of logical by pattern ignored_string
#                            purrr::map(~ to_keep(.x, text = text)) %>%
#                            # get a logical vector of rows to keep
#                            purrr::pmap_lgl(all)
#                        ))
#   }
#   # q <- 1
#   
# }
# 
# # # bind_rows!!
# morphologyObs500 <- do.call(bind_rows, lapply(as.matrix(filtered)[,1], 
#                                            function(x) read.csv(x, 
#                                                                 stringsAsFactors = FALSE,
#                                                                 sep = ',', na.strings=c("","NA"),
#                                                                 colClasses = "character")))
# 
# morphologyObs500 <- type_convert(morphologyObs)

# JOIN COASTLINE MORPHOLOGY OBSERVATIONS WITH COASTLINE CHANGES
# select folders
processedFolder <- './data/processed'
folderSelect <- as.matrix(list.files(paste0(processedFolder, '/offshore_points'), 
                                              full.names = T))
df <- rewrite(folderSelect);
# only csv's
df <- df[grep('offshore.csv', folderSelect, ignore.case = T),]

filtered <- vector('list', 100)
for (q in seq_along(years)) {
  for (x in seq_along(aoi)){
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
  }
  # q <- 1
  
}
filtered <- unique(filtered)

# # bind_rows!!
allObs <- do.call(bind_rows, lapply(as.matrix(filtered)[,1], 
                           function(x) read.csv(x, 
                                  stringsAsFactors = FALSE,
                                  sep = ',', na.strings=c("","NA"),
                                  colClasses = "character")))

allObs <- type_convert(allObs)



# required ouput for annual analysis:
# transect details: country, year_col, pos, original bearing, 
# coastlinePosition: coast_median, xy coordinates and deltaCoast
# mudbankPosition: median_offshore, distX, distY, nomudbank, meanFraction, 
# mudbankObs, validMudbankObs




# go to 1 observation per transect pos per year
allObs_annual <- allObs %>%
  # filter(mudbank_outlier == 0) %>%   # outliers
  # filter(axisDist != -1)    %>%         # nonsens observations
  dplyr::group_by(Country,year_col, pos) %>% 
  # dplyr::distinct(deltaCoast, .keep_all = T) %>%
  dplyr::summarize(bearing = bearing[1],
                   coast_median = median(coast_median,na.rm=T),
                   deltaCoast= deltaCoast[1],
                   # mudbank_extent = mudbank_extent [1], #
                   # axisDist = axisDist[1],              #
                   meanFraction = meanFraction[1],      # mean SmoothedPeakFract per position per year
                   mean_smoothedMeanFract = mean(SmoothedPeakFract[SmoothedPeakFract>0],
                                                 na.rm = T), # this one might be a bit different.
                   medianOffshore = medianOffshore[1], # median distance mudbank boundary from transect source
                   distX = distX[1], # x corresponding to median offshore dist
                   distY = distY[1], # Y corresponding to median offshore dist
                   noMudbank = noMudbank[1],
                   mudbankObs = mudbankObs[1], # amount of observations with mudbank boundary
                   validMudbankObs = validMudbankObs[1] # Amount of observations with valid mudbank boundary
                   ) %>%
  ungroup()


# reformat ROI value  to match the coastline ROI names
morphologyObs$roi <- gsub(' ', '', morphologyObs$roi)

# Join with the original dates
df_join <- allObs_annual %>%
  filter(year_col %in% unique(morphologyObs$year_col) &
           pos %in% unique(morphologyObs$pos) ) %>% # make sure only join is performed on years where there is data
  left_join(morphologyObs, 
            c("year_col" = "year_col",
              "Country" = "roi",
              "pos" = "pos"), keep = T, suffix = c("", ".y")
  ) %>%
  dplyr::select(year_col, Country, pos,
                # transect details
                bearing,  
                # originX, originY, seasons, five_year_col,collectiontype,

                # coastline details
                coast_median, deltaCoast,
                # coast_outlier, coastObs, coastDist, 
                # coastX, coastY,slope, locf,
                
                # mudbank details
                medianOffshore, meanFraction, 
                mean_smoothedMeanFract, distX, distY,noMudbank, 
                mudbankObs,validMudbankObs,
                
                # morphology calculation output
                matches("meanCurvature"), matches("minC"),matches("maxC"), 
                matches("medianC"),matches("bearing"), matches("centeredOrientation"),
                matches("centeredOrientation2"), matches("coastlineLength"),
                matches("coastlineLengthOrdered"), matches("sinuositySmoothed"),
                matches("x_bins"), matches('transectBuffer'),
                # distX, distY <- median offshore coordinates
                # x, y <- offshore coordinates individual observations

  )



if(exportFunction){
  
  uniqueDates2 <- unique(df_join$year_col)
  
  for (year in unique(format(as.Date(uniqueDates2), '%Y'))){
    # year <- unique(format(as.Date(uniqueDates2), '%Y'))[1]
    # print(year)
    start_year <- as.Date(ISOdate(year, 1, 1))
    end_year <- as.Date(ISOdate(year, 12, 31)) 
    
    per_year <-subset(df_join,
                      as.Date(year_col) >= start_year &
                        as.Date(year_col) <= end_year)
    
    # check if export is correct? Seems to go wrong when there is
    # two buffer widths included in the annualTableWide

    write_csv(do.call(cbind.data.frame, per_year),  
              paste0(wd,"/data/processed/", aoi,
                     '_', year, '_coastline_mudbanks.csv'))
    
    print( paste0(wd,"/data/processed/coastlines/", aoi,
                  '_', year, '_coastlines_mudbanks.csv'))
    remove(per_year, start_year, end_year)
  }
}

