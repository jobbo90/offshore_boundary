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
wd<-getwd()

## ---------------------------

options(scipen = 6, digits = 4) # I prefer to view outputs in non-scientific notation
memory.limit(30000000)     # this is needed on some PCs to increase memory allowance, but has no impact on macs.

#  Map view options:
# https://r-spatial.github.io/mapview/articles/articles/mapview_02-advanced.html

## ---------------------------

#' load up the packages 
source("./src/packages.R")       # loads up all the packages we need
# Sys.setenv("RETICULATE_PYTHON" = "C:/Users/5600944/AppData/Local/r-miniconda/envs/rgee/python.exe")
# ee_Initialize()

library(rgeos)

## ---------------------------
source("./src/functions.R")

location <- './data'
country <- 'Suriname'

# select folders
folderSelect <- as.matrix(list.files(paste0(location, '/raw/shapes'), full.names = T))
df <- rewrite(folderSelect);
# only csv's
df <- df[grep('.shp$', folderSelect, ignore.case = T),]


# filtered <- vector('list', 100)
# for (q in seq_along(years)) {
#   for (x in seq_along(aoi)){
    # year = years[q]
    # region = aoi[x]
    
    filters = c(country)
    
    filtered = df %>% 
                       dplyr::filter(
                         filters %>%
                           # apply the filter of all the text rows for each pattern
                           # you'll get one list of logical by pattern ignored_string
                           purrr::map(~ to_keep(.x, text = text)) %>%
                           # get a logical vector of rows to keep
                           purrr::pmap_lgl(all)
                       )
  # }
  # q <- 1
  
# }
kustOI <- readOGR(as.character(filtered[1,1]))

# https://stackoverflow.com/questions/10077415/replacing-nas-in-r-with-nearest-value
fill_NA_nearest <- function(dat) {
  N <- length(dat)
  na.pos <- which(is.na(dat))
  if (length(na.pos) %in% c(0, N)) {
    return(dat)
  }
  non.na.pos <- which(!is.na(dat))
  intervals  <- findInterval(na.pos, non.na.pos,
                             all.inside = TRUE)
  left.pos   <- non.na.pos[pmax(1, intervals)]
  right.pos  <- non.na.pos[pmin(N, intervals+1)]
  left.dist  <- na.pos - left.pos
  right.dist <- right.pos - na.pos
  
  dat[na.pos] <- ifelse(left.dist <= right.dist,
                        dat[left.pos], dat[right.pos])
  return(dat)
}

# function to define transect characteristics: spacing, length (of the transect) and a smoothing factor
evenspace <- function(line, sep, start=0, tlen, smoothing_factor){ 
  
  # ##for testing:
  # smoothing_factor <- 5
  # sep <- 1000
  # tlen <- 30000
  # start <- 0
  # line <- kustlijn_smoothed2
  # # ### line <- kustlijn_total
  
  # test <- st_combine(kustlijn_smoothed2)
  
  # if(length(line)>1){
  #   line <- gLineMerge(line) # make one feature out of all line features ==> not working?
  # } else {
  #   line <- line
  # }
  
  # transform line in UTM (allow calculation in Meters)
  # projection EPSG:31121 (epsg projection 31121 - zanderij / utm zone 21n): http://spatialreference.org/ref/epsg/31121/
  line <- spTransform(line, CRS(as.character("+proj=utm +zone=21 +ellps=intl +towgs84=-265,120,-358,0,0,0,0 +units=m +no_defs")))
  
  AllTransects <- vector('list', 100000) # DB that should contain all transects
  
  # for each element in geometry calculate line positions seperately:
  # for (i in unique(line$ID)){
  for (i in 1: length(line)){
    # i <- 2
    
    ###### 
    ## 
    ## Define transect starting points
    #####

    subset_geometry <- data.frame(geom(line[i,]))[, c('x', 'y')]
    
    # Center Point
    center <- matrix(colMeans(subset_geometry),ncol=2)
    
    # which point is furthest away: origin
    ix <- which.max(pointDistance(subset_geometry,center,lonlat = F))
    
    # make sure index of position is sequential (following orientation of line)
    cur = c(ix);
    sorted = c(ix);
    ind = c(1:nrow(subset_geometry));
    ind = ind[!ind %in% cur];

    while(length(ind)>=2){
      pos = ind[which.min(rowSums((subset_geometry[ind,]-   subset_geometry[cur,][rep(1, nrow(subset_geometry[ind,])), ])^2))];
      ## At each iteration we remove the newly identified pos from the indexes in ind
      ## and consider it as the new current position to look at
      ind = setdiff(ind,pos);
      cur = pos;
      sorted = c(sorted,pos)}
    sorted = c(sorted,ind)
    
    subset_geometry<-subset_geometry[sorted, ]

    # is the plotorder causing problems? YES
    # retrieve_order <- SpatialPolygons(list(Polygons(list(Polygon(data.frame(x = subset_geometry[,'x'], y = subset_geometry[,'y']))),1)),
                                    # proj4string = CRS(as.character(line@proj4string)))
    # plot(retrieve_order, col = rainbow(length(unique(retrieve_order@polygons))))
    
    
    # Build data frame for each position along line
    dx <- c(0, diff(subset_geometry[,'x'])) # Calculate difference at each cell comapred to next line segment
    dy <- c(0, diff(subset_geometry[,'y']))
    
    dseg <- sqrt(dx^2+dy^2)                 # get rid of negatives and transfer to uniform distance per segment (pythagoras)
    dtotal <- cumsum(dseg)                  # cumulative sum total distance of segments
    
    linelength = sum(dseg)                  # total linelength ( in units of original line input)
    pos = seq(start,linelength, by=sep)     # Array with postions numbers in meters
    whichseg = unlist(lapply(pos, function(x){sum(dtotal<=x)})) # select Segments corresponding to distance
    
    # Data frame with positions ON the coastline (starting points)
    pos=data.frame(pos=pos,                            # keep only 
                   whichseg=whichseg,                  # segment number the position is corresponding to
                   x0=subset_geometry[whichseg,1],     # x-coordinate of the segment
                   y0=subset_geometry[whichseg,2],     # y-coordinate of the segment
                   dseg = dseg[whichseg+1],            # segment length selected (sum of all dseg in that segment)
                   dtotal = dtotal[whichseg],          # Accumulated length
                   
                   x1=subset_geometry[whichseg+1,1],   # Get X coordinate line segment next on line 
                   y1=subset_geometry[whichseg+1,2]    # Get Y coordinate line segment next on line
    )
    
    # ensure equal spacing (from segments to points)
    pos$further =  pos$pos - pos$dtotal       # which is the next position (in meters)
    pos$f = pos$further/pos$dseg              # fraction next segment of its distance
    
    # because of smoothing factor this changes starting point
    pos$x = pos$x0 + pos$f * (pos$x1-pos$x0)  # X Position of point on line which is x meters away from x0: ensure equal spacing
    pos$y = pos$y0 + pos$f * (pos$y1-pos$y0)  # Y Position of point on line which is x meters away from y0: ensure equal spacing
    
    # smoothing_factor <- 5
    pos$xnext = as.data.frame(mutate(pos, xnext = lead(x,smoothing_factor)))[, 'xnext']
    pos$ynext = as.data.frame(mutate(pos, ynext = lead(y,smoothing_factor)))[, 'ynext']

    pos$yprev = as.data.frame(mutate(pos, yprev = lag(y,smoothing_factor)))[, 'yprev'] # previous y (position before)
    pos$xprev = as.data.frame(mutate(pos, xprev = lag(x,smoothing_factor)))[, 'xprev'] # previous x (position before)
    
    # calculate angle 
    # pos$theta = atan2(pos$y-pos$ynext, pos$yprev-pos$xnext)
    pos$theta = atan2(pos$yprev-pos$ynext, pos$xprev-pos$xnext)  # Angle between points on the line in radians
    
    # last in line are affected by smoothing window: give same angle 
    # pos$theta[seq(nrow(pos)-(abs(smoothing_factor-2)+smoothing_factor-2)/2,nrow(pos))] <- pos$theta[nrow(pos)-(abs(smoothing_factor-3)+smoothing_factor-3)]
    
    # fill NA with relevant values
    pos$theta <- fill_NA_nearest(pos$theta)
    # pos$theta[which(is.na(pos$theta))] = pos$theta[which(is.na(pos$theta))+1] # fill NA with neighbourh theta
    pos$xprev[which(is.na(pos$xprev))] = pos$x[which(is.na(pos$xprev))]
    pos$yprev[which(is.na(pos$yprev))] = pos$y[which(is.na(pos$yprev))]
    
    pos$xnext[which(is.na(pos$xnext))] = pos$x[which(is.na(pos$xnext))]
    pos$ynext[which(is.na(pos$ynext))] = pos$y[which(is.na(pos$ynext))]
    
    pos$object = i

    ###### 
    ## 
    ## Define transect end points
    #####
    pos$thetaT = pos$theta+pi/2         # Get the angle in degrees? 
    
    dx_poi <- tlen*cos(pos$thetaT)      # coordinates of point of interest as defined by position length (sep)
    dy_poi <- tlen*sin(pos$thetaT) 
    
    # transect is defined by x0,y0 and x1,y1 with x,y the coordinate on the line
    output <-     data.frame(pos = pos$pos,
                             x0 = pos$x + dx_poi,       # X coordinate away from line
                             y0 = pos$y + dy_poi,       # Y coordinate away from line
                             x1 = pos$x + dx_poi,       
                             y1 = pos$y + dy_poi,       
                             theta = pos$thetaT,    # angle
                             x = pos$x,             # Line coordinate X
                             y = pos$y,             # Line coordinate Y
                             object = pos$object,
                             nextx = pos$x1,
                             nexty = pos$y1) 
    
    # create polygon from object to select correct segment of the transect (coastal side only) 
    points_for_polygon <- rbind(output[,c('x', 'y','nextx', 'nexty')])# select points
    pol_for_plot <- SpatialPolygons(list( Polygons(list(Polygon(points_for_polygon[,1:2])),1)),
                                          proj4string = CRS(as.character(line@proj4string)))
    # plot(SpatialPoints(data.frame(x = points_for_polygon[,'x'], y = points_for_polygon[,'y']),CRS(as.character(line@proj4string))),
    # axes = T, add = T,col = 'blue')
    # plot(SpatialPoints(data.frame(x = output[,'x0'], y = output[,'y0']),CRS(as.character(line@proj4string))),
            # axes = T, add = T, col = 'red')
    # plot(SpatialPoints(data.frame(x = output[,'x1'], y = output[,'y1']),CRS(as.character(line@proj4string))),
         # axes = T, add = T, col = 'green')
    
    # plot(pol_for_plot, axes = T, add = T, col = 'orange')
    
    # determine first and last point on the coastline
    firstForPlot <- data.frame(x = points_for_polygon$x[1], y = points_for_polygon$y[1])
    lastForPlot <- data.frame(x = points_for_polygon$x[length(points_for_polygon$x)],
                              y = points_for_polygon$y[length(points_for_polygon$y)])
    
    # plot_all <- SpatialPoints(points_for_polygon, CRS(as.character(line@proj4string)))
    plot_first <- SpatialPoints(firstForPlot, CRS(as.character(line@proj4string)))
    plot_last <- SpatialPoints(lastForPlot, CRS(as.character(line@proj4string)))
    # plot(plot_first, add = T, col = 'red')
    # plot(plot_last, add = T, col = 'purple')
    # plot(plot_all, add = T)
    
    ## Corners of bounding box
    LX <- min(points_for_polygon$x)
    UX <- max(points_for_polygon$x)
    LY <- min(points_for_polygon$y)
    UY <- max(points_for_polygon$y)
    # polygon(x = c(LX, UX, UX, LX), y = c(LY, LY, UY, UY), lty = 2)
    # polygon(x = c(LX, UX, LX), y = c(LY, LY, UY), lty = 2)
    
    # grow polygon
    left1 <- data.frame(x = LX - (tlen + 1), y = LY) 
    right1 <- data.frame(x = UX + (tlen + 1), y = LY) 
    LL_corner <- data.frame(x = LX - 2 * tlen, y = LY - 2 * tlen)
    LR_corner <- data.frame(x = UX + tlen, y = LY - 2 * tlen)
    # UR_corner <- data.frame(x = LX - tlen, y = UY - tlen)
    corners <- rbind(LL_corner, LR_corner,left1,right1)
    bbox_add <- SpatialPoints(corners, CRS(as.character(line@proj4string)))
    # plot(bbox_add ,col = 'blue', axes = T, add = T)
    
    # Control plot order of points to avoid weird shapes by using nearest point first
    firstForPlot$near <- apply(gDistance(bbox_add,plot_last, byid = T), 1, which.min)
    lastForPlot$near <- apply(gDistance(bbox_add,plot_first, byid = T), 1, which.min)
    
    secondForPlot <- apply(gDistance(bbox_add[row.names(bbox_add) != firstForPlot$near & # not equal to first & last for plot
                                                row.names(bbox_add) != lastForPlot$near,], 
                                     plot_last, byid = T), 1, which.min)
    secondplot <- SpatialPoints(rbind(corners[secondForPlot,]), CRS(as.character(line@proj4string)))
    
    thirdForPlot <- apply(gDistance(bbox_add[row.names(bbox_add) != secondForPlot,], 
                                    secondplot, byid = T), 1, which.min)
    thirdplot <- SpatialPoints(rbind(corners[thirdForPlot,]), CRS(as.character(line@proj4string)))
    # plot(secondplot, add = T, col = 'red')
    # plot(thirdplot, add = T, col = 'purple')
    
    # increase polygon with corresponding points
    points_for_polygon_incr <- rbind(points_for_polygon[,1:2], corners[firstForPlot$near,], 
                                     # corners[row.names(corners) != firstForPlot$near,],
                                     corners[secondForPlot,],
                                     corners[thirdForPlot,],
                                     corners[lastForPlot$near,]
                                     )
    pol_for_intersect_incr <- SpatialPolygons( list( Polygons(list(Polygon(points_for_polygon_incr)),1)),
                                               proj4string = CRS(as.character(line@proj4string)))
    # plot(pol_for_intersect_incr, col = 'brown', add = T)
    
    # plot
    coordsxy <- data.frame(x = output$x, y = output$y)
    plotxy <- SpatialPoints(coordsxy, proj4string = CRS(as.character(line@proj4string)))
    # plot(plotxy, add = T, col = 'blue')
    coordsx1y1 <- data.frame(x = output$x1, y = output$y1)
    plotx1y1 <- SpatialPoints(coordsx1y1, proj4string = CRS(as.character(line@proj4string)))
    # plot(plotx1y1, add = T)
    coordsx0y0 <- data.frame(x = output$x0, y = output$y0)
    plotx0y0 <- SpatialPoints(coordsx0y0, proj4string = CRS(as.character(line@proj4string)))
    # plot(plotx0y0, add = T, col = 'red')
    
    #intersect
    # output[, "x1y1"] <- over(plotx1y1, pol_for_intersect_incr) #NA denotes the point does not fall in a polygon; 
    # output[, "x0y0"] <- over(plotx0y0, pol_for_intersect_incr)

    # CONSIDER CHANGING THIS AND JUST ASSIGNING X1 and Y1 as the end locations of the transect. 
    # WHEN ORDER OF PLOTTING IS CORRECT (NOW FIxED) TRANSECTS ARE ALSO BETTER ORIENTATED
    # indexx0y0 <- with(output, is.na(output$x1y1))
    output[, 'endx'] <- output[, 'x0']
    output[, 'endy'] <- output[, 'y0']
    
    
    # # Assign x1 and y1 if end coordinate is not inside the polygon
    # indexx0y0 <- with(output, is.na(output$x1y1))
    # output[indexx0y0, 'endx'] <- output[indexx0y0, 'x1']
    # output[indexx0y0, 'endy'] <- output[indexx0y0, 'y1']
    # # assign x0 and y0 if end coordinate is 'clipped'
    # index <- with(output, is.na(output$x0y0))
    # output[index, 'endx'] <- output[index, 'x0']
    # output[index, 'endy'] <- output[index, 'y0']
    
    # drop rows that have both points intersecting with polygon
    output <- output[with(output, !is.na(output$endx)),]
    
    AllTransects = rbind(AllTransects, output)
  }
  
  
  # create all transects 
  id <- rownames(AllTransects)
  AllTransects <- cbind(id=id, AllTransects)
  
  lines <- vector('list', nrow(AllTransects))
  for(n in seq_along(lines)){
    # n = 141
    col_names <- list('lon', 'lat')
    row_names <- list('begin', 'end')
    # dimnames < list(row_names, col_names)
    
    begin_coords <- data.frame(lon = AllTransects$x, lat = AllTransects$y)       # Coordinates on the original line
    end_coords <- data.frame(lon = AllTransects$endx, lat = AllTransects$endy)   # coordinates as determined by the over: remove implement in row below by selecting correct column from output
    x <- as.matrix(rbind(begin_coords[n,], end_coords[n,]))
    
    # dimnames(x) <- list(row_names, col_names)
    lines[[n]] <- Lines(list(Line(x)), ID = as.character(AllTransects[n, 1]))
    
  }
  lines_sf <- SpatialLines(lines, proj4string = CRS(as.character(line@proj4string)))
  # plot(lines_sf, col = 'black')
  df <- SpatialLinesDataFrame(lines_sf, data.frame(AllTransects, row.names = AllTransects[, 1]))
  
  # clean transects
  # 1: remove parts that intersect with coastline
  for(trans in 1: length(df)){ # kijk in elke transect
    # trans <- 145
    transect <- df[trans,]
    # plot(transect, add = T, col = 'red')

    subset_trans <- data.frame(geom(transect))[, c('x', 'y')]

    pnt_trans <- gBuffer(SpatialPoints(data.frame(x = subset_trans[,'x'], y = subset_trans[,'y']),
                               CRS(as.character(line@proj4string)))[1], width = 3000) # select points from transect + buffer
    intersect <- gIntersection(pnt_trans, line) # intersect points with coastline
    start_buf <- gBuffer(intersect, width = 100) # Apply a buffer to the intersection point
    #  gbuffer fails if begin point is not near the initial coastline feature
    clipTrans <- gDifference(transect, start_buf) # reduce the transect by the buffer

    # after clipping on the buffer: test for coastline intersect
    intersect_final <- any(gIntersects(clipTrans,line)) # true for intersection
    
    # test <- gIntersection(clipTrans, line, byid=c(TRUE, TRUE))
    if(intersect_final){
      # where do they intersect
      getIntersect <- intersect(clipTrans, as(line, "SpatialLines"))
      intersectBuffer <- gBuffer(SpatialPointsDataFrame(getIntersect, data = data.frame(getIntersect@data)), width = 100)
      
      # mapview(intersectBuffer) + mapview(st_as_sf(clipTrans)) #+ mapview(st_as_sf(line))
      
      intersected2 <- gDifference(transect, intersectBuffer, byid = T)
      CoordinatesFirst <- coordinates(intersected2)[[1]][[1]] # get coordinates (1st set is the origin?)
      
      # for plotting
      toLine <-  st_sfc(st_linestring(CoordinatesFirst),crs = as.character(crs(line@proj4string@projargs)))
      dat_sf = st_as_sf(toLine)
  
      
      # mapview(dat_sf) + mapview(transect) + mapview(line)
      
      # overwrite if intersect_final
    
    
      lines[[trans]]<- NA
      lines[[trans]] <- Lines(list(Line(CoordinatesFirst)), ID = trans)
        
        # df[trans,]@data
      }

    }
  # df = df[df$intersect != 1,]
  
  # update lines_sf & df!
  lines_sf <- SpatialLines(lines, proj4string = CRS(as.character(line@proj4string)))
  df <- SpatialLinesDataFrame(lines_sf, data.frame(AllTransects, row.names = AllTransects[, 1]))
  
  return(df)
} 

#
df <- SpatialLinesDataFrame(kustOI,data.frame(kustOI))
lines_sf <- st_as_sf(df)
transects <- evenspace(kustOI, sep = 1000, tlen = 30000, smoothing_factor = 5)

# if country is Suriname: flip pos values to ensure west-east goes from 0 to 380000
MaxPos <- max(transects$pos)
transects$pos <- abs(transects$pos -MaxPos)
  # dplyr::mutate(pos = ifelse(Country == "Suriname", ,
                             # pos))

df_transects <- SpatialLinesDataFrame(transects,data.frame(transects@data))
transects_sf <- st_as_sf(df_transects)

mapview(lines_sf, col.regions = c("red")) + mapview(transects_sf)

# trans_transform <- spTransform(transects, crs(kustlijn_smoothed2@proj4string@projargs))
# 
# plot(trans_transform, add =T)
                          
writeOGR(transects, dsn = paste0(location, '/raw/shapes/transects'),
         layer = paste0('transects_kustlijn_', country), driver = "ESRI Shapefile")

# writeOGR(transects, dsn = 'D:/BackUp_D_mangroMud_202001/Site1_Suriname_all/Analysis/IntertidalArea/Transects',
#           layer = 'transects_kustlijn_braamspunt', driver = "ESRI Shapefile")
