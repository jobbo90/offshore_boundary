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
dataFolder <- './data/raw'
years <- c('2018', '2019') 
# transect  <- readOGR(paste0(dataFolder, '/transects'), '2009_WnZ_transect')

# select folders
folderSelect <- as.matrix(list.files(paste0(dataFolder, '/GEE_exports'), full.names = T))
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
filtered <- unique(filtered)[1:2,]

allFiles <- do.call(rbind, lapply(as.matrix(filtered)[,1], function(x) read.csv(x, stringsAsFactors = FALSE,
                                                                                sep = ',', na.strings=c("","NA")
                                                                                )))

col_dates <- col_of_interest(allFiles, 'DATE_ACQUIRED$')
col_coastDist <- col_of_interest(allFiles, 'coastDist$')

# all unique dates
uniqueDates <- unique(allFiles[,col_dates]);
# all unique transect (id's)
allPos <- unique(allFiles[, col_of_interest(allFiles, 'pos$')]);
uniqueX<- unique(allFiles[, col_of_interest(allFiles, 'originX$')]);
uniqueY<- unique(allFiles[, col_of_interest(allFiles, 'originY$')]);
geo<- unique(allFiles[, col_of_interest(allFiles, '.geo')]);


coastlines <- reshape_csvPoints(allFiles, 'coastX', 'coastY', c('coastDist'))
keep_columns <- c('axisDist', 'mudFract', 'endDrop')  # necessary for mudbank output
mudbanks <- reshape_csvPoints(allFiles, 'peakCoordX', 'peakCoordY', keep_columns)

# plotting example
collectionL5 <- ee$ImageCollection("LANDSAT/LT05/C01/T1_TOA")$
  filterBounds(ee$Geometry$Point(-55.54, 5.94))

collectionL7 <- ee$ImageCollection("LANDSAT/LE07/C01/T1_TOA")$
  filterBounds(ee$Geometry$Point(-55.54, 5.94))
  # merge(collection)

collectionL8 <- ee$ImageCollection("LANDSAT/LC08/C01/T1_TOA")$
  filterBounds(ee$Geometry$Point(-55.54, 5.94))

collection <- collectionL8$merge(collectionL7)$merge(collectionL5)

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



for (i in uniqueDates){
  # i <- uniqueDates[2]
  
  # for testing: set to cloudfree
  cloudFree <- ee$Image(collection$filter(ee$Filter$gt("CLOUD_COVER", 10))$
                          filterDate(as.character(as.Date(min(uniqueDates))-1), as.character(as.Date(max(uniqueDates))+1))$
                          sort("CLOUD_COVER")$first())
  id <- eedate_to_rdate(cloudFree$get("system:time_start"))

  i <- uniqueDates[uniqueDates == as.Date(id)]
  
  
  coastlines_selection <-subset(coastlines, coastlines$DATE_ACQUIRED == i &
                                  coastlines$coastDist >= 0) 
  mudbanks_selection <-subset(mudbanks, mudbanks$DATE_ACQUIRED == i & 
                                mudbanks$axisDist >= 0) 
  # order
  coastlines_selection[order(as.character(coastlines_selection$pos)),]
  mudbanks_selection[order(as.character(mudbanks_selection$pos)),]
  
  
  # collection for testing
  filtCollect <- collection$filterDate(as.character(as.Date(i)-1), as.character(as.Date(i)+1))
  dates <- ee_get_date_ic(filtCollect, time_end = FALSE)
  
  id <- eedate_to_rdate(filtCollect$first()$get("system:time_start"))
  
  first <- Map$addLayer(filtCollect$first(), visParams, paste0(i))
  Map$centerObject(filtCollect$first())
  
  
  first+mapview(coastlines_selection, col.regions = c("red"), layer.name = i) +
    mapview(mudbanks_selection, col.regions = c("green"), layer.name = i )
  
  
  for (pnt in 1:nrow(mudbanks_selection)){
    # pnt <- 100
    # pos_of_interst <- 223000
    
    # selected_point <-mudbanks_selection[pnt,]
    selected_point <-mudbanks_selection[which(mudbanks_selection$pos == pos_of_interst),]
    
    # select nearby points
    # a) nearest 2?
    # b) based on pos
    ajoining_points <- subset(mudbanks_selection, as.character(pos) <=  as.numeric(as.character(selected_point$pos))+2000 &
                                as.character(pos) >= as.numeric(as.character(selected_point$pos))-2000 &
                                as.character(pos) != as.numeric(as.character(selected_point$pos)))
    
    geom <- st_coordinates(ajoining_points)
    test_spline<-smooth.spline(geom[,1] ~ geom[,2], spar=0.50)
    
    SpatialPoints <- SpatialPointsDataFrame(data.frame(test_spline$y, test_spline$x ), 
                                            data = data.frame(DATE_ACQUIRED = ajoining_points$DATE_ACQUIRED,
                                                              mudFract = ajoining_points$mudFract),
                                            proj4string=CRS("+proj=longlat +datum=WGS84"))
    
    # points_sf <- st_as_sf(SpatialPoints)
    
    # plot ajoining points
    plot(test_spline$x, test_spline$y)
    points(st_coordinates(selected_point)[,'Y'], st_coordinates(selected_point)[,'X'], col = 'blue') # point of interest
    segments(test_spline$x[1],test_spline$y[1],  
             test_spline$x[length(test_spline$x)], test_spline$y[length(test_spline$y)])            # linear fit
    
    # distance from selected point to fitting line 
    dist <- shortestDistanceToLines(Mx=st_coordinates(selected_point)[,'Y'],My=st_coordinates(selected_point)[,'X'], 
                            Ax=test_spline$x[1],Ay=test_spline$y[1], 
                            Bx=test_spline$x[length(test_spline$x)],By=test_spline$y[length(test_spline$y)])
    
    
    plot(ajoining_points$axisDist,ajoining_points$mudFract)
    points(selected_point$axisDist, selected_point$mudFract, col = 'blue')
    
    # if dist < threshold (e.g. 0.01?) then keep
    # else check for index value, if sufficient: keep it 
    # http://www.scielo.br/scielo.php?script=sci_arttext&pid=S0104-65002004000100006
    # http://www.geoinfo.info/proceedings_geoinfo2006.split/paper1.pdf
    # https://www.tandfonline.com/doi/pdf/10.1559/152304099782424901?casa_token=9wn9uSUp3zYAAAAA:XYDB0pKcZcH69STl6eOAlKoMPEwIbvxtlwUwzZ00q4V-z8yOfAREUCePnd4fiZbS9H2A-woJqt0mIg 
    
    # test douglasPeuckerEpsilon
    # library(kmlShape)
    
    
    transform<-st_transform(mudbanks_selection, 32621)
    geom <- cbind(st_coordinates(mudbanks_selection), pos = as.numeric(as.character(mudbanks_selection$pos)))
    
    # order on position 
    geom_ordered <- geom[order(geom[,'pos']),]
    
    range <- 9:16 # for testing
    
    plot( geom_ordered[range,1], geom_ordered[range,2],type="p") # original points
    points( geom_ordered[range[1],1], geom_ordered[range[1],2],type="p", col = 'blue') # begin point
    functionD <- DouglasPeuckerEpsilon(trajx = geom_ordered[range,1],trajy = geom_ordered[range,2], epsilon = 0.01, spar = NA)
    # testD <- DouglasPeuckerNbPoints( geom_ordered[range,1], trajy = geom_ordered[range,2], 2, spar=NA)
    # points( testD[,1], testD[,2],type="p", col = 'red')
    points( functionD[,1], functionD[,2],type="p", col = 'red')

      points( functionD[,1], functionD[,2],type="p", col = 'red')
    
    # douglas pecker algorithm looks at the points that are furthest away from line, if large enough: included as vertext.
    # this assumes outliers are filtered sufficiently
    # e.g. first filter (e.g. by using a spline function (spar in Douglas function))
    # or manual filter that looks at a combination of distance on a line, and the fraction (within a range)
    
    # Alternatively; inverse of douglas filtering. 
    # Look at point, if it's distance is to far away exclude it
    # But that will only work if whe apply it on sub polylines that are on a imaginary line that corresponds to a boundary
    # http://www.scielo.br/scielo.php?script=sci_arttext&pid=S0104-65002004000100006
    
    findClosestPoint_manual <- function(trajx,trajy){
      
      # trajx <- geom_ordered[range,1]
      # trajy <- geom_ordered[range,2]
      
      dmax <- 1
      index <- 1
      end <- length(trajx)
      
      if(end==2){
        index <- 1
        dmax <- 0
      }else{
        for(i in 2:(end-1)){ # for each point but the first and last
          i <- 2
          # calculate the distance
          d <- shortestDistanceToLines(Mx=trajx[i],My=trajy[i], Ax=trajx[1],Ay=trajy[1], Bx=trajx[end],By=trajy[end])
          if ( d < dmax ) {
            # update dmax & index with the distance
            # in the end only the max distance is included (due to the if(d>dmax))
            index <- i
            dmax <- d
          }else{} # don't do anything
        }
      }
      
      output <- c(index, dmax)
      names(output) <- c('index', 'dmax')
      
      return(output)
      # return(c(index=index,dmax=dmax))
    }
    
    nearestPoint <- findClosestPoint_manual(geom_ordered[range,1],geom_ordered[range,2])
    points( geom_ordered[nearestPoint['index'],1], geom_ordered[nearestPoint['index'],2],type="p", col = 'green')
    segments(trajx[1],trajy[1],  trajx[end], trajy[end])
    
    
    farestPoint <- findFarestPoint_manual(geom_ordered[range,1], geom_ordered[range,2])
    
    points(geom_ordered[farestPoint['index'],1], geom_ordered[farestPoint['index'],2],type="p", col = 'green')
    
    

    

    first +
      mapview(mudbanks_selection, col.regions = c("blue"), layer.name = c('mudbanks_selection')) +
      mapview(selected_point, col.regions = c("red"), layer.name = c('selected_point')) +
      mapview(ajoining_points, col.regions = c("green"), layer.name = c('ajoining_points')) +
      mapview(SpatialPoints, col.regions = c("yellow"), layer.name = c('spline')) 
    
  }
  
  
  # https://gis.stackexchange.com/questions/68359/creating-average-polygon

  

}







# library(plotly)
# library(grid)
# ggplotly(p)
#  
# # downViewport('panel.3-4-3-4') 
# pushViewport(dataViewport(x, y, c(0,10), c(0.1, 0.5))) 
# pick<-grid.locator('in')
# pick.n <- as.numeric(pick)
# view.x <- as.numeric(convertX( unit(x,'native'), 'in' ))
# view.y <- as.numeric(convertY( unit(y,'native'), 'in' ))
# w <- which.min((view.x-pick.n[1])^2 + (view.y-pick.n[2])^2)
# 
# 
# ggidentify <- function (x, y, labels, xscale=NULL, yscale=NULL) { 
#   depth <- downViewport('ROOT')
#   pushViewport(dataViewport(x,y, xscale, yscale))
#   pick <- grid.locator('in')
#   while(!is.null(pick)) {
#     pick.n <- as.numeric(pick)
#     view.x <- as.numeric(convertX( unit(x,'native'), 'in' ))
#     view.y <- as.numeric(convertY( unit(y,'native'), 'in' ))
#     d <- min( (view.x-pick.n[1])^2 + (view.y-pick.n[2])^2 )
#     w <- which.min((view.x-pick.n[1])^2 + (view.y-pick.n[2])^2)
#     if (d>0.1) {
#       print("Closest point is too far")
#     } else {  
#       popViewport(n=1)
#       upViewport(depth)
#       print(last_plot() + annotate("text", label=labels[w], x = x[w], y = y[w], 
#                                    size = 5, hjust=-0.5, vjust=-0.5))
#       depth <- downViewport('panel.3-4-3-4')
#       pushViewport(dataViewport(x,y, xscale, yscale))
#     }
#     pick <- grid.locator('in')
#   }
#   popViewport(n=1)
#   upViewport(depth)
# }
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
