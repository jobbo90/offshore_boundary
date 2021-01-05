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
keep_columns <- c('axisDist', 'mudFract', 'endDrop', 'coastDist')  # necessary for mudbank output
mudbanks <- reshape_csvPoints(allFiles, 'peakCoordX', 'peakCoordY', keep_columns)


mudbanks$coastDist[mudbanks$coastDist == -1] <- NA

# sort al rows based on position & date
mudbanks<-mudbanks[with(mudbanks, order(pos, DATE_ACQUIRED)), ]

# if coastDist = -1; replace with nearest observations
mudbanks$dist_locf <- na.locf(mudbanks$coastDist, option = "locf")   # Last Obs. Carried Forward

# normalize mudbank Distance:
mudbanks$distance <- mudbanks$axisDist - mudbanks$dist_locf   

# set outlier to 0
mudbanks$outlier <- 0

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
#'      

# for testing: set to cloudfree
cloudFree <- ee$Image(collection$filter(ee$Filter$gt("CLOUD_COVER", 0))$
                        filterDate(as.character(as.Date(min(uniqueDates))-1), as.character(as.Date(max(uniqueDates))+1))$
                        sort("CLOUD_COVER")$first())
id <- eedate_to_rdate(cloudFree$get("system:time_start"))

for (i in uniqueDates){
  # i <- uniqueDates[3]
  # i <- uniqueDates[uniqueDates == as.Date(id)]
  
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
  
  # id <- eedate_to_rdate(filtCollect$first()$get("system:time_start"))
  
  first <- Map$addLayer(filtCollect$first(), visParams, paste0(i))
  Map$centerObject(filtCollect$first())
  
  # plot
  # first+mapview(coastlines_selection, col.regions = c("red"), layer.name = i) +
    # mapview(mudbanks_selection, col.regions = c("green"), layer.name = i )

  
  for (pnt in 1:nrow(mudbanks_selection)){
    # pos_of_interst <- 	206000 #130000
    # selected_point <-mudbanks_selection[which(mudbanks_selection$pos == pos_of_interst),]
    selected_point <-mudbanks_selection[pnt,]
    
    
    # select nearby points
    # a) nearest 2?
    # b) based on pos
    ajoining_points <- subset(mudbanks_selection, as.character(pos) <=  as.numeric(as.character(selected_point$pos))+4000 &
                                as.character(pos) >= as.numeric(as.character(selected_point$pos))-4000 &
                                as.character(pos) != as.numeric(as.character(selected_point$pos)))
    
    combined <- rbind(selected_point,ajoining_points)
    
    # order by pos
    combined_ordered <-combined[order(as.numeric(as.character(combined$pos))),]
    
    # plot selected points
    # first +
      # mapview(mudbanks_selection, col.regions = c("blue"), layer.name = c('mudbanks_selection')) +
      # mapview(selected_point, col.regions = c("red"), layer.name = c('selected_point')) +
      # mapview(ajoining_points, col.regions = c("green"), layer.name = c('ajoining_points'))

    # plot(st_coordinates(combined)[,'X'], st_coordinates(combined)[,'Y'],
         # ylim = c(min(c(combined$y, selected_point$y)),max(c(combined$y, selected_point$y))))

    # points(st_coordinates(selected_point)[,'X'], st_coordinates(selected_point)[,'Y'], col = 'red') # point of interest

    
    # segments(test_spline$y[1],test_spline$x[1],
    #          test_spline$y[length(test_spline$y)], test_spline$x[length(test_spline$x)])            # linear fit
    
    # distance from selected point to fitting line 
    # dist <- shortestDistanceToLines(Mx=st_coordinates(selected_point)[,'Y'],My=st_coordinates(selected_point)[,'X'], 
    #                         Ax=test_spline$x[1],Ay=test_spline$y[1], 
    #                         Bx=test_spline$x[length(test_spline$x)],By=test_spline$y[length(test_spline$y)])

        # m<-nls(y~a*x/(b+x))
    # 2nd degree Parabolic:
    # z = a + bx + cy + ex2 + fy2
    # Minimum number of points required: 5
    
    # 2nd degree: z = a + bx + cy + dxy + ex2 + fy2
    # a + (positions*b) + (c*positions) + (d*x*y) + e*x^2 + f*y^2
    
    positions<- as.numeric(as.character(combined_ordered$pos))
    distances <-combined_ordered$distance # grap the normalized distances
    fractions <- combined_ordered$mudFract
    datatest <- data.frame(positions=positions,distances=distances, fractions = fractions)
    
    # test <- loess(distances ~ a +(positions*b) + (c*positions) + (d*positions*distances) + e*positions^2 + f*distances^2,
    #               data = datatest,
    #               start = list(a=1, b = 1, c = 1, d = 1, e = 1, f =1), span=.75)
    # # https://stats.stackexchange.com/questions/176361/trouble-in-fitting-data-to-a-curve-nls?noredirect=1&lq=1
    # e <- nls(distances~a*positions/(b+positions), #
    # start = c(b = 195000, a =3000),algorithm = "plinear"
    # )#

    plot(positions, distances)
    abline(lm(datatest$distances~as.numeric(datatest$positions)),lty = 2)
    
    # plot(positions,fractions, col ='red')
    # abline(lm(datatest$fractions~as.numeric(datatest$positions)),lty = 2)
    
    # plot(fractions, distances, col = 'blue')
    # abline(lm(datatest$distances~as.numeric(datatest$fractions)),lty = 2)
    # 
    
    # calculate linear fit
    lm.out <- lm(datatest$distances~as.numeric(datatest$positions))
    
    # points(positions,lm.out$fitted.values)
    intercept <-lm.out$coefficients[1]
    slope <- lm.out$coefficients[2]
    # potential benefit: slope says something about direction; negative slope; fron of mudbank
    # low pos > further east, decreasing distance ==> front of mud bank. increasing 
    
    #improve;
    # calculate these stats per year.
    
    # residuals
    resid <- lm.out$residuals
    maxResid <- which.max(abs(resid))
    # if maxResid is same as selected point
    # remove it? ==> probably give indication that it needs to be dropped when all computations are finished
    # yet there is allways a largest residual, not perse a significant one...
    
    
    # Perhaps consider the running for outliers again/pos
     
    
    # # distance > 1 considered as outlier?
    # cooksd <- cooks.distance(lm.out)
    # plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance") 
    # abline(h = 4*mean(cooksd, na.rm=T), col="red")
    # 
    # influential <- as.numeric(names(cooksd)[(cooksd > (4/nrow(datatest)))])
    # 
    # # outliers test?
    # library(outliers)
    # out_dist <- outlier(distances) # max from mean
    # score_dist <- scores(distances) # x-mean/sd
    # chi_score_dist <- scores(distances, type="chisq", prob = 0.75)     
    # 
    # scores(distances, type="z", prob=0.95)  # beyond 95th %ile based on z-scores
    # scores(distances, type="t", prob=0.95)  # beyond 95th %ile based on t-scores
    # 
    
    outlier_test <- car::outlierTest(lm.out) # Bonferroni-adjusted outlier test (test largest absolute standardized residual)
    
    # if no outliers detected:
    
    # allways returns one point? Even if it is random
    outlierIndex <- ifelse(length(outlier_test$rstudent) > 0,
                           as.numeric(as.character(combined_ordered[as.numeric(names(outlier_test$rstudent)),]$pos)),
                           min(as.numeric(as.character(mudbanks_selection$pos)))-1000)
    
    # drop outlier ==> not sure if necessary 
    # new_dataTest <- datatest[-outlierIndex,]
    
    # test if it influences the regression line? or is the outlier test sufficient?
    
    if (outlierIndex == as.numeric(as.character(selected_point$pos))){
    # or consider allways giving +1 when detected as outlier?
      mudbanks[which(row.names(mudbanks) == row.names(selected_point)), "outlier"] <-
            as.data.frame(mudbanks[
              which(row.names(mudbanks) == 
                      row.names(selected_point)),"outlier"])[,1] + 1

    }
    # for (ind in 1:length(outlierIndex)){
    #   corresponding_pos <- as.numeric(as.character(combined_ordered[outlierIndex,]$pos))
    #   
    #   
    #   mudbanks[which(mudbanks$pos == corresponding_pos & 
    #                    mudbanks$DATE_ACQUIRED == i), "outlier"] <- 
    #     as.data.frame(mudbanks[which(row.names(mudbanks) == row.names(selected_point)),"outlier"])[,1] + 1
    #     
    # }  
  
    

    


  }
  # plot & redefine mudbank selection
  mudbanks_selection <-subset(mudbanks, mudbanks$DATE_ACQUIRED == i & 
                                mudbanks$axisDist >= 0 & 
                                outlier == 0) 
  mudbank_selection_Outlier <- subset(mudbanks, mudbanks$DATE_ACQUIRED == i & 
                                        mudbanks$axisDist >= 0 &
                                        outlier >= 1)
  
  first+mapview(coastlines_selection, col.regions = c("red"), layer.name = i) +
    mapview(mudbanks_selection, col.regions = c("green"), layer.name = 'non outlier' ) +
    mapview(mudbank_selection_Outlier, col.regions = c("orange"), layer.name = 'outlier' )
  
  
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
# gausslin.start <- function(x,y) {
#   # https://stats.stackexchange.com/questions/62995/how-to-choose-initial-values-for-nonlinear-least-squares-fit
#   # x <- positions
#   # y <- distances
#   
#   theilreg <- function(x,y){
#     yy <- outer(y, y, "-")
#     xx <- outer(x, x, "-")
#     z  <- yy / xx
#     slope     <- median(z[lower.tri(z)])
#     intercept <- median(y - slope * x)
#     cbind(intercept=intercept,slope=slope)
#   }
#   
#   
#   # plot(x,y)
#   tr <- theilreg(x,y)
#   abline(tr,col=4)
#   Ds = tr[2] # slope
#   Es = tr[1] # intercept
#   
#   
#   # yf = y-(Ds*x)-Es # Y = ax+b
#   
#   yf  <- y-Ds*x-Es 
#   yfl <- loess(yf~x,span=.75)
#   
#   # assumes there are enough points that the maximum there is 'close enough' to 
#   #  the true maximum
#   
#   yflf   <- yfl$fitted    
#   # plot(x, yflf)
#   locmax <- yflf==max(yflf)
#   Bs     <- x[locmax]    # peak index
#   As     <- yflf[locmax] # peak value
#   
#   qs     <- y>.6*As # yflf>.6*As
#   ys     <- yfl$fitted[qs]
#   xs     <- x[qs]-Bs
#   lf     <- lm(ys~xs+I(xs^2))
#   bets   <- lf$coefficients
#   Bso    <- Bs
#   Bs     <- Bso-bets[2]/bets[3]/2
#   Cs     <- sqrt(-1/bets[3])
#   ystart <- As*exp(-((x-Bs)/Cs)^2)+Ds*x+Es
#   
#   y1a <- y-As*exp(-((x-Bs)/Cs)^2)
#   tr  <- theilreg(x,y1a)
#   Ds  <- tr[2]
#   Es  <- tr[1]
#   res <- data.frame(As=As, Bs=Bs, Cs=Cs, Ds=Ds, Es=Es)
#   res
# }
# 
# plot(distances~positions)
# https://rdrr.io/cran/ICSNP/man/spatial.median.html
# cov.matrix <- matrix(c(3,2,1,2,4,-0.5,1,-0.5,2), ncol=3)
# X <- rmvnorm(100, c(0,0,0), cov.matrix)
# spatial.median(X)
# 
# plot(X)
# points(spatial.median(X), col = 'red')
