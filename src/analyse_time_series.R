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
allFiles <- do.call(rbind, lapply(as.matrix(filtered)[,1], function(x) read.csv(x, stringsAsFactors = FALSE,
                                                                                sep = ',', na.strings=c("","NA")
                                                                                )))

# all unique transect (id's)
allPos <- unique(allFiles[, col_of_interest(allFiles, 'pos$')]);
uniqueX<- unique(allFiles[, col_of_interest(allFiles, 'originX$')]);
uniqueY<- unique(allFiles[, col_of_interest(allFiles, 'originY$')]);
geo<- unique(allFiles[, col_of_interest(allFiles, '.geo')]);

keep_columns <- c('axisDist', 'dist_locf', 'distance')  # necessary for mudbank output
# mudbanks <- reshape_csvPoints(allFiles, 'x', 'y', keep_columns)

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
uniqueDates <- unique(allFiles[,col_dates]);

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
reference_date <- as.Date("2005-06-15")
nearestDate <- uniqueDates[1:length(uniqueDates) == 
                             which.min(abs(as.Date(uniqueDates) - reference_date))]

mudbanks_selection <-subset(mudbanks, 
                            as.Date(mudbanks$DATE_ACQUIRED) == nearestDate & 
                              axisDist >= 0 & 
                              distance >= 0 & 
                              outlier == 0) 

mudbank_selection_Outlier <- subset(mudbanks[order(as.numeric(as.character(mudbanks$pos))),], 
                                    as.Date(DATE_ACQUIRED) == nearestDate & 
                                      mudbanks$axisDist >= 0 &
                                      outlier >= 1)

# collection for testing
filtCollect <- collection$filterDate(as.character(as.Date(nearestDate)-1), 
                                     as.character(as.Date(nearestDate)+1))
dates <- ee_get_date_ic(filtCollect, time_end = FALSE)

first <- Map$addLayer(filtCollect$first(), visParams, paste0('landsat: ',nearestDate))
Map$centerObject(filtCollect$first())


first + mapview(coastlines_selection, col.regions = c("red"), 
              layer.name = paste0('coastlines ',nearestDate)) +
  mapview(mudbanks_selection, col.regions = c("green"), layer.name = 'non outlier' ) +
  mapview(mudbank_selection_Outlier, col.regions = c("orange"), layer.name = 'outlier' )
  # mapview(mudbanks_selection2, col.regions = c("blue"), layer.name = '2009-11-15' )

plot(as.numeric(as.character(mudbanks_selection$pos)), mudbanks_selection$distance)

points(as.numeric(as.character(mudbank_selection_Outlier$pos)), mudbank_selection_Outlier$distance, col = 'red')



mudbanks_selection2 <-subset(mudbanks, 
                            as.Date(DATE_ACQUIRED) == c('2009-11-15') & 
                              axisDist >= 0 & 
                              distance >= 0 & 
                              outlier == 0) 
points(as.numeric(as.character(mudbanks_selection2$pos)), mudbanks_selection2$distance, col = 'blue')


# filter points in river mouths:
# 139000 - 147000 (suriname Rivier)
# 242000 -252000  (saramacca rivier / coppename)


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
