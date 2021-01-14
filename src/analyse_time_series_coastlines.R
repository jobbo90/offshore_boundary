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
years <-years <- c('2005', '2006','2007', '2008','2009') #c('2015', '2016', '2017','2018', '2019', '2020') # c('2005','2006','2007', '2008', '2009')

reference_date <- as.Date("2006-06-16")

aoi <- c('229_56') # path_row

# select folders
folderSelect <- as.matrix(list.files(paste0(dataFolder, '/offshore_points'), full.names = T))
df <- rewrite(folderSelect);
# only csv's
df <- df[grep('.csv', folderSelect, ignore.case = T),]


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
# filtered <- unique(filtered)[1:2,]

allFiles <- do.call(rbind, lapply(as.matrix(filtered)[,1], function(x) read.csv(x, stringsAsFactors = FALSE,
                                                                                sep = ',', na.strings=c("","NA")
                                                                                )))


# somehow all the dates lost 1 hour (due to timezone def?)
allFiles$year_col <- as.Date(as.POSIXct(paste(as.POSIXct(allFiles$year_col), "23:00:00")) + 120*60)
allFiles$date_col <- as.Date(as.POSIXct(paste(as.POSIXct(allFiles$date_col), "23:00:00")) + 120*60)


col_dates <- col_of_interest(allFiles, 'DATE_ACQUIRED$')
col_coastDist <- col_of_interest(allFiles, 'coastDist$')

# all unique dates
uniqueDates <- unique(allFiles[,col_dates]);
# all unique transect (id's)
allPos <- unique(allFiles[, col_of_interest(allFiles, 'pos$')]);
uniqueX<- unique(allFiles[, col_of_interest(allFiles, 'originX$')]);
uniqueY<- unique(allFiles[, col_of_interest(allFiles, 'originY$')]);
geo<- unique(allFiles[, col_of_interest(allFiles, '.geo')]);

# keep_columns <- c('axisDist', 'mudFract', 'endDrop')  # necessary for mudbank output
coastlines <- st_as_sf(SpatialPointsDataFrame(data.frame(
  allFiles$coastX, allFiles$coastY),
  proj4string=CRS("+proj=longlat +datum=WGS84"),
  data = data.frame(allFiles)))

# get transects

transects <- build_csvLines(allFiles)

visParams = list(
    bands = c("B5", "B4", "B3"),
    min = 0.05, max = 0.4, gamma = 1.4
  )



#' 
#' 
#' spatial visualization
#' 
#' 
#-------

# collection 
collectionL8 <- ee$ImageCollection("LANDSAT/LC08/C01/T1_TOA")$
  filterBounds(ee$Geometry$Point(-55.54, 5.94))

collectionL5 <- ee$ImageCollection("LANDSAT/LT05/C01/T1_TOA")$
  filterBounds(ee$Geometry$Point(-55.54, 5.94))

collectionL7 <- ee$ImageCollection("LANDSAT/LE07/C01/T1_TOA")$
  filterBounds(ee$Geometry$Point(-55.54, 5.94))

collection <- collectionL8$merge(collectionL5)$merge(collectionL7)$
  filter(ee$Filter$lt("CLOUD_COVER", 90))
  
# ee_print(filtCollect)

filtCollect <- collection$filterDate(as.character(reference_date-45), as.character(reference_date+45))
dates <- ee_get_date_ic(filtCollect, time_end = FALSE)[,2]

image <- ee$Image(filtCollect$sort("CLOUD_COVER")$first())   #
# properties <- ee_print(image)

id <- eedate_to_rdate(image$get("system:time_start"))

first <- Map$addLayer(image, visParams,  as.character(as.Date(id)))
# Map$centerObject(filtCollect$first())
Map$centerObject(image, 12)

# coastline Ponts
coastlines_selection <-subset(coastlines, coastlines$DATE_ACQUIRED == as.character(as.Date(id)) &
                                coastlines$coastDist >= 0)

coastlines_selection_2 <-subset(coastlines, coastlines$DATE_ACQUIRED >= as.character(as.Date(min(dates))) &
                                  coastlines$DATE_ACQUIRED <= as.character(as.Date(max(dates))) &
                                coastlines$coastDist >= 0)

coastlines_selection[order(as.character(coastlines_selection$pos)),]

# plot Map
first  + mapview(coastlines_selection_2, col.regions = c("orange"), layer.name = 'test') +
  mapview(coastlines_selection, col.regions = c("blue"), 
                layer.name = paste0(as.character(as.Date(id)), ' points')) +
  mapview(transects)

#---------------------------
#'
#' now multi temporal
#' 
#---------------------------
allFiles_gt0 <- subset(arrange(allFiles, pos, DATE_ACQUIRED), coastDist >=0)

# re-calculate median value per year, per transect position
allFiles_gt0 <- allFiles_gt0 %>% group_by(pos, year_col, coast_outlier) %>%
  mutate(coast_median = median(coastDist))

# set outlier median dist to NA
allFiles_gt0$coast_median[allFiles_gt0$coast_outlier == 0] <- NA

allFiles_gt0$coast_median[is.na(allFiles_gt0$coast_median)] <- 
  with(allFiles_gt0, ave(coast_median, allFiles_gt0$year_col, 
                         FUN = function(x) median(x, na.rm = TRUE))
  )[is.na(allFiles_gt0$coast_median)]

# test simple 2d plot 
twoD_pos <- 123000
subset2d_for_testPlot <- subset(allFiles_gt0, pos == twoD_pos)

plot(as.Date(subset2d_for_testPlot$DATE_ACQUIRED), subset2d_for_testPlot$coastDist,
     xlab="DATE_ACQUIRED", ylab="coastDist [m]",
     main = paste0('coastline position: ',twoD_pos, ' [m]'))
points(as.Date(subset2d_for_testPlot[subset2d_for_testPlot$coast_outlier == 0, 'DATE_ACQUIRED']),
       subset2d_for_testPlot[subset2d_for_testPlot$coast_outlier == 0, 'coastDist'],
       col = 'red')
points(as.Date(subset2d_for_testPlot$DATE_ACQUIRED),  subset2d_for_testPlot$coast_median,
       col = 'blue')


# abline(v=as.Date(reference_date), lwd =2,lty=2, col = 'red')
# text(as.Date(reference_date)-20, quantile(subset2d_for_testPlot$coastDist, probs = c(0.12)),
     # "reference date", col = "red", srt=90)


# or on multiple entries 
pos_to_test <- seq(from = 165000, to = 210000, by = 1000)
 

group_dates<-unique(allFiles_gt0$year_col)
group_pos <- unique(allFiles_gt0$pos)


subset_for_testPlot <- subset(allFiles_gt0, pos %in% pos_to_test)

outliers <- to_spatial_df(subset(subset_for_testPlot, coast_outlier == 0), 
                          'coastX', 'coastY')
nonOutliers <- to_spatial_df( subset(subset_for_testPlot, coast_outlier == 1), 
                             'coastX', 'coastY')

first +
  mapview(nonOutliers, col.regions = c("blue"),layer.name = 'nonOutliers') +
  mapview(outliers, col.regions = c("red"),layer.name = 'outliers')

# median values with steps per 3 months ()
# points(as.Date(nonOutliers$date_col), nonOutliers$mn, col = 'blue')

# heatmap / space-time plot





# # get for all transects an oldest coastline observation as baseline
allFiles_gt0$baseline <- 0 
allFiles_gt0$slope <- -1

for (sid in allPos) {
  # sid = 139000
  # print(sid)
  i <- allFiles_gt0$pos == sid # create a logical index
  
  subsetPos <- subset(allFiles_gt0, allFiles_gt0$pos == sid &
                        allFiles_gt0$coastDist >= 0) 
  
  
  plot(as.Date(subsetPos$DATE_ACQUIRED), subsetPos$coastDist,
       xlab="DATE_ACQUIRED", ylab="coastDist [m]",
       main = paste0('coastline position: ',twoD_pos, ' [m]'))
  points(as.Date(subsetPos[subsetPos$coast_outlier == 0, 'DATE_ACQUIRED']),
         subset2d_for_testPlot[subsetPos$coast_outlier == 0, 'coastDist'],
         col = 'red')
  
  
  outliers <- subset(subsetPos, coast_outlier == 0)
  nonOutliers <- subset(subsetPos, coast_outlier == 1)
  
  # you'd want to normalize for the coastline position around the reference date
  # get first date after reference date:
  index <- which.min(abs(as.Date(nonOutliers$DATE_ACQUIRED)-reference_date))
  
  if(nrow(subsetPos) == 0){
    coastObs <- 0 # if there is no observation in the transect set coast & slope to 0
    slope <- 0
  } else {
    
    # get a reference distance 
    # e.g. observation closest to reference date OR
    # median observation over 1 year near the reference date 
    coastObs <- subsetPos[index, 'coastDist']  

    # test normalize - seems to work.
    # normalized <- subsetPos$coastDist - as.numeric(coastObs)
    # plot(as.Date(subsetPos$DATE_ACQUIRED), normalized, ylim = c(min(normalized)-30,max(normalized)+ 30))
    
    # calculate linear fit
    lm.out <- lm(nonOutliers$coastDist~as.numeric(as.Date(nonOutliers$DATE_ACQUIRED)))
    intercept <-lm.out$coefficients[1]
    slope <- lm.out$coefficients[2]
    
    #improve;
    # calculate these stats per year.
    
    # residuals
    resid <- lm.out$residuals
    
    maxResid <- which.max(abs(resid))
    estimated <- intercept + (as.numeric(as.Date(nonOutliers$DATE_ACQUIRED))*slope)
    
    
    #' 
    #' plotting for trouble shooting
    #' 
    #' 
    # plot(as.Date(subsetPos$DATE_ACQUIRED), subsetPos$coastDist, ylim = c(min(subsetPos$coastDist)-30,max(subsetPos$coastDist)+ 30))
    # points(as.Date(outliers$DATE_ACQUIRED), outliers$coastDist, col = 'red')
    # points(as.Date(nonOutliers$DATE_ACQUIRED[maxResid]), nonOutliers$coastDist[maxResid], col = 'purple')
    
    # plot the fitted line
    # abline(lm(nonOutliers$coastDist~as.numeric(as.Date(nonOutliers$DATE_ACQUIRED))),lty = 2)
    # use identify when necesary!
    # date <- identify(as.Date(subsetPos$DATE_ACQUIRED), subsetPos$coastDist, n=1, labels=as.Date(subsetPos$DATE_ACQUIRED))
    
    
    
    # some testing to detect trend breaks...
    # firstSubset <- nonOutliers[1:maxResid,]
    # abline(lm(firstSubset$coastDist~as.numeric(as.Date(firstSubset$DATE_ACQUIRED))),lty = 2)
    # abline(lm(secondSubset$coastDist~as.numeric(as.Date(secondSubset$DATE_ACQUIRED))),lty = 2)
    
    
    # secondSubset <- nonOutliers[maxResid:nrow(nonOutliers),]
    # firstSubset_lm <- lm(firstSubset$coastDist~as.numeric(as.Date(firstSubset$DATE_ACQUIRED)))
    # secondSubset_lm <- lm(secondSubset$coastDist~as.numeric(as.Date(secondSubset$DATE_ACQUIRED)))
    
    # firstSubset_est <- firstSubset_lm$coefficients[1] + as.numeric(as.Date(nonOutliers$DATE_ACQUIRED)) * firstSubset_lm$coefficients[2]
  }
  
  allFiles_gt0$baseline[i] <- as.numeric(coastObs)
  allFiles_gt0$slope[i] <- as.numeric(slope)
}


# subtract each that values from each obs (ensure positive values)
# to normalize
allFiles_mutate <- allFiles_gt0 %>% mutate(year = year(DATE_ACQUIRED),
                                               month = month(DATE_ACQUIRED, label=TRUE),
                                               day = day(DATE_ACQUIRED),
                                               full_date= date(DATE_ACQUIRED),
                                               years = date(year_col))

# subtract baseline value from original
allFiles_mutate$normalized <- allFiles_mutate$coastDist - allFiles_mutate$baseline
# subtract baseline value from median
# allFiles_mutate$mn_normalized <- allFiles_mutate$mn - allFiles_mutate$baseline

# filter outliers & negative coastal distances
# doesn't make the figure more readable... Exclude for now 
# allFiles_mutate <- allFiles_mutate %>% 
  # filter(!(coastDist == -1) & outlier == 0) 

# subsetPos <- subset(allFiles_mutate, allFiles_mutate$pos == pos_to_test &
#                       allFiles_mutate$coastDist >= 0)
# plot(as.Date(subsetPos$DATE_ACQUIRED), as.numeric(subsetPos$normalized), ylim = c(min(subsetPos$normalized)-30,max(subsetPos$normalized)+ 30))


# check: y is date per timestep (e.g. 3 months), the fill needs to be with the 
# same timestep.
# ensure it is normalized for a reference date ==> what is de effect of this date?

p <-ggplot(allFiles_mutate,aes(x = pos,y = years, fill=coast_median))+ 
  geom_tile(color= "white",size=0.1) +
  # scale_fill_gradient2(low="red", mid="white", breaks = c(-100,0,100),
                        # high="blue", midpoint =0)
  scale_fill_gradient2(limits = c(0,20000), breaks = c(0, 10000, 20000),
                        guide = guide_colourbar(nbin=100, draw.ulim = FALSE, draw.llim = FALSE))
  # scale_fill_gradientn(colours=topo.colors(7),#na.value = "transparent",
  #                      breaks=c(0,median(allFiles_mutate$coastDist)),
  #                      labels=c("Minimum","Maximum"),
  #                      limits=c(0,median(allFiles_mutate$coastDist)))
  # scale_fill_viridis(name="Max Distance",option ="C", limits = c(-1000, 1000), oob = scales::squish)
# geom_text(data = labeled.dat, aes(pos,full_date, label = pos), hjust = 2)

p

# plot P shows the max distance of the coastline compared to a reference date.
# this might become problematic when time series are increasing because:
# dist might still be positive while actually the trend is negative 
# also vice versa, increasing coastlineDist while the abs value is still negative.
# different shades help, but don't solve it completely. 
# try to use the coeffients from linear fits per 3 months?


























