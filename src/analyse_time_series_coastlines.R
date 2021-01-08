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
years <-c('2015', '2016', '2017','2018', '2019', '2020') # c('2005','2006','2007', '2008', '2009')

reference_date <- as.Date("2016-06-01")

aoi <- c('Suriname') # 'Suriname', 'Braamspunt', 'WegNaarZee'

# select folders
folderSelect <- as.matrix(list.files(paste0(dataFolder, '/GEE_exports'), full.names = T))
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
coastlines <- reshape_csvPoints(allFiles, 'coastX', 'coastY', c('coastDist'))
# get transects
transects <- reshape_csvLines(allFiles)

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

filtCollect <- collection$filterDate(as.character(reference_date-15), as.character(reference_date+30))
dates <- ee_get_date_ic(filtCollect, time_end = FALSE)

image <- ee$Image(filtCollect$sort("CLOUD_COVER")$first())   #
# properties <- ee_print(image)

id <- eedate_to_rdate(image$get("system:time_start"))

first <- Map$addLayer(image, visParams,  as.character(as.Date(id)))
Map$centerObject(filtCollect$first())

# coastline Ponts
coastlines_selection <-subset(coastlines, coastlines$DATE_ACQUIRED == as.character(as.Date(id)) &
                                coastlines$coastDist >= 0)
coastlines_selection[order(as.character(coastlines_selection$pos)),]

# plot Map
first  +mapview(coastlines_selection, col.regions = c("blue"), layer.name = as.character(as.Date(id))) +
  mapview(transects)

#---------------------------
#'
#' now multi temporal
#' 
#---------------------------

# test simple 2d plot coastline dist
pos_to_test <- c('117000')

# or on all entries 
allFiles_gt0 <- subset(arrange(allFiles, pos, DATE_ACQUIRED), coastDist >=0) 

# make groups of 3 months per transect
test_allFiles <- allFiles_gt0 %>%  
  mutate(date_col = as.POSIXct(cut(lubridate::date(allFiles_gt0$DATE_ACQUIRED), "3 months"))) %>%
  mutate(year_col = as.POSIXct(cut(lubridate::date(allFiles_gt0$DATE_ACQUIRED), "1 year"))) 

group_dates<-unique(test_allFiles$year_col)
group_pos <- unique(test_allFiles$pos)

# assume nothing is outlier
test_allFiles$outlier <- 1

#'
#'  estimate outliers with rosner test
#'  
#'  Test for outliers on annual frequency for now
#'  Improve: is annual frequency sufficient?
#'  Is Rosner test applicable and correctly implemented
subset_for_testPlot <- subset(test_allFiles, pos == pos_to_test)

plot(as.Date(subset_for_testPlot$DATE_ACQUIRED), subset_for_testPlot$coastDist,
     xlab="DATE_ACQUIRED", ylab="coastDist [m]",
     main = paste0('coastline position: ',pos_to_test, ' [m]'))
abline(v=as.Date(reference_date), lwd =2,lty=2, col = 'red')
text(as.Date(reference_date)-20, quantile(subset_for_testPlot$coastDist, probs = c(0.12)), 
     "reference date", col = "red", srt=90)


for(i in group_dates){
  # i<-group_dates[group_dates == c("2017-01-01")]
  
  for(q in group_pos){
    # q <- group_pos[group_pos == pos_to_test]
    subsets <- subset(test_allFiles, year_col == i & pos == q)
    # plot(as.Date(subsets$DATE_ACQUIRED), subsets$coastDist)
    rownr <- strtoi(rownames(subset(test_allFiles, year_col == i & pos == q)))
    
    # detect outliers (give them a 0!!!)
    test_allFiles[rownr, 'outlier'] <- rosner(subsets$coastDist)
    
  }
}


# plot per group after removing outliers
test_allFiles_mn <- test_allFiles %>% group_by(pos, date_col, outlier) %>%
  mutate(mn = median(coastDist)) 

testSubset <- subset(test_allFiles_mn, 
                     pos == pos_to_test)

outliers <- subset(testSubset, outlier == 0)
nonOutliers <- subset(testSubset, outlier == 1)

# median values with steps per 3 months ()
# points(as.Date(nonOutliers$date_col), nonOutliers$mn, col = 'blue')

# heatmap / space-time plot

# # get for all transects an oldest coastline observation as baseline
test_allFiles_mn$baseline <- 0 
test_allFiles_mn$slope <- -1


for (sid in allPos) {
  # sid = pos_to_test
  # print(sid)
  i <- test_allFiles_mn$pos == sid # create a logical index
  
  subsetPos <- subset(test_allFiles_mn, test_allFiles_mn$pos == sid &
                          test_allFiles_mn$coastDist >= 0) 
  
  outliers <- subset(subsetPos, outlier == 0)
  nonOutliers <- subset(subsetPos, outlier == 1)
  
  # you'd want to normalize for the coastline position around the reference date
  # get first date after reference date:
  index <- which.min(abs(as.Date(nonOutliers$DATE_ACQUIRED)-reference_date))
  
  if(nrow(subsetPos) == 0){
    coastObs <- 0 # if there is no observation in the transect set coast & slope to 0
    slope <- 0
  } else {
    
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
  
  test_allFiles_mn$baseline[i] <- as.numeric(coastObs)
  test_allFiles_mn$slope[i] <- as.numeric(slope)
}


# subtract each that values from each obs (ensure positive values)
# to normalize
allFiles_mutate <- test_allFiles_mn %>% mutate(year = year(DATE_ACQUIRED),
                                               month = month(DATE_ACQUIRED, label=TRUE),
                                               day = day(DATE_ACQUIRED),
                                               full_date= date(DATE_ACQUIRED),
                                               full_date2= date(date_col))

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

p <-ggplot(allFiles_mutate,aes(x = pos,y = full_date2, fill=normalized))+ 
  geom_tile(color= "white",size=0.1) +
  # scale_fill_gradient2(low="red", mid="white", breaks = c(-100,0,100),
                        # high="blue", midpoint =0)
  scale_fill_gradient2(limits = c(-500,500), breaks = c(-500, 0, 500),
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


























