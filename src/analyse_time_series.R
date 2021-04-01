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
# does this work
## ---------------------------
source("./src/functions.R")

## ---------------------------

mapviewOptions(basemaps = c( "Esri.WorldImagery","Esri.WorldShadedRelief", "OpenStreetMap.DE"))
dataFolder <- './data/processed'
# years <- c('2005', '2006','2007', '2008','2009')
years <- seq(from = 1985, to = 2020, by = 1)
aoi <- c('Suriname')

posToExclude <- c(seq(138000,147000,1000),
                  seq(241000, 255000, 1000))  

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
# 
# allFiles <- allFiles %>% dplyr::mutate(year = year(DATE_ACQUIRED),
#                                            month = month(DATE_ACQUIRED, label=TRUE),
#                                            day = day(DATE_ACQUIRED))

# all unique transect (id's)
allPos <- unique(allFiles[, col_of_interest(allFiles, 'pos$')]);
uniqueX<- unique(allFiles[, col_of_interest(allFiles, 'originX$')]);
uniqueY<- unique(allFiles[, col_of_interest(allFiles, 'originY$')]);
# geo<- unique(allFiles[, col_of_interest(allFiles, '.geo')]);

keep_columns <- colnames(allFiles)#c('axisDist', 'dist_locf', 'distance', 'outlier', 'mudFract',
                 # 'originX', 'originY')  # necessary for mudbank output
#'
#' create an image collection
#' 

pol <- ee$Geometry$Polygon(
  coords = list(
    c(-56.856912, 5.836168),
    c(-56.821485, 6.120976),
    c(-54.262531, 6.009777),
    c(-54.255509, 5.772303)
  ),
  proj = "EPSG:4326",
  geodesic = FALSE
)


collectionL4 <- ee$ImageCollection("LANDSAT/LT04/C01/T1_TOA")$
  filterBounds(pol)

collectionL5 <- ee$ImageCollection("LANDSAT/LT05/C01/T1_TOA")$
  filterBounds(pol)

collectionL7 <- ee$ImageCollection("LANDSAT/LE07/C01/T1_TOA")$
  filterBounds(pol)
  # merge(collection)

collectionL8 <- ee$ImageCollection("LANDSAT/LC08/C01/T1_TOA")$
  filterBounds(pol)

collection <- collectionL8$merge(collectionL5)$merge(collectionL7)$
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
all_years <- unique(allFiles$year_col)
group_pos <- unique(allFiles$pos)

# allFiles$mudbankObs <- NA
# allFiles$mudbankObsOutlier <- NA

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


    # plot(combinedPnt$pos, combinedPnt$mudbank_extent)
    
    # plot(as.Date(as.character(subsets$DATE_ACQUIRED)), subsets$axisDist,
    #      main = paste0('mudbank position position: ',position, ' [m]'),
    #      ylim = c(min(c(subsets$axisDist, subsets$coastDist)-300, na.rm = T),
    #           max(c(subsets$axisDist, subsets$coastDist), na.rm = T)),
    #      xlab = paste0(format(as.Date(selected_year), "%Y")),
    #      ylab = 'distance from transect origin')
    # points(as.Date(as.character(subsets$DATE_ACQUIRED)), subsets$coastDist,col = 'red')

    if (nrow(subsets) > 1){
      
      # get the coastline distance;
      # meanCoast <- mean(subsets$coastDist, na.rm = T)
      # modalCoast <- modal(subsets$coast_median)
      
      medianOffshore <- median(subsets$axisDist, na.rm=T)
      
      bearing <- median(subsets$bearing)
      originX <- median(subsets$originX)
      originY <- median(subsets$originY)
      
      # set origin at correct location or add coastline dist to dist of interest?
      destPoint <- destPoint(SpatialPoints(data.frame(x = originX, y = originY),
                                           CRS("+proj=longlat +datum=WGS84")), 
                             bearing, medianOffshore)

      allFiles[which(row.names(allFiles) %in% row.names(subsets)), 'distX'] <-
        # pdens$lat[which.max(pdens$count)]
        destPoint[1]
      
      allFiles[which(row.names(allFiles) %in% row.names(subsets)), 'distY'] <-
        # pdens$lon[which.max(pdens$count)]
        destPoint[2]

    }
    
   
    
  }
}

# plot alongshore variability of mud fractions
# allFiles$SmoothedPeakFract
range <- round(quantile(subset(allFiles, 
                               !is.na(SmoothedPeakFract) & 
                               SmoothedPeakFract > 0 )$SmoothedPeakFract,c(0.05, 0.99), 
                        na.rm=T), 2)

p <-ggplot(subset(allFiles, !is.na(SmoothedPeakFract) & SmoothedPeakFract > 0 ),
           aes(x = pos,y = as.Date(year_col), fill=SmoothedPeakFract))+  #y = as.Date(quarterly_col)
  # fill=slope / deltaCoast / normalized / normalized2 / coastDist
  geom_tile(color= "white",size=0.1, na.rm = TRUE) +
  scale_fill_gradient2(limits = c(range[[1]],range[[2]]), 
                       breaks = c(range[[1]], range[[1]]/2, 0, range[[2]]/2, range[[2]]),
                       low = "#a50026", high = "#313695", mid = '#f7f7f7',
                       guide = guide_colourbar(nbin=100, draw.ulim = FALSE,
                                               draw.llim = FALSE),
                       oob=squish, na.value = NA) + #"grey50"
  labs(y = 'Date', x = 'position') +
  scale_x_reverse(lim=c(max(allFiles$pos)+4000, 0), expand = c(0,0))  








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
reference_date <- as.Date("2009-09-12") #as.Date("2003-11-15") # uniqueDates[150]
years_to_test <- year(reference_date)
nearestDate <- uniqueDates[1:length(uniqueDates) == 
                             which.min(abs(as.Date(uniqueDates) - as.Date(reference_date)))]

# collection for testing
filtCollect <- collection$filterDate(as.character(as.Date(nearestDate)-0), 
                                     as.character(as.Date(nearestDate)+1))
dates <- ee_get_date_ic(filtCollect, time_end = FALSE)
first <- Map$addLayer(filtCollect$first(), visParams, paste0('landsat: ',nearestDate))
Map$centerObject(filtCollect$first(), 14)


# one image - all observations
subset_for_testPlot <- subset(allFiles, DATE_ACQUIRED == reference_date &
                                !(pos %in% posToExclude))
mudbanks_selection <-subset(subset_for_testPlot, mudbank_outlier < 1 &
                              mudbank_extent > 0)

mudbank_selection_Outlier <- subset(subset_for_testPlot,
                                    mudbank_outlier >= 1 |
                                      mudbank_extent < 0)

mudbankPos <- sp_pnt_ee(mudbanks_selection$x,
                        mudbanks_selection$y,  
                        'non outlier abs', "#ece7f2")

outlierPos <- sp_pnt_ee(mudbank_selection_Outlier$x,
                         mudbank_selection_Outlier$y,  'outlier',
                        "orange")

# first + mudbankPos + outlierPos


#'
#' Plot 1 date with all observations 
#' 
#' 

uniqueDates[which.min(abs(as.Date(uniqueDates) - as.Date("2009-11-3")))]

reference_date <- as.Date(c("2009-09-12","2009-09-28", "2009-11-07", "2009-11-15")) #as.Date("2003-11-15") 
subsetSelectedDates <- subset(allFiles, as.Date(DATE_ACQUIRED) %in% reference_date)
facet <- 'DATE_ACQUIRED'
subsetSelectedDates$meanMud

alongshoreFracts <- ggplot(subsetSelectedDates, aes(x= pos, y = SmoothedPeakFract,  #meanMud
                               colour = as.factor(mudbank_outlier))) + 
  
  geom_point(size = 3, alpha = 0.6) + # ,
  geom_smooth(method='lm') +
  # geom_point(aes(x= pos, y = meanMud, colour = as.factor(mudbank_outlier)),
  #            size = 3, alpha = 0.6, colour = 'black') +
  facet_wrap(paste0('~', facet)) + # , labeller = as_labeller(unlist(variable_names))
  # scale_color_manual(labels = c("non-outlier","outlier"), values = c("blue", "red")) +
  # guides(color=guide_legend("my title")) +
  scale_x_reverse(lim=c(max(subsetSelectedDates$pos)+4000, 
                        min(subsetSelectedDates$pos)-4000), expand = c(0,0)) +
  scale_y_continuous(lim=c(0,1)) +
  # ggtitle( paste0(unique(subsetSelectedDates$DATE_ACQUIRED))) +
  labs(x = "Position", y = "Fractions", col = 'outlier') +
  theme(axis.line.x = element_line(size = 0.5, colour = "black"),
        axis.line.y = element_line(size = 0.5, colour = "black"),
        axis.line = element_line(size= 1, colour = "black"),
        # axis.title.y = element_text(size = 14, face = 'bold'),
        # axis.title.x = element_text(size = 14, face = 'bold'),
        axis.text.x = element_text(size = 12,  hjust = .5, vjust = .5),
        axis.text.y = element_text(size = 12, hjust = .5, vjust = .5),
        # strip.text.x = element_blank(), # remove panel labels
        legend.title = element_text(colour = 'black', size = 14, face = "bold"),
        legend.key = element_rect(fill = NA),
        legend.text = element_text(size = 15),
        # legend.position = c(.78, .5),
        panel.grid.major = element_blank(), # remove grid lines
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.background = element_rect(fill = '#d9d9d9'),
        strip.text.x = element_text(size = 14, face = 'bold'))

# ggsave(filename = paste0("./results/temp_maps/", 'Suriname_',reference_date[1], 
#                          '_alongshore_fractions',
#                          '_',  format(Sys.Date(), "%Y%m%d"),'.jpeg'),
#        width = 13.1, height = 7.25, units = c('in'), dpi = 1200)


# plot(rev(mudbanks_selection$pos), rev(mudbanks_selection$mudbank_extent+
#                                         mudbanks_selection$coast_median),
#      xlim= rev(range(rev(subset_for_testPlot$pos))),
#      xlab="alongshore position", ylab="along transect distance [m]",
#      main = paste0(reference_date))
# points(rev(mudbank_selection_Outlier$pos),
#        rev(mudbank_selection_Outlier$mudbank_extent +
#              + mudbank_selection_Outlier$coast_median), col='red')
# points(rev(subset_for_testPlot$pos), rev(subset_for_testPlot$coast_median),
#        col = 'blue')
# legend("topleft",
#        legend = c("boundary", "outliers", 'median coastal position'),
#        col = c('black', 'red', 'blue'),
#        pt.cex = 2,pch = c(1,1))
# 
# allFiles$five_year_col

ggplot(subset(allFiles, mudbank_outlier == 0 & mudbank_extent > 0), 
       aes(x= pos, y = SmoothedPeakFract)) +  # colour = five_year_col
  
  geom_point(size = 1, alpha = 0.1) + # ,
  # facet_wrap(paste0('~five_year_col')) +
  geom_smooth(method="lm", col="firebrick", size=2) +

  scale_x_reverse(lim=c(max(subsetSelectedDates$pos)+4000, 
                        min(subsetSelectedDates$pos)-4000), expand = c(0,0)) +
  scale_y_continuous(lim=c(0,1)) +
  # ggtitle( paste0(unique(subsetSelectedDates$DATE_ACQUIRED))) +
  # labs(x = "Position", y = "Fractions", col = 'outlier') +
  theme(axis.line.x = element_line(size = 0.5, colour = "black"),
        axis.line.y = element_line(size = 0.5, colour = "black"),
        axis.line = element_line(size= 1, colour = "black"),
        # axis.title.y = element_text(size = 14, face = 'bold'),
        # axis.title.x = element_text(size = 14, face = 'bold'),
        axis.text.x = element_text(size = 12,  hjust = .5, vjust = .5),
        axis.text.y = element_text(size = 12, hjust = .5, vjust = .5),
        # strip.text.x = element_blank(), # remove panel labels
        legend.title = element_text(colour = 'black', size = 14, face = "bold"),
        legend.key = element_rect(fill = NA),
        legend.text = element_text(size = 15),
        # legend.position = c(.78, .5),
        panel.grid.major = element_blank(), # remove grid lines
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.background = element_rect(fill = '#d9d9d9'),
        strip.text.x = element_text(size = 14, face = 'bold'))



# now all obs within a year
annual_obs <- subset(allFiles,  
                     as.Date(year_col) == as.Date(paste(2003, 1, 1, sep = "-")) &
                       !(pos %in% posToExclude) &
                       mudbank_outlier == 0 &
                       mudbank_extent > 0)
annual_obs_outlier <- subset(allFiles,  
                        as.Date(year_col) == as.Date(paste(2003, 1, 1, sep = "-")) &
                       !(pos %in% posToExclude) &
                       mudbank_outlier > 0)

annual_obs <- annual_obs %>%
  dplyr::group_by(pos) %>%
  dplyr::mutate(testMean = mean(meanMud, na.rm = T)) %>%
  dplyr::mutate(testSD = sd(meanMud, na.rm = T)) %>%
  ungroup()

# ggplot idea: https://www.earthdatascience.org/tutorials/visualize-2d-point-density-ggmap/
pointDensity <- ggplot(annual_obs, aes(x = x, y = y, colour = SmoothedPeakFract)) +
  geom_point(size = 0.5, alpha = 0.2) +
  scale_colour_gradient2(low = "#2166ac",
                         high = "#b2182b",
                         mid = '#fddbc7', midpoint = 0.4,
                         na.value = NA,
                         guide = guide_colourbar(direction = 'horizontal')) +
  # geom_point(annual_obs2, mapping = aes(x = x, y = y),
  #            size = 0.5, alpha = 0.1, color = 'red') +
  coord_equal() +
  labs(x = "Longitude", y = "Latitude", col = 'Fraction') +
  # xlab('Longitude') +
  # ylab('Latitude') +
  theme(axis.line.x = element_line(size = 0.5, colour = "black"),
        axis.line.y = element_line(size = 0.5, colour = "black"),
        axis.line = element_line(size= 1, colour = "black"),
        # axis.title.y = element_text(size = 14, face = 'bold'),
        # axis.title.x = element_text(size = 14, face = 'bold'),
        axis.text.x = element_text(size = 12,  hjust = .5, vjust = .5),
        axis.text.y = element_text(size = 12, hjust = .5, vjust = .5),
        # strip.text.x = element_blank(), # remove panel labels
        legend.title = element_text(colour = 'black', size = 14, face = "bold"),
        legend.key = element_rect(fill = NA),
        legend.text = element_text(size = 10),
        legend.position = c(.48, -.8),
        panel.grid.major = element_blank(), # remove grid lines
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.background = element_rect(fill = '#d9d9d9'),
        strip.text.x = element_text(size = 14, face = 'bold'))
pointDensity

# hist(annual_obs$meanMud,  xlim=c(0,1),
#   breaks = c(seq(floor(min(annual_obs$meanMud)),ceiling(max(annual_obs$meanMud)), 0.01)))

annual_obs_mean <- subset(allFiles,  
                             as.Date(year_col) == as.Date(paste(years_to_test, 1, 1, sep = "-")) &
                               !(pos %in% posToExclude) &
                               mudbank_outlier == 0 &
                            meanMud < 0.05)

annual_obs_sp <- sp_pnt_ee(annual_obs$x[!is.na(annual_obs$x)],
                           annual_obs$y[!is.na(annual_obs$x)],  
                           'axisDist',
          "orange")

annual_obs_mean_sp <- sp_pnt_ee(annual_obs_mean$x[!is.na(annual_obs_mean$x)],
                                annual_obs_mean$y[!is.na(annual_obs_mean$x)],  
                            'high mean mud',
                            "blue")

# annual_obs_sp + annual_obs_mean_sp
# calculated mean position to spatial dataset
meanPos <- SpatialPoints(data.frame(x = annual_obs$distX[complete.cases(annual_obs$distX)],
                                    y =  annual_obs$distY[complete.cases(annual_obs$distY)]),
                         CRS("+proj=longlat +datum=WGS84"))

meanPos_sp <- sp_pnt_ee(meanPos$x,meanPos$y, 'meanPos', "red")


# meanPos_sp +annual_obs_sp

# or on combination data set( neighbouring transects)
# combined <- rbind(subsets, ajoining_points)
selection_density <- pointdensity(df = annual_obs, lat_col = "x", 
                                  lon_col = "y", date_col = NULL, 
                                  grid_size = 0.1, radius = 2)


countRange <- round(quantile(selection_density$count,c(0.25, 0.75), na.rm=T))

# now it is the trick to find the max count for the selected transect
Tempcount <- merge(annual_obs, selection_density, by.x=c('x', 'y'), 
                   by.y=c('lat', 'lon'))

# find max count for each position
# e.g. the location where the density is largest
test <- unique(Tempcount) %>% 
  dplyr::group_by(pos) %>%
  dplyr::filter(count > countRange[1]) %>% # not yet  very succesfull
  top_n(1, count) %>%
  ungroup()  %>%
  dplyr::select(!c(CLOUD_COVER, SmoothedPeak, SmoothedPeakFract,areaName, coastDist,
             coastX, coastY, ndwi_threshold, offsetLast, originX, originY,
             bearing, geometry, quarterly_col, date_col, five_year_col,
             year_col, coast_outlier, locf, deltaCoast, month, day, dateavg))

plot(test$pos, test$mudbankObs)

colnames(Tempcount)

testUniqueCount <- test %>% 
  dplyr::group_by(pos) %>%
  dplyr::summarise(value = max(count)) %>%
  ungroup()

hist(testUniqueCount$value)

isItMedian <-  median(testUniqueCount$value)
isItMean <- mean(testUniqueCount$value)
isItMode <- Mode(testUniqueCount$value)
isItSD <- sd(testUniqueCount$value)


testFilter <- subset(test, count > isItMedian)
# toevoegen; per pos maar 1x meenemen



# filter out positions with a low count (compared to the observations?)


# maxCount <- Tempcount[Tempcount$count == max(Tempcount$count),]
maxCount <- SpatialPoints(data.frame(x = testFilter$x,y = testFilter$y),
                                  CRS("+proj=longlat +datum=WGS84"))

maxCount_spatial <- sp_pnt_ee(maxCount$x,maxCount$y,
                              'density', "yellow")
first + annual_obs_sp + maxCount_spatial





#'
#' Test Mas observations versus fractions
#' 
#' 

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

