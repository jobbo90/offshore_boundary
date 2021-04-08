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

allFiles$distX <- NA
allFiles$distY <- NA
allFiles$medianOffshore <- NA

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


collectionL4 <- ee$ImageCollection("LANDSAT/LT04/C01/T1_RT")$ #T1_TOA
  filterBounds(pol)

collectionL5 <- ee$ImageCollection("LANDSAT/LT05/C01/T1_RT")$
  filterBounds(pol)

collectionL7 <- ee$ImageCollection("LANDSAT/LE07/C01/T1_RT")$
  filterBounds(pol)
  # merge(collection)
# LANDSAT/LC08/C01/T1_RT
collectionL8 <- ee$ImageCollection("LANDSAT/LC08/C01/T1_RT")$
  filterBounds(pol)

collection <- collectionL8$merge(collectionL5)$merge(collectionL7)$
  merge(collectionL4)


visParams <- list(bands = c('B4', 'B3', 'B2'), min = 0, max = 72)

visParamsToa = list(
    bands = c("B5", "B4", "B3"),
    min = 0.05, max = 0.4, gamma = 1.4
  )

#' implement workflow
#' 1) filter outliers & transects with NO mudbank(see pre-processing)
#' 2) create annual estimates of mudbank position or mudbank 
#' 3) alternatively apply douglas pecker algorithm
#'      - Requires to define subsections (see https://www.tandfonline.com/doi/pdf/10.1559/152304099782424901?casa_token=9wn9uSUp3zYAAAAA:XYDB0pKcZcH69STl6eOAlKoMPEwIbvxtlwUwzZ00q4V-z8yOfAREUCePnd4fiZbS9H2A-woJqt0mIg  )
#'      to ensure separate mudbanks are recognized
#'      
# all unique dates
uniqueDates <- unique(allFiles[,'DATE_ACQUIRED']);
all_years <- unique(allFiles$year_col)
group_pos <- unique(allFiles$pos)

# plot alongshore variability of mud fractions
# allFiles$meanMud # SmoothedPeakFract
range <- round(quantile(subset(allFiles, 
                               !is.na(SmoothedPeakFract) & 
                                 #allFiles$mudbank_outlier <1 &
                                 SmoothedPeakFract > 0 )$SmoothedPeakFract,c(0.05,0.5, 0.99), 
                        na.rm=T), 2)

# alongshore variation of mud fractions
hovmoller <-ggplot(subset(allFiles, !is.na(SmoothedPeakFract) & SmoothedPeakFract > 0 
                  & #allFiles$mudbank_outlier <1 &
                    !(pos %in% posToExclude)),
           aes(x = pos,y = as.Date(year_col), fill=SmoothedPeakFract))+  
  geom_tile(color= "white",size=0.1, na.rm = TRUE) +
  scale_fill_gradient2(limits = c(range[[1]], range[[3]]), 
                       breaks = c(range[[1]], range[[2]], range[[3]]),
                       low = "#313695", high ="#a50026", mid = '#f7f7f7',
                       midpoint = range[[2]],
                       guide = guide_colourbar(nbin=100, draw.ulim = FALSE,
                                               draw.llim = FALSE),
                       oob=squish, na.value = NA) + #"grey50"
  labs(y = 'Date', x = 'position') +
  scale_x_reverse(lim=c(max(allFiles$pos)+4000, 0), expand = c(0,0))  

# hovmoller


nonOutliers <- subset(allFiles, !is.na(SmoothedPeakFract) & SmoothedPeakFract > 0 &
         !(pos %in% posToExclude))

# plot(nonOutliers$deltaCoast, nonOutliers$SmoothedPeakFract)


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

first + mudbankPos + outlierPos


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
  facet_wrap(paste0('~', facet), ncol = 1) + # , labeller = as_labeller(unlist(variable_names))
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

alongshoreFracts

# ggsave(filename = paste0("./results/temp_maps/", 'Suriname_',reference_date[1], 
#                          '_alongshore_fractions',
#                          '_',  format(Sys.Date(), "%Y%m%d"),'.jpeg'),
#        width = 13.1, height = 7.25, units = c('in'), dpi = 1200)


mudbankPos <- sp_pnt_ee(subset(subsetSelectedDates, mudbank_outlier == 0)$x,
                        subset(subsetSelectedDates, mudbank_outlier == 0)$y,  
                        'non outlier abs', "#ece7f2")

outlierPos <- sp_pnt_ee(subset(subsetSelectedDates, mudbank_outlier == 1)$x,
                        subset(subsetSelectedDates, mudbank_outlier == 1)$y,  'outlier',
                        "orange")
first + mudbankPos + outlierPos

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

# For All non outlier observations plot the smoothed peak fraction alongshore
# ggplot(subset(allFiles, mudbank_outlier == 0 & mudbank_extent > 0), 
#        aes(x= pos, y = SmoothedPeakFract)) +  # colour = five_year_col
#   
#   geom_point(size = 1, alpha = 0.1) + # ,
#   # facet_wrap(paste0('~five_year_col')) +
#   geom_smooth(method="lm", col="firebrick", size=2) +
# 
#   scale_x_reverse(lim=c(max(subsetSelectedDates$pos)+4000, 
#                         min(subsetSelectedDates$pos)-4000), expand = c(0,0)) +
#   scale_y_continuous(lim=c(0,1)) +
#   # ggtitle( paste0(unique(subsetSelectedDates$DATE_ACQUIRED))) +
#   # labs(x = "Position", y = "Fractions", col = 'outlier') +
#   theme(axis.line.x = element_line(size = 0.5, colour = "black"),
#         axis.line.y = element_line(size = 0.5, colour = "black"),
#         axis.line = element_line(size= 1, colour = "black"),
#         # axis.title.y = element_text(size = 14, face = 'bold'),
#         # axis.title.x = element_text(size = 14, face = 'bold'),
#         axis.text.x = element_text(size = 12,  hjust = .5, vjust = .5),
#         axis.text.y = element_text(size = 12, hjust = .5, vjust = .5),
#         # strip.text.x = element_blank(), # remove panel labels
#         legend.title = element_text(colour = 'black', size = 14, face = "bold"),
#         legend.key = element_rect(fill = NA),
#         legend.text = element_text(size = 15),
#         # legend.position = c(.78, .5),
#         panel.grid.major = element_blank(), # remove grid lines
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         plot.background = element_rect(fill = '#d9d9d9'),
#         strip.text.x = element_text(size = 14, face = 'bold'))

# collection for testing
dateForTest <- as.Date("2008-01-01")

filtCollectAnnual <- collection$filterDate(as.character(dateForTest), 
                                     as.character(dateForTest + 365))
dates <- ee_get_date_ic(filtCollectAnnual, time_end = FALSE)

medianComp <- filtCollectAnnual$median()
composite <- ee$Algorithms$Landsat$simpleComposite(filtCollectAnnual)#$uint32
Map$centerObject(filtCollectAnnual$first(), 14)
addComposite <- Map$addLayer(composite, visParams, paste0('landsat: ',dateForTest))



# now all obs within a year
annual_obs <- subset(allFiles,  
                     as.Date(year_col) == dateForTest &
                       !(pos %in% posToExclude))

annual_obs_nonOutlier <- subset(allFiles,  
                             as.Date(year_col) == dateForTest &
                               !(pos %in% posToExclude) &
                               mudbank_outlier == 0)
annual_obs_outlier <- subset(allFiles,  
                        as.Date(year_col) == dateForTest &
                       !(pos %in% posToExclude) &
                       mudbank_outlier > 0)
# annual_obs$validMudbankObs
# visualize amaunt of observations
# alongshore variable coastline change as a result of mud fraction
posToTest <- 299000

# ggplot(subset(annual_obs,  pos == posToTest), 
#        aes(x = DATE_ACQUIRED, y = validMudbankObs, colour = as.factor(mudbank_outlier))) + 
#   geom_point(size = 1, alpha =1) +
#   geom_point(data = subset(annual_obs,  pos == posToTest), 
#              aes(x = DATE_ACQUIRED, y = mudbankObs),
#              colour = 'black',
#              size = 1, alpha =1)
# 
# ggplot(subset(annual_obs,  mudbank_outlier == 0),
#        aes(x = pos, y = validMudbankObs/mudbankObs, 
#            colour = as.factor(mudbank_outlier))) + 
#   geom_point(size = 1, alpha =1) 

# make it spatial.  
annual_mudbankPos <- sp_pnt_ee(annual_obs_nonOutlier$x,
                        annual_obs_nonOutlier$y,  
                        'non outlier abs', "#ece7f2")

annual_outlierPos <- sp_pnt_ee(annual_obs_outlier$x,
                        annual_obs_outlier$y,  'outlier',
                        "orange")

# remove from these annual observations the positions that are 
# frequently considered as outlier
# hist(annual_obs$validMudbankObs/annual_obs$mudbankObs)
# mean(annual_obs$validMudbankObs/annual_obs$mudbankObs)


# adding a indicaiton of noMudbank positions (so all observations on that POS recieve 1)
# only when at least 50%? of observation in a pos is a mudbank
annual_obs2 <- annual_obs %>%
  group_by(year_col, pos) %>%
  dplyr::mutate(
    noMudbank = case_when(
      validMudbankObs/mudbankObs > 0.5 ~ 0, 
      TRUE ~ 1)) %>% 
  
  ungroup() #%>%
  # dplyr::select(year_col, pos, mudbank_outlier, axisDist, noMudbank) 


# for plotting purpose, filter them
annual_obs_filter <- annual_obs2 %>%
  filter(noMudbank == 0) %>%         # positions that are indicated as no mudbank
  filter(mudbank_outlier == 0) %>%   # outliers
  filter(axisDist != -1)             # nonsens observations


  
# also make it spatial for plotting
annual_obs_filter_sp <- sp_pnt_ee(annual_obs_filter$x,
                                     annual_obs_filter$y,  
                               'filtered', "red")

# addComposite + annual_mudbankPos + annual_outlierPos + annual_obs_filter_sp


# get median position for each year
# only for pos which are considered to have a mudbank
# also exclude outliers and nonsense observations
# how to avoid using the filtered collection ==> you'd want to keep the dataframe intact
# but still calculate the median offshore only on the relevant observations

# testMedianPos <- annual_obs_filter %>% # how to use the not filtered here ==> use replace/mutate to override anything that is an outlier with the most occuring value in the group (year/pos)??
#   group_by(year_col, pos, noMudbank) %>%
#   dplyr::mutate(
#     medianOffshore = case_when(
#       axisDist > 0 & mudbank_outlier == 0 & noMudbank == 0 ~ median(axisDist, na.rm=T)
#                                            )) %>%
#   dplyr::mutate(
#     percentile = case_when(
#       axisDist > 0 & mudbank_outlier == 0 & noMudbank == 0 ~ round(quantile(axisDist,c(0.70), na.rm=T))[1]
#     )) %>%
# 
#   dplyr::mutate(
#     distX = destPoint(SpatialPoints(data.frame(x = originX, y = originY),
#                                     CRS("+proj=longlat +datum=WGS84")), 
#                       median(bearing, na.rm = T), median(medianOffshore, na.rm = T))[,1],
#     distY = destPoint(SpatialPoints(data.frame(x = originX, y = originY),
#                                     CRS("+proj=longlat +datum=WGS84")), 
#                       median(bearing, na.rm = T), median(medianOffshore, na.rm = T))[,2]) %>%
#     ungroup()

for(y in 1:length(all_years)){
  # y <- 4
  # selected_year <- '2008-01-01'
  selected_year <- all_years[y]
  
  
  for (p in 1:length(group_pos)){
    # p = 2#113
    position = group_pos[p]
    # position = 32000
    
    subsets <- subset(annual_obs2, year_col == selected_year &
                        pos == position &
                        axisDist > 0 & # nonsens observations (or use mudbank extent?)
                        mudbank_outlier == 0 & # no outlier
                        noMudbank == 0) # positions that are indicated as mudbank
    
    # plot(subsets$pos, subsets$axisDist)
    # points(subsets$pos, subsets$mudbank_extent, col = 'blue')
    
    if (nrow(subsets) > 1){
      
      # get the distance;
      meanOffshore <- mean(subsets$axisDist, na.rm = T)
      # abline(h = meanOffshore, lty = 2)
      
      # modalCoast <- modal(subsets$axisDist, na.rm = T)
      # abline(h = modalCoast, lty = 3)
      
      medianOffshore <- median(subsets$axisDist, na.rm=T) #
      # abline(h = medianOffshore)
      
      bearing <- median(subsets$bearing)
      originX <- median(subsets$originX)
      originY <- median(subsets$originY)
      
      # set origin at correct location or add coastline dist to dist of interest?
      destPoint <- destPoint(SpatialPoints(data.frame(x = originX, y = originY),
                                           CRS("+proj=longlat +datum=WGS84")),
                             bearing, medianOffshore)
      
      # in the original file all rows recieve the distX
      rowNumbers <- which(annual_obs2$year_col == selected_year &
              annual_obs2$pos == position )
      
      
      annual_obs2$distX[rowNumbers] <- destPoint[1]
      annual_obs2$distY[rowNumbers] <- destPoint[2]
      annual_obs2$medianOffshore[rowNumbers] <-  medianOffshore
      print(paste0(position, ' with median: ', medianOffshore))
      
      
    }
    
    
    
  }
}
# calculated distance to spatial for plotting
meanPos <- SpatialPoints(data.frame(x = annual_obs2$distX[complete.cases(annual_obs2$distX)],
                                    y =  annual_obs2$distY[complete.cases(annual_obs2$distY)]),
                         CRS("+proj=longlat +datum=WGS84"))

meanPos_sp <- sp_pnt_ee(meanPos$x,meanPos$y, 'meanPos', "yellow")

addComposite + annual_mudbankPos + annual_outlierPos + annual_obs_filter_sp + meanPos_sp


####
#'
#'
#'
#####
mudbankPressence <- ggplot(annual_obs2,  aes(x = pos,y = as.Date(year_col), 
                                             fill=as.factor(noMudbank))) +
  geom_tile(na.rm = TRUE) + # color= "white",size=0.1,
  scale_fill_manual(breaks = c("0", "1"),
                    values= c("#ef6548", "#41b6c4"),
                    labels = c('mudbank', 'no mudbank')) +

  scale_x_reverse(lim=c(max(annual_obs2$pos)+4000, 
                        min(annual_obs2$pos)-4000), expand = c(0,0)) +
  
  # ggtitle( paste0(unique(subsetSelectedDates$DATE_ACQUIRED))) +
  labs(x = "Position", y = "date", fill = '') +
  theme(axis.line.x = element_line(size = 0.5, colour = "black"),
        axis.line.y = element_line(size = 0.5, colour = "black"),
        axis.line = element_line(size= 1, colour = "black"),
        # axis.title.y = element_text(size = 14, face = 'bold'),
        # axis.title.x = element_text(size = 14, face = 'bold'),
        axis.text.x = element_text(size = 12,  hjust = .5, vjust = .5),
        axis.text.y = element_text(size = 12, hjust = .5, vjust = .5),
        # strip.text.x = element_blank(), # remove panel labels
        legend.title = element_blank(),
        legend.key = element_rect(fill = NA),
        legend.text = element_text(size = 15),
        # legend.position = c(.78, .5),
        panel.grid.major = element_blank(), # remove grid lines
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.background = element_rect(fill = '#d9d9d9'),
        strip.text.x = element_text(size = 14, face = 'bold'))
# mudbankPressence


# for now make it a data.frame (required for assigning data)
# annual_obs_filter <- as.data.frame(annual_obs_filter)


#'
#'   or from a spatial perspective
#'

# highest density
# apply only to valid points.
# test if it helps to do this per transect.
# as alternative to using the median position
selection_density <- pointdensity(df = annual_obs_filter, lat_col = "x", 
                                  lon_col = "y", date_col = NULL, 
                                  grid_size = 0.1, radius = 1)

countRange <- round(quantile(selection_density$count,c(0.01, 0.99), na.rm=T))

# now it is the trick to find the max count for the selected transect
Tempcount <- merge(annual_obs_filter, selection_density, by.x=c('x', 'y'), 
                   by.y=c('lat', 'lon'))

# find max count for each position
# e.g. the location where the density is largest
density <- unique(Tempcount) %>% 
  dplyr::group_by(pos) %>%
  dplyr::filter(count > countRange[1]) %>% # not yet  very succesfull
  top_n(1, count) %>%
  ungroup()  %>%
  dplyr::select(!c(CLOUD_COVER, SmoothedPeak, SmoothedPeakFract,areaName, coastDist,
             coastX, coastY, ndwi_threshold, offsetLast, originX, originY,
             bearing, geometry, quarterly_col, date_col, five_year_col,
             year_col, coast_outlier, locf, deltaCoast,dateavg))

plot(density$pos, density$mudbankObs)

# testUniqueCount <- density %>% 
#   dplyr::group_by(pos) %>%
#   dplyr::summarise(value = max(count)) %>%
#   ungroup()
# 
# hist(testUniqueCount$value)
# 
# isItMedian <-  median(testUniqueCount$value)
# isItMean <- mean(testUniqueCount$value)
# isItMode <- Mode(testUniqueCount$value)
# isItSD <- sd(testUniqueCount$value)
# testFilter <- subset(density, count > isItMedian)

# toevoegen; per pos maar 1x meenemen
# filter out positions with a low count (compared to the observations?)

# make it spatial
maxCount <- SpatialPoints(data.frame(x = density$x,y = density$y),
                                  CRS("+proj=longlat +datum=WGS84"))

maxCount_spatial <- sp_pnt_ee(maxCount$x,maxCount$y,
                              'density', "orange")
addComposite + meanPos_sp + maxCount_spatial


# alongshore variable coastline change as a result of mud fraction
ggplot(annual_obs, aes(x = pos, y = deltaCoast, colour = SmoothedPeakFract)) +
  geom_point(size = 1, alpha =1) +
  scale_colour_gradient2(low = "#2166ac", high = "#b2182b", mid = '#fddbc7', 
                         limits = c(0.2, 0.6),
                         midpoint = 0.4, na.value = NA,
                         guide = guide_colourbar(direction = 'vertical')) +
  theme(axis.line.x = element_line(size = 0.5, colour = "black"),
        axis.line.y = element_line(size = 0.5, colour = "black"),
        axis.line = element_line(size= 1, colour = "black"),
        axis.text.x = element_text(size = 12,  hjust = .5, vjust = .5),
        axis.text.y = element_text(size = 12, hjust = .5, vjust = .5),
        legend.title = element_text(colour = 'black', size = 14, face = "bold"),
        legend.key = element_rect(fill = NA),
        legend.text = element_text(size = 10),
        panel.grid.major = element_blank(), # remove grid lines
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.background = element_rect(fill = '#d9d9d9'))


# ggplot idea: https://www.earthdatascience.org/tutorials/visualize-2d-point-density-ggmap/
pointDensity <- ggplot(subset(annual_obs2,   # mudbank_outlier == 0 &
                                x != -1), 
                       aes(x = x, y = y, colour = SmoothedPeakFract)) +
  geom_point(size = 0.2, alpha = 0.2) +
  
  scale_colour_gradient2(low = "#2166ac",
                         high = "#b2182b",
                         mid = '#fddbc7', midpoint = 0.4,
                         na.value = NA,
                         guide = guide_colourbar(direction = 'horizontal')) +
  geom_point(data = subset(annual_obs2,  mudbank_outlier == 0 &
                           x != -1 & noMudbank == 0),
             aes(x = distX, y = distY), colour = 'black', size = 0.8, alpha = 1) + 

  coord_equal() +
  labs(x = "Longitude", y = "Latitude", col = 'Fraction') +
  theme(axis.line.x = element_line(size = 0.5, colour = "black"),
        axis.line.y = element_line(size = 0.5, colour = "black"),
        axis.line = element_line(size= 1, colour = "black"),
        axis.text.x = element_text(size = 12,  hjust = .5, vjust = .5),
        axis.text.y = element_text(size = 12, hjust = .5, vjust = .5),
        legend.title = element_text(colour = 'black', size = 14, face = "bold"),
        legend.key = element_rect(fill = NA),
        legend.text = element_text(size = 10),
        legend.position = c(.5, -.9),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.background = element_rect(fill = '#d9d9d9'),
        strip.text.x = element_text(size = 14, face = 'bold'))
pointDensity


annual_obs_sp <- sp_pnt_ee(annual_obs2$x[!is.na(annual_obs2$x) & annual_obs2$x != -1],
                           annual_obs2$y[!is.na(annual_obs2$x) & annual_obs2$x != -1],  
                           'axisDist',
                           "orange")

# annual_obs_mean_sp <- sp_pnt_ee(annual_obs2$x[!is.na(annual_obs2$x) & annual_obs2$meanMud],
#                                 annual_obs2$y[!is.na(annual_obs2$x)],  
#                                 'high mean mud',
#                                 "blue")

# annual_obs_sp + annual_obs_mean_sp



#'
#' Test Mas observations versus fractions
#' 
#' 

mas_folder <- 'D:/BackUp_D_mangroMud_202001/Research/External_Data/Spatial/Kustmetingen_MAS/surveys'
# relevant files: 
# 4-6-2002 (near Suriname river)
# 31-12-2008 (entire coast of Suriname)

# mas_folder2 <- 'D:/BackUp_D_mangroMud_202001/Research/External_Data/Source/KustmetingMAS_source/shp'
mas_folder2 <- 'D:/WOTRO/Research/External_Data/Source/KustmetingMAS_source/shp'
# relevant file: 2016_west 
# to lesser extent 2014_oost

folderSelect <- as.matrix(list.files(paste0(mas_folder2), full.names = T))
df <- rewrite(folderSelect);
df <- df[grep('.shp$', folderSelect, ignore.case = T),]



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
allMasPoints <- shapefile(paste0(df[1,]))

allMasPointsWGS84 <- spTransform(allMasPoints, CRS = CRS("+proj=longlat +datum=WGS84"))
coordinatesMAS <- coordinates(allMasPointsWGS84)
coordinatesMAS_sp <- sp_pnt_ee(coordinatesMAS[,1], coordinatesMAS[,2],
                              'masPoints', "#e5f5e0")

# maxCount <- SpatialPoints(data.frame(x = testFilter$x,y = testFilter$y),
#                           CRS("+proj=longlat +datum=WGS84"))
# 
# maxCount_spatial <- sp_pnt_ee(maxCount$x,maxCount$y,
#                               'density', "yellow")

addComposite + coordinatesMAS_sp + annual_obs_sp + 
  annual_outlierPos + annual_obs_filter_sp + meanPos_sp


# library(geosphere)
library(rgeos)
# library(spatialEco)
library(sp)

# get nearest observation for 2005
dist_with_markers <- gDistance(maxCount_spatial[50],allMasPoints, byid=T)
nearest <- apply(dist_with_markers,2, which.min)
nearest_dist <- apply(dist_with_markers, 2, min) 


mapView(allMasPoints[nearest,])+mapView(maxCount_spatial[50])


# testMedianPos <- annual_obs2 %>% #annual_obs_filter %>% 
#   # how to use the not filtered here ==> use replace/mutate to override anything 
#   # that is an outlier with the most occuring value in the group (year/pos)??
#   
#   # pos == 82000 (path/row combination result in offshore boundary mainly in one) affects the median position computation 
#   group_by(year_col, pos, noMudbank, mudbank_outlier) %>%
#   dplyr::mutate(medianOffshore = median(axisDist, na.rm=T)) %>%
#   ungroup() %>%
#   
#   dplyr::mutate(
#     medianOffshore = case_when(
#       axisDist > 0 & mudbank_outlier == 0 & noMudbank == 0 ~ axisDist
#     )) %>%
#   dplyr::group_by(year_col, pos) %>%
#   dplyr::mutate(medianOffshore = if_else(is.na(medianOffshore),
#                                          Mode(medianOffshore), medianOffshore)) %>%
#   dplyr::mutate(medianOffshore = if_else(is.na(medianOffshore) & axisDist > 0,
#                                          median(medianOffshore), medianOffshore)) %>%
#   
#   
#   ###############
# # dplyr::mutate(
# #   medianOffshore = case_when(
# #     axisDist > 0 & mudbank_outlier == 0 & noMudbank == 0 ~ median(axisDist, na.rm=T)
# #   )) %>%
# # dplyr::mutate(
# #   percentile = case_when(
# #     axisDist > 0 & mudbank_outlier == 0 & noMudbank == 0 ~ round(quantile(axisDist,c(0.70), na.rm=T))[1]
# #   )) %>%
# # dplyr::group_by(year_col, pos) %>%
# # dplyr::mutate(medianOffshore = if_else(is.na(medianOffshore), 
# #                                         Mode(medianOffshore), medianOffshore)) %>%
# 
# dplyr::mutate(
#   distX = destPoint(SpatialPoints(data.frame(x = originX, y = originY),
#                                   CRS("+proj=longlat +datum=WGS84")), 
#                     median(bearing, na.rm = T), median(medianOffshore, na.rm = T))[,1],
#   distY = destPoint(SpatialPoints(data.frame(x = originX, y = originY),
#                                   CRS("+proj=longlat +datum=WGS84")), 
#                     median(bearing, na.rm = T), median(medianOffshore, na.rm = T))[,2]) %>%
#   
#   ungroup()

# functionForm <- function(y) { #
#   # get ranges where NO mudbank occurs
#   # requiredOutput <- 1
#   # y <- testMedianPos$pos
#   yOrdered <- sort(y) # all positions that  contain a mudbank
#   positionDifference <- c(1000, diff(unique(yOrdered)))
#   
#   # index of positions where difference is > 1000
#   idx <- unlist(lapply(positionDifference, function(x){sum(1001<=x)}))
#   
#   seqLengths <- positionDifference[which(idx == 1)]#[which(positionDifference > 1001)]
#   ends <- unique(yOrdered)[which(idx == 1)]-1000 # positions of no mudbank POS (missing pos) (non inclusive. subtract 1000)
#   starts <- ends - (seqLengths-2000)
#   
#   posOfInt <- unlist(Map('seq',starts, ends, 1000))
#   posOfInt<- posOfInt[!(posOfInt %in% unique(yOrdered))]
#   
#   # 
#   # if(requiredOutput == 0){
#   # 
#   #   output <- ends
#   # }
#   # if(requiredOutput == 1){
#   # 
#   #   output <- starts
#   # }
#   return(posOfInt)
# # }
