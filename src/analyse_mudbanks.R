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

reference_date <- as.Date(c("2009-09-12")) #as.Date("2003-11-15") 
subsetSelectedDates <- subset(allFiles, as.Date(DATE_ACQUIRED) %in% reference_date)
facet <- 'DATE_ACQUIRED'

alongshoreFracts <- ggplot(subsetSelectedDates, aes(x= pos, y = SmoothedPeakFract,  #meanMud
                               colour = as.factor(mudbank_outlier))) + 
  
  geom_point(size = 6) + # ,
  facet_wrap(paste0('~', facet), ncol = 1) + 
  scale_color_manual(labels = c("Mudbank","No mudbank"), values = c("#8c510a", "#35978f")) +
  scale_x_reverse(lim=c(max(subsetSelectedDates$pos)+4000, 
                        min(subsetSelectedDates$pos)-4000), expand = c(0,0)) +
  scale_y_continuous(lim=c(0,1)) +
  labs(x = "Position", y = "Fractions", col = ' ') +
  theme(axis.line.x = element_line(size = 0.5, colour = "black"),
        axis.line.y = element_line(size = 0.5, colour = "black"),
        axis.line = element_line(size= 1, colour = "black"),
        axis.title.y = element_text(size = 30, face = 'bold'),
        axis.title.x = element_text(size = 30, face = 'bold'),
        axis.text.x = element_text(size = 18,  hjust = .5, vjust = .5),
        axis.text.y = element_text(size = 18, hjust = .5, vjust = .5),
        
        legend.key = element_rect(fill = NA),
        legend.text = element_text(size = 50),
        legend.position = c(.2, .8),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        strip.text.x = element_blank(),#element_text(size = 14, face = 'bold'),
        strip.background = element_rect(fill = NA, colour = NA)) # "#d9d9d9"

alongshoreFracts

# ggsave(filename = paste0("./results/methodology_figures/",'alongshore_fractions',
#                          '_',reference_date[1], '_',  format(Sys.Date(), "%Y%m%d"),'.jpeg'),
#        width = 20.1, height = 7.25, units = c('in'), dpi = 1200)


#'
#' select 1 year as example
#'  for plotting purposes
#'  

dateForTest <- as.Date("2016-01-01")

# build image collection around that year
filtCollectAnnual <- collection$filterDate(as.character(dateForTest), 
                                           as.character(dateForTest + 365))
dates <- ee_get_date_ic(filtCollectAnnual, time_end = FALSE)

# medianComp <- filtCollectAnnual$median()
composite <- ee$Algorithms$Landsat$simpleComposite(filtCollectAnnual)#$uint32
Map$centerObject(filtCollectAnnual$first(), 11)
addComposite <- Map$addLayer(composite, visParams, paste0('landsat: ',dateForTest))

# now all obs within the selected year
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

# make it spatial.  
annual_mudbankPos <- sp_pnt_ee(annual_obs_nonOutlier$x,
                               annual_obs_nonOutlier$y,  
                               'non outlier abs', "#ece7f2")

annual_outlierPos <- sp_pnt_ee(annual_obs_outlier$x,
                               annual_obs_outlier$y,  'outlier',
                               "#fee0d2")

# calculated distance to spatial for plotting
meanPos <- SpatialPoints(data.frame(x = annual_obs$distX[complete.cases(annual_obs$distX)],
                                    y =  annual_obs$distY[complete.cases(annual_obs$distY)]),
                         CRS("+proj=longlat +datum=WGS84"))
meanPos_sp <- sp_pnt_ee(meanPos$x,meanPos$y, 'meanPos', "yellow")


# for plotting purpose, filter them
annual_obs_filter <- annual_obs %>%
  filter(noMudbank == 0) %>%         # positions that are indicated as no mudbank
  filter(mudbank_outlier == 0) %>%   # outliers
  filter(axisDist != -1)             # nonsens observations


# also make it spatial for plotting
annual_obs_filter_sp <- sp_pnt_ee(annual_obs_filter$x,
                                  annual_obs_filter$y,  
                                  'filtered', "red")

# addComposite + annual_mudbankPos + annual_outlierPos + annual_obs_filter_sp + meanPos_sp
posOfInterest <- 50000
subsetPos <- subset(allFiles, (pos %in% posOfInterest))
coastDistRange <- round(quantile(subsetPos$coastDist,c(0.005, 0.99), na.rm=T))

# all years considered a mudbank
getYears <- unique(subsetPos[subsetPos$noMudbank == 0, 'year_col'])

sequences <- split(as.Date(getYears), cumsum(c(0, diff(as.Date(getYears)) == 365)));

# # drop sublist with only x amount of consequetive mudbank observations
# filtSequences <- Filter(function(x){length(x)>3}, sequences)
# 
# startPos <- vapply(filtSequences, head, n = 1L, FUN.VALUE = numeric(1))
# endPos <- vapply(filtSequences, tail, n = 1L, FUN.VALUE = numeric(1))
# 
# dateFrames <- data.frame(id = posOfInterest, fill = NA,
#                          xmin = as.Date(getYears$year_col), 
#                          xmax = as.Date(getYears$year_col) + 365, 
#                          ymin = coastDistRange[1], 
#                          ymax = coastDistRange[2])

####
#'
#' overview plots
#'
#####

# remove duplicate observations to have accurate stats
allFiles3 <- allFiles %>% 
  group_by(as.Date(DATE_ACQUIRED), pos) %>% 
  filter(row_number(pos) == 1) %>%
  ungroup()

# get only 1 observation per year per pos here
transformed <- allFiles3 %>% 
  dplyr::group_by(year_col, pos) %>% 
  dplyr::distinct(deltaCoast, .keep_all = T) %>%
  # filter(n()>1) %>% 
  ungroup() %>%
  dplyr::select(DATE_ACQUIRED, pos, year_col, deltaCoast, noMudbank, 
                mudbank_outlier)


# change values only??
coastalChange <- subset(transformed, deltaCoast != 0 ) # & mudbank_outlier == 0 
# transformed
medianMudbank <- median(coastalChange$deltaCoast[which(coastalChange$noMudbank == 0)], 
                        na.rm = T)
medianNoMudbank <- median(coastalChange$deltaCoast[which(coastalChange$noMudbank == 1)], 
                          na.rm = T)

meanMudbank <- mean(coastalChange$deltaCoast[which(coastalChange$noMudbank == 0)], na.rm = T)
meanNoMudbank <- mean(coastalChange$deltaCoast[which(coastalChange$noMudbank == 1)], na.rm = T)

# denisty plot  
density_mudbank <- 
  # ggplot(data = subset(allFiles3, mudbank_outlier == 0 & 
  #                                         (deltaCoast < -30 | deltaCoast > 30))) +  
  ggplot(data = coastalChange) +
  # geom_density(adjust = 1.5, alpha = 0.5,
  #              aes(x=deltaCoast, fill=as.factor(noMudbank))) +
  
  geom_histogram(position = 'identity', aes(x=deltaCoast,
                                            fill=as.factor(noMudbank)),
                 binwidth = 20, alpha = 0.6) +
  scale_x_continuous(limits=c(-750, 750)) +
  scale_y_continuous(expand = c(0,0)) +
  guides(fill=guide_legend(override.aes=list(alpha=1))) +
  # 0 = mudbank, 1 = no mudbank
  
  annotate("text", label = paste0('Mean: ', round(meanMudbank)),
             x = 400,
             y = 2000,
             size = 12, colour = "black") +
  annotate("text", label = paste0('Mean: ', round(meanNoMudbank)),
             x = -400,
             y = 2000,
             size = 12, colour = "black") +
  
  scale_fill_manual(labels = c('Mudbank', 'No mudbank'), values = c('#F8766D', '#00BFC4')) +
  theme(
    axis.line.x = element_line(size = 0.5, colour = "black"),
    axis.line.y = element_line(size = 0.5, colour = "black"),
    axis.line = element_line(size=1, colour = "black"),
    axis.text.x = element_blank(),
    axis.text.y = element_text(color = "grey20", size = 18, hjust = .5, vjust = .5, face = "bold"),
    axis.title.y = element_text(size = 20, face = 'bold'),
    axis.title.x = element_blank(),
    
    legend.background = element_rect(fill = '#d9d9d9',  colour = '#d9d9d9'),
    legend.key = element_rect(fill = NA),
    legend.text = element_text(size = 25),#element_blank(),
    legend.title =  element_blank(),
    legend.position = c(.8, .4),
    
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.spacing.x = unit(2, 'lines'),
    strip.background = element_rect(fill = "#d9d9d9", colour = "#d9d9d9"),
    # strip.text.x = element_text(size = 12), # Facet titles
    plot.background = element_rect(fill = '#d9d9d9',  colour = '#d9d9d9'))

# density_mudbank
# legendDensity <- get_legend(density_mudbank)
# density_mudbank <- density_mudbank + theme(legend.position = 'none')

boxplot <- 
  # ggplot(data = subset(allFiles3, mudbank_outlier == 0 &
  #                                (deltaCoast < -30 | deltaCoast > 30))) +

  ggplot(data = coastalChange) +
  geom_boxplot(aes(x=deltaCoast, y = noMudbank, fill = as.factor(noMudbank))) +
  scale_fill_manual(labels = c('Mudbank', 'No mudbank'), 
                    values = c('#F8766D', '#00BFC4')) +
  scale_x_continuous(limits=c(-750, 750), name = 'Coastline change [m/yr]') +
  theme(
    axis.line.x = element_line(size = 0.5, colour = "black"),
    axis.line.y = element_line(size = 0.5, colour = "black"),
    axis.line = element_line(size=1, colour = "black"),
    axis.text.x = element_text(color = "grey20", size = 18, hjust = .5, vjust = .5, face = "bold"),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.x = element_text(size = 20, face = 'bold'),

    legend.position = 'none',#c(.8, .5),
    
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.spacing.x = unit(2, 'lines'),
    strip.background = element_rect(fill = "#d9d9d9", colour = "#d9d9d9"),
    
    plot.background = element_rect(fill = '#d9d9d9',  colour = '#d9d9d9'))
  
# boxplot

final <- plot_grid(density_mudbank, boxplot, align = 'v', ncol = 1, nrow =2,
                   rel_heights = c(2.5, 1.5))

# ggsave(final, filename = paste0("./results/temp_maps/", 'Suriname_coastlineChange_histogram_',
#                                 '_',  format(Sys.Date(), "%Y%m%d"),'.jpeg'),
#       width = 13.1, height = 7.25, units = c('in'), dpi = 1200)

mudbankObs <- subset(coastalChange, noMudbank == 0)
NomudbankObs <- subset(coastalChange, noMudbank == 1)

# boxplot(coastalChange$deltaCoast ~ coastalChange$noMudbank, ylim = c(-400,400))

qqplot <- ggplot(NomudbankObs, aes(sample = deltaCoast)) + 
  stat_qq() +
  scale_color_manual(values = c("#00AFBB", "#E7B800"))+
  scale_y_continuous(limits=c(-3000, 3000), name = 'Coastline change [m/yr]')
  # labs(y = "coastline change [m/yr]")

qqplot

sampleDelta <- sample(allFiles3$deltaCoast, 2500)
shapiroTest<- shapiro.test(sampleDelta) # P<0.05 ==> not normally distributed

# thus apply wilcox test (non-parametric test)
# http://www.sthda.com/english/wiki/unpaired-two-samples-wilcoxon-test-in-r
wilcoxTest <- wilcox.test(coastalChange$deltaCoast ~ coastalChange$noMudbank, 
            alternative = "two.sided", mu = 0, conf.int=T, paired = F, exact = F,
            correct = T)
# P value < 0.05 thus the median is significantly different with a P ~ 0.00000 


#'
#'   from a spatial perspective
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

# make it spatial
maxCount <- SpatialPoints(data.frame(x = density$x,y = density$y),
                                  CRS("+proj=longlat +datum=WGS84"))

maxCount_spatial <- sp_pnt_ee(maxCount$x,maxCount$y,
                              'density', "orange")
# addComposite + meanPos_sp + maxCount_spatial

# ggplot idea: https://www.earthdatascience.org/tutorials/visualize-2d-point-density-ggmap/
pointDensity <- ggplot(subset(annual_obs,   # mudbank_outlier == 0 &
                                x != -1), 
                       aes(x = x, y = y, colour = SmoothedPeakFract)) +
  geom_point(size = 1.6, alpha = 0.3) +
  
  scale_colour_gradient2(low = "#2166ac",
                         high = "#b2182b",
                         mid = '#fddbc7', midpoint = 0.4,
                         na.value = NA,
                         guide = guide_colourbar(direction = 'horizontal', order = 1)) +
  geom_point(data = subset(annual_obs,  mudbank_outlier == 0 &
                           x != -1 & noMudbank == 0),
             aes(x = distX, y = distY, fill = 'black'), colour = 'black', 
             size = 4, alpha = 1) + 
  scale_fill_manual(name = ' ', values = c('black'= 'black'), 
                    labels = c('mudbank'), 
                    guide = guide_legend(order =2, label.position = 'left',
                                         label.theme = element_text(size = 40))) +
  coord_equal(xlim = c(-57,-54), ylim = c(5.8, 6.2), ratio = 1.5) +
  labs(x = "Longitude", y = "Latitude", col = 'Fraction', 
       title = paste0(year(dateForTest))) +
  theme(axis.line.x = element_line(size = 0.5, colour = "black"),
        axis.line.y = element_line(size = 0.5, colour = "black"),
        axis.line = element_line(size= 1, colour = "black"),
        axis.text.x = element_text(size = 18,  hjust = .5, vjust = .5),
        axis.text.y = element_text(size = 18, hjust = .5, vjust = .5),
        axis.title.x = element_text(size = 40, face = "bold"), 
        axis.title.y = element_text(size = 40, face = "bold"), 
        legend.title = element_text(colour = 'black', size = 40),
        legend.key = element_rect(fill = NA),
        legend.text = element_text(size = 18),
        legend.position = c(.5, -.4),
        legend.direction = "horizontal",
        legend.box = "horizontal",
        plot.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.background = element_rect(fill = NA), # '#d9d9d9'
        strip.text.x = element_blank())
pointDensity


# ggsave(filename = paste0("./results/methodology_figures/",'annual_fractions',
#                          '_',dateForTest[1], '_',  format(Sys.Date(), "%Y%m%d"),'.jpeg'),
#        width = 21.1, height = 7.25, units = c('in'), dpi = 1200)


#'
#' Test Mas observations versus fractions
#' 
#' 

mas_folder <- 'D:/BackUp_D_mangroMud_202001/Research/External_Data/Spatial/Kustmetingen_MAS/surveys'
# relevant files: 
# 4-6-2002 (near Suriname river)
# 31-12-2008 (entire coast of Suriname)

# mas_folder2 <- 'D:/BackUp_D_mangroMud_202001/Research/External_Data/Source/KustmetingMAS_source/shp'
mas_folder2 <- 'D:/WOTRO/Research/External_Data/Spatial/Kustmetingen_MAS/surveys'
# D:\WOTRO\Research\External_Data\Spatial\Kustmetingen_MAS\surveys
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

# df <- df[grep('2008_', df, ignore.case = T),]
allMasPoints <- shapefile(paste0(df[24,])) # 23 == 2008 / 24 = 2016

allMasPoints <- spTransform(allMasPoints, CRS = CRS("+init=epsg:32621"))
coordinatesMasUtm <- coordinates(allMasPoints)

allMasPointsWGS84 <- spTransform(allMasPoints, CRS = CRS("+proj=longlat +datum=WGS84"))
coordinatesMAS <- coordinates(allMasPointsWGS84)
coordinatesMAS_sp <- sp_pnt_ee(coordinatesMAS[,1], coordinatesMAS[,2],
                              'masPoints', "#e5f5e0")

addComposite + coordinatesMAS_sp + annual_mudbankPos +
  annual_outlierPos + annual_obs_filter_sp + meanPos_sp


hist(allMasPoints$z)


# library(geosphere)
library(rgeos)
# library(spatialEco)
# library(sp)

# get nearest observation for 2005
annual_obs_nonOutlier_sp <- SpatialPoints(data.frame(x = annual_obs_nonOutlier$x,
                                                     y = annual_obs_nonOutlier$y),
                                        CRS("+proj=longlat +datum=WGS84"))
# do in UTM 21 to get distance in meters
annual_obs_nonOutlier_sp_utm <- spTransform(annual_obs_nonOutlier_sp, CRS = CRS("+init=epsg:32621"))

pnts <- data.frame(DATE_ACQUIRED = as.Date(character()),
                   pos = character(),dropClass = character(),
                   axisDist = double(),mudFract = double(),
                   x = double(), y = double(),
                   xMas = double(), yMax = double(),
                   zMax = double(), distance = double())
# for all relevant points 
for(pnt in 1:length(annual_obs_nonOutlier_sp_utm)){
  # pnt <- 1
  # print(pnt)
  
  dist_with_markers <- gDistance(annual_obs_nonOutlier_sp_utm[pnt], allMasPoints, byid=T)
  nearest <- apply(dist_with_markers,2, which.min)
  nearest_dist <- apply(dist_with_markers, 2, min) 
  
  
  # get the details of nearest points in a data.frame together
  pnts<- rbind(pnts, data.frame(DATE_ACQUIRED = annual_obs_nonOutlier$DATE_ACQUIRED[pnt],
             pos = annual_obs_nonOutlier$pos[pnt],
             dropClass = annual_obs_nonOutlier$dropClass[pnt],
             axisDist = annual_obs_nonOutlier$axisDist[pnt],
             mudFract = annual_obs_nonOutlier$mudFract[pnt],
             x = annual_obs_nonOutlier$x[pnt],
             y = annual_obs_nonOutlier$y[pnt],
             xMas = coordinatesMAS[nearest,1],# coordinates remain in WGS84 for consistency
             yMax = coordinatesMAS[nearest,2],
             zMax = coordinatesMAS[nearest,3],
             distance = nearest_dist))
}

filtered <- annual_obs %>% distinct(distX, distY, .keep_all = TRUE)

annual_obs_meanPos_sp <- SpatialPoints(data.frame(x = filtered$distX[complete.cases(filtered$distX)],
                                                  y = filtered$distY[complete.cases(filtered$distX)]),
                                       CRS("+proj=longlat +datum=WGS84"))
annual_obs_meanPos_sp_utm <- spTransform(annual_obs_meanPos_sp, CRS = CRS("+init=epsg:32621"))

pnts_boundary <- data.frame(DATE_ACQUIRED = as.Date(character()),
                   pos = character(),dropClass = character(),
                   axisDist = double(),mudFract = double(),
                   x = double(), y = double(),
                   xMas = double(), yMax = double(),
                   zMax = double(), distance = double())


for(pnt in 1:length(annual_obs_meanPos_sp_utm)){
  # pnt <- 1
  # print(pnt)
  
  dist_with_markers <- gDistance(annual_obs_meanPos_sp_utm[pnt], allMasPoints, byid=T)
  nearest <- apply(dist_with_markers,2, which.min)
  nearest_dist <- apply(dist_with_markers, 2, min) 
  
  
  # get the details of nearest points in a data.frame together
  pnts_boundary<- rbind(pnts_boundary, data.frame(DATE_ACQUIRED = filtered$DATE_ACQUIRED[pnt],
                                pos = filtered$pos[pnt],
                                dropClass = filtered$dropClass[pnt],
                                axisDist = filtered$axisDist[pnt],
                                mudFract = filtered$mudFract[pnt],
                                x = filtered$distX[pnt],
                                y = filtered$distY[pnt],
                                xMas = coordinatesMAS[nearest,1],# coordinates remain in WGS84 for consistency
                                yMax = coordinatesMAS[nearest,2],
                                zMax = coordinatesMAS[nearest,3],
                                distance = nearest_dist))
}

# limit allowed distance
pnts_limitDist <- subset(pnts, distance<1000)
pnts_boundary_limitDist <- subset(pnts_boundary, distance<2000 & mudFract != -1)
plot(pnts_limitDist$zMax, pnts_limitDist$mudFract,
     main = paste0('2008'), xlab = paste0('depth'), ylab = 'fraction')           
points(pnts_boundary$zMax, pnts_boundary$mudFract, 
       pch = 16, col = 'red')           


mapView(allMasPoints[nearest,]) + mapView(annual_obs_nonOutlier_sp[5000])

