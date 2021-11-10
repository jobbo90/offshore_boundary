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
# Sys.setenv("RETICULATE_PYTHON" = "C:/Users/5600944/AppData/Local/r-miniconda/envs/rgee/python.exe")
ee_Initialize()
## ---------------------------
source("./src/functions.R")


## ---------------------------

# mapviewOptions(basemaps = c( "Esri.WorldImagery","Esri.WorldShadedRelief", "OpenStreetMap.DE"))
dataFolder <- './data/processed'

mapviewOptions(basemaps = c( "Esri.WorldImagery","Esri.WorldShadedRelief", "OpenStreetMap.DE"))
leaflet() %>%
  addProviderTiles("Esri.WorldImagery")

years <- seq(from = 1985, to = 2020, by = 1)

# near river mouths estimates for coastlines in old version of GEE script are 
# questionable, should partially be solved in newest versions (11-2-2021)
posToExclude <- c(seq(138000,147000,1000),
                  seq(241000, 255000, 1000))  

reference_date <- as.Date("1986-01-01")
aoi <- c('Suriname') # FrenchGuiana

# select folders
folderSelect <- as.matrix(list.files(paste0(dataFolder, '/coastlines'), full.names = T))
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
filtered <- unique(filtered)

# bind_rows!!!
allFiles <- do.call(bind_rows, lapply(as.matrix(filtered)[,1], function(x) read.csv(x, stringsAsFactors = FALSE,
                                                                                sep = ',', na.strings=c("","NA")
                                                                                )))

# where are the duplicates comming from when loading? 
allFiles3 <- allFiles %>% 
  group_by_at(vars(DATE_ACQUIRED, pos)) %>% 
  filter(n()>1) %>% 
  ungroup()

col_dates <- col_of_interest(allFiles, 'DATE_ACQUIRED$')
col_coastDist <- col_of_interest(allFiles, 'coastDist$')

# all unique dates
uniqueDates <- unique(allFiles[,col_dates])

# all unique transect (id's)
allPos <- unique(allFiles[, col_of_interest(allFiles, 'pos$')]);
uniqueX<- unique(allFiles[, col_of_interest(allFiles, 'originX$')])
uniqueY<- unique(allFiles[, col_of_interest(allFiles, 'originY$')])
geo<- unique(allFiles[, col_of_interest(allFiles, '.geo')])

# keep_columns <- c('axisDist', 'mudFract', 'endDrop')  # necessary for mudbank output
coastlines <- st_as_sf(SpatialPointsDataFrame(data.frame(
  allFiles$coastX, allFiles$coastY),
  proj4string=CRS("+proj=longlat +datum=WGS84"),
  data = data.frame(allFiles)))

# get transects

# transects <- build_csvLines(allFiles)

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
collectionL8 <- ee$ImageCollection("LANDSAT/LC08/C01/T1_TOA")

collectionL5 <- ee$ImageCollection("LANDSAT/LT05/C01/T1_TOA")

collectionL7 <- ee$ImageCollection("LANDSAT/LE07/C01/T1_TOA")

collection <- collectionL8$merge(collectionL5)$#merge(collectionL7)$
  filter(ee$Filter$lt("CLOUD_COVER", 30))
  
# ee_print(filtCollect)

filtCollect <- collection$filterDate(as.character(reference_date-1), as.character(reference_date+1))$
  filterBounds(ee$Geometry$Point(-55.54, 5.94))
dates <- ee_get_date_ic(filtCollect, time_end = FALSE)[,2]

# properties <- ee_print(image)
selectImage <- filtCollect$first()
id <- eedate_to_rdate(selectImage$get("system:time_start"))
first <- Map$addLayer(selectImage, visParams,  as.character(as.Date(id)))

Map$centerObject(selectImage, 14)

# coastline Ponts
coastlines_selection <-subset(coastlines, coastlines$DATE_ACQUIRED == as.character(as.Date(reference_date)) &
                                coastlines$coastDist >= 0)
# coastlines_selection <- subset(coastlines_selection, select = -c(geometry))
coastlines_selection <- coastlines_selection[ ,-which(names(coastlines_selection) == 'geometry')]

coastlines_selection_sp <- SpatialPointsDataFrame(data.frame(
  coastlines_selection$coastX, coastlines_selection$coastY),
  proj4string=CRS("+proj=longlat +datum=WGS84"),
  data = data.frame(coastlines_selection$DATE_ACQUIRED))
# mapview(coastlines_selection_sp)

shapefile(coastlines_selection_sp, filename = 
            paste0(wd,'/data/raw/shapes/coastline_', gsub('-', '', reference_date), '.shp'))
 
# writeOGR(obj=coastlines_selection_sp[,-()], dsn=paste0(wd,'/data/raw/shapes'), 
#          layer=paste0("coastline_", gsub('-', '', reference_date)), 
#          driver="ESRI Shapefile")

coastlines_selection_2 <-subset(coastlines, coastlines$DATE_ACQUIRED >= as.character(as.Date(min(dates))) &
                                  coastlines$DATE_ACQUIRED <= as.character(as.Date(max(dates))) &
                                coastlines$coastDist >= 0)

coast_spatial <- sp_pnt_ee(coastlines_selection$coastX,
                           coastlines_selection$coastY,  paste0(as.character(as.Date(id)), ' points'),
                           "#e34a33")

coast_spatial2 <- sp_pnt_ee(coastlines_selection_2$coastX,
                           coastlines_selection_2$coastY,  'obs within 90 days',
                           "#ece7f2")

# plot Map
# first  +coast_spatial2+ coast_spatial 
  
#---------------------------
#'
#' now multi temporal
#' 
#---------------------------

# drop POS near  river mouths
# 139000 - 147000 (suriname Rivier)
# 242000 -252000  (saramacca rivier / coppename)
allFiles_dropPOS<- subset(allFiles, !(pos %in% posToExclude))

group_dates<-unique(allFiles_dropPOS$year_col)
group_quart <- unique(allFiles_dropPOS$quarterly_col)
group_pos <- unique(allFiles_dropPOS$pos)


# # get for all transects an coastline observation near
## reference date as baseline
allFiles_dropPOS$baseline <- 0
# allFiles_dropPOS$slope <- -1
# 
allFiles_dropPOS$baseline2 <- 0
# allFiles_dropPOS$grp <- NA


# normalize for the coastline position around a reference date
for (sid in allPos) {
  # sid = 189000
  
  # get a reference distance 
  # e.g. observation closest to reference date OR
  # median observation over 1 year near the reference date 
  subsetAllObs <- subset(allFiles_dropPOS, allFiles_dropPOS$pos == sid &
                           allFiles_dropPOS$coastDist >= 0) 
  nonOutliersAll <- subset(subsetAllObs, coast_outlier == 1)

  # index of all relevant (>0) observations of that position
  idx <- which(allFiles_dropPOS$pos == sid &
                 allFiles_dropPOS$coastDist >= 0)
  
  # nearest observation (median and original distance)
  index <- which.min(abs(as.Date(nonOutliersAll$DATE_ACQUIRED)-reference_date))
  coastObs <- subsetAllObs[index, 'coastDist']
  coastObs2 <- subsetAllObs[index, 'coast_median']

  allFiles_dropPOS$baseline[idx] <- as.numeric(coastObs)
  allFiles_dropPOS$baseline2[idx] <- as.numeric(coastObs2) # median val

}

# mutate the dataframe 
# add date properties as seperate columns
allFiles_mutate <- allFiles_dropPOS %>% dplyr::mutate(year = year(DATE_ACQUIRED),
                                               month = month(DATE_ACQUIRED, label=TRUE),
                                               day = day(DATE_ACQUIRED),
                                               full_date= date(DATE_ACQUIRED),
                                               years = date(quarterly_col))

# subtract baseline value from original
# Baseline is either a observation close to reference date. (baseline)
# Or the annual median observation of they reference date (baseline2)
allFiles_mutate$normalized <- allFiles_mutate$coastDist - allFiles_mutate$baseline
allFiles_mutate$normalized2 <- allFiles_mutate$coastDist - allFiles_mutate$baseline2

#################################
#' 
#' test simple 2d plot 
#' 
#################################
twoD_pos <- 230000#299000
subset2d_for_testPlot <- subset(allFiles_mutate, pos == twoD_pos)


# plot temporal evolution for given transect
# now in ggplot form
subset2d_for_testPlot$DATE_ACQUIRED <- as.Date(subset2d_for_testPlot$DATE_ACQUIRED)
subset2d_for_testPlot$year_col <- as.Date(subset2d_for_testPlot$year_col)
# subset2d_for_testPlot$coast_median <- as.numeric(levels(subset2d_for_testPlot$coast_median))[subset2d_for_testPlot$coast_median]


ggplot(subset2d_for_testPlot, aes(x= DATE_ACQUIRED, y = coastDist)) + # color=coast_outlier)
  # geom_line(aes(y=coast_median),size=2, linetype= "dotted") + # add median dtrendline?
  geom_line(inherit.aes = FALSE, aes(x = DATE_ACQUIRED, y = coast_median),
                                     # linetype = as.factor(pos)),
            alpha = 0.5, size = 1.2) +
  
  geom_point(size = 8, aes(colour = as.factor(coast_outlier)), alpha = 0.6) +
  
  scale_color_manual(name = "Legend",
    values = c('red', 'blue'),
    labels = c("outlier", "distance")) +
  scale_x_date(labels = date_format("%Y")) +
  ggtitle( paste0('position: ', twoD_pos)) +
  guides(color=guide_legend(override.aes=list(fill='#d9d9d9'), ncol = 1)) +
  labs(x = "Year", y = "coastline position [m]") +
  theme(axis.line.x = element_line(size = 0.5, colour = "black"),
        axis.line.y = element_line(size = 0.5, colour = "black"),
        axis.line = element_line(size= 1, colour = "black"),
        axis.title.y = element_text(size = 70, face = 'bold'),
        axis.title.x = element_text(size = 70, face = 'bold'),
        axis.text.x = element_text(size = 35,  hjust = .5, vjust = .5),
        axis.text.y = element_text(size = 35, hjust = .5, vjust = .5),
        
        legend.title = element_blank(),
        legend.background = element_rect(fill = alpha('grey50',0.8)),
        plot.title = element_text(hjust = 0.5, size = 50, face = 'bold',
                                  vjust = -2), 
        legend.text = element_text(size = 70),
        legend.position = c(.7, .2),

        panel.grid.major = element_blank(), # remove grid lines
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        plot.background = element_rect(fill = NA))

# ggsave(filename = paste0("./results/methodology_figures/twoD_distance_",
#                          '_pos',twoD_pos,'_',  format(Sys.Date(), "%Y%m%d"),'.jpeg'),
#        width = 20.1, height = 10.25, units = c('in'), dpi = 1200)


outliers <- sp_pnt_ee(subset(subset2d_for_testPlot, coast_outlier == 0)$coastX,
                      subset(subset2d_for_testPlot, coast_outlier == 0)$coastY,  'outliers',
                      "red")

nonOutliers <- sp_pnt_ee(subset(subset2d_for_testPlot, coast_outlier == 1)$coastX,
                         subset(subset2d_for_testPlot, coast_outlier == 1)$coastY,  'nonOutliers',
                         "blue")

# coordinatesAOI <- nonOutliers$x$setView[[1]]

aoiCollect <- collection$filterBounds(ee$Geometry$Point(
  median(subset(subset2d_for_testPlot, coast_outlier == 1 & coastX != -1)$coastX, na.rm = T),
  median(subset(subset2d_for_testPlot, coast_outlier == 1 & coastX != -1)$coastY, na.rm = T)))

imageAOI <- ee$Image(aoiCollect$sort("CLOUD_COVER")$first())   #

# properties <- ee_print(imageAOI)

idAOI <- eedate_to_rdate(imageAOI$get("system:time_start"))

firstAOI <- Map$addLayer(imageAOI, visParams,  as.character(as.Date(idAOI)))

# plot view
test2 <- firstAOI + nonOutliers + outliers
setView(test2, subset(subset2d_for_testPlot, coast_outlier == 1)$coastX[1], 
        subset(subset2d_for_testPlot, coast_outlier == 1)$coastY[1], 14, options = list())





#################################
#' 
#' Alongshore variability in coastline position
#' 
#################################

p <-ggplot(subset(allFiles_mutate, !is.na(deltaCoast) & !(pos %in% posToExclude)),
           aes(x = pos,y = as.Date(year_col), fill=deltaCoast)) + 
  
  geom_tile(color= "white",size=0.1, na.rm = TRUE) +
  
  # resolve here that values larger than the range are indicated in the colourbar 
  # now it reads as if the largest value is -125m/yr where as actually there is locations that
  # have larger values.
  # scale_fill_gradient2(name = 'change [m/yr] \n',limits = c(range[[1]],range[[2]]),
  #                      # breaks = c(range[[1]], range[[1]]/2, 0, range[[2]]/2, range[[2]]),
  # 
  #                      low = '#7b3294', high = "#008837", mid = '#f7f7f7',
  #                      guide = guide_colourbar(nbin=100, draw.ulim = FALSE,
  #                                              draw.llim = FALSE),
  #                      na.value = NA, oob=squish) + # squish clamps all values to be within min & max of limits arguments


scale_fill_gradientn(name = 'change [m/yr] \n',
                     breaks = c(range[[1]], range[[1]]/2, 0, range[[2]]/2, range[[2]]),
                     limits = c(range[[1]],range[[2]]),
                     colours = c('#7b3294', '#f7f7f7', "#008837"),
                     guide = guide_colourbar(nbin=100, draw.ulim = FALSE, draw.llim = FALSE),
                     oob=squish,
                     values = scales::rescale(c(range[[1]], -50, 0, 50, range[[2]]))
) +
  geom_vline(xintercept = posOfInterest, color= 'red',
             linetype="dashed") +
  geom_text() +
  annotate("text", label = as.roman(1:length(poiOriginX)),
           x = posOfInterest + 6000,
           y = rep(as.Date('1984-06-30'),length(poiOriginX)),
           size = 6, colour = "red") +
  
  geom_rect(data = rectangles, inherit.aes = FALSE,
            aes(xmin = xmin, xmax = xmax,
                ymin = ymin,
                ymax = ymax, colour = colour), fill = NA, size = 1) + 
  
  scale_colour_manual(name = ' ', values = c("black"),
                      labels = c('mudbank'),
                      guide = guide_legend(ncol = 2))+
  
  labs(y = 'Year', x = '') + #Alongshore Position [km]
  scale_x_reverse(lim=c(max(allFiles$pos)+4000, 0), expand = c(0,0),
                  labels = unit_format(unit = "", scale = 0.001)) + # 
  theme(axis.line.x = element_line(size = 0.5, colour = "black"),
        axis.line.y = element_line(size = 0.5, colour = "black"),
        axis.line = element_line(size= 1, colour = "black"),
        axis.title.y = element_text(size = 12, face = 'bold'),
        axis.title.x = element_text(size = 12, face = 'bold'),
        axis.text.x = element_text(size = 12,  hjust = .5, vjust = .5),
        axis.text.y = element_text(size = 12, hjust = .5, vjust = .5),
        legend.background = element_rect(fill = '#d9d9d9',  colour = '#d9d9d9'),
        legend.title = element_text(colour = 'black', size = 14, face = "bold"),
        # legend.key = element_rect(fill = NA),
        legend.text = element_text(size = 12),
        # legend.position = 'none',
        panel.grid.major = element_blank(), # remove grid lines
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        plot.background = element_rect(fill = '#d9d9d9',  colour = '#d9d9d9'))











# similar to figure by Pieter Augustinus
# filter outliers!

augustinus <- ggplot(allFiles_mutate, mapping = aes(x= pos, y = normalized2)) + # color=coast_outlier) / deltaCoast / 
  scale_y_continuous(limits=c(-5000, 5000)) +
  geom_point(size = 2, alpha = 0.1, # , color = "black"
             aes(color = five_year_col))  +
  scale_color_manual(name = "Legend", 
                     values = c('#41b6c4','#f768a1','#dd3497','#ae017e','#7a0177','#99d594','#00441b'),
                     guide = guide_legend(override.aes = list(alpha = 1),
                                           title = 'cross shore position')) +
  scale_x_reverse() +   
  labs(x = "along shore position", y = "Distance coastline position") +
  theme(axis.line.x = element_line(size = 0.5, colour = "black"),
        axis.line.y = element_line(size = 0.5, colour = "black"),
        axis.line = element_line(size= 1, colour = "black"),
        axis.title.y = element_text(size = 14, face = 'bold'),
        axis.title.x = element_text(size = 14, face = 'bold'),
        axis.text.x = element_text(size = 12,  hjust = .5, vjust = .5),
        axis.text.y = element_text(size = 12, hjust = .5, vjust = .5),
        legend.title = element_text(colour = 'black', size = 14, face = "bold"),
        plot.title = element_text(hjust = 0.5, size = 18, face = 'bold',
                                  vjust = -5), 
        panel.grid.major = element_blank(), # remove grid lines
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        plot.background = element_rect(fill = '#d9d9d9'))
  
augustinus

# or ty to find differnce in coastline change per position
facet <- 'five_year_col'

coastlineChange <- ggplot(subset(allFiles_mutate, coast_outlier ==1), 
       aes(x= pos, y = deltaCoast, group = as.factor(pos))) + 
  scale_y_continuous(limits=c(-500, 500)) +
  # geom_point(size = 2, alpha = 0.1,
             # aes(color = five_year_col))  +
  geom_boxplot(outlier.colour="black", outlier.shape=NA, width=0.6) +
  # geom_smooth(method='lm') +
  facet_wrap(paste0('~', facet)) +
  scale_x_reverse() +   
  labs(x = "along shore position", y = "coastline change") +
  theme(axis.line.x = element_line(size = 0.5, colour = "black"),
        axis.line.y = element_line(size = 0.5, colour = "black"),
        axis.line = element_line(size= 1, colour = "black"),
        axis.title.y = element_text(size = 14, face = 'bold'),
        axis.title.x = element_text(size = 14, face = 'bold'),
        axis.text.x = element_text(size = 12,  hjust = .5, vjust = .5),
        axis.text.y = element_text(size = 12, hjust = .5, vjust = .5),
        legend.title = element_text(colour = 'black', size = 14, face = "bold"),
        plot.title = element_text(hjust = 0.5, size = 18, face = 'bold',
                                  vjust = -5), 
        panel.grid.major = element_blank(), # remove grid lines
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        plot.background = element_rect(fill = '#d9d9d9'))

# coastlineChange


testFigure <- ggplot(subset(allFiles_mutate, coast_outlier == 1), 
       aes(x= pos, y = normalized2)) + 
  geom_point(size = 2, alpha = 0.1, aes(color = five_year_col))  +
  # geom_boxplot(outlier.colour="black", outlier.shape=NA, width=0.6) +
  geom_smooth(method='lm', aes(group = five_year_col, color = five_year_col)) +
  # geom_text(x = 25, y = 300, label = lm_eqn(subset(allFiles_mutate, coast_outlier ==1)), 
  #           parse = TRUE) +
  # stat_poly_eq(formula = my.formula, 
  #              aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
  #              parse = TRUE) +         
  facet_wrap(paste0('~', facet)) +
  scale_x_reverse() +   
  labs(x = "along shore position", y = "coastline change") +
  theme(axis.line.x = element_line(size = 0.5, colour = "black"),
        axis.line.y = element_line(size = 0.5, colour = "black"),
        axis.line = element_line(size= 1, colour = "black"),
        axis.title.y = element_text(size = 14, face = 'bold'),
        axis.title.x = element_text(size = 14, face = 'bold'),
        axis.text.x = element_text(size = 12,  hjust = .5, vjust = .5),
        axis.text.y = element_text(size = 12, hjust = .5, vjust = .5),
        legend.title = element_text(colour = 'black', size = 14, face = "bold"),
        plot.title = element_text(hjust = 0.5, size = 18, face = 'bold',
                                  vjust = -5), 
        panel.grid.major = element_blank(), # remove grid lines
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        plot.background = element_rect(fill = '#d9d9d9'))




#' 
#' 
#' alongshore variability
#' 
posEast <- c(seq(0 ,138000,1000))
posWest <- c(seq(255000 ,max(allFiles_mutate$pos),1000))

allFiles_mutate$alongshore = c('Center')

allFiles_mutate <- allFiles_mutate %>%
  dplyr::mutate(alongshore = ifelse(pos %in% posEast, c('East'),alongshore)) %>%
  dplyr::mutate(alongshore = ifelse(pos %in% posWest, c('West'),alongshore))

# calculate means
meansOI <- allFiles_mutate %>%
  dplyr::group_by(alongshore) %>% # , five_year_col -> include in group_by when face wrap is ggplot is used
  dplyr::summarize(mean=mean(normalized, na.rm = T)) %>%
  ungroup()

overallMean <- mean(allFiles_mutate$normalized, na.rm = T)

# coastline position compared to reference date (if x = normalized)

xAxis_seq <- quantile(allFiles_mutate$normalized,c(0.001, 0.999), na.rm = T)

overallMean <- allFiles_mutate %>%
  dplyr::summarize(mean=mean(normalized, na.rm = T))

library(ggridges)

spatialVariability_ridges <- ggplot(data = subset(allFiles_mutate, coast_outlier ==1), 
       aes(x=normalized, fill=alongshore, y=alongshore)) + 
  geom_density_ridges(
    stat = "binline", bins = 250, scale = 3,
    draw_baseline = F, alpha = 0.7) + 
  geom_segment(data = meansOI, 
               aes(x = mean,xend=mean, 
                   y=as.numeric(as.factor(alongshore))-0.3,
                   yend = as.numeric(as.factor(alongshore))+3), 
               size = 3, color = 'black') +
  geom_segment(data = meansOI, 
               aes(x = mean,xend=mean, 
                   y=as.numeric(as.factor(alongshore))-0.3,
                   yend = as.numeric(as.factor(alongshore))+3, 
                   color=alongshore), size = 1.5) +
  geom_vline(data = overallMean, aes(xintercept= mean, color = 'Black'), size = 1,
             linetype="dashed") +
  geom_text(data = meansOI, aes(label=sprintf("%1.1f", meansOI$mean), x = mean+500,
                                y=as.numeric(as.factor(alongshore))+2.5),
            colour = c('#1b9e77', '#d95f02', '#7570b3'), size = 5, fontface = 'bold') +
  
  scale_color_manual(values = c('Black'= 'black', 
                                'East' = '#d95f02',
                                'Center' = '#1b9e77',
                                'West' = '#7570b3')) +
  scale_fill_manual(values = c('East' = '#d95f02', # 
                               'Center' = '#1b9e77',
                               'West' = '#7570b3')) +
  scale_x_continuous(breaks = sort(c(-2000, 0, 2000, 4000,
                                     round(overallMean$mean))),
                     guide = guide_axis(n.dodge=1)) +
  
  labs(y = '', x = 'Coastline position [m]') +
  
  theme(axis.line.x = element_line(size = 0.5, colour = "black"),
        axis.line.y = element_line(size = 0.5, colour = "black"),
        axis.line = element_line(size= 1, colour = "black"),
        axis.title.y = element_text(size = 20, face = 'bold'),
        axis.title.x = element_text(size = 20, face = 'bold'),
        axis.text.x = element_text(size = 18,  hjust = .5, vjust = .5),
        axis.text.y = element_text(size = 20, colour = "black", face = 'bold', 
                                   hjust = .5, vjust = .5),
        legend.title = element_blank(), #element_text(colour = 'black', size = 14, face = "bold"),
        legend.background = element_rect(fill = '#d9d9d9',  colour = '#d9d9d9'),
        legend.position = "none",
        legend.text = element_text(size = 20),
        panel.grid.major = element_blank(), # remove grid lines
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        plot.background = element_rect(fill = '#d9d9d9'))

spatialVariability_ridges

# ggsave(spatialVariability_ridges, filename = paste0("./results/temp_maps/",
#                                              'spatial_variability1985-2020',
#                                    '_',  format(Sys.Date(), "%Y%m%d"),'_ridges.jpeg'),
#        width = 13.1, height = 7.25, units = c('in'), dpi = 1200)


spatialVariability <- ggplot(data = subset(allFiles_mutate, coast_outlier ==1), # & (deltaCoast > 30 | deltaCoast < -30)
       aes(x=normalized, fill=alongshore, alpha = alongshore)) + 
  # geom_freqpoly(binwidth = 25 ) + #, colour = alongshore
  geom_histogram(position = 'identity', binwidth = 25) + #, alpha = 0.7
  
  facet_wrap(~alongshore, ncol = 1, nrow = 3) +
  geom_vline(data = meansOI, aes(xintercept= mean), size = 3.5, colour = 'black',
             linetype="solid") +
  geom_vline(data = meansOI, aes(xintercept= mean, color=alongshore), size = 2,
             linetype="solid") +
  geom_vline(data = overallMean, aes(xintercept= mean, color = 'Black'), size = 2,
             linetype="dashed") +
  scale_color_manual(values = c('Black'= 'black', 
                                'East' =  "#33a02c",
                                'Center' = "#2E9FDF",
                                'West' = "#E7B800")) +
  scale_fill_manual(values = c('East' = "#33a02c",#'#d95f02', # 
                               'Center' = "#2E9FDF",#'#1b9e77',
                               'West' = "#E7B800"))+ #'#7570b3'
  # c("#E7B800", "#2E9FDF", "#33a02c")
  
  scale_alpha_manual(values= c('East' = 1,
                       'Center' = 1,
                       'West' = 1)) +
  geom_text(data = meansOI, aes(label=sprintf("%1.1f", mean),
                                x = c(1500,-200,-450),
                                y=c(1350,1400,1150)),
            colour = c('Black', 'Black', 'Black'), 
            size = 8, fontface = 'bold', show.legend = FALSE) +

  scale_y_continuous(expand = c(0,0), breaks = c(0, 750, 1500)) + 
  scale_x_continuous(breaks = sort(c(-2000, 0, 2000, 
                                     4000, round(overallMean$mean))),
                     limits = c(-3000, 3000)) +
  guides(fill = guide_legend(override.aes = list(size = 10, alpha =1,
                              linetype = 0)),
         colour = F) +
  labs(y = 'Observations [n]', x = 'Coastline position [m]') +
  
  theme(axis.line.x = element_line(size = 0.5, colour = "black"),
        axis.line.y = element_line(size = 0.5, colour = "black"),
        axis.line = element_line(size= 1, colour = "black"),
        axis.title.y = element_text(size = 20, face = 'bold'),
        axis.title.x = element_text(size = 20, face = 'bold'),
        axis.text.x = element_text(size = 18,  hjust = .5, vjust = .5),
        axis.text.y = element_text(size = 18, hjust = .5, vjust = .5),
        legend.title = element_blank(), #element_text(colour = 'black', size = 14, face = "bold"),
        legend.background = element_rect(fill = '#d9d9d9',  colour = '#d9d9d9'),
        legend.position = c(0.9,0.4),
        legend.text = element_text(size = 20),
        panel.grid.major = element_blank(), # remove grid lines
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        strip.text.x = element_blank(),
        plot.background = element_rect(fill = '#d9d9d9'))

spatialVariability

# ggsave(spatialVariability, filename = paste0("./results/temp_maps/",
#                                              'spatial_variability1985-2020',
#                                    '_',  format(Sys.Date(), "%Y%m%d"),'.jpeg'),
#        width = 13.1, height = 7.25, units = c('in'), dpi = 1200)
#   
# unique(allFiles_mutate$normalized)


# see if sections of the coast have different average coastline positions.
range<- seq(0,140000,1000) # 140000 - 240000 / 240000 - 450000

spatioTemporalVar <- subset(allFiles_mutate, coast_outlier == 1 & pos %in% range) %>% 
  dplyr::group_by(year) %>%
  dplyr::mutate(n = n()) %>%
  dplyr::summarize(mean_val = mean(normalized2, na.rm = T), count = mean(n)) 


averagePos <- ggplot(data = spatioTemporalVar, 
                     aes(x = year, y = mean_val, fill = year)) +
  geom_point()
  
averagePos


#################################
#' 
#' annual coastline change for coastline orientation
#' 
#################################

# pre-requisites: 
# groups of angles
angles <-  c(0,5,10,15,20,180, 290, 
             300, 330, 335, 340, 345, 350, 355, 360)

angle_group <- unique(cut(allFiles_mutate$bearing, angles))
allFiles_mutate$angle_group <- cut(allFiles_mutate$bearing,angles)

# epochs: <20000, 2000 - 2010, 2010-2020
allFiles_mutate$fiveyear <- as.Date(cut(lubridate::date(allFiles_mutate$DATE_ACQUIRED), 
                                        "5 year"))


# exclude river mouth obs
allFiles_mutate <- subset(allFiles_mutate, !(pos %in% posToExclude))

# For now exlude rediculous large slope values
allFiles_mutate <- subset(allFiles_mutate, !(slope > 500 |
                                               slope < -500))

xaxis <- 'pos'

# distribution of transect bearings
ggplot(allFiles_mutate, aes(x=eval(as.name(xaxis)), y = coast_median)) +
  # facet_wrap(paste0('~', 'fiveyear')) + # labeller = as_labeller(unlist(variable_names))
  geom_point(aes(colour = factor(fiveyear))) +
  scale_x_reverse() # west to east orientation

level_order <- c("(180,290]","(290,300]", "(300,330]", 
                 "(330,335]", "(335,340]", "(340,345]", "(345,350]",
                 "(350,355]", "(355,360]",
                 "(0,5]", "(5,10]", "(10,15]", "(15,20]", "(20,180]")
allFiles_mutate$angle_group<-factor(allFiles_mutate$angle_group, levels=level_order)
variable_names <- data.frame(matrix(ncol = 3, nrow = 1))

for(i in seq_len(length(level_order))){
  # i <- 1
  level_order[i] 
  
  variable_names[i,1] <- qdapRegex::ex_between(as.character(level_order[i] ), "(", "]")[[1]]
  variable_names[i,2] <- startNr <- as.numeric(qdapRegex::ex_between(
    as.character(level_order[i] ), "(", ",")[[1]])
  variable_names[i,3] <- endNr <- as.numeric(qdapRegex::ex_between(
    as.character(level_order[i] ), ",", "]")[[1]])
  
}

# if facet wrap enabled, per 5 yer timestep
# per 10 is probably better?
ggplot(allFiles_mutate, aes(x=angle_group, y = slope)) +
  facet_wrap(paste0('~', 'fiveyear'), labeller = as_labeller(unlist(unique(allFiles_mutate$fiveyear)))) +
  geom_boxplot(outlier.colour="black", outlier.size=2, width=0.6) +          # boxplot properties
  scale_x_discrete(labels=c(variable_names[,1])) +
  labs(y = "annual rate of change [m/yr]", x ='transect bearing') +
  theme(
    axis.line.x = element_line(size = 0.5, colour = "black"),
    axis.line.y = element_line(size = 0.5, colour = "black"),
    axis.line = element_line(size=1, colour = "black"),
    axis.text.x = element_text(color = "grey20", size = 14, hjust = 1, 
                               face = "bold", angle = 45),
    axis.text.y = element_text(color = "grey20", size = 14, hjust = .5, vjust = .5, face = "bold"),
    axis.title.x = element_text(size = 18, face = 'bold'),
    axis.title.y = element_text(size = 18, face = 'bold'),
    
    strip.background = element_rect(fill = "white", colour = "white"),
    legend.key = element_rect(fill = NA),
    legend.text = element_text(size = 18),
    # legend.position = c(.9, .8),
    legend.title = element_text(colour = 'black', size = 20, face = 'bold'),
    
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.spacing.x = unit(2, 'lines'),
    
    strip.text.x = element_text(size = 16, face = 'bold') # Facet titles
  )






