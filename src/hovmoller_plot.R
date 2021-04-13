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




#'
#' 
#' 
#'
#'

summarisePos <- 10000

# adding a indicaiton of noMudbank positions (so all observations on that POS recieve 1)
# only when at least 50%? of observation in a pos is a mudbank
allFiles2 <- allFiles %>% # annual_obs %>%
  group_by(year_col, pos) %>%
  dplyr::mutate(
    noMudbank = case_when(
      validMudbankObs/mudbankObs > 0.6 ~ 0, 
      TRUE ~ 1)) %>% 
  
  ungroup() #%>%
  # dplyr::select(year_col, pos, mudbank_outlier, axisDist, noMudbank) 


breaks <- seq(0, max(allFiles2$pos), summarisePos)
posLabel <- rollmean(breaks, 2) # set label to middle point of the aggregated groups
allFiles2$negPos <- 1
allFiles2$alongshore_negPos <- 1
myColors <- data.frame(id = character(), col = character(), year = character(),
                       positionGroup = character(), mean = double())

allFiles2 <- allFiles2 %>%
  dplyr::mutate(newPos =cut(pos,breaks, right = FALSE, labels = posLabel)) %>%
  ungroup()

rectangles <- data.frame(id = character(),fill =  character(),
                         colour = character,
                         xmin = double(), xmax = double(), 
                         ymin = double(), ymax = double())

# get median position for each year
# only for pos which are considered to have a mudbank
# also exclude outliers and nonsense observations
# how to avoid using the filtered collection ==> you'd want to keep the dataframe intact
# but still calculate the median offshore only on the relevant observations

for(y in 1:length(all_years)){
  # y <- 4
  # selected_year <- '2008-01-01'
  selected_year <- all_years[y]
  annualSubset <- subset(allFiles2,as.Date(year_col) == selected_year &
                           coast_outlier == 1)
  idx <- which(allFiles2$year_col == selected_year & 
                 allFiles2$coast_outlier == 1)
  
  # # all posiotions considered a mudbank during given year
  mudbankObs <- subset(annualSubset, !(pos %in% posToExclude) &
                     noMudbank == 0)
  getPos <- unique(mudbankObs$pos)
  
  # sequences
  sequences <- split(getPos, cumsum(c(0, diff(getPos) > 1000)));
  
  # drop sublist with only x amount of consequetive mudbank observations
  filtSequences <- Filter(function(x){length(x)>3}, sequences)
  
  startPos <- vapply(filtSequences, head, n = 1L, FUN.VALUE = numeric(1))
  endPos <- vapply(filtSequences, tail, n = 1L, FUN.VALUE = numeric(1))
  
  rectangles <- rbind(rectangles, data.frame(id = selected_year,fill = 'black',
                                             colour = 'black',
                                             xmin = startPos, xmax = endPos, 
                                             ymin = as.Date(selected_year)-184, 
                                             ymax = as.Date(selected_year)+181))
  
  # get mean coastal change for given year
  # hist(unlist(annualSubset[which(annualSubset$deltaCoast != 0), 'deltaCoast']))
  meanVal <- mean(annualSubset$deltaCoast, na.rm =T)
  # medianVal <- median(annualSubset$deltaCoast, na.rm =T)
  
  myColors <- rbind(myColors, data.frame(id = 'annualChange', year =  selected_year,
                                         positionGroup = 'all', mean = meanVal,
                                         col =  ifelse(meanVal>0 , '#2166ac', # blue if positive
                                 ifelse(meanVal < 0, '#b2182b', # red if negative
                                        "grey90"))))
  
  if(meanVal < 0){
    print(paste0(selected_year, ': ', round(meanVal,2)))
    allFiles2$negPos[idx] <- 0
  }
}

# for each group of positions
for (np in unique(allFiles2$newPos[!is.na(allFiles2$newPos)])){
  # np <- unique(annualSubset$newPos)[10]
  posSubset <- subset(allFiles2, newPos == as.numeric(as.character(np)) &
                        coast_outlier == 1)
  # corresponding index in original file
  subsetIDX <- which(allFiles2$newPos == as.numeric(as.character(np)) &
                       allFiles2$coast_outlier == 1)
  
  meanCoastlineChange <- mean(posSubset$deltaCoast, na.rm = T)
  
  myColors <- rbind(myColors, data.frame(id = 'positionChange', year =  'all',
                                         positionGroup = as.character(np), mean = meanCoastlineChange,
                                         col =  ifelse(meanCoastlineChange>0 , '#2166ac', # blue if positive
                                                       ifelse(meanCoastlineChange < 0, '#b2182b', # red if negative
                                                              "grey90"))))
  
  if(meanCoastlineChange < 0 ){
    allFiles2$alongshore_negPos[subsetIDX] <- 0
  }
  
  
  
}

# boxplot idicating the annual variation in coastline changes
AllNonOutliers <- subset(allFiles2, coast_outlier ==1)

annualVariation <- ggplot(AllNonOutliers,
             aes(x=as.Date(year_col), y = deltaCoast , group = as.factor(year_col))) +  # alpha = negPos
  geom_boxplot(aes(fill = as.factor(negPos)),  outlier.shape=NA)+#outlier.colour="black") +          
  scale_fill_manual('mean change', labels = c('negative', 'positive'),
                    values = c('#7b3294',  "#008837")) +   
  guides()+
  coord_flip() +
  labs(y ='Coastline Change [m]', y = "") +
  scale_y_continuous(limits=c(-250,250)) +
  theme(
    axis.line.x = element_line(size = 0.5, colour = "black"),
    axis.line.y = element_line(size = 0.5, colour = "black"),
    axis.text.x = element_text(color = "grey20", size = 12, hjust = .5, vjust = .5),
    axis.text.y = element_text(color = "grey20", size = 12, hjust = .5, vjust = .5),
    axis.title.x = element_text(size = 12, face = 'bold'),
    axis.title.y = element_blank(), 
    strip.background = element_rect(fill = "white", colour = "white"),
    legend.background = element_rect(fill = '#d9d9d9',  colour = '#d9d9d9'),
    legend.text = element_text(size = 12),
    legend.position = c(.8, .8),
    legend.title = element_text(colour = 'black', size = 16, face = 'bold'),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.spacing.x = unit(2, 'lines'),
    plot.background = element_rect(fill = '#d9d9d9', colour = '#d9d9d9'))

# annualVariation
legendAnnual <- get_legend(annualVariation)
annualVariation <- annualVariation + theme(legend.position = 'none')


AllNonOutliers$newPos <-  as.numeric(as.character(AllNonOutliers$newPos))
# now similarly to annual variation get for each group of position the variation
# in coastline change

aggregatedPos <- ggplot( subset(AllNonOutliers, !(pos %in% posToExclude)),
                        aes(x=newPos, y = deltaCoast, group = newPos)) +  
  geom_boxplot(aes(fill = as.factor(alongshore_negPos)), outlier.shape=NA)+      
  scale_fill_manual('mean change', labels = c('negative', 'positive'),
                    values = c('#7b3294', "#008837"),
                    guide = guide_legend(reverse=T)) +

  labs(y =  'Coastline Change \n [m]', x = "Alongshore Position [m]") +
  scale_y_continuous(limits=c(-250,250)) +
  scale_x_reverse(lim=c(max(allFiles2$pos)+4000, 0), expand = c(0,0)) +
  theme(
    axis.line.x = element_line(size = 0.5, colour = "black"),
    axis.line.y = element_line(size = 0.5, colour = "black"),#element_blank(),
    axis.text.x = element_text(color = "grey20", size = 12, hjust = .5, vjust = .5),
    axis.title.x = element_text(size = 12, face = 'bold'),
    axis.text.y = element_text(color = "grey20", size = 12, hjust = .5, vjust = .5),
    axis.title.y = element_text(size = 12, face = 'bold'),
    legend.background = element_rect(fill = '#d9d9d9',  colour = '#d9d9d9'),
    legend.key = element_rect(fill = NA),
    legend.position = c(.8, .8),
    legend.title = element_blank(),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.spacing.x = unit(2, 'lines'),
    plot.background = element_rect(fill = '#d9d9d9',  colour = '#d9d9d9'),
    strip.text.x = element_text(size = 16, face = 'bold') # Facet titles
    
  )

# aggregatedPos

legendPos <- get_legend(aggregatedPos)
aggregatedPos <- aggregatedPos + theme(legend.position = 'none')

####
#'
#' hovmoller plots
#'
#####
posOfInterest <- c(50000, 210000, 310000)
dataPOI <- subset(allFiles2, (pos %in% posOfInterest))

poiOriginX <- c(as.matrix(tapply(dataPOI$originX, dataPOI$pos, median)))
poiOriginY <- c(as.matrix(tapply(dataPOI$originY, dataPOI$pos, median)))

# 
# Spatio temporal variation
range <- round(quantile(allFiles2$deltaCoast,c(0.05, 0.95), na.rm=T))

p <-ggplot(subset(allFiles2, !is.na(deltaCoast) & !(pos %in% posToExclude)),
           aes(x = pos,y = as.Date(year_col), fill=deltaCoast)) + 

  geom_tile(color= "white",size=0.1, na.rm = TRUE) +

  scale_fill_gradient2(name = 'change [m/yr] \n',limits = c(range[[1]],range[[2]]), 
                       breaks = c(range[[1]], range[[1]]/2, 0, range[[2]]/2, range[[2]]),
                       low = '#7b3294', high = "#008837", mid = '#f7f7f7', # "#a50026" , "#313695"
                       guide = guide_colourbar(nbin=100, draw.ulim = FALSE,
                                               draw.llim = FALSE),
                       na.value = NA, oob=squish) + # squish clamps all values to be within min & max of limits arguments
  
  geom_vline(xintercept = posOfInterest, color= 'red',
             linetype="dashed") +
  # geom_text() + 
  annotate("text", label = as.roman(1:length(poiOriginX)), 
           x = posOfInterest + 6000, 
           y = rep(as.Date('1984-06-30'),length(poiOriginX)), 
           size = 6, colour = "red") +
  
  geom_rect(data = rectangles, inherit.aes = FALSE,
            aes(xmin = xmin, xmax = xmax,
                ymin = ymin,
                ymax = ymax, colour = colour), fill = NA) + 
  
  scale_colour_manual(name = ' ', values = c("black"),
                      labels = c('mudbank'),
                      guide = guide_legend(ncol = 2))+

  labs(y = 'Year', x = '') + #Alongshore Position [m]
  scale_x_reverse(lim=c(max(allFiles2$pos)+4000, 0), expand = c(0,0)) + # 
  theme(axis.line.x = element_line(size = 0.5, colour = "black"),
        axis.line.y = element_line(size = 0.5, colour = "black"),
        axis.line = element_line(size= 1, colour = "black"),
        axis.title.y = element_text(size = 12, face = 'bold'),
        axis.title.x = element_text(size = 12, face = 'bold'),#element_blank(), #element_text(size = 14, face = 'bold'),
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
# p

legend <- get_legend(p)

p <- p + theme(legend.position = 'none')


kustlijn <- readOGR(dsn = paste0(wd,'/data/raw/transects'),
                    layer = 'class10_line')

shapefile_df <- fortify(kustlijn)

floor_dec <- function(x, level=1) round(x - 5*10^(-level-1), level)
ceiling_dec <- function(x, level=1) round(x + 5*10^(-level-1), level)

mapped <- ggplot() +
  geom_path(data = shapefile_df, 
            aes(x = long, y = lat, group = group)) +
  geom_point(aes(x = poiOriginX,  y = poiOriginY), colour = 'red', 
             size = 3) +
  scale_x_continuous(limits=c(-57.1, -53.95),
                     expand = c(0,0)) +
  scale_y_continuous(breaks=c(5.8, 6.0)) +
  geom_text() +
  annotate("text", label = as.roman(1:length(poiOriginX)), 
           x = poiOriginX, 
           y = poiOriginY-0.15, 
           size = 4, colour = "red") +
  theme(axis.line.x = element_line(size = 0.5, colour = "black"),
        axis.line.y = element_line(size = 0.5, colour = "black"),
        axis.line = element_line(size= 1, colour = "black"),
        axis.title.y = element_text(size = 14, face = 'bold'),
        axis.title.x = element_text(size = 14, face = 'bold'),
        axis.text.x = element_text(size = 12,  hjust = .5, vjust = .5),
        axis.text.y = element_text(size = 12, hjust = .5, vjust = .5),
        panel.grid.major = element_blank(), # remove grid lines
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        plot.background = element_rect(fill = '#d9d9d9',  colour = '#d9d9d9')) 

# coord_map changes the aspect ratio, making it impossible to define relative heights in
# patchwork library: https://stackoverflow.com/questions/48924219/vertically-align-of-plots-of-different-heights-using-cowplotplot-grid-when-u
map_projected <- mapped +
  coord_map()
  # scale_x_continuous(limits=c(-57.1, -53.95),
                     # expand = c(0,0))#, limits=c(0,30000),)


left2 <- plot_grid(p, aggregatedPos, mapped, ncol = 1, align = 'v',
                   nrow = 3, rel_heights = c(2.5, 1, 0.5),         # adjust lay out 
                   labels = c('A', 'C', 'D'), vjust =c(+4, -1,-1), # adjust label position
                   hjust = -2)

legends <- plot_grid(legend, NULL, legendAnnual, nrow = 1, ncol = 3, rel_widths = c(1,-2,3),
                     axis = 't', align = 'v') + 
  theme(panel.background = element_rect(fill = '#d9d9d9',  colour = '#d9d9d9')) #plot.margin = unit(c(0,0,0,0), 'cm')
legends

right <- plot_grid(annualVariation, legends, ncol = 1, nrow = 2, 
                   rel_heights = c(2.5, 1.5),
                   labels = c('B'), hjust=-4, vjust = +4) +  
  theme(panel.background = element_rect(fill = '#d9d9d9',  colour = '#d9d9d9'))

final <- plot_grid(left2, right, align = 'h', ncol = 2,
          rel_widths = c(2.5, 1)) + 
  theme(panel.background = element_rect(fill = '#d9d9d9', colour = '#d9d9d9'))

# final

ggsave(plot = final, filename = paste0("./results/temp_maps/", 'Suriname_hovmollerFigure_1985_2020_',
                         format(Sys.Date(), "%Y%m%d"),'.jpeg'),
       width = 13.1, height = 7.25, units = c('in'), dpi = 1200)

















# 2d dimensional plot corresponding to the point of interest 

subsetPos <- subset(allFiles2, (pos %in% posOfInterest))
coastDistRange <- round(quantile(subsetPos$coastDist,c(0.005, 0.99), na.rm=T))

# all years considered a mudbank
getYears <- unique(subsetPos[subsetPos$noMudbank == 0, 'year_col'])

# sequences <- split(as.Date(getYears$year_col), cumsum(c(0, diff(as.Date(getYears$year_col)) == 365)));

# drop sublist with only x amount of consequetive mudbank observations
filtSequences <- Filter(function(x){length(x)>3}, sequences)

startPos <- vapply(filtSequences, head, n = 1L, FUN.VALUE = numeric(1))
endPos <- vapply(filtSequences, tail, n = 1L, FUN.VALUE = numeric(1))

dateFrames <- data.frame(id = posOfInterest, fill = NA,
                         xmin = as.Date(getYears$year_col), 
                         xmax = as.Date(getYears$year_col) + 365, 
                         ymin = coastDistRange[1], 
                         ymax = coastDistRange[2])


posToExclude <- c(seq(138000,147000,1000),
                  seq(241000, 255000, 1000))

twoDPlot <- ggplot(subsetPos, 
                   aes(x= as.Date(DATE_ACQUIRED), y = coastDist)) + 
  geom_rect(data = dateFrames, inherit.aes = FALSE, 
            aes(xmin = xmin, xmax = xmax, ymin = ymin,ymax = ymax), 
            fill = 'grey50', colour = NA) +
  geom_line(inherit.aes = FALSE, aes(x = as.Date(DATE_ACQUIRED), y = coast_median),
            alpha = 0.5, size = 1.2) +
  geom_point(size = 3, aes(colour = as.factor(coast_outlier)), alpha = 0.6) +
  scale_y_continuous(limits=c(coastDistRange[1],coastDistRange[2])) +
  scale_color_manual(name = "Legend",
                     values = c('red', 'blue'),
                     labels = c("outlier", "coastal distance")) +
  scale_x_date(labels = date_format("%Y")) +
  ggtitle( paste0('position: ', posOfInterest)) +
  labs(x = "year", y = "Distance coastline position") +
  theme(axis.line.x = element_line(size = 0.5, colour = "black"),
        axis.line.y = element_line(size = 0.5, colour = "black"),
        axis.line = element_line(size= 1, colour = "black"),
        axis.title.y = element_text(size = 14, face = 'bold'),
        axis.title.x = element_text(size = 14, face = 'bold'),
        axis.text.x = element_text(size = 12,  hjust = .5, vjust = .5),
        axis.text.y = element_text(size = 12, hjust = .5, vjust = .5),
        # strip.text.x = element_blank(), # remove ☺panel labels
        legend.title = element_text(colour = 'black', size = 14, face = "bold"),
        # legend.key = element_rect(fill = NA),
        # legend.text = element_text(size = 15),
        plot.title = element_text(hjust = 0.5, size = 18, face = 'bold',
                                  vjust = -5), 
        # legend.position = c(.78, .5),
        panel.grid.major = element_blank(), # remove grid lines
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        plot.background = element_rect(fill = '#d9d9d9'))
# twoDPlot
