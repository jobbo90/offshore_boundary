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


# data\raw\GEE_exports\bathy_obs
# select folders
folderSelect <- as.matrix(list.files(paste0(dataFolder, '/GEE_exports/bathy_obs'), full.names = T))
df <- rewrite(folderSelect)
# only csv's
df <- df[grep('.csv', folderSelect, ignore.case = T),]
aoi <-  c('212-11') # bathymetry_2005_all / 212-11

filtered <- vector('list', 100)

for (x in seq_along(aoi)){
      # x <- 1
      # year = as.character(years[q])
      region = aoi[x]
      
      filters = c(region)
      
      # df %>% mutate(year = year(DATE_ACQUIRED))
      
      
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
allFiles <- do.call(rbind, lapply(as.matrix(filtered)[,1], 
                                  function(x) {
                                    # x  <- as.matrix(filtered)[1,1]
                                    dat <- read.csv(x, stringsAsFactors = FALSE,
                                                      sep = ',', na.strings=c("","NA"))
                                  dat$acq_date <- as.Date(strsplit(basename(x), '_')[[1]][4], 
                                                      "%Y%m%d")
                                  
                                  # as.Date(c("1/1/2001", "1/2/2001", "1/3/2001"), "%m/%d/%Y")
                                  
                                    # basename(x)
                                  dat}
                                  ))

# observation dates in files
obs_dates <- unique(allFiles$acq_date)

# get 2008 dates only
dates2008 <- as.Date('20080121', "%Y%m%d")
# dates2004 <- as.Date('20050201', "%Y%m%d")
# dates2005 <- as.Date('20050923', "%Y%m%d")

selection <-subset(allFiles, 
                    allFiles$acq_date <= dates2005 &
                   allFiles$acq_date >= dates2004 ) 

# boxplot <- 
ggplot(selection, aes(x= z, y = intertide)) + 
  geom_point(shape = 1) +
  # stat_ellipse() +
  # geom_smooth() +
  facet_wrap(~acq_date, ncol=2) +
  labs(y = "fraction", x = 'depth') + 
  scale_y_continuous( limits = c(0, 1),expand = c(0,0))+
    
  theme(
    axis.line.x = element_line(size = 0.5, colour = "black"),
    axis.line.y = element_line(size = 0.5, colour = "black"),
    axis.line = element_line(size=1, colour = "black"),
    axis.text.x = element_text(color = "grey20", size = 10, hjust = .5, vjust = .5),
    axis.text.y = element_text(color = "grey20", size = 10, hjust = .5, vjust = .5),
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
    
    strip.text.x = element_text(size = 14, face = 'bold') # Facet titles
    
  )


library(ggridges)
  
  # Marginal density plot of fraction 
ggplot(selection, aes(x = intertide, y = as.character(acq_date), fill = ..x..)) + 
  geom_density_ridges_gradient(scale = 1, rel_min_height = 0.01) +
  scale_x_continuous( limits = c(0, 1),expand = c(0.1,0)) +
  scale_fill_viridis(name = "fraction", option = "C")
  
# higher densities (> 1??) appear to contain invalid depth estimates

# Marginal density plot of the depth distribution
depth_density <- ggplot(selection, aes(z)) + 
  geom_density(alpha=.5) + 
  # scale_fill_manual(values = c('#999999','#E69F00')) + 
  theme(legend.position = "none")
  

# what about distance from shoreline?
# first plot seems to indicate that at greater depths fractions don't go
# above 0.75. Sees to be similar for most observations at shallower depths.
# At the observations further offshore (2005 data) the fractions don't
# go higher than +/- 0.5 
# Also after 40m depth there is no more mud fraction observations. The decrease
# in fractions starts roughly at +/- 35 meters.

# combine depth x fraction plot with the original satellite obs. 
# pick a date:

dateOfInt <- as.Date('20081112', "%Y%m%d")


# collection 
collectionL8 <- ee$ImageCollection("LANDSAT/LC08/C01/T1_TOA")$
  filterBounds(ee$Geometry$Point(-55.54, 5.94))

collectionL5 <- ee$ImageCollection("LANDSAT/LT05/C01/T1_TOA")$
  filterBounds(ee$Geometry$Point(-55.54, 5.94))

collectionL7 <- ee$ImageCollection("LANDSAT/LE07/C01/T1_TOA")$
  filterBounds(ee$Geometry$Point(-55.54, 5.94))

collection <- collectionL8$merge(collectionL5)$merge(collectionL7)$
  filter(ee$Filter$lt("CLOUD_COVER", 80))

# ee_print(filtCollect)

filtCollect <- collection$filterDate(as.character(dateOfInt-1), as.character(dateOfInt+1))
dates <- ee_get_date_ic(filtCollect, time_end = FALSE)

image <- ee$Image(filtCollect$sort("CLOUD_COVER")$first())   #
# properties <- ee_print(image)

id <- eedate_to_rdate(image$get("system:time_start"))


selectionOfInt <-subset(allFiles, 
                   as.Date(allFiles$acq_date) ==  as.Date(as.Date(id),  "%Y%m%d"))

spatialSelection <- st_as_sf(SpatialPointsDataFrame(data.frame(
  selectionOfInt$longitude, selectionOfInt$latitude),
  proj4string=CRS("+proj=longlat +datum=WGS84"),
  data = data.frame(selectionOfInt)))


visParams = list(
  bands = c("B5", "B4", "B3"),
  min = 0.05, max = 0.4, gamma = 1.4
)
first <- Map$addLayer(image, visParams,  as.character(as.Date(id)))
# Map$centerObject(filtCollect$first())
Map$centerObject(image, 12)


# plot Map
# subset for plotting smoothed
sampled <- spatialSelection[sample(nrow(spatialSelection), 
                                   nrow(spatialSelection)*0.2), ]

first +
  mapView(spatialSelection, zcol = "z")

ggplot(selectionOfInt, aes(x= z, y = intertide)) + 
  geom_point(shape = 1) +
  # stat_ellipse() +
  # geom_smooth() +
  # facet_wrap(~acq_date, ncol=2) +
  labs(y = "fraction", x = 'depth') + 
  scale_y_continuous( limits = c(0, 1),expand = c(0,0))+
  
  theme(
    axis.line.x = element_line(size = 0.5, colour = "black"),
    axis.line.y = element_line(size = 0.5, colour = "black"),
    axis.line = element_line(size=1, colour = "black"),
    axis.text.x = element_text(color = "grey20", size = 10, hjust = .5, vjust = .5),
    axis.text.y = element_text(color = "grey20", size = 10, hjust = .5, vjust = .5),
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
    
    strip.text.x = element_text(size = 14, face = 'bold') # Facet titles
  )
