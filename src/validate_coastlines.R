## ---------------------------
#'
#' Script name: Validate coastline positions with UAV data
#'
#' Short Description: 
#' 
#'
#' Author: Job de Vries
#'
#' Date Created: 2021-02-23
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

years <- seq(from = 2015, to = 2020, by = 1)

aoi <- c('Braamspunt') # WegNaarZee / Braamspunt
# select folders
folderSelect <- as.matrix(list.files(paste0(dataFolder, '/coastlines'), full.names = T))
df <- rewrite(folderSelect);
# only csv's
df <- df[grep('.csv', folderSelect, ignore.case = T),]

# acquisition dates of drone data
# reference_dates <- c("2019-06-20", '2019-07-13', '2020-02-19') # weg naar zee
reference_dates <- c('2019-07-24') #, '2020-02-03') # Braamspunt


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
allFiles <- do.call(bind_rows, 
                    lapply(as.matrix(filtered)[,1], 
                           function(x) read.csv(x, stringsAsFactors = FALSE, 
                                                sep = ',', na.strings=c("","NA")
              )))

col_dates <- col_of_interest(allFiles, 'DATE_ACQUIRED$')
col_coastDist <- col_of_interest(allFiles, 'coastDist$')

# all unique dates
uniqueDates <- unique(allFiles[,col_dates])

# all unique transect (id's)
allPos <- unique(allFiles[, col_of_interest(allFiles, 'pos$')]);
uniqueX<- unique(allFiles[, col_of_interest(allFiles, 'originX$')])
uniqueY<- unique(allFiles[, col_of_interest(allFiles, 'originY$')])
geo<- unique(allFiles[, col_of_interest(allFiles, '.geo')])

# get transects
transects <- build_csvLines(allFiles)

visParams = list(
  bands = c("B5", "B4", "B3"),
  min = 0.05, max = 0.4, gamma = 1.4
)


#' 
#' spatial visualization
#' 
#' weg naar zee:2019-03-16: partially cloudy, no great  visibility 
#'              2019-04-01: perfect example (reference-85 + reference-70)
#'              2019-06-06 partially cloudy with +/- 30 obs of coastlines in old export file
#'              2019-08-31 Landsat7 image partial coudy
#'              2019-09-08 landsat 7 partial cloudy
#'              2019-09-16 poor quality
#'              
#'  braamspunt: 2019-08-07
#'              

#-------

# collection 
collectionL8 <- ee$ImageCollection("LANDSAT/LC08/C01/T1_TOA")$
  filterBounds(ee$Geometry$Point(-55.54, 5.94))

collectionL7 <- ee$ImageCollection("LANDSAT/LE07/C01/T1_TOA")$
  filterBounds(ee$Geometry$Point(-55.54, 5.94))

collection <- collectionL8$merge(collectionL7)$
  filter(ee$Filter$lt("CLOUD_COVER", 75))

# ee_print(filtCollect)

reference_date <- as.Date(reference_dates[1])

filtCollect <- collection$filterDate(as.character(reference_date+90), as.character(reference_date+100))
dates <- ee_get_date_ic(filtCollect, time_end = FALSE)[,2]

image <- ee$Image(filtCollect$sort("DATE_ACQUIRED")$first())   #

id <- eedate_to_rdate(image$get("system:time_start"))

first <- Map$addLayer(image, visParams,  as.character(as.Date(id)))
# Map$centerObject(filtCollect$first())
Map$centerObject(image, 12)


# coastline Points
coastlines_selection <-subset(allFiles, allFiles$DATE_ACQUIRED == as.character(as.Date(id)) &
                                allFiles$coastX != 0)

coast_spatial <- sp_pnt_ee(coastlines_selection$coastX,
                           coastlines_selection$coastY, 
                           paste0('date: ',as.character(as.Date(id))),
                           "#d95f0e")

# plot Map
# combination seems to be broken?s
first + coast_spatial  


# select folders
shapeFolders <- as.matrix(list.files(paste0(dataFolder, '/validation/', aoi), 
                                     full.names = T))
df_shapes <- rewrite(shapeFolders);
# only shp's
df_shapes <- df_shapes[grep('.shp', shapeFolders, ignore.case = T),]

# formatted <- gsub(x=reference_date,pattern="-",replacement="",fixed=T)

formatted <- reference_dates %>%
  gsub(pattern="-", replacement="") 

filtered <- vector('list', 100)
for (q in formatted) {
    
    filters = c(q)
    
    filtered = rbind(filtered, df_shapes %>% 
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


# create empty df with outputs
output <- data.frame(distance=double(),
           lon=double(), 
           lat=double(), 
           id=double(),
           pos=integer(),
           DATE_ACQUIRED = character(),
           uavdate = character(),
           shapeName = character(),
           pattern = character(),
           stringsAsFactors=FALSE)

scenarios <- c('2019-03-16', '2019-04-01', '2019-08-23',
               '2019-09-08', '2019-08-31', '2019-10-10',
               '2019-09-24', '2019-08-07', '2019-06-04')



scenariosBraamspunt <- c('2019-08-07', '2019-08-23',
               '2019-09-08','2019-09-24', '2019-10-10', "2019-10-26")

scenarios2020 <- c('2019-12-29', '2020-03-02', '2020-03-10',
                   '2020-03-18')

# nog zorgen dat bij scenarios de juiste shapefiles worden aangeroepen
# kan het nu zo zijn dat de 2020 shapefile met beelden uit 2019 worden vergeleken.

for (im in scenariosBraamspunt){
  # im <- scenarios[1]
  # coastline Points
  coastlines_selection <-subset(allFiles, allFiles$DATE_ACQUIRED == as.character(as.Date(im)) &
                                  allFiles$coastX != 0)
  
  for (f in 1:nrow(filtered)){
    # f <- 3
    
    file <- filtered[f,1]
    # print(file)
    # remove(file)
    # read shapefile
    strings<- str_split(file, '/')[[1]]
    
    shape <- gsub(x=strings[6] ,pattern=".shp",replacement="",fixed=T)
    
    pattern <- str_split(shape, "_")[[1]][3:4]
    uavdate <-  str_split(strings[6], "_")[[1]][1]
    
    coastline <- readOGR(paste0(strings[1:5], collapse ='/' ),
                         shape, verbose = F)
    
    # define CRS
    line <- spTransform(coastline, CRS("+proj=longlat +datum=WGS84"))
    # get bbox
    pol_bbox <- as(extent(line), "SpatialPolygons")
    proj4string(pol_bbox) <- CRS("+init=epsg:4326")
    
    # reproject to allow a gbuffer 
    pol_bbox <- spTransform(pol_bbox, 
                            CRS(as.character("+proj=utm +zone=21 +ellps=intl +towgs84=-265,120,-358,0,0,0,0 +units=m +no_defs")))
    
    bbox_buf <- rgeos::gBuffer(spgeom = pol_bbox, byid = TRUE, width = 25)
    
    # back to wgs
    bbox_buf <- spTransform(bbox_buf, CRS("+proj=longlat +datum=WGS84"))
    
    coastlines_selection_sp <- SpatialPointsDataFrame(data.frame(
      coastlines_selection$coastX, coastlines_selection$coastY),
      proj4string=CRS("+proj=longlat +datum=WGS84"),
      data = data.frame(coastlines_selection))
    
    # clip points
    pointsOfInt <- intersect(coastlines_selection_sp, bbox_buf)
    
    # if no points found, break look
    if(length(pointsOfInt) == 0){break}
    
    testDist <- cbind(data.frame(dist2Line(pointsOfInt, line, distfun=distGeo)), 
                      pos = pointsOfInt$pos, DATE_ACQUIRED = pointsOfInt$DATE_ACQUIRED, 
                      uavdate, shape, pattern = paste0(pattern[1:2], collapse ='_'))
    
    output<-rbind(output,testDist)
    
    
    
    
    # reference_date <- as.Date(im)
    # 
    # filtCollect <- collection$filterDate(as.character(reference_date-1), as.character(reference_date+1))
    # dates <- ee_get_date_ic(filtCollect, time_end = FALSE)[,2]
    # 
    # image <- ee$Image(filtCollect$sort("DATE_ACQUIRED")$first())   #
    # 
    # id <- eedate_to_rdate(image$get("system:time_start"))
    # 
    # first <- Map$addLayer(image, visParams,  as.character(as.Date(id)))
    # Map$centerObject(filtCollect$first())
    # Map$centerObject(image, 12)
    
    # first + line
      mapview(line) + bbox_buf + pointsOfInt
                         
  }
}

n_fun <- function(x){
  return(data.frame(y = max(x, na.rm = T) + 10,
                    label = paste0(length(x))))
}

# filter scenarios on the amount of observations. (only plot > 10)


# controls the panels
facet <- 'uavdate'  
xaxis <- 'pattern'           
# controls the boxes to plot (fill)
boxes <- 'DATE_ACQUIRED'  

ggplot(output, aes(x=eval(as.name(xaxis)), y = distance, fill = eval(as.name(boxes)))) +
  facet_wrap(paste0('~', facet)) + #labeller = as_labeller(unlist(variable_names)) # implement if there is another variable changing ==> results in a second plot
  geom_boxplot(outlier.colour="black", outlier.size=2, width=0.6) +           # boxplot properties
  stat_summary(fun.data = n_fun, geom = "text",  hjust = 0.5,
               position = position_dodge(.75)) +
  labs(y = "error [m]", x = xaxis, fill = boxes) + 
  # scale_x_discrete(expand=c(0.2,0)) +
  
  theme(
    axis.line.x = element_line(size = 0.5, colour = "black"),
    axis.line.y = element_line(size = 0.5, colour = "black"),
    axis.line = element_line(size=1, colour = "black"),
    axis.text.x = element_text(color = "grey20", size = 14, hjust = .5, vjust = .5, face = "bold", angle = 45),
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
# boxplot
