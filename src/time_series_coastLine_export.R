## ---------------------------
#'
#' Script name: create gif of time series coastline position
#'
#' Short Description: 
#' 
#'
#' Author: Job de Vries
#'
#' Date Created: 2021-03-21
#'
#' Copyright (c) Job de Vries, 2021
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

reference_date <- as.Date("2020-01-01")
aoi <- c('Suriname') 

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
allFiles3 <- allFiles %>% group_by_at(vars(DATE_ACQUIRED, pos)) %>% 
  filter(n()>1) %>% ungroup()

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

collection <- collectionL5#collectionL8$merge(collectionL5)$#merge(collectionL7)$
  filter(ee$Filter$lt("CLOUD_COVER", 30))

# ee_print(filtCollect)

filtCollect <- collection$filterDate(as.character(reference_date-300), as.character(reference_date+300))$
  filterBounds(ee$Geometry$Point(-55.54, 5.94))
dates <- ee_get_date_ic(filtCollect, time_end = FALSE)[,2]

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

# mutate the dataframe 
# add date properties as seperate columns
allFiles_mutate <- allFiles_dropPOS 

# test simple 2d plot 
twoD_pos <- 230000#299000
subset2d_for_testPlot <- subset(allFiles_mutate, pos == twoD_pos)

# plot temporal evolution for given transect
# now in ggplot form
subset2d_for_testPlot$DATE_ACQUIRED <- as.Date(subset2d_for_testPlot$DATE_ACQUIRED)
subset2d_for_testPlot$year_col <- as.Date(subset2d_for_testPlot$year_col)
# subset2d_for_testPlot$coast_median <- as.numeric(levels(subset2d_for_testPlot$coast_median))[subset2d_for_testPlot$coast_median]

nonOutliers <- subset(subset2d_for_testPlot, coast_outlier == 1 &
                      coastX != -1)

coastlines_selection_sp <- SpatialPointsDataFrame(data.frame(
  nonOutliers$coastX, 
  nonOutliers$coastY),
  proj4string=CRS("+proj=longlat +datum=WGS84"),
  data = data.frame(nonOutliers))

# get bbox
pol_bbox <- as(extent(coastlines_selection_sp), "SpatialPolygons")
proj4string(pol_bbox) <- CRS("+init=epsg:4326")

# reproject to allow a gbuffer 
pol_bbox <- spTransform(pol_bbox, 
                        CRS(as.character("+proj=utm +zone=21 +ellps=intl +towgs84=-265,120,-358,0,0,0,0 +units=m +no_defs")))
bbox_buf <- rgeos::gBuffer(spgeom = pol_bbox, byid = TRUE, width = 1000)

# back to wgs
bbox_buf <- spTransform(bbox_buf, CRS("+proj=longlat +datum=WGS84"))
ext <- extent(bbox_buf)

expcrs <- collection$select('B1')$first()$projection()$crs()$getInfo()
expextent <- ee$Geometry$LinearRing(list(c(ext@xmin, ext@ymin), c(ext@xmax, ext@ymin), 
                                        c(ext@xmax, ext@ymax), c(ext@xmin, ext@ymax), c(ext@xmin, ext@ymin)), 
                                   'EPSG:4326', F)
Map$addLayer(expextent)

m2 <- mapview(bbox_buf)
m3 <- mapview(coastlines_selection_sp,  col.regions = c('red'))
m4@map <- m2 + m3

aoiCollect <- collection$filterBounds(ee$Geometry$Point(
  coordinates(bbox_buf)[1,1],
  coordinates(bbox_buf)[1,2]))

# eedate_to_rdate(aoiCollect$aggregate_array())
ic_names <- aoiCollect %>%
  ee$ImageCollection$aggregate_array("DATE_ACQUIRED") %>%
  ee$List$getInfo()


# names <- img$bandNames()$getInfo()
export_bands = c("B1","B2", "B3", "B4", "B5", "B6", "B7")

# get all images for download
# https://rdrr.io/github/r-spatial/rgee/man/ee_imagecollection_to_local.html
potential_dates <- nonOutliers$DATE_ACQUIRED

for(da in ic_names){
  # da <- ic_names[1]
  img <- ee$Image(aoiCollect$filterDate(as.character(as.Date(da)-1), 
                                 as.character(as.Date(da)+1))$
    sort("system:time_start")$first())
                               
                               
  idAOI <- eedate_to_rdate(img$get("system:time_start"))
  # firstAOI <- Map$addLayer(img, visParams,  as.character(as.Date(idAOI)))
  
  clouds <- img$get('CLOUD_COVER')$getInfo()
  
  if(clouds < 50){
    # get bandstack of the hourly data for current var
    expimg = img$select(export_bands)
    
    sceneID <- img$get("LANDSAT_SCENE_ID")$getInfo()
    
    # export
    task =ee$batch$Export$image$toDrive(
      # ee_image_to_drive(
      image=expimg,
      region=expextent,
      scale=30,
      crs=expcrs,
      description= paste0(sceneID),
      folder ='GEE',
      fileNamePrefix= paste0(sceneID, '_',twoD_pos, '_',
                             gsub('-', '',  as.Date(da))))
    
    task$start()
    paste0(sceneID, ' started')
  }
}
