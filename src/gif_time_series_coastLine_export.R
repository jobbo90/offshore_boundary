## ---------------------------
#'
#' Script name: create gif of time series coastline position
#'
#' Short Description: 
#' 
#'
#' Author: Job de Vries
#' 
#' after: 
#' Philip Kraaijenbrink. (2021, July 9). 
#' ERA5-Land globe animation (Version 1.0.0). 
#' Zenodo. http://doi.org/10.5281/zenodo.5084524
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

ee_Initialize()
## ---------------------------

# Rewrite table
rewrite <- function(txt){
  #' @title rewrite  character strings
  #' @description this function rewrites 
  #' character strings to lists with tibble
  #' @param txt character string
  #' @return return the list
  return(tibble::tibble(text = c(txt)))
  
}

# detect string to to_keep 
to_keep <- function(fixed_string, text) {
  #' @title string to keep
  #' @description detect string in text
  #' @param fixed_string the pattern to match
  #' @param text the text to search in
  #' @return string to keep
  return(stringr::str_detect(text, stringr::fixed(fixed_string, ignore_case = TRUE)))
}

## ---------------------------

# mapviewOptions(basemaps = c( "Esri.WorldImagery","Esri.WorldShadedRelief", "OpenStreetMap.DE"))
dataFolder <- './data/processed'
# dataFolder <- 'D:/WOTRO/Research/Software/Projects/offshore_boundary/data/processed/coastlines'


mapviewOptions(basemaps = c( "Esri.WorldImagery","Esri.WorldShadedRelief", "OpenStreetMap.DE"))
leaflet() %>%
  addProviderTiles("Esri.WorldImagery")

years <- seq(from = 1985, to = 2021, by = 1)

# # near river mouths estimates for coastlines in old version of GEE script are 
# # questionable, should partially be solved in newest versions (11-2-2021)
# posToExclude <- c(seq(139000,147000,1000),
#                   seq(241000, 255000, 1000))  

reference_date <- as.Date("1985-01-01")
aoi <- c('Suriname') 

# select folders
folderSelect <- as.matrix(list.files(paste0(dataFolder), full.names = T))
df <- rewrite(folderSelect);
# only csv's
df <- df[grep('coastlines.csv', folderSelect, ignore.case = T),]


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
allFiles <-unique(do.call(rbind, lapply(as.matrix(filtered)[,1],
                                        function(x) read.csv(x,
                                                             stringsAsFactors = FALSE,
                                                             sep = ',',
                                                             na.strings=c("","NA")
                                        ))))


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
  bands = c("NIR", "Red", "Green"),
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
  filter(ee$Filter$lt("CLOUD_COVER", 50))

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
# 137000 138000 Braamspunt / 156000 WnZ
posOfInterest <- c(50000, 210000, 310000)
twoD_pos <- 50000#156000#230000#138000#299000
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
bbox_buf <- rgeos::gBuffer(spgeom = pol_bbox, byid = TRUE, width = 3000)

# back to wgs
bbox_buf <- spTransform(bbox_buf, CRS("+proj=longlat +datum=WGS84"))
ext <- extent(bbox_buf)

# 
# expcrsL5 <- collectionL5$select('B3')$first()$projection()$crs()$getInfo()
expcrs <- collection$select('B3')$first()$projection()$crs()$getInfo()
expextent <- ee$Geometry$LinearRing(list(c(ext@xmin, ext@ymin), c(ext@xmax, ext@ymin), 
                                        c(ext@xmax, ext@ymax), c(ext@xmin, ext@ymax), c(ext@xmin, ext@ymin)), 
                                   'EPSG:4326', F)
bboxForPlot <- Map$addLayer(expextent)

# m1 <- mapview(ext)
m2 <- mapview(bbox_buf)
m3 <- mapview(coastlines_selection_sp,  col.regions = c('red'))
m4 <- m2 + m3

aoiCollect <- collection$filterBounds(ee$Geometry$Point(
  coordinates(bbox_buf)[1,1],
  coordinates(bbox_buf)[1,2]))

# eedate_to_rdate(aoiCollect$aggregate_array())
ic_names <- aoiCollect %>%
  ee$ImageCollection$aggregate_array("DATE_ACQUIRED") %>%
  ee$List$getInfo()

# names <- img$bandNames()$getInfo()
export_bands = c("Blue", "Green", "Red", "NIR", "SWIR1", "SWIR2")

opt_selectorsL5 <- c("B1","B2", "B3", "B4", "B5", "B7")
opt_namesL5 <- c("Blue", "Green", "Red", "NIR", "SWIR1", "SWIR2" )

opt_selectorsL8 <- c("B2", "B3", "B4", "B5", "B6", "B7")
opt_namesL8 <- c("Blue", "Green", "Red", "NIR", "SWIR1", "SWIR2" )

# get all images for download
# https://rdrr.io/github/r-spatial/rgee/man/ee_imagecollection_to_local.html
# potential_dates <- nonOutliers$DATE_ACQUIRED

retry <- which(as.Date(ic_names) > as.Date(c('2009-01-01')) & 
        as.Date(ic_names) < as.Date(c('2014-01-01')) )

for(da in ic_names[retry]){
  # da <- ic_names[212]
  # da <- '2013-05-02'
  # da <- '2009-09-28'
  
  img <- ee$Image(aoiCollect$filterDate(as.character(as.Date(da)-1), 
                                 as.character(as.Date(da)+1))$
    sort("system:time_start")$first())
  
  idAOI <- eedate_to_rdate(img$get("system:time_start"))
  landsat <- img$get("SPACECRAFT_ID")$getInfo()
  
  # img <- img$reproject(collectionL5$select('B3')$first()$projection())
  # img <- img$reproject(crs = "EPSG:4326", scale = 30) # 32621
  # imgForPlot <- Map$addLayer(img, visParams,  as.character(as.Date(idAOI)))
  # img$get('MAP_PROJECTION')$getInfo()
  # img$get('UTM_ZONE')$getInfo()
  # m4@map + imgForPlot + bboxForPlot
                               
  
  if (landsat == 'LANDSAT_5'){
    img<-img$select(
      opt_selectors = opt_selectorsL5,
      opt_names = opt_namesL5
    )
  }
  
  if (landsat == 'LANDSAT_8'){
    img<-img$select(
      opt_selectors = opt_selectorsL8,
      opt_names = opt_namesL8
    )
  }
  
  # firstAOI <- Map$addLayer(img, visParams,  as.character(as.Date(idAOI)))
  
  clouds <- img$get('CLOUD_COVER')$getInfo()
  
  if(clouds < 50){
    # get bandstack of the hourly data for current var
    expimg = img$select(export_bands)
    
    # img$select(opt_selectors = c("Blue"))$projection()$getInfo()
    
    sceneID <- img$get("LANDSAT_SCENE_ID")$getInfo()
    
    # export
    task =ee$batch$Export$image$toDrive(
      image=expimg,
      region=expextent,
      scale=30,
      # crs=expcrs, # for now leaving at system default seems to work best for landsat
      description= paste0(sceneID),
      folder ='GEE_job',
      fileNamePrefix= paste0(sceneID, '_',twoD_pos, '_',
                             gsub('-', '',  as.Date(da))))
    paste0(sceneID, ' started')
    task$start()
    
  }
}
