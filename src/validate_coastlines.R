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

aoi <- c('WegNaarZee') # WegNaarZee / Braamspunt
# select folders
folderSelect <- as.matrix(list.files(paste0(dataFolder, '/coastlines'), full.names = T))
df <- rewrite(folderSelect);
# only csv's
df <- df[grep('.csv', folderSelect, ignore.case = T),]

# acquisition dates of drone data
#c("2019-06-20, '2019-07-13') # weg naar zee
reference_dates <-c("2019-06-20", '2019-07-13')  #, '2020-02-03' 2019-07-24 # Braamspunt


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
# Map$centerObject(image, 12)


# coastline Points
coastlines_selection <-subset(allFiles, allFiles$DATE_ACQUIRED == as.character(as.Date(id)) &
                                allFiles$coastX != 0)

coast_spatial <- sp_pnt_ee(coastlines_selection$coastX,
                           coastlines_selection$coastY, 
                           paste0('date: ',as.character(as.Date(id))),
                           "#d95f0e")

# plot Map
# first + coast_spatial  


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


# select correct transects & images (scenarios)
if(aoi == 'Braamspunt' & '2019-07-24' %in% reference_dates){
    scenarios <- c('2019-08-07', '2019-08-23',
                 '2019-09-08','2019-09-24', '2019-10-10', "2019-10-26")
  # POS range: braamspunt 26970 - 27900 by 30 m
    posRange <- seq(26970, 27900, 30)
  }else if(aoi == 'Braamspunt' & '2020-02-03' %in% reference_dates){
    scenarios <- c('2019-12-29', '2020-03-02', '2020-03-10','2020-03-18')
    posRange <- seq(26970, 27900, 30)
  }else if(aoi == 'WegNaarZee' & ('2019-06-20' %in% reference_dates |
                                  '2019-07-13' %in% reference_dates)) { 
    scenarios <- c('2019-03-16', '2019-04-01', '2019-08-23',
                 '2019-09-08', '2019-08-31', '2019-10-10',
                 '2019-09-24', '2019-08-07', '2019-06-04')
    posRange <- c(seq(13050, 14160, 30), seq(16410, 17310, 30)) # for both east & west locations
  }else if(aoi == 'WegNaarZee' & '2020-02-19' %in% reference_dates){
    scenarios <- c('2019-12-29', '2020-03-02', '2020-03-10','2020-03-18')
    posRange <- seq(13050, 14160, 30)}

allLines <- vector('list', nrow(filtered))

for (im in scenarios){
  # im <- scenarios[3]
  # all coastline Points detected in landsat image
  coastlines_selection <-subset(allFiles, allFiles$DATE_ACQUIRED == as.character(as.Date(im)) &
                                  allFiles$coastX != 0 &
                                  (pos %in% posRange))
  
  for (f in 1:nrow(filtered)){
    # f <- 5
    # read coastline file
    file <- filtered[f,1]
    strings<- str_split(file, '/')[[1]]
    shape <- gsub(x=strings[6] ,pattern=".shp",replacement="",fixed=T)
    
    # extract details
    pattern <- str_split(shape, "_")[[1]][3:4]
    uavdate <-  str_split(strings[6], "_")[[1]][1]
    coastline <- readOGR(paste0(strings[1:5], collapse ='/' ),
                         shape, verbose = F)

    # class(coastline) ==> spatialLines DataFrame 'sp'
    
    # define CRS
    line <- spTransform(coastline, CRS("+proj=longlat +datum=WGS84"))
    # get bbox
    pol_bbox <- as(extent(line), "SpatialPolygons")
    proj4string(pol_bbox) <- CRS("+init=epsg:4326")
    
    # reproject to allow a gbuffer 
    pol_bbox <- spTransform(pol_bbox, 
                            CRS(as.character("+proj=utm +zone=21 +ellps=intl +towgs84=-265,120,-358,0,0,0,0 +units=m +no_defs")))
    
    bbox_buf <- rgeos::gBuffer(spgeom = pol_bbox, byid = TRUE, width = 250)
    
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

    
    reference_date <- as.Date(im)

    filtCollect <- collection$filterDate(as.character(reference_date-1), as.character(reference_date+1))
    dates <- ee_get_date_ic(filtCollect, time_end = FALSE)[,2]

    image <- ee$Image(filtCollect$sort("DATE_ACQUIRED")$first())   #

    id <- eedate_to_rdate(image$get("system:time_start"))

    first <- Map$addLayer(image, visParams,  as.character(as.Date(id)))
    Map$centerObject(filtCollect$first())
    Map$centerObject(image, 12)
    
    
    allLines[[f]] <- coastline
    # allLines[[f]]$id <- f

    
    
# # https://r-spatial.github.io/mapview/articles/articles/mapview_02-advanced.html#adjusting-opacity-1
      # m0 <- mapview(transects)
      # # allLines <-  allLines[lengths(allLines) != 0]
      # m1 <- mapview(allLines, alpha.regions = 0.2) # zcol = "id", col.regions = c("snow", "grey", "red")
      # m2 <- mapview(bbox_buf, legend = list(T, F))
      # m3 <- mapview(pointsOfInt,  col.regions = c('red'))
      # m4 <- m0 + m1 + m2 + m3
      # # 
      # m4@map + first

      testDist <- cbind(data.frame(dist2Line(pointsOfInt, line, distfun=distGeo)), 
                        pos = pointsOfInt$pos, DATE_ACQUIRED = pointsOfInt$DATE_ACQUIRED, 
                        uavdate, shape, pattern = paste0(pattern[1:2], collapse ='_'), 
                        coastX = pointsOfInt$coastX, coastY = pointsOfInt$coastY)
      
      output<-rbind(output,testDist)
      
                         
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

boxplot<- ggplot(output, aes(x=eval(as.name(xaxis)), y = distance, fill = eval(as.name(boxes)))) +
  facet_wrap(paste0('~', facet)) + #labeller = as_labeller(unlist(variable_names)) 
  geom_boxplot(outlier.colour="black", outlier.size=2, width=0.6) + 
  stat_summary(fun.data = n_fun, geom = "text",  hjust = 0.5,
               position = position_dodge(.75)) +
  labs(y = "error [m]", x = 'interface', fill = boxes) + 
  # scale_x_discrete(expand=c(0.2,0)) +
  geom_hline(yintercept = 30, linetype="dashed") +
  scale_y_continuous( breaks = c(0, 30, seq(100,round(max(output$distance), -2),100))) +
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
boxplot


datesForExport <- paste0(unique(format(as.Date(reference_dates), "%Y")), collapse = '_')
ggsave(filename = paste0("./results/Validation/", aoi, '_',datesForExport, '_coastlines_',
                          '_',  format(Sys.Date(), "%Y%m%d"),'.jpeg'),
       width = 13.1, height = 7.25, units = c('in'), dpi = 1200)


# which image to plot
dateToplot <- as.Date(scenarios[3])
subsetOutput <- subset(output, DATE_ACQUIRED == as.character(dateToplot))
                       
filtCollect <- collection$filterDate(as.character(dateToplot-1), as.character(dateToplot+1))
dates <- ee_get_date_ic(filtCollect, time_end = FALSE)[,2]

image <- ee$Image(filtCollect$sort("DATE_ACQUIRED")$first())   #

id <- eedate_to_rdate(image$get("system:time_start"))

first <- Map$addLayer(image, visParams,  as.character(as.Date(id)))
Map$centerObject(filtCollect$first())
Map$centerObject(image, 12)



subsetOutput_sp <- SpatialPointsDataFrame(data.frame(
  subsetOutput$coastX, subsetOutput$coastY),
  proj4string=CRS("+proj=longlat +datum=WGS84"),
  data = data.frame(subsetOutput))


# https://r-spatial.github.io/mapview/articles/articles/mapview_02-advanced.html#adjusting-opacity-1
      # m0 <- mapview(transects, alpha.regions = 0.1,
      #               homebutton = FALSE)
m1 <- mapview(allLines) # zcol = "id", col.regions = c("snow", "grey", "red")
    
m3 <- mapview(subsetOutput_sp,  col.regions = c('red'))
m4 <- m1 + m3

final <- m4@map + first # tm_compass(type = "8star", position = c("left", "top"))

map <- setView(final, mean(subsetOutput_sp$lon), 
        mean(subsetOutput_sp$lat), 15, options = list()) %>%
  # specific baselayer with name          
  addProviderTiles("Esri.WorldImagery", group = "Aerial") %>% 
  # specific baselayer with name
  # addProviderTiles("OpenTopoMap", group = "Topography")%>% 
  addScaleBar(position = 'bottomleft') %>%
  clearControls() # Remove all legend items
  # addLayersControl(
  #   # these groups are labelled above
  #   baseGroups = c("Aerial"),
  #   # collapse options
  #   # options = layersControlOptions(collapsed = F)
  #   ) 


map %>%
  clearMarkerClusters()
  # addMiniMap(zoomLevelOffset = -6) #%>% 
  # hideGroup(c("allLines[[1]]", "allLines[[2]]", "allLines[[3]]",
  #             "allLines[[3]]", "allLines[[3]]", "allLines[[3]]"))



# mapshot(map, file =  paste0(getwd(), "/map.pdf"), remove_controls = c("homeButton", "layersControl"))
#           
#           
#paste0("./results/Validation/", aoi, '_',datesForExport, '_coastlines_', 
 #                           '_',  as.character(as.Date(id)),'.png'))
