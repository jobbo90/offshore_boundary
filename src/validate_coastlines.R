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


exportSwitch <- T # or F when exporing images in GEE not necessary
aoi <- c('WegNaarZee') # WegNaarZee / Braamspunt
# select folders
folderSelect <- as.matrix(list.files(paste0(dataFolder, '/coastlines'), full.names = T))
df <- rewrite(folderSelect);
# only csv's
df <- df[grep('.csv', folderSelect, ignore.case = T),]

# acquisition dates of drone data
#c("2019-06-20, '2019-07-13') # weg naar zee
reference_dates <- c('2019-06-20')#c('2020-02-19')#c("2019-06-20", '2019-07-13') #)  #, '2019-07-13', '2020-02-03' 2019-07-24 # Braamspunt


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

# all unique dates
uniqueDates <- unique(allFiles$DATE_ACQUIRED)

# all unique transect (id's)
allPos <- unique(allFiles$pos);
uniqueX<- unique(allFiles$originX)
uniqueY<- unique(allFiles$originY)

# get transects
# transects <- build_csvLines(allFiles)

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
                 '2019-09-24', '2019-06-04')
    posRange <- c(seq(13050, 14160, 30), seq(16410, 17310, 30)) # for both east & west locations
  }else if(aoi == 'WegNaarZee' & '2020-02-19' %in% reference_dates){
    scenarios <- c('2019-12-29', '2020-03-02', '2020-03-10')
    posRange <- seq(13050, 14160, 30)}

allLines <- vector('list', nrow(filtered))

for (im in scenarios){
  # im <- scenarios[4]
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
    
    # mapview(coastlines_selection_sp)
    
    # clip points
    pointsOfInt <- intersect(coastlines_selection_sp, bbox_buf)
    
    # if no points found, break look
    if(length(pointsOfInt) == 0){break}

    allLines[[f]] <- coastline
      testDist <- cbind(data.frame(dist2Line(pointsOfInt, line, distfun=distGeo)), 
                        pos = pointsOfInt$pos, DATE_ACQUIRED = pointsOfInt$DATE_ACQUIRED, 
                        uavdate, shape, pattern = paste0(pattern[1:2], collapse ='_'), 
                        coastX = pointsOfInt$coastX, coastY = pointsOfInt$coastY)
      
      output<-rbind(output,testDist)
      
                         
  }
  
  reference_date <- as.Date(im)
  
  filtCollect <- collection$filterDate(as.character(reference_date-1), as.character(reference_date+1))
  dates <- ee_get_date_ic(filtCollect, time_end = FALSE)[,2]
  
  image <- ee$Image(filtCollect$sort("DATE_ACQUIRED")$first())   #
  
  id <- eedate_to_rdate(image$get("system:time_start"))
  
  first <- Map$addLayer(image, visParams,  as.character(as.Date(id)))
  idAOI <- eedate_to_rdate(image$get("system:time_start"))
  landsat <- image$get("SPACECRAFT_ID")$getInfo()
  
  # names <- img$bandNames()$getInfo()
  export_bands = c("Blue", "Green", "Red", "NIR", "SWIR1", "SWIR2")
  
  opt_selectorsL5 <- c("B1","B2", "B3", "B4", "B5", "B7")
  opt_selectorsL8 <- c("B2", "B3", "B4", "B5", "B6", "B7")
  opt_selectorsL7 <- c("B1", "B2", "B3", "B4", "B5", "B7")
  
  if (landsat == 'LANDSAT_5'){
    image<-image$select(
      opt_selectors = opt_selectorsL5,
      opt_names = export_bands)}
  
  if (landsat == 'LANDSAT_8'){
    image<-image$select(
      opt_selectors = opt_selectorsL8,
      opt_names = export_bands)}
  if (landsat == 'LANDSAT_7'){
    image<-image$select(
      opt_selectors = opt_selectorsL7,
      opt_names = export_bands)}
  
  
  # firstAOI <- Map$addLayer(img, visParams,  as.character(as.Date(idAOI)))
  if(exportSwitch == T){
    
    # create larger buffer around aoi for plotting
    bbox_buf_large <- rgeos::gBuffer(spgeom = pol_bbox, byid = TRUE, width = 2000)
    # back to wgs
    bbox_buf_large <- spTransform(bbox_buf_large, CRS("+proj=longlat +datum=WGS84"))
    ext <- extent(bbox_buf_large)
    
    expextent <- ee$Geometry$LinearRing(list(c(ext@xmin, ext@ymin), c(ext@xmax, ext@ymin), 
                                             c(ext@xmax, ext@ymax), c(ext@xmin, ext@ymax), c(ext@xmin, ext@ymin)), 
                                        'EPSG:4326', F)
    bboxForPlot <- Map$addLayer(expextent)
    
    # get bandstack of the hourly data for current var
    expimg = image$select(export_bands)
    
    sceneID <- image$get("LANDSAT_SCENE_ID")$getInfo()
    
    # export
    task =ee$batch$Export$image$toDrive(
      image=expimg,
      region=expextent,
      scale=30,
      # crs=expcrs, # for now leaving at system default seems to work best for landsat
      description= paste0(sceneID),
      folder ='GEE_job',
      fileNamePrefix= paste0('acquisitionDate_', gsub('-', '',  reference_date), 
                             '_uavDate_',uavdate, '_', sceneID))
    paste0(sceneID, ' started')
    task$start()
    
  }
  
}

# 10^1.2

n_fun <- function(x){
  
  # x1 <- runif(1, 1, 5)
  return(data.frame(y = max(x, na.rm = T) + 10,
                    label = paste0(length(x))))
}

# filter scenarios on the amount of observations. (only plot > 10)


# controls the panels
facet <- 'uavdate'  
xaxis <- 'pattern'           
# controls the boxes to plot (fill)
boxes <- 'ordered_date'  

# what patterns are there
NR_facets <- unique(output[facet])#unique(output[facet])
NR_xax <- unique(output[xaxis])
variable_names <- data.frame(matrix(ncol = nrow(NR_xax), nrow = 1))
for(i in seq_len(nrow(NR_xax))){
  # i <- 1
  variable_names[1,i] <- paste0(xaxis, ': ', as.character(NR_xax[i,]))
  
  colnames(variable_names)[i] <- as.character(NR_xax[i,])
  
}


output2 <- output %>%
  arrange(factor(DATE_ACQUIRED))
  # dplyr::mutate(test = fct_reorder(as.character(DATE_ACQUIRED), DATE_ACQUIRED)) #%>%
  # dplyr::mutate(test2 = fct_inorder(as.character(sort(as.Date(DATE_ACQUIRED)) )))

# order the levels
output2$ordered_date <- factor(as.character(output$DATE_ACQUIRED), 
                              levels = c(sort(unique(as.Date(output$DATE_ACQUIRED)))))
output2 <- output2[order(as.Date(output2$DATE_ACQUIRED)),]
# append the values (such that they are also correctly ordered)
output2$ordered_date <- as.character(output[order(as.Date(output$DATE_ACQUIRED)), 'DATE_ACQUIRED'])

# levels(output2$DATE_ACQUIRED2)

# define a set of colours that is at least as long as the longest image range
colours <- c('#6a51a3', '#e935a1', '#66a61e','#d95f02','#e6ab02','#537eff','#666666','#a65628')

# output$DATE_ACQUIRED2 <- factor(output$DATE_ACQUIRED, unique(output$DATE_ACQUIRED),
#                                labels = c('1', '2', '4', '6',
#                                           '5', '8', '7', '3'),
#                                ordered = F)

boxplot<- ggplot(output2, aes(x=eval(as.name(xaxis)), y = distance, 
                             fill = eval(as.name(boxes)))) +
  
  # some hacky examples to override x-axis for categorial
  facet_wrap_custom(paste0('~', xaxis), scales = "free_x", ncol = 4, scale_overrides = list(
    scale_override(1, scale_x_discrete(labels = c('High-tide'))),
    scale_override(2, scale_x_discrete(labels = c('Dike'))),
    scale_override(3, scale_x_discrete(labels = c('Dry-wet'))),
    scale_override(4, scale_x_discrete(labels = c('Mud-vegetation')))
  )) +
  # facet_wrap(paste0('~', xaxis), ncol = 4, scales = "free_x",
  #            labeller = labeller(as.name(xaxis) = dose.labs)) +
  geom_boxplot(outlier.colour="black", outlier.size=2, width=0.6, 
               position=position_dodge(width = 1)) + 
  stat_summary(fun.data = n_fun, geom = "text",  hjust = 0.5,
               position = position_dodge2(1)) +
  scale_fill_manual(values=colours[1:length(unique(output2$DATE_ACQUIRED))]) +
  
  # geom_text_repel(aes(label = stat(y), group = eval(as.name(xaxis))), stat = 'summary', fun = sum) +
  labs(y = "Error [m] \n", x = ' ', fill = boxes) + 
  geom_hline(yintercept = 30, linetype="dashed") +
  scale_y_continuous( breaks = c(0, 30, seq(100,round(max(output2$distance), -2),100))) +
  guides(fill=guide_legend(ncol=3)) +
  theme(
    axis.line.x = element_line(size = 0.5, colour = "black"),
    axis.line.y = element_line(size = 0.5, colour = "black"),
    axis.line = element_line(size=1, colour = "black"),
    axis.text.x = element_text(color = "grey20", size = 14, hjust = .5, vjust = .5, face = "bold", angle = 45),
    axis.text.y = element_text(color = "grey20", size = 14, hjust = .5, vjust = .5, face = "bold"),
    axis.title.y = element_text(size = 14, face = 'bold'),
    
    strip.background = element_rect(fill = "white", colour = "white"),
    legend.background = element_rect(fill = '#d9d9d9',  colour = '#d9d9d9'),
    legend.key = element_rect(fill = NA),
    legend.text = element_text(size = 14),
    legend.position = c(.7, .9),
    legend.title =  element_blank(),#element_text(colour = 'black', size = 20, face = 'bold'),
    
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.spacing.x = unit(2, 'lines'),
    
    strip.text.x = element_blank(),#element_text(size = 16, face = 'bold') # Facet titles
    plot.background = element_rect(fill = '#d9d9d9',  colour = '#d9d9d9')
    )
boxplot

datesForExport <- paste0(unique(format(as.Date(reference_dates), "%Y")), collapse = '_')
ggsave(filename = paste0("./results/Validation/", aoi, '_',datesForExport, '_coastlines_',
        '_',  format(Sys.Date(), "%Y%m%d"),'.jpeg'),
        width = 13.1, height = 7.25, units = c('in'), dpi = 1200)

# read transects
# transects <- rewrite(as.matrix(list.files(paste0('./data/raw/shapes/transects')), full.names = T)) %>%
  # filter(str_detect(text,c('transects_kustlijn_WnZ_v3.shp$'))) 

# transects_sp <- rgdal::readOGR(dsn = './data/raw/shapes/transects', 
                               # layer = 'transects_kustlijn_WnZ_v3',#as.character(transects[1,1]),
                               # verbose = F)       
# transects_sp <- spTransform(transects_sp, CRS("+proj=longlat +datum=WGS84")) #%>%
  # fortify()
  
# select images
imgSelect <- as.matrix(list.files(paste0('./data/raw/GEE_exports/validationImages'), full.names = T))
df_img <- rewrite(imgSelect) 
df_img <- unique(df_img[grep('.tif', imgSelect, ignore.case = T),])

datePattern <- gsub(x=reference_dates,
                    pattern="-",replacement="",fixed=T)
# selected landsat observations neer the reference date
file <- df_img %>% 
  dplyr::filter(str_detect(text,datePattern)) #%>%
  # dplyr::mutate(new_id = str_extract(text, "[^_]+$"))
  # arrange_all()

  
  
# also needs to ordered on date!

# test for 1 file to start with

for (ir in 1:nrow(file)){
  # ir<-4
  path <- file[ir,]
  strings <- str_split(path, '/')[[1]]

  fullName <- gsub(x=strings[6] ,pattern=".tif",replacement="",fixed=T)
  
  # extract image details
  landsatID <- str_split(fullName, "_")[[1]][5]
  uavDate <-  as.Date(str_split(fullName, "_")[[1]][4], "%Y%m%d")
  date <- as.Date(str_split(fullName, "_")[[1]][2], "%Y%m%d")
  dateShort <- gsub(x=date,
       pattern="-",replacement="",fixed=T)
  
  
  brickRGB <- brick(paste0(path))
  repr <- projectRaster(brickRGB,
                        crs = CRS(as.character("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")))
  # data.frame(transects[,1])
  # corresponding points 
  correspoints <- subset(output, as.Date(DATE_ACQUIRED) == date)
  
  # unique(output$DATE_ACQUIRED)
  
  correspoints$title <-  paste0(date)
  
  # min(correspoints$coastX)
  # max(correspoints$coastX)
  
  
  rgb_plt  <-
    ggRGB(img = repr,  r = 'NIR', g = 'Red', b = 'Green', stretch = "lin", alpha = 0.8)+
    # geom_line(data = transects_sp,
    #           mapping = aes(x = long, y=lat, group = group),
    #           size = 0.1, linetype = "solid", alpha=0.8) + 
    geom_point(data = data.frame(correspoints), 
               mapping = aes(x = coastX, y = coastY),
               colour = 'white', size = 7, alpha = 1) +
      geom_point(data = data.frame(correspoints), 
                 mapping = aes(x = coastX, y = coastY),
                 colour = colours[ir], size = 6, alpha = 1) +

      # make the colour fit the boxplot corresponding to the date
      
      facet_grid(. ~ title) + # workaround to get the title in a box
    labs(y = 'Lat', x = 'Long') + # , title = paste0(date)
    # scale_x_continuous(labels = scaleFUN)+
      coord_cartesian(xlim=c(-55.227, -55.22),
                      ylim = c(5.896, 5.908)) +
      
    # scale_x_continuous(limits = c(-55.229, -55.215)) +
    # scale_y_continuous(limits = c(5.895, 5.909)) +
    
    theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", 
                                          size = 0.5),
          panel.background = element_blank(),
          plot.title = element_blank(), #element_text(hjust = 0.5, size = 18, face = 'bold',
                                      #vjust = 0),
          strip.background = element_rect(fill=colours[ir]),
          strip.text = element_text(size=25, colour="white", face = 'bold'),
          
          axis.line.x = element_line(size = 0.5, colour = "black"),
          axis.line.y = element_line(size = 0.5, colour = "black"),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          
          axis.title.x = element_text(size = 14, face = 'bold'),
          axis.title.y = element_text(size = 14, face = 'bold'))
  # rgb_plt  
  ggsave(rgb_plt, filename = paste0("./results/Validation/", 'uavDate_',
                                    datePattern, '_landsatDate_',dateShort,
          '_version',  format(Sys.Date(), "%Y%m%d"),'.png'),
          width = 10, height = 8, dpi = 96, units = "in")
      
}



