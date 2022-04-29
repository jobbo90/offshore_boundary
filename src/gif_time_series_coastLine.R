## ---------------------------
#'
#' Script name: create gif of time series coastline position
#'
#' Short Description: 
#' 
#'
#' Author: Job de Vries
#' after Phillip Kraaijenbrink: https://github.com/kraaijenbrink/ERA5-Land-globe-animation
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
# posOfInterest <- c(50000, 210000, 310000)
posToPlot <- 210000#138000#156000#230000
  
reference_date <- as.Date("2020-01-01")
aoi <- c('Suriname') 

# select folders
folderSelect <- as.matrix(list.files(paste0(dataFolder, '/coastlines'), full.names = T))
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

allFiles <- unique(do.call(rbind, lapply(as.matrix(filtered)[,1], 
                                         function(x) read.csv(x, stringsAsFactors = FALSE,
                                                              sep = ',', 
                                                              na.strings=c("","NA")
                                         ))))

# where are the duplicates comming from when loading? 
allFiles3 <- allFiles %>% 
  group_by_at(vars(DATE_ACQUIRED, pos)) %>% 
  filter(n()>1) %>% 
  ungroup()

col_dates <- col_of_interest(allFiles, 'DATE_ACQUIRED$')

# all unique transect (id's)
uniqueX<- unique(allFiles[, col_of_interest(allFiles, 'originX$')])
uniqueY<- unique(allFiles[, col_of_interest(allFiles, 'originY$')])
geo<- unique(allFiles[, col_of_interest(allFiles, '.geo')])

# keep_columns <- c('axisDist', 'mudFract', 'endDrop')  # necessary for mudbank output
coastlines <- st_as_sf(SpatialPointsDataFrame(data.frame(
  allFiles$coastX, allFiles$coastY),
  proj4string=CRS("+proj=longlat +datum=WGS84"),
  data = data.frame(allFiles)))


# select images
imgSelect <- as.matrix(list.files(paste0('./data/raw/GEE_exports/GIF/pos',posToPlot), 
                                  full.names = T))
# %>% 
#   str_extract('(?<=city"":"").*(?="":COMMENT"")')


df_img <- rewrite(imgSelect) 

df_img <- unique(df_img[grep('.tif', imgSelect, ignore.case = T),])


#---------------------------
#'
#' now multi temporal
#' 
#---------------------------

allFiles_POS <- subset(allFiles, (pos %in% c(380000-posToPlot)))

# all unique dates
uniqueDates <- unique(allFiles_POS[,col_dates])
group_dates<-unique(allFiles_POS$year_col)
group_quart <- unique(allFiles_POS$quarterly_col)
group_pos <- unique(allFiles_POS$pos)

# # get for all transects an coastline observation near
## reference date as baseline
allFiles_POS$baseline <- 0
# allFiles_dropPOS$slope <- -1
# 
allFiles_POS$baseline2 <- 0
# allFiles_dropPOS$grp <- NA


# normalize for the coastline position around a reference date

for (sid in group_pos) {
  # sid = 189000
  
  # get a reference distance 
  # e.g. observation closest to reference date OR
  # median observation over 1 year near the reference date 
  subsetAllObs <- subset(allFiles_POS, allFiles_POS$pos == sid &
                           allFiles_POS$coastDist >= 0) 
  nonOutliersAll <- subset(subsetAllObs, coast_outlier == 1)
  
  # index of all relevant (>0) observations of that position
  idx <- which(allFiles_POS$pos == sid &
                 allFiles_POS$coastDist >= 0)
  
  # nearest observation (median and original distance)
  index <- which.min(abs(as.Date(nonOutliersAll$DATE_ACQUIRED)-reference_date))
  coastObs <- subsetAllObs[index, 'coastDist']
  coastObs2 <- subsetAllObs[index, 'coast_median']
  
  allFiles_POS$baseline[idx] <- as.numeric(coastObs)
  allFiles_POS$baseline2[idx] <- as.numeric(coastObs2) # median val
  
}

# mutate the dataframe 
# add date properties as seperate columns
allFiles_mutate <- allFiles_POS %>% 
  dplyr::mutate(year = year(DATE_ACQUIRED),
                month = month(DATE_ACQUIRED, label=TRUE),
                day = day(DATE_ACQUIRED),
                full_date= date(DATE_ACQUIRED),
                years = date(quarterly_col))

# subtract baseline value from original
# Baseline is either a observation close to reference date. (baseline)
# Or the annual median observation of they reference date (baseline2)
allFiles_mutate$normalized <- allFiles_mutate$coastDist - allFiles_mutate$baseline
allFiles_mutate$normalized2 <- allFiles_mutate$coastDist - allFiles_mutate$baseline2



nonOutliers <- subset(allFiles_mutate, coast_outlier == 1 &
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

# create ggplot with time series (only needed once!)

maxVal <-quantile(allFiles_mutate$coastDist,c(0.99),
                  na.rm=T)
#max(allFiles_mutate$coastDist, na.rm = T)

minVal <- min(allFiles_mutate$coastDist, na.rm = T)

time_series <- ggplot(allFiles_mutate, aes(x= as.Date(DATE_ACQUIRED), y = coastDist)) + # color=coast_outlier)
  # geom_line(aes(y=coast_median),size=2, linetype= "dotted") + # add median dtrendline?
  geom_line(inherit.aes = FALSE, aes(x = as.Date(DATE_ACQUIRED), y = coast_median),
            # linetype = as.factor(pos)),
            alpha = 0.5, size = 1.2) +
  
  geom_point(size = 3, aes(colour = as.factor(coast_outlier)), alpha = 0.6) +
  
  scale_color_manual(name = "Legend",
                     values = c('red', 'blue'),
                     labels = c("outlier", "coastal distance")) +
  # scale_linetype_manual(name = ' ', values = c('dashed'), labels = c('median')) +
  scale_y_continuous(limits=c(minVal, maxVal+100)) +
  
  scale_x_date(labels = date_format("%Y")) +
  ggtitle( paste0('position: ', c(380000-posToPlot))) +
  labs(x = "year", y = "Distance coastline position") +
  theme(axis.line.x = element_line(size = 0.5, colour = "black"),
        axis.line.y = element_line(size = 0.5, colour = "black"),
        axis.line = element_line(size= 1, colour = "black"),
        axis.title.y = element_text(size = 14, face = 'bold'),
        axis.title.x = element_text(size = 14, face = 'bold'),
        axis.text.x = element_text(size = 12,  hjust = .5, vjust = .5),
        axis.text.y = element_text(size = 12, hjust = .5, vjust = .5),
        # strip.text.x = element_blank(), # remove panel labels
        legend.title = element_text(colour = 'black', size = 14, face = "bold"),
        # legend.key = element_rect(fill = NA),
        # legend.text = element_text(size = 15),
        plot.title = element_text(hjust = 0.5, size = 18, face = 'bold',
                                  vjust = -2), 
        legend.position = c(.88, .72),
        panel.grid.major = element_blank(), # remove grid lines
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        plot.background = element_blank())#element_rect(fill = '#d9d9d9'))

time_series

# put this in a function to facilitate creating frames for creating GIF 
allDates <- unique(allFiles_mutate$DATE_ACQUIRED) # consider non-outlier dates only?

# do 1 frame per day to contorl frame speed 

i <- 1
for(da in allDates){         #1:nrow(df_img)){
  # da <- allDates[162]
  # da <- '2013-05-02'
  # da <- '2009-09-28'
  
  datePattern <- gsub(x=da,
                      pattern="-",replacement="",fixed=T)
  
  file <- df_img %>% 
    filter(str_detect(text,datePattern)) %>%
    filter(str_detect(text, as.character(posToPlot)))

  # file <- df_img[da,1]
  strings <- str_split(file, '/')[[1]]
  fullName <- gsub(x=strings[7] ,pattern=".tif",replacement="",fixed=T)
  
  # extract details
  landsatID <- str_split(fullName, "_")[[1]][1]
  pos <-  str_split(fullName, "_")[[1]][2]
  date <- as.Date(str_split(fullName, "_")[[1]][3], "%Y%m%d")
  
  # improve: also upload 10 (?) historic observations with lower alpha val and 
  # different colour
  
  # load point obs
  poi <- subset(allFiles_mutate, as.Date(as.character(DATE_ACQUIRED)) == as.Date(da) & 
                  !is.na(coastX) &
                  coastX != -1)
  
  # create time series plot th moving horizontal line
  forPlot<- time_series + geom_vline(xintercept = as.Date(da),size=1, linetype= "solid") 
  
  # create the RGB plot if there was a image
  if(nrow(poi) > 0 & nrow(file)  > 0){
    
    brickRGB <- brick(paste0(file))
    # stacked <- stack(paste0(file))
    # crs(brickRGB) <- CRS("+init=epsg:4326")#"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    
    # test <- projectRaster(brickRGB, crs=CRS("+init=epsg:4326"), res=30)
    
    repr <- projectRaster(brickRGB,
                          # CRS("+init=epsg:4326"))
                          crs = CRS(as.character("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")))
    # CRS(as.character('+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0'))
    # names(brickRGB) <- bandNames # depends on sensor..
    # bandNames <- c('blue', 'green', 'red', 'nir', 'swir1')
    rgb_plt <-
      ggRGB(img = repr,  r = 'NIR', g = 'Red', b = 'Green', stretch = "lin", alpha = 0.8) +
      geom_point(data = data.frame(poi), 
                 mapping = aes(x = coastX, y = coastY),
                 colour = "blue", size = 3, alpha = 0.8) +
      # annotation_scale(location = "br", width_hint = 0.3, 
      #                  pad_x = unit(4, "cm"), pad_y = unit(0.8, "cm"),
      #                  text_col = c('white'), text_cex = 1) + # north arrow or scale bar?
      
      #+proj=utm +zone=28 +datum=WGS84 +units=m +no_defs 
      
      labs(y = 'Lat', x = 'Long', title = paste0(date)) +
      labs(caption='Landsat (USGS)                                                   @JdV') +
      theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", 
                                            size = 0.5),
            panel.background = element_blank(),
            plot.title = element_text(hjust = 0.5, size = 18, face = 'bold',
                                      vjust = -5), 
            
            axis.line.x = element_line(size = 0.5, colour = "black"),
            axis.line.y = element_line(size = 0.5, colour = "black"),
            axis.text.x = element_text(size = 10),
            axis.text.y = element_text(size = 10),
            
            axis.title.x = element_text(size = 14, face = 'bold'),
            axis.title.y = element_text(size = 14, face = 'bold'))
    
  } #else{
    
    # nearest Date
    # which.min(as.Date(unique(allFiles_mutate$DATE_ACQUIRED)) - date)
  #   nearestDate <- which.min(replace(abs(as.Date(allFiles_mutate$DATE_ACQUIRED) - date), 
  #                                    abs(as.Date(allFiles_mutate$DATE_ACQUIRED) - date)>10*356, NA))
  #   
  #   poi <- allFiles_mutate[nearestDate,]
  #   
  #   
  # }

  plot <- rgb_plt + forPlot + plot_layout(ncol = 2, nrow = 1, widths = c(1,1))
  
  # if there is now POI update times series
  # and make sure that brick RGB is still plotted but with the previous image
  
  ggsave(file.path("./results/GIF/frames",sprintf('%04d.png',i)),
         dpi=200, width=31, height=18, units='cm')

  # 
  # ggsave(file.path(paste0("./results/GIF/frames/", pos, '_',
  #                         str_split(fullName, "_")[[1]][3],
  #                         '.png')),plot,
  #        dpi=200, width=31, height=18, units='cm')

  i <- i + 1
}


# 
# # plot view
# nonOutliers_sp <- sp_pnt_ee(nonOutliers$coastX, nonOutliers$coastY,  
#                             'nonOutliers',"blue")
# coordinatesAOI <- nonOutliers_sp$x$setView[[1]]
# 
# test2 <- firstAOI + nonOutliers_sp
# setView(test2, subset(subset2d_for_testPlot, coast_outlier == 1)$coastX[1], 
#         subset(subset2d_for_testPlot, coast_outlier == 1)$coastY[1], 14, options = list())
# 
# # epochs: <20000, 2000 - 2010, 2010-2020
# allFiles_mutate$fiveyear <- as.Date(cut(lubridate::date(allFiles_mutate$DATE_ACQUIRED), 
#                                         "5 year"))
# 
# 
# # exclude river mouth obs
# allFiles_mutate <- subset(allFiles_mutate, !(pos %in% posToExclude))
# 
# # For now exlude rediculous large slope values
# allFiles_mutate <- subset(allFiles_mutate, !(slope > 500 |
#                                                slope < -500))
# 
# 
# # Spatio temporal plot:
# # either slope or normalized distance seems to be best / clearest but needs to be checked with full dataset
# # Also deltaCoast requries additional test when median positions are all calculated
# # range <- round(quantile(allFiles_mutate$coast_median,c(0.05, 0.95), na.rm=T))
# range <- round(quantile(allFiles_mutate$slope,c(0.05, 0.95), na.rm=T))
# testSubset<- allFiles_mutate[allFiles_mutate$pos == 2000,]
# 
# 
# 
# 
# p <-ggplot(subset(allFiles_mutate, !is.na(slope)),
#            aes(x = pos,y = as.Date(year_col), fill=slope))+  #y = as.Date(quarterly_col)
#   # fill=slope / deltaCoast / normalized / normalized2 / coastDist
#   geom_tile(color= "white",size=0.1, na.rm = TRUE) +
#   scale_fill_gradient2(limits = c(range[[1]],range[[2]]), 
#                        breaks = c(range[[1]], range[[1]]/2, 0, range[[2]]/2, range[[2]]),
#                        low = "#a50026", high = "#313695", mid = '#f7f7f7',
#                        guide = guide_colourbar(nbin=100, draw.ulim = FALSE,
#                                                draw.llim = FALSE),
#                        oob=squish, na.value = NA) + #"grey50"
#   labs(y = 'Date', x = 'position') +
#   scale_x_reverse(lim=c(max(allFiles_mutate$pos)+4000, 0), expand = c(0,0)) + # 
#   # geom_segment(data = data.frame(x = pos, 
#   #                                xend= pos, 
#   #                                y=reference_date,
#   #                                yend=reference_date),
#   #              aes(x=x, y=y, xend=xend, yend=yend),
#   #              linetype="dashed") +
#   # geom_hline(yintercept = reference_date, linetype="dashed") +
#   # geom_segment(y=reference_date, yend = reference_date, linetype="dashed",
#   #              size = 1,
#   #              x=300000, xend=100000) + # doesn't work after applying reverse?
#   # geom_text(aes(max(pos)+1000,reference_date,label = 'reference date'),
# #           vjust = -2)+
# theme(axis.line.x = element_line(size = 0.5, colour = "black"),
#       axis.line.y = element_line(size = 0.5, colour = "black"),
#       axis.line = element_line(size= 1, colour = "black"),
#       axis.title.y = element_text(size = 14, face = 'bold'),
#       axis.title.x = element_text(size = 14, face = 'bold'),
#       axis.text.x = element_text(size = 12,  hjust = .5, vjust = .5),
#       axis.text.y = element_text(size = 12, hjust = .5, vjust = .5),
#       # strip.text.x = element_blank(), # remove panel labels
#       legend.title = element_text(colour = 'black', size = 14, face = "bold"),
#       # legend.key = element_rect(fill = NA),
#       # legend.text = element_text(size = 15),
#       
#       # legend.position = 'none',
#       panel.grid.major = element_blank(), # remove grid lines
#       panel.grid.minor = element_blank(), 
#       panel.background = element_blank(),
#       plot.background = element_rect(fill = '#d9d9d9'))
# 
# # 'echte' kustlijn gebruiken
# kustlijn <- readOGR('D:/BackUp_D_mangroMud_202001/Site1_Suriname_all/Analysis/IntertidalArea/Coastlines',
#                     'class10_line_v5')
# 
# shapefile_df <- fortify(kustlijn)
# 
# mapped <- ggplot() +
#   geom_path(data = shapefile_df, 
#             aes(x = long, y = lat, group = group)) +
#   theme(axis.line.x = element_line(size = 0.5, colour = "black"),
#         axis.line.y = element_line(size = 0.5, colour = "black"),
#         axis.line = element_line(size= 1, colour = "black"),
#         axis.title.y = element_text(size = 14, face = 'bold'),
#         axis.title.x = element_text(size = 14, face = 'bold'),
#         axis.text.x = element_text(size = 12,  hjust = .5, vjust = .5),
#         axis.text.y = element_text(size = 12, hjust = .5, vjust = .5),
#         # legend.title = element_blank(), #element_text(colour = 'black', size = 14, face = "bold"),
#         panel.grid.major = element_blank(), # remove grid lines
#         panel.grid.minor = element_blank(), 
#         panel.background = element_blank(),
#         plot.background = element_rect()) # fill = '#d9d9d9'
# 
# map_projected <- mapped +
#   coord_map() +
#   scale_x_continuous(limits=c(-57.1, -53.95),
#                      expand = c(0,0))#, limits=c(0,30000),)
# 
# # alignedPLots <- align_patches(p, map_projected)
# # p + map_projected + plot_layout(ncol = 1, nrow = 2, heights = c(1,0.1), alig)
# cowplot::plot_grid(p, map_projected, align = "v", axis = "lr", ncol =1, rel_heights = c(1, 0.3))
# # https://stackoverflow.com/questions/54153906/aligning-axes-of-r-plots-on-one-side-of-a-grid-together
# 
# 
# 
# library(cowplot)
# legend <- get_legend(p)
# 
# p <- p + theme(legend.position = 'none')
# 
# 
# allFiles_mutate <- allFiles_mutate %>% 
#   group_by(year_col) %>%
#   dplyr::mutate(delta_median = median(deltaCoast, na.rm = T)) %>%
#   dplyr::mutate(negPos = ifelse(delta_median > 0, 1,0)) %>%
#   ungroup()
# 
# allFiles_mutate$negPos <- 1
# myColors <- c()
# for( i in unique(allFiles_mutate$year_col)){
#   # i <- unique(allFiles$year_col)[29]
#   
#   annualSubset <- subset(allFiles_mutate,as.Date(year_col) == i &
#                            coast_outlier == 1)
#   idx <- which(allFiles_mutate$year_col == i & 
#                  allFiles_mutate$coast_outlier == 1)
#   
#   
#   meanVal <- mean(annualSubset$deltaCoast, na.rm =T)
#   # medianVal <- median(annualSubset$deltaCoast, na.rm =T)
#   
#   myColors <- c(myColors, ifelse(meanVal>0 , '#2166ac', # blue if positive
#                                  ifelse(meanVal<0, '#b2182b', # red if negative
#                                         "grey90")))
#   
#   if(meanVal < 0){
#     print(paste0(i, ': ', meanVal))
#     allFiles_mutate$negPos[idx] <- 0
#   }
#   
# }
# 
# test<- subset(allFiles_mutate, coast_outlier == 1 & as.Date(year_col) == i)
# # test$negPos
# 
# 
# # combine space-time plot and coastline change plot
# # p3 <- cowplot::plot_grid(p,p2, align = "h", ncol =2, rel_widths = c(1, 0.3))
# 
# # p4 <- cowplot::plot_grid(p3, map_projected, align = "v", axis = "lr", ncol =1, rel_heights = c(1, 0.3))
# # 
# # cowplot::plot_grid(p, map_projected, align = "v", axis = "lr", ncol =1, rel_heights = c(1, 0.3))
# 
# # patchwork
# # https://stackoverflow.com/questions/41569817/align-multiple-plots-in-ggplot2-when-some-have-legends-and-others-dont
# upper <- p + p2+map_projected +legend+ plot_layout(ncol = 2, nrow = 2, widths = c(1,0.3),
#                                                    heights = c(1,0.3))
# 
# # final <- upper + map_projected + plot_layout(ncol = 2, nrow = 2, heights = c(1,0.3))
# # put the legend of space time plot below the coastline change plot??
# 
# 
# 
# # http://www.sthda.com/english/wiki/wiki.php?id_contents=7930
# blankPlot <- ggplot()+geom_blank(aes(1,1)) + 
#   cowplot::theme_nothing()
# 
# grid.arrange(p, p2,  blankPlot, legend,
#              ncol=2, nrow = 2, 
#              widths = c(2.7, 2.7), heights = c(0.2, 2.5))










