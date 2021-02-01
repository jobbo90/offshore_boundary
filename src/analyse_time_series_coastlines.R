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
dataFolder <- './data/processed'
years <- seq(from = 1985, to = 2020, by = 1)
  
  
  #c('2015', '2016', '2017','2018', '2019', '2020') # c('2005','2006','2007', '2008', '2009')

reference_date <- as.Date("2010-01-01")

aoi <- c('229_56') # path_row

# select folders
folderSelect <- as.matrix(list.files(paste0(dataFolder, '/offshore_points'), full.names = T))
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
# filtered <- unique(filtered)[1:2,]

allFiles <- do.call(rbind, lapply(as.matrix(filtered)[,1], function(x) read.csv(x, stringsAsFactors = FALSE,
                                                                                sep = ',', na.strings=c("","NA")
                                                                                )))


# somehow all the dates lost 1 hour (due to timezone def?)
allFiles$year_col <- as.Date(as.POSIXct(paste(as.POSIXct(allFiles$year_col), "23:00:00")) + 120*60)
allFiles$date_col <- as.Date(as.POSIXct(paste(as.POSIXct(allFiles$date_col), "23:00:00")) + 120*60)
allFiles$quarterly_col <- as.Date(as.POSIXct(paste(as.POSIXct(allFiles$quarterly_col), "23:00:00")) + 360*60)

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

transects <- build_csvLines(allFiles)

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
collectionL8 <- ee$ImageCollection("LANDSAT/LC08/C01/T1_TOA")$
  filterBounds(ee$Geometry$Point(-55.54, 5.94))

collectionL5 <- ee$ImageCollection("LANDSAT/LT05/C01/T1_TOA")$
  filterBounds(ee$Geometry$Point(-55.54, 5.94))

collectionL7 <- ee$ImageCollection("LANDSAT/LE07/C01/T1_TOA")$
  filterBounds(ee$Geometry$Point(-55.54, 5.94))

collection <- collectionL8$merge(collectionL5)$#merge(collectionL7)$
  filter(ee$Filter$lt("CLOUD_COVER", 30))
  
# ee_print(filtCollect)

filtCollect <- collection$filterDate(as.character(reference_date-300), as.character(reference_date+300))
dates <- ee_get_date_ic(filtCollect, time_end = FALSE)[,2]

image <- ee$Image(filtCollect$sort("CLOUD_COVER")$first())   #
# properties <- ee_print(image)

id <- eedate_to_rdate(image$get("system:time_start"))

first <- Map$addLayer(image, visParams,  as.character(as.Date(id)))
# Map$centerObject(filtCollect$first())
Map$centerObject(image, 14)

# coastline Ponts
coastlines_selection <-subset(coastlines, coastlines$DATE_ACQUIRED == as.character(as.Date(id)) &
                                coastlines$coastDist >= 0)

coastlines_selection_2 <-subset(coastlines, coastlines$DATE_ACQUIRED >= as.character(as.Date(min(dates))) &
                                  coastlines$DATE_ACQUIRED <= as.character(as.Date(max(dates))) &
                                coastlines$coastDist >= 0)

coastlines_selection[order(as.character(coastlines_selection$pos)),]

# plot Map
first  + mapview(coastlines_selection_2, col.regions = c("orange"), 
                 layer.name = 'obs within 90 days') +
  mapview(coastlines_selection, col.regions = c("blue"), 
                layer.name = paste0(as.character(as.Date(id)), ' points')) +
  mapview(transects)

#---------------------------
#'
#' now multi temporal
#' 
#---------------------------
allFiles_gt0 <- subset(arrange(allFiles, pos, DATE_ACQUIRED), coastDist >=0)

# # re-calculate median value per year, per transect position
# allFiles_gt0 <- allFiles_gt0 %>% group_by(pos, year_col, coast_outlier) %>%
#   mutate(coast_median = median(coastDist))

# set outlier median dist to NA
# allFiles_gt0$coast_median[allFiles_gt0$coast_outlier == 0] <- NA
# 
# allFiles_gt0$coast_median[is.na(allFiles_gt0$coast_median)] <- 
#   with(allFiles_gt0, ave(coast_median, allFiles_gt0$year_col, 
#                          FUN = function(x) median(x, na.rm = TRUE))
#   )[is.na(allFiles_gt0$coast_median)]


group_dates<-unique(allFiles_gt0$year_col)
group_quart <- unique(allFiles_gt0$quarterly_col)
group_pos <- unique(allFiles_gt0$pos)


# # get for all transects an coastline observation near
## reference date as baseline
allFiles_gt0$baseline <- 0 
allFiles_gt0$slope <- -1
allFiles_gt0$baseline2 <- 0
allFiles_gt0$grp <- NA


for (sid in allPos) {
  # sid = 189000
  
  # get a reference distance 
  # e.g. observation closest to reference date OR
  # median observation over 1 year near the reference date 
  subsetAllObs <- subset(allFiles_gt0, allFiles_gt0$pos == sid &
                        allFiles_gt0$coastDist >= 0) 
  nonOutliersAll <- subset(subsetAllObs, coast_outlier == 1)
  
  # nonOutliersAll$grp[nonOutliersAll$coast_outlier == 1]
  # 
  idx <- which(allFiles_gt0$pos == sid &
                 allFiles_gt0$coastDist >= 0)
  
  # you'd want to normalize for the coastline position around the reference date
  # get first date after reference date:
  index <- which.min(abs(as.Date(nonOutliersAll$DATE_ACQUIRED)-reference_date))
  
  coastObs <- subsetAllObs[index, 'coastDist']
  coastObs2 <- subsetAllObs[index, 'coast_median']

  allFiles_gt0$baseline[idx] <- as.numeric(coastObs)
  allFiles_gt0$baseline2[idx] <- as.numeric(coastObs2) # median val
  
  # group by 5/10/15?? non Outlier observation
  nonOutliersAll <- nonOutliersAll %>% 
    dplyr::group_by(DATE_ACQUIRED) %>% 
    as_tibble() %>% 
    dplyr::mutate(grp = floor(1 + (row_number() - 1) / 5)) #%>%
    # pull(grp) # if needed as seperate vector?

  groups_of_obs <- unique(nonOutliersAll$grp)
  
  # probably move this to pre-processing
  # also the slope is not a very visual signal yet. 
  # It is a good indication of magnitude though
  
  # for every 5 observation:
  for (qdate in groups_of_obs){
    # qdate <- groups_of_obs[2]
    
    # observation dates
    obs_dates <- as.Date(nonOutliersAll[nonOutliersAll$grp == qdate, ]$DATE_ACQUIRED)
    
    # indices inside original data.frame
    i <- which(allFiles_gt0$pos == sid & as.Date(allFiles_gt0$DATE_ACQUIRED) 
               %in% as.Date(obs_dates)) # create a logical index
  
    subsetPos <- unique(subset(allFiles_gt0, allFiles_gt0$pos == sid &
                  as.Date(allFiles_gt0$DATE_ACQUIRED) %in% as.Date(obs_dates) &
                  allFiles_gt0$coastDist >= 0)) 
    
    # plot(as.Date(subsetPos$DATE_ACQUIRED), subsetPos$coastDist,
    #      xlab="DATE_ACQUIRED", ylab="coastDist [m]",
    #      main = paste0('coastline position: ',twoD_pos, ' [m]'))
    # points(as.Date(subsetPos[subsetPos$coast_outlier == 0, 'DATE_ACQUIRED']),
    #        subsetPos[subsetPos$coast_outlier == 0, 'coastDist'],
    #        col = 'red')

    outliers <- subset(subsetPos, coast_outlier == 0)
    nonOutliers <- subset(subsetPos, coast_outlier == 1)
    
    if(nrow(nonOutliers) <2){
      coastObs <- 0 # if there is no observation in the transect set coast & slope to 0
      slope <- NA
    } else {
    
      # calculate linear fit
      lm.out <- lm(nonOutliers$coastDist~as.numeric(as.Date(nonOutliers$DATE_ACQUIRED)))
      intercept <-lm.out$coefficients[1]
      slope <- round(lm.out$coefficients[2], 5)

      # resid <- lm.out$residuals
      # maxResid <- which.max(abs(resid))
      estimated <- intercept + (as.numeric(as.Date(nonOutliers$DATE_ACQUIRED))*slope)

      # plot(as.Date(subsetPos$DATE_ACQUIRED), subsetPos$coastDist, ylim = c(min(subsetPos$coastDist)-30,max(subsetPos$coastDist)+ 30))
      # points(as.Date(outliers$DATE_ACQUIRED), outliers$coastDist, col = 'red')
      
      # plot the fitted line
      # abline(lm(nonOutliers$coastDist~as.numeric(as.Date(nonOutliers$DATE_ACQUIRED))),lty = 2)
      
      }
    
   
    allFiles_gt0$slope[i] <- as.numeric(slope)
  }
}


# subtract each that values from each obs (ensure positive values)
# to normalize
allFiles_mutate <- allFiles_gt0 %>% mutate(year = year(DATE_ACQUIRED),
                                               month = month(DATE_ACQUIRED, label=TRUE),
                                               day = day(DATE_ACQUIRED),
                                               full_date= date(DATE_ACQUIRED),
                                               years = date(quarterly_col))

# subtract baseline value from original
allFiles_mutate$normalized <- allFiles_mutate$coastDist - allFiles_mutate$baseline
allFiles_mutate$normalized2 <- allFiles_mutate$coastDist - allFiles_mutate$baseline2
# subtract baseline value from median
# allFiles_mutate$mn_normalized <- allFiles_mutate$mn - allFiles_mutate$baseline



# test simple 2d plot 
twoD_pos <- 299000
subset2d_for_testPlot <- subset(allFiles_mutate, pos == twoD_pos)

# filter outliers & negative coastal distances
# doesn't make the figure more readable... Exclude for now 
# allFiles_mutate <- allFiles_mutate %>% 
  # filter(!(coastDist == -1) & outlier == 0) 
plot(as.Date(subset2d_for_testPlot$DATE_ACQUIRED), subset2d_for_testPlot$coastDist,
     xlab="alongshore position", ylab="mudbank distance [m]",
     main = paste0('position: ',twoD_pos, ' [m]'))
points(as.Date(subset2d_for_testPlot[subset2d_for_testPlot$coast_outlier == 0, 'DATE_ACQUIRED']),
       subset2d_for_testPlot[subset2d_for_testPlot$coast_outlier == 0, 'coastDist'],
       col = 'red')
points(as.Date(subset2d_for_testPlot$DATE_ACQUIRED),  subset2d_for_testPlot$coast_median,
       col = 'blue')

plot(as.Date(subset2d_for_testPlot$DATE_ACQUIRED[!is.na(subset2d_for_testPlot$slope)]),
     subset2d_for_testPlot$slope[!is.na(subset2d_for_testPlot$slope)])
abline(0, 0)

outliers <- to_spatial_df(subset(subset2d_for_testPlot, coast_outlier == 0), 
                          'coastX', 'coastY')
nonOutliers <- to_spatial_df( subset(subset2d_for_testPlot, coast_outlier == 1), 
                              'coastX', 'coastY')

first +
  mapview(nonOutliers, col.regions = c("blue"),layer.name = 'nonOutliers') +
  mapview(outliers, col.regions = c("red"),layer.name = 'outliers') +
  mapview(transects)

# allFiles_mutate$pos <- factor(allFiles_mutate$pos,levels=rev(unique(allFiles_mutate$pos)))

p <-ggplot(allFiles_mutate,aes(x = pos,y = as.Date(quarterly_col), fill=normalized2))+ 
  geom_tile(color= "white",size=0.1) +
  # scale_fill_gradient2(low="red", mid="white", breaks = c(-100,0,100),
                        # high="blue", midpoint =0)
  scale_fill_gradient2(limits = c(-1000,1000), breaks = c(-800, -400, 0, 400, 800),
                       low = "#a50026", high = "#313695", mid = '#f7f7f7',
                       # na.value = "grey50",
                        guide = guide_colourbar(nbin=100, draw.ulim = FALSE,
                                                draw.llim = FALSE),
                       oob=squish) +
  labs(y = 'Date', x = 'position') +
  scale_x_reverse() +
  # geom_segment(data = data.frame(x = pos, 
  #                                xend= pos, 
  #                                y=reference_date,
  #                                yend=reference_date),
  #              aes(x=x, y=y, xend=xend, yend=yend),
  #              linetype="dashed") +
  geom_hline(yintercept = reference_date, linetype="dashed") +
  # geom_segment(y=reference_date, yend = reference_date, linetype="dashed",
               # size = 1,
               # x=300000, xend=100000) + # doesn't work after applying reverse?
  geom_text(aes(max(pos)+1000,reference_date,label = 'reference date'),
            vjust = -2)+
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
        
        # legend.position = c(.78, .5),
        panel.grid.major = element_blank(), # remove grid lines
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        plot.background = element_rect(fill = '#d9d9d9'))

p

# plot P shows the max distance of the coastline compared to a reference date.
# this might become problematic when time series are increasing because:
# dist might still be positive while actually the trend is negative 
# also vice versa, increasing coastlineDist while the abs value is still negative.
# different shades help, but don't solve it completely. 
# try to use the coeffients from linear fits per 3 months?


























