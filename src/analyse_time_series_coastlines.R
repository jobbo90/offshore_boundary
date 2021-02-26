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

years <- seq(from = 1985, to = 2002, by = 1)

# near river mouths estimates for coastlines in old version of GEE script are 
# questionable, should partially be solved in newest versions (11-2-2021)
posToExclude <- c(seq(138000,147000,1000),
                  seq(241000, 255000, 1000))  
  
  #c('2015', '2016', '2017','2018', '2019', '2020') # c('2005','2006','2007', '2008', '2009')

reference_date <- as.Date("1993-01-01")

aoi <- c('229_56', '228_56') # path_row

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
filtered <- unique(filtered)

# bind_rows!!!
allFiles <- do.call(bind_rows, lapply(as.matrix(filtered)[,1], function(x) read.csv(x, stringsAsFactors = FALSE,
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

coast_spatial <- sp_pnt_ee(coastlines_selection$coastX,
                           coastlines_selection$coastY,  paste0(as.character(as.Date(id)), ' points'),
                           "#e34a33")

coast_spatial2 <- sp_pnt_ee(coastlines_selection_2$coastX,
                           coastlines_selection_2$coastY,  'obs within 90 days',
                           "#ece7f2")

# plot Map
# combination seems to be broken?s
first  +coast_spatial2+ coast_spatial 
  

# mapview(transects)

#---------------------------
#'
#' now multi temporal
#' 
#---------------------------

# drops NA values
allFiles_dropNA <- subset(arrange(allFiles, pos, DATE_ACQUIRED), !is.na(coastDist))
# consider to not drop them but fill them with median value of that year?
# probably only necessary when all observations need to be maintained in matrix
# in case of calculating rolling average etc. 

# drop POS near  river mouths
# 139000 - 147000 (suriname Rivier)
# 242000 -252000  (saramacca rivier / coppename)
allFiles_dropNA <- subset(allFiles_dropNA, !(pos %in% posToExclude))

group_dates<-unique(allFiles_dropNA$year_col)
group_quart <- unique(allFiles_dropNA$quarterly_col)
group_pos <- unique(allFiles_dropNA$pos)


# # get for all transects an coastline observation near
## reference date as baseline
allFiles_dropNA$baseline <- 0
# allFiles_dropNA$slope <- -1
# 
allFiles_dropNA$baseline2 <- 0
# allFiles_dropNA$grp <- NA

for (sid in allPos) {
  # sid = 189000
  
  # get a reference distance 
  # e.g. observation closest to reference date OR
  # median observation over 1 year near the reference date 
  subsetAllObs <- subset(allFiles_dropNA, allFiles_dropNA$pos == sid &
                        allFiles_dropNA$coastDist >= 0) 
  nonOutliersAll <- subset(subsetAllObs, coast_outlier == 1)
  
  # nonOutliersAll$grp[nonOutliersAll$coast_outlier == 1]
  # 
  idx <- which(allFiles_dropNA$pos == sid &
                 allFiles_dropNA$coastDist >= 0)
  
  # you'd want to normalize for the coastline position around the reference date
  # get first date after reference date:
  index <- which.min(abs(as.Date(nonOutliersAll$DATE_ACQUIRED)-reference_date))
  
  coastObs <- subsetAllObs[index, 'coastDist']
  coastObs2 <- subsetAllObs[index, 'coast_median']

  allFiles_dropNA$baseline[idx] <- as.numeric(coastObs)
  allFiles_dropNA$baseline2[idx] <- as.numeric(coastObs2) # median val
  
  # probably move this to pre-processing
  # also the slope is not a very visual signal yet. 
  # It is a good indication of magnitude though
  
  # group by 5/10/15?? non Outlier observation
  # nonOutliersAll <- nonOutliersAll %>% 
  #   dplyr::group_by(DATE_ACQUIRED) %>% 
  #   as_tibble() %>% 
  #   dplyr::mutate(grp = floor(1 + (row_number() - 1) / 5)) #%>%
    # pull(grp) # if needed as seperate vector?

  # groups_of_obs <- unique(nonOutliersAll$grp)

  # for every so many observations 5 observation:
  # determine slope
  # for (qdate in groups_of_obs){
    # qdate <- groups_of_obs[2]
    
    # observation dates
    # obs_dates <- as.Date(nonOutliersAll[nonOutliersAll$grp == qdate, ]$DATE_ACQUIRED)
    # 
    # # indices inside original data.frame
    # i <- which(allFiles_dropNA$pos == sid & as.Date(allFiles_dropNA$DATE_ACQUIRED) 
    #            %in% as.Date(obs_dates)) # create a logical index
    # 
    # subsetPos <- unique(subset(allFiles_dropNA, allFiles_dropNA$pos == sid &
    #               as.Date(allFiles_dropNA$DATE_ACQUIRED) %in% as.Date(obs_dates) &
    #               allFiles_dropNA$coastDist >= 0)) 
    # 
    # 
    # 
    # outliers <- subset(subsetPos, coast_outlier == 0)
    # nonOutliers <- subset(subsetPos, coast_outlier == 1)
    # 
    # if(nrow(nonOutliers) <2){
    #   coastObs <- 0 # if there is no observation in the transect set coast & slope to 0
    #   slope <- NA
    # } else {
    # 
    #   # calculate linear fit
    #   lm.out <- lm(nonOutliers$coastDist~as.numeric(as.Date(nonOutliers$DATE_ACQUIRED)))
    #   intercept <-lm.out$coefficients[1]
    #   slope <- round(lm.out$coefficients[2], 5) # change per unit of x (=days)
    #   
    #   m_per_year <- slope*365
    # 
    #   # resid <- lm.out$residuals
    #   # maxResid <- which.max(abs(resid))
    #   # estimated <- intercept + (as.numeric(as.Date(nonOutliers$DATE_ACQUIRED))*slope)
    #   
    #   
    #   # plot(as.Date(subsetPos$DATE_ACQUIRED), subsetPos$coastDist,
    #   #      xlab="DATE_ACQUIRED", ylab="coastDist [m]",
    #   #      main = paste0('coastline position: ',sid, ' [m]'),
    #   #      ylim = c(min(subsetPos$coastDist)-30,max(subsetPos$coastDist)+ 30))
    #   # points(as.Date(subsetPos[subsetPos$coast_outlier == 0, 'DATE_ACQUIRED']),
    #   #        subsetPos[subsetPos$coast_outlier == 0, 'coastDist'],
    #   #        col = 'red')
    #   
    #   # plot the fitted line
    #   # abline(lm(nonOutliers$coastDist~as.numeric(as.Date(nonOutliers$DATE_ACQUIRED))),lty = 2)
    #   
    #   }
    # 
    # 
    # allFiles_dropNA$slope[i] <- as.numeric(m_per_year)
  #}
}

allFiles_mutate <- allFiles_dropNA %>% mutate(year = year(DATE_ACQUIRED),
                                               month = month(DATE_ACQUIRED, label=TRUE),
                                               day = day(DATE_ACQUIRED),
                                               full_date= date(DATE_ACQUIRED),
                                               years = date(quarterly_col))

# subtract baseline value from original
# Baseline is either a observation close to reference date. (baseline)
# Or the annual median observation of they reference date (baseline2)
allFiles_mutate$normalized <- allFiles_mutate$coastDist - allFiles_mutate$baseline
allFiles_mutate$normalized2 <- allFiles_mutate$coastDist - allFiles_mutate$baseline2

# test simple 2d plot 
twoD_pos <- 299000#299000
subset2d_for_testPlot <- subset(allFiles_mutate, pos == twoD_pos)

# filter outliers & negative coastal distances
# doesn't make the figure more readable... Exclude for now 
# allFiles_mutate <- allFiles_mutate %>% 
  # filter(!(coastDist == -1) & outlier == 0) 

runnAve <- data.frame(dated=as.Date(subset2d_for_testPlot$DATE_ACQUIRED), 
                     Color = zoo::rollmean(subset2d_for_testPlot$locf, 10, 
                                           fill = NA),
                     col2 = zoo::rollmean(subset2d_for_testPlot$coastDist, 5, 
                                          fill = NA),
                     col3 = zoo::rollmean(subset2d_for_testPlot$coast_median, 1, 
                                          fill = NA))

# plot temporal evolution for given transect

# now in ggplot form
subset2d_for_testPlot$DATE_ACQUIRED <- as.Date(subset2d_for_testPlot$DATE_ACQUIRED)

ggplot(subset2d_for_testPlot, aes(x= DATE_ACQUIRED, y = coastDist)) + # color=coast_outlier)
  geom_point(size = 3, color = "white") +
  geom_point(aes(colour = factor(coast_outlier)), alpha = 0.5) +
  scale_x_date(labels = date_format("%Y")) +
  labs(x = "year", y = "Distance coastline position") +
  # geom_smooth(mapping = aes(x= DATE_ACQUIRED, y = coastDist)) +
  geom_line(data=runnAve, aes(dated, col3)) +
  scale_color_manual(name = "outlier",
                     values = c("black", "black"),
                     labels = c("coastal distance", "outlier")) +
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




outliers <- sp_pnt_ee(subset(subset2d_for_testPlot, coast_outlier == 0)$coastX,
                      subset(subset2d_for_testPlot, coast_outlier == 0)$coastY,  'outliers',
                      "red")

nonOutliers <- sp_pnt_ee(subset(subset2d_for_testPlot, coast_outlier == 1)$coastX,
                         subset(subset2d_for_testPlot, coast_outlier == 1)$coastY,  'nonOutliers',
                         "blue")
first + outliers + nonOutliers


# plot annual coastline change
# pre-requisites: 
# groups of angles

ggplot(data=allFiles_mutate, aes(bearing)) +
  geom_histogram(binwidth = 2)

angles <-  c(0,5,10,15,20,180, 290, 
             300, 310, 320, 330, 335, 340, 345, 350, 355, 360)

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

level_order <- c("(180,290]","(290,300]", "(300,310]", "(310,320]", "(320,330]", 
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

#plot bearing over rate of change
# if facet wrap enabled, per 4 yer timestep
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



p <-ggplot(allFiles_mutate,aes(x = pos,y = as.Date(quarterly_col), fill=slope))+ 
  geom_tile(color= "white",size=0.1) +
  # scale_fill_gradient2(low="red", mid="white", breaks = c(-100,0,100),
                        # high="blue", midpoint =0)
  scale_fill_gradient2(limits = c(-300,300), breaks = c(-300, -150, 0, 150, 300),
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

kustlijn <- readOGR('D:/BackUp_D_mangroMud_202001/Site1_Suriname_all/Analysis/IntertidalArea/Coastlines',
                              'class10_line_v5')

shapefile_df <- fortify(kustlijn)

mapped <- ggplot() +
  geom_path(data = shapefile_df, 
            aes(x = long, y = lat, group = group))

map_projected <- mapped +
  coord_map()

p + map_projected + plot_layout(ncol = 1, nrow = 2, heights = c(1,0.5))

# plot P shows the max distance of the coastline compared to a reference date.
# this might become problematic when time series are increasing because:
# dist might still be positive while actually the trend is negative 
# also vice versa, increasing coastlineDist while the abs value is still negative.

# try to use the slope, fine tune how many observations are needed for slope?
# at the moment it is 5 observations which might be a bit small.
# or consider both the absolute distance plot ==> order of magnitude
# and the slope ==> moment of change + magnitude


























