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
dataFolder <- './data/processed/offshore_points'
# dataFolder <- './data/processed/offshore_points/old'

mapviewOptions(basemaps = c( "Esri.WorldImagery","Esri.WorldShadedRelief", "OpenStreetMap.DE"))
leaflet() %>%
  addProviderTiles("Esri.WorldImagery")


aoi <- c('FrenchGuiana', 'Suriname', 'Guyana')  #'FrenchGuiana', 'Suriname', 'Guyana'
years <- seq(from = 1985, to = 2021, by = 1)

# near river mouths estimates for coastlines in old version of GEE script are 
# pos to exlcude for mudbank boundary estimates / outlier detection
posToExcludeSUR <- c(
  seq(130000,137000,1000), # coppename
  seq(234000, 243000, 1000)) # Suriname River

posToExcludeFG <- c(
  seq(261000,270000,1000), # approuage River
  seq(315000,334000,1000),# baia oiapoque 
  seq(223000,225000,1000), # orapu
  seq(205000,207000,1000), # cayenne
  seq(335000,403000,1000) # Brazil
) 
posToExcludeGUY <- c(
  seq(0,39000,1000), # Venezuela
  seq(527000,532000,1000), # Courantyne River
  seq(460000, 462000,1000),# berbice River
  seq(364000,365000,1000), # demerara River
  seq(294000,345000,1000), # Essequibo River delta
  seq(72000,74000,1000) # waini River
) 

allPos <- list('Suriname' = posToExcludeSUR,
               "FrenchGuiana" = posToExcludeFG,
               "Guyana" = posToExcludeGUY )

# groups for Suriname
posWest <- c(seq(0 ,130000,1000))
posEast <- c(seq(234000 , 380000 ,1000))
posCentre <- c(seq(131000 , 233000 ,1000))
surList = list('posWest' = posWest,
               'posEast' = posEast,
               'posCentre' = posCentre)

# groups For Guyana
Barima <- c(seq(0 ,223000,1000))
supernaam <- c(seq(224000 , 316000 ,1000))
georgetown <- c(seq(317000 , 460000 ,1000)) # georgetow - berbice
berbice <- c(seq(461000, 532000, 1000)) # berbice - corenteyne

guyList = list('Barima' = Barima,
               'supernaam' = supernaam,
               'georgetown' = georgetown,
               'berbice' = berbice)

# groups for french-guiana
maroni <- c(seq(0 ,110000,1000)) # maroni sinamary
sinermary <- c(seq(111000 , 207000 ,1000)) # sinermary - cayenne
cayenne <- c(seq(208000 , 403000 ,1000)) # cayene - brazil

fgList = list('maroni' = maroni,
              'sinermary' = sinermary,
              'cayenne' = cayenne)

# posToExclude <- allPos[[aoi]]
reference_date <- as.Date("2006-01-01")
normalize_date <- as.Date("1986-01-01")

# select folders
folderSelect <- as.matrix(list.files(paste0(dataFolder), full.names = T))
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

# Guyana <- filtered %>% 
#   filter(str_detect(text,'Guyana')) 
# FrenchGuiana <- filtered %>% 
#   filter(str_detect(text,'French'))
# Suriname <- filtered %>% 
#   filter(str_detect(text,'Suriname'))
# 

# allFiles = bind_rows(allFilesFG, allFilesSur,allFilesGuy)
allFiles <- unique(do.call(rbind, lapply(as.matrix(filtered)[,1],
                                         function(x) read.csv(x,
                                                              stringsAsFactors = FALSE,
                                                              sep = ',',
                                                              na.strings=c("","NA")
                                         ))))

# remove duplicate observations to have accurate stats
# these are the triplicates?
allObsFilter <- allFiles %>% 
  group_by(as.Date(DATE_ACQUIRED), Country, pos) %>% 
  filter(row_number(pos) == 1) %>%
  ungroup()

allObsFilter <- type_convert(allObsFilter)

# duplicates? 
allFilesDuplicates <- allObsFilter %>% 
  group_by_at(vars(Country, DATE_ACQUIRED, pos)) %>%  # , collectiontype
  filter(n()>1) %>% 
  ungroup()


# keep_columns <- c('axisDist', 'mudFract', 'endDrop')  # necessary for mudbank output
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

collection <- collectionL8$merge(collectionL5)$#merge(collectionL7)$
  filter(ee$Filter$lt("CLOUD_COVER", 30))
  
# ee_print(filtCollect)

filtCollect <- collection$filterDate(as.character(reference_date-365), as.character(reference_date+365))$
  filterBounds(ee$Geometry$Point(-55.54, 5.94))
dates <- ee_get_date_ic(filtCollect, time_end = FALSE)[,2]
example_date <- as.Date(dates)

# properties <- ee_print(image)
selectImage <- filtCollect$first()
id <- eedate_to_rdate(selectImage$get("system:time_start"))
first <- Map$addLayer(selectImage, visParams,  as.character(as.Date(id)))

Map$centerObject(selectImage, 14)

# # coastline Ponts
# coastlines_selection <-subset(coastlines, as.Date(coastlines$DATE_ACQUIRED) == 
#                                       as.Date('2009-11-30'))
# # coastlines_selection <- subset(coastlines_selection, select = -c(geometry))
# coastlines_selection <- coastlines_selection[ ,-which(names(coastlines_selection) == 'geometry')]
# 
# coastlines_selection_sp <- SpatialPointsDataFrame(data.frame(
#   coastlines_selection$coastX, coastlines_selection$coastY),
#   proj4string=CRS("+proj=longlat +datum=WGS84"),
#   data = data.frame(coastlines_selection$DATE_ACQUIRED))
# mapview(coastlines_selection_sp)

# shapefile(coastlines_selection_sp, filename = 
#             paste0(wd,'/data/raw/shapes/coastline_', gsub('-', '', example_date), '.shp'))
 
# writeOGR(obj=coastlines_selection_sp[,-()], dsn=paste0(wd,'/data/raw/shapes'), 
#          layer=paste0("coastline_", gsub('-', '', reference_date)), 
#          driver="ESRI Shapefile")


#---------------------------
#'
#' now multi temporal
#' 
#---------------------------

# drop POS near  river mouths
# allFiles_dropPOS <- subset(allFiles, !(pos %in% posToExclude))
allFiles_dropPOS <- allObsFilter %>%
  dplyr::mutate(toFilter = 0) %>%
  dplyr::mutate(toFilter = ifelse((Country == "Suriname" & 
                                    pos %in% posToExcludeSUR),1,toFilter),
                toFilter = ifelse((Country == "FrenchGuiana" & 
                                    pos %in% posToExcludeFG),1,toFilter),
                toFilter = ifelse((Country == "Guyana" & 
                                    pos %in% posToExcludeGUY),1,toFilter)) %>%
  filter(toFilter == 0) %>%
  dplyr::select(-c(toFilter))

# # get for all transects an coastline observation near
## reference date as baseline
allFiles_dropPOS$baseline <- 0
allFiles_dropPOS$baseline2 <- 0

allFiles_refDate <- allFiles_dropPOS %>%
  dplyr::mutate(nonOutlier = ifelse(coast_outlier == 1 & coastDist > 0 & 
                                      !(is.na(coastDist)), 1, 0)) %>%
  dplyr::group_by(Country, pos, nonOutlier) %>% 
  
  # nearestDate
  dplyr::mutate(nearestDate =  as.Date(DATE_ACQUIRED[
    which.min(abs(as.Date(DATE_ACQUIRED)-reference_date))])) %>%
  
  # corresponding coast median values assigned as baseline
  dplyr::mutate(baseline =  coast_median[which.min(abs(as.Date(DATE_ACQUIRED)-reference_date))]) %>%
  dplyr::mutate(baseline2 =  coast_median[which.min(abs(as.Date(DATE_ACQUIRED)-normalize_date))]) %>%
  
  # overwrite for entire group (country, pos) the baseline values
  dplyr::group_by(Country, pos) %>%
  dplyr::mutate(
    nearestDate = ifelse(nonOutlier == 0, NA, nearestDate),
    baseline2 = ifelse(nonOutlier == 0, NA, baseline2),
    baseline = ifelse(nonOutlier == 0, NA, baseline)) %>%
  
  #overWrite NA values with most occuring group value
  dplyr::mutate(
    nearestDate = na.aggregate(nearestDate, FUN=Mode),
    baseline = na.aggregate(baseline, FUN=Mode),
    baseline2 = na.aggregate(baseline2, FUN=Mode)) %>%
  # remaining NA values are posToExclude (rivermouths, there is no median computed)
  
  # unique ID for each combination of ID and Pos  
  dplyr::mutate(countryPos = paste0(Country,pos)) %>%
  
  ungroup()
  

# mutate the dataframe 
# add date properties as seperate columns
# allFiles_mutate <- allFiles_refDate %>%
#   dplyr::mutate(year = year(DATE_ACQUIRED),
#                 month = month(DATE_ACQUIRED, label=TRUE),
#                 day = day(DATE_ACQUIRED),
#                 full_date= date(DATE_ACQUIRED))

# subtract baseline value from original
# Baseline is either a observation close to reference date. (baseline)
# Or the annual median observation of they reference date (baseline2)
allFiles_refDate$normalized <- as.numeric(allFiles_refDate$coastDist) - 
                              as.numeric(allFiles_refDate$baseline)
allFiles_refDate$normalized2 <- as.numeric(allFiles_refDate$coastDist) - 
                               as.numeric(allFiles_refDate$baseline2)

#################################
#' 
#' test simple 2d plot 
#' 
#################################
twoD_pos <- 299000
subset2d_for_testPlot <- subset(allFiles_refDate, 
                                Country == 'Suriname' & pos == twoD_pos) 

# unique(allFiles_refDate$pos)
# plot temporal evolution for given transect
# now in ggplot form
# subset2d_for_testPlot$DATE_ACQUIRED <- as.Date(subset2d_for_testPlot$DATE_ACQUIRED)
# subset2d_for_testPlot$year_col <- as.Date(subset2d_for_testPlot$year_col)
# subset2d_for_testPlot$coast_median <- as.numeric(levels(subset2d_for_testPlot$coast_median))[subset2d_for_testPlot$coast_median]


ggplot(subset2d_for_testPlot, aes(x= DATE_ACQUIRED, y = as.numeric(coastDist))) + # color=coast_outlier)
  # geom_line(aes(y=coast_median),size=2, linetype= "dotted") + # add median dtrendline?
  geom_line(inherit.aes = FALSE, aes(x = DATE_ACQUIRED, y = as.numeric(coast_median)),
                                     # linetype = as.factor(pos)),
            alpha = 0.5, size = 1.2) +
  
  geom_point(size = 3, aes(colour = as.factor(coast_outlier)), alpha = 0.6) + # collectiontype
  # facet_wrap(~Country) +
  scale_color_manual(name = "Legend",
    values = c('red', 'blue'),
    labels = c("outlier", "distance")) +
  # ylim(min(subset2d_for_testPlot$coastDist, na.rm = T),
        # max(subset(subset2d_for_testPlot, coast_outlier != 0)$coastDist,  na.rm = T)) +
  scale_x_date(labels = date_format("%Y")) +
  ggtitle( paste0('position: ', twoD_pos)) +
  guides(color=guide_legend(override.aes=list(fill='#d9d9d9'), ncol = 1)) +
  labs(x = "Year", y = "coastline position [m]") +
  theme(axis.line.x = element_line(size = 0.5, colour = "black"),
        axis.line.y = element_line(size = 0.5, colour = "black"),
        axis.line = element_line(size= 1, colour = "black"),
        axis.title.y = element_text(size = 20, face = 'bold'),
        axis.title.x = element_text(size = 20, face = 'bold'),
        axis.text.x = element_text(size = 15,  hjust = .5, vjust = .5),
        axis.text.y = element_text(size = 15, hjust = .5, vjust = .5),
        
        legend.title = element_blank(),
        legend.background = element_rect(fill = alpha('grey50',0.8)),
        plot.title = element_text(hjust = 0.5, size = 20, face = 'bold',
                                  vjust = -2), 
        legend.text = element_text(size = 20),
        legend.position = c(.8, .6),

        panel.grid.major = element_blank(), # remove grid lines
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        plot.background = element_rect(fill = NA))

# ggsave(filename = paste0("./results/methodology_figures/twoD_distance_",
#                          '_pos',twoD_pos,'_',  format(Sys.Date(), "%Y%m%d"),'.jpeg'),
#        width = 20.1, height = 10.25, units = c('in'), dpi = 1200)





# outliers <- sp_pnt_ee(as.numeric(subset(subset2d_for_testPlot, coast_outlier == 0)$coastX),
#                       as.numeric(subset(subset2d_for_testPlot, coast_outlier == 0)$coastY),  'outliers',
#                       "red")
# 
# nonOutliers <- sp_pnt_ee(as.numeric(subset(subset2d_for_testPlot, coast_outlier == 1)$coastX),
#                          as.numeric(subset(subset2d_for_testPlot, coast_outlier == 1)$coastY),  'nonOutliers',
#                          "blue")
# 
# aoiCollect <- collection$filterBounds(ee$Geometry$Point(
#   median(as.numeric(subset(subset2d_for_testPlot, coast_outlier == 1 & coastX != -1)$coastX), na.rm = T),
#   median(as.numeric(subset(subset2d_for_testPlot, coast_outlier == 1 & coastX != -1)$coastY), na.rm = T)))
# 
# imageAOI <- ee$Image(aoiCollect$sort("CLOUD_COVER")$first())   #
# 
# # properties <- ee_print(imageAOI)
# 
# idAOI <- eedate_to_rdate(imageAOI$get("system:time_start"))
# 
# firstAOI <- Map$addLayer(imageAOI, visParams,  as.character(as.Date(idAOI)))
# 
# # plot view
# test2 <- firstAOI + nonOutliers + outliers
# setView(test2, subset(subset2d_for_testPlot, coast_outlier == 1)$coastX[1], 
#         subset(subset2d_for_testPlot, coast_outlier == 1)$coastY[1], 14, options = list())

#################################
#' 
#' Alongshore variability in coastline position
#' 
#################################

range <- round(quantile(as.numeric(allFiles_refDate$deltaCoast),c(0.05, 0.95), na.rm=T))

outputName <- ifelse(length(aoi) > 1, 
                     'Guianas', aoi)

HovMollerPlot = subset(allFiles_refDate, !is.na(deltaCoast))

p <-ggplot(transform(HovMollerPlot,
                     Country=factor(Country,levels=c("FrenchGuiana","Suriname","Guyana"))), 
           aes(x = pos,y = as.Date(year_col), fill=as.numeric(deltaCoast))) + 
  
  geom_tile(size=0.1, na.rm = TRUE) +
  facet_wrap(~Country, ncol = 1, nrow = 3) +

  scale_fill_gradientn(name = 'change [m/yr] \n',
                     breaks = c(range[[1]], range[[1]]/2, 0, range[[2]]/2, range[[2]]),
                     limits = c(range[[1]],range[[2]]),
                     colours = c('#7b3294', '#f7f7f7', "#008837"),
                     guide = guide_colourbar(nbin=100, draw.ulim = FALSE, draw.llim = FALSE),
                     oob=squish,
                     values = scales::rescale(c(range[[1]],range[[1]]/2 ,0, range[[2]]/2, range[[2]]))) +
  # geom_vline(xintercept = 238000, color= 'red',linetype="dashed") +
  labs(y = 'Year', x = 'Alongshore Posistion (East - West) [km]') + 
  scale_x_continuous(expand = c(0,0),labels = unit_format(unit = "", scale = 0.001)) + #
  theme(axis.line.x = element_line(size = 0.5, colour = "black"),
        axis.line.y = element_line(size = 0.5, colour = "black"),
        axis.line = element_line(size= 1, colour = "black"),
        axis.title.y = element_text(size = 12, face = 'bold'),
        axis.title.x = element_text(size = 12, face = 'bold'),
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


p

# ggsave(p, filename = paste0("./results/temp_maps/",
#                             outputName, '_1985-2020_coastlineChange_',
#                             format(Sys.Date(), "%Y%m%d"),'.jpeg'),
#        width = 13.1, height = 7.25, units = c('in'), dpi = 1200)



# similar to figure by Pieter Augustinus
# filter outliers!

# augustinus <- ggplot(allFiles_mutate, mapping = aes(x= pos, y = normalized2)) + # color=coast_outlier) / deltaCoast / 
#   scale_y_continuous(limits=c(-5000, 5000)) +
#   geom_point(size = 2, alpha = 0.1, # , color = "black"
#              aes(color = five_year_col))  +
#   scale_color_manual(name = "Legend", 
#                      values = c('#41b6c4','#f768a1','#dd3497','#ae017e','#7a0177','#99d594','#00441b'),
#                      guide = guide_legend(override.aes = list(alpha = 1),
#                                            title = 'cross shore position')) +
#   scale_x_reverse() +   
#   labs(x = "along shore position", y = "Distance coastline position") +
#   theme(axis.line.x = element_line(size = 0.5, colour = "black"),
#         axis.line.y = element_line(size = 0.5, colour = "black"),
#         axis.line = element_line(size= 1, colour = "black"),
#         axis.title.y = element_text(size = 14, face = 'bold'),
#         axis.title.x = element_text(size = 14, face = 'bold'),
#         axis.text.x = element_text(size = 12,  hjust = .5, vjust = .5),
#         axis.text.y = element_text(size = 12, hjust = .5, vjust = .5),
#         legend.title = element_text(colour = 'black', size = 14, face = "bold"),
#         plot.title = element_text(hjust = 0.5, size = 18, face = 'bold',
#                                   vjust = -5), 
#         panel.grid.major = element_blank(), # remove grid lines
#         panel.grid.minor = element_blank(), 
#         panel.background = element_blank(),
#         plot.background = element_rect(fill = '#d9d9d9'))
#   



#' 
#' 
#' alongshore variability Guianas 
#' And Suriname
#' 
#' 
# posWest <- c(seq(0 ,129000,1000))
# posEast <- c(seq(240000 ,max(allFiles_mutate$pos.x),1000))
# 
allFiles_refDate$alongshore = NA
# 

allFiles_mutate <- allFiles_refDate #%>%
  rowwise() %>%
  dplyr::mutate(
    # a bit slow but fine for now?
    alongshore = ifelse(Country == "Suriname", 
                        gsub('([0-9])', '',  names(which(unlist(surList)==pos)), perl = T),
                        alongshore),
    alongshore = ifelse(Country == "FrenchGuiana",
                        gsub('([0-9])', '',  names(which(unlist(fgList)==pos)), perl = T),
                        alongshore),
    alongshore = ifelse(Country == "Guyana",
                        gsub('([0-9])', '',  names(which(unlist(guyList)==pos)), perl = T),
                        alongshore)
  )

length(which(is.na(allFiles_mutate$normalized2)))

#get NA's
subsetNA <- subset(allFiles_mutate, is.na(normalized2))

# filter outliers and get 1 observation per year, pos 
subsetVariability <- subset(allFiles_mutate, coast_outlier == 1) %>%
  dplyr::group_by(Country, pos, year_col) %>%
  dplyr::summarize(
    normalized = mean(normalized, na.rm = T),
    normalized2 = mean(normalized2, na.rm = T),
    deltaCoast = mean(deltaCoast, na.rm = T))

subsetVariability <- transform(subsetVariability,
          Country=factor(Country,
                         levels=c("FrenchGuiana","Suriname","Guyana")))

# calculate means
meansOI <- subsetVariability %>%
  # filter(deltaCoast < 250 & deltaCoast > -250) %>%
  dplyr::group_by(Country) %>% # , five_year_col -> include in group_by when face wrap is ggplot is used
  dplyr::summarize(mean=mean(normalized2, na.rm = T),
                   n = n()) %>%
  ungroup()

# overallMean <- mean(subsetVariability$normalized, na.rm = T)
overallMean <- subsetVariability %>%

  # filter(deltaCoast < 250 & deltaCoast > -250) %>%
  dplyr::summarize(mean=mean(normalized2, na.rm = T))

spatialVariability <- ggplot(data = subsetVariability
                     # %>% filter(deltaCoast < 250 & deltaCoast > -250)
       , aes(x=as.numeric(normalized2), fill = Country), alpha = 1) + 
  
  facet_wrap(~Country, ncol = 1, nrow = nrow(meansOI)) +
  geom_vline(data = meansOI, aes(xintercept= mean), size = 3.5, colour = 'black',
             linetype="solid") +
  geom_vline(data = meansOI, aes(xintercept= mean, color=Country), size = 2,
             linetype="solid") +
  geom_vline(data = overallMean, aes(xintercept= mean),color = 'Black', size = 2,
             linetype="dashed") +
  geom_histogram(position = 'identity', binwidth = 25, alpha = 0.8) +
  # scale_color_manual(values = c('Black'= 'black',
  #                               'East' =  "#33a02c",
  #                               'Center' = "#2E9FDF",
  #                               'West' = "#E7B800")) +
  # scale_fill_manual(values = c('East' = "#33a02c",#'#d95f02', #
  #                              'Center' = "#2E9FDF",#'#1b9e77',
  #                              'West' = "#E7B800"))+ #'#7570b3'
  # scale_alpha_manual(values= c('East' = 1,
  #                      'Center' = 1,
  #                      'West' = 1)) +
  geom_text(data = meansOI, aes(label=sprintf("%1.1f", mean),
                                x = c(-200,500,-300),
                                y=c(1500,1500,1500)),
            colour = c('Black', 'Black', 'Black'),
            size = 8, fontface = 'bold', show.legend = FALSE) +

  scale_y_continuous(expand = c(0,0), breaks = c(0, 750, 1500)) +
  # scale_y_log10(name = 'log(obs. count)', expand = c(0,0))+

  scale_x_continuous(breaks = sort(c(-1000, -500,0, 500, 1000,round(overallMean$mean))),
                     limits = c(-1500, 1500)) +
  guides(fill = guide_legend(
    override.aes = list(size = 10, alpha =1,linetype = 0)),colour = F) +
  labs(y = 'Annual coastline position estimates [n]', x = 'Coastline position [m]') +
  
  theme(axis.line.x = element_line(size = 0.5, colour = "black"),
        axis.line.y = element_line(size = 0.5, colour = "black"),
        axis.line = element_line(size= 1, colour = "black"),
        axis.title.y = element_text(size = 20, face = 'bold'),
        axis.title.x = element_text(size = 20, face = 'bold'),
        axis.text.x = element_text(size = 18,  hjust = .5, vjust = .5),
        axis.text.y = element_text(size = 18, hjust = .5, vjust = .5),
        legend.title = element_blank(), #element_text(colour = 'black', size = 14, face = "bold"),
        legend.background = element_rect(fill = '#d9d9d9',  colour = '#d9d9d9'),
        legend.position = c(0.9,0.8),
        legend.text = element_text(size = 20),
        panel.grid.major = element_blank(), # remove grid lines
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        strip.text.x = element_blank(),
        plot.background = element_rect(fill = '#d9d9d9'))

spatialVariability

# ggsave(spatialVariability, filename = paste0("./results/temp_maps/",
#                                              'spatial_variability_', outputName,'_1985-2020',
#                                    '_',  format(Sys.Date(), "%Y%m%d"),'.jpeg'),
       # width = 13.1, height = 7.25, units = c('in'), dpi = 1200)
#
# ggsave(filename = paste0("D:/WOTRO/Research/Reporting/Publications/",
#                          "AlongshoreVariability_mudbanks/submissionFiles/",
#                          "figures/Figure_7.pdf"),
#        width = 13.1, height = 7.25, units = c('in'), dpi = 1200)


countryOI <-c('Suriname')

allObsSuriname = subset(allFiles_mutate,Country == countryOI & coast_outlier == 1) %>%
  dplyr::group_by(Country, pos, year_col) %>%
  dplyr::summarize(normalized2 = mean(normalized2, na.rm = T),
                   deltaCoast = mean(deltaCoast, na.rm = T),
                   alongshore = alongshore[1]) %>%
  ungroup()

allObsSuriname <- allObsSuriname %>%
  dplyr::mutate(alongshore = "Centre") %>%
  dplyr::mutate(alongshore = ifelse(pos %in% posEast, c('East'),alongshore)) %>%
  dplyr::mutate(alongshore = ifelse(pos %in% posWest, c('West'),alongshore))
allObsSuriname$alongshore <- factor(allObsSuriname$alongshore,
                                    levels = c('East','Centre','West'))
# calculate means
meansSur <- allObsSuriname %>%
  dplyr::group_by(alongshore) %>% # , five_year_col -> include in group_by when face wrap is ggplot is used
  dplyr::summarize(mean=mean(normalized2, na.rm = T)) %>%
  ungroup()

# coastline position compared to reference date (if x = normalized)
MeanSu <- allObsSuriname %>%
  dplyr::summarize(mean=mean(normalized2, na.rm = T))

spatialVariabilitySur <- ggplot(data = allObsSuriname, 
                                aes(x=normalized2, fill=alongshore)) + 
  geom_histogram(position = 'identity', binwidth = 25)  +
  
  facet_wrap(~alongshore, ncol = 1, nrow = 3) +
  
  geom_vline(data = meansSur, aes(xintercept= mean), size = 3.5, colour = 'black',
             linetype="solid") +
  geom_vline(data = meansSur, aes(xintercept= mean, color=alongshore), size = 2,
             linetype="solid") +
  geom_vline(data = MeanSu, aes(xintercept= mean),color = 'Black', size = 2,
             linetype="dashed") +
  scale_color_manual(values = c('Black'= 'black',
                                'East' =  "#33a02c",
                                'Centre' = "#2E9FDF",
                                'West' = "#E7B800")) +
  scale_fill_manual(values = c('East' = "#33a02c",
                             'Centre' = "#2E9FDF",
                             'West' = "#E7B800")) +

  scale_alpha_manual(values= c('East' = 1,
                     'Center' = 1,
                     'West' = 1)) +
  geom_text(data = meansSur, aes(label=sprintf("%1.1f", mean),
                                 x = c(1000,1500,-1000),
                                 y=c(250,250,250)),
            colour = c('Black', 'Black', 'Black'), alpha =1,
            size = 8, fontface = 'bold', show.legend = FALSE) +

  scale_y_continuous(expand = c(0,0), breaks = c(0, 100, 200,300)) + 
  scale_x_continuous(breaks = sort(c(-2000, 0, 2000,round(MeanSu$mean))), #
    limits = c(-3000, 3000),
    expand = c(0,0)) +
  guides(fill = guide_legend(override.aes = list(size = 10, alpha =1,
                                                 linetype = 0)),
         colour = F) +
  labs(y = 'Coastline observations [n]', x = 'Coastline position [m]') +
  
  theme(axis.line.x = element_line(size = 0.5, colour = "black"),
        axis.line.y = element_line(size = 0.5, colour = "black"),
        axis.line = element_line(size= 1, colour = "black"),
        axis.title.y = element_text(size = 20, face = 'bold'),
        axis.title.x = element_text(size = 20, face = 'bold'),
        axis.text.x = element_text(size = 18,  hjust = .5, vjust = .5),
        axis.text.y = element_text(size = 18, hjust = .5, vjust = .5),
        legend.title = element_blank(), #element_text(colour = 'black', size = 14, face = "bold"),
        legend.background = element_rect(fill = '#d9d9d9',  colour = '#d9d9d9'),
        # legend.position = c(0.9,0.4),
        legend.text = element_text(size = 20),
        panel.grid.major = element_blank(), # remove grid lines
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        strip.text.x = element_blank(),
        plot.background = element_rect(fill = '#d9d9d9', colour = '#d9d9d9'))

spatialVariabilitySur

# ggsave(spatialVariabilitySur, filename = paste0("./results/temp_maps/",
#                                              'spatial_variability_', countryOI,'_1985-2020',
#                                    '_',  format(Sys.Date(), "%Y%m%d"),'.jpeg'),
#        width = 13.1, height = 7.25, units = c('in'), dpi = 1200)


# get per year stats
allFiles_annual <- subsetVariability %>%
  dplyr::group_by(year_col, Country) %>% # Country / alongshore
  # extra filter? deltaCoast < 500 meter (per jaar?)
  # filter(abs(deltaCoast) < 500) %>%
  dplyr::summarize(
                   deltaCoast = mean(deltaCoast, na.rm = T),
                   normalized =  mean(normalized, na.rm = T),
                   normalized2 =  mean(normalized2, na.rm = T),
                   obsCount = n(),
                   
  ) %>%
  ungroup()

library(ggpubr)


####################### 
#' 
#' Chapter 3: figure 3
#' 
#' Temporal variability in coastline changes 

allFiles_annual <- transform(allFiles_annual,
                             Country=factor(Country, levels=c("FrenchGuiana","Suriname","Guyana")))

# important to use normalized (2006-01-01) and not normalized2 (1985-01-01)
temporalTrend <- ggplot(data = allFiles_annual,
            aes(x=as.Date(year_col),y = normalized, 
                colour = Country
              ), alpha = 1) + 
  
  facet_wrap(~Country, ncol = 1, nrow = 3,  scales = "free_y") +
  geom_smooth(method='lm', se = FALSE, linetype = "dashed",
          formula = y ~ poly(x, 1, raw = TRUE)) + # 1st order polynomial
  geom_point(size = 3, alpha = 0.6) +
  stat_regline_equation(label.x=c(as.Date('1985-01-01'),
                                  as.Date('1985-01-01'),
                                  as.Date('1985-01-01')),
                        label.y=0, color = 'black') +

  labs(y = 'average normalized coastline position [m]', x = 'Year') +
  
  theme(axis.line.x = element_line(size = 0.5, colour = "black"),
        axis.line.y = element_line(size = 0.5, colour = "black"),
        axis.line = element_line(size= 1, colour = "black"),
        axis.title.y = element_text(size = 20, face = 'bold'),
        axis.title.x = element_text(size = 20, face = 'bold'),
        axis.text.x = element_text(size = 18,  hjust = .5, vjust = .5),
        axis.text.y = element_text(size = 18, hjust = .5, vjust = .5),
        legend.title = element_blank(), #element_text(colour = 'black', size = 14, face = "bold"),
        legend.background = element_rect(fill = '#d9d9d9',  colour = '#d9d9d9'),
        legend.position = 'none',#c(0.9,0.3),
        legend.text = element_text(size = 20),
        panel.grid.major = element_blank(), # remove grid lines
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        strip.text.x = element_text(size = 20, face = 'bold'),
        plot.background = element_rect(fill = '#d9d9d9'))

temporalTrend

# ggsave(temporalTrend, filename = paste0("./results/temp_maps/",outputName,
#                                              '_temporalTrendCoastPos_1985-2020',
#                                    '_',  format(Sys.Date(), "%Y%m%d"),'.jpeg'),
#         width = 13.1, height = 7.25, units = c('in'), dpi = 1200)




####################### 
#' 
#'  
#' link coastline changes to climate data
#' 
#' Not produced as figure

climateDataTest <- read.csv('D:/WOTRO/Research/Software/Projects/offshore_boundary/data/raw/MEI_NAO_winter_yrMei.csv',
         stringsAsFactors = FALSE,
         sep = ',',
         na.strings=c("","NA"))

climateDataTest <- type_convert(climateDataTest)

corrTest <- subsetVariability %>%
  dplyr::group_by(Country, year_col) %>%
  dplyr::summarize(mean=mean(deltaCoast, na.rm = T)) %>%
  left_join(climateDataTest, 
            c("year_col" = "date"), keep = T
  ) 
  
plotCorrs <- ggplot(data = corrTest,#%>% filter(Country == countryOI)
                    aes(x=as.numeric(yr_mei), y=as.numeric(mean), color = Country)) +
  facet_wrap(~Country, ncol = 2, nrow = 5) +
  geom_point(alpha =0.5)

plotCorrsScatter <- ggscatter(corrTest, x = "yr_mei", y = "mean", add = "reg.line",
          color = 'Country') +
  facet_wrap(~Country, ncol = 2, nrow = 5) +
  stat_cor(label.y = -100)
  # stat_regline_equation(label.y = 100)


# ggsave(plotCorrsScatter, filename = paste0("./results/temp_maps/",
#                                              'coastlineChangeCorrelateion_yr_mei_byCountry', 
#                                    '_',  format(Sys.Date(), "%Y%m%d"),'.jpeg'),
#        width = 13.1, height = 7.25, units = c('in'), dpi = 1200)


