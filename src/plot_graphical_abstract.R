## ---------------------------
#'
#' Script name: graphical abstract
#'
#' Short Description: 
#' 
#'
#' Author: Job de Vries
#'
#' Date Created: 2021-06-01
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
# library(rgee)
# Sys.setenv("RETICULATE_PYTHON" = "C:/Users/5600944/AppData/Local/r-miniconda/envs/rgee/python.exe")
ee_Initialize()

## ---------------------------
source("./src/functions.R")

## ---------------------------

mapviewOptions(basemaps = c( "Esri.WorldImagery","Esri.WorldShadedRelief", "OpenStreetMap.DE"))
dataFolder <- './data/processed'
# years <- c('2005', '2006','2007', '2008','2009')
years <- seq(from = 1985, to = 2020, by = 1)
aoi <- c('Suriname') # FrenchGuiana / Guayana / Suriname

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

# select folders
folderSelect <- as.matrix(list.files(paste0(dataFolder, '/offshore_points'), full.names = T))
# folderSelect <- as.matrix(list.files(paste0(dataFolder, '/offshore_points'), full.names = T))
df <- rewrite(folderSelect);

# only csv's
df <- df[grep('.csv', folderSelect, ignore.case = T),]
filtered <- vector('list', 100)
for (q in seq_along(years)) {
  for (x in seq_along(aoi)){
    # q <- 1
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
  }}


filtered <- unique(filtered)
Guyana <- filtered %>% 
  filter(str_detect(text,'Guyana')) 
FrenchGuiana <- filtered %>% 
  filter(str_detect(text,'French'))
Suriname <- filtered %>% 
  filter(str_detect(text,'Suriname'))


# # bind_rows!!!
# allFilesGuy <- do.call(bind_rows, lapply(as.matrix(Guyana)[,1], 
#                                          function(x) read.csv(x, 
#                                                               stringsAsFactors = FALSE,
#                                                               sep = ',', na.strings=c("","NA"),
#                                                               colClasses = "character"
#                                          )))
# allFilesGuy$Country = 'Guyana'
# 
# allFilesSur <- do.call(bind_rows, lapply(as.matrix(Suriname)[,1], 
#                                          function(x) read.csv(x, 
#                                                               stringsAsFactors = FALSE,
#                                                               sep = ',', na.strings=c("","NA"),
#                                                               colClasses = "character"
#                                          )))
# 
# allFilesSur$Country = 'Suriname'
# 
# allFilesFG <- do.call(bind_rows, lapply(as.matrix(FrenchGuiana)[,1], 
#                                         function(x) read.csv(x, 
#                                                              stringsAsFactors = FALSE,
#                                                              sep = ',', na.strings=c("","NA"),
#                                                              colClasses = "character"
#                                         )))
# allFilesFG$Country = 'FrenchGuiana'
# 
# 
# allFiles = bind_rows(allFilesFG, allFilesSur,allFilesGuy)
allFiles <- unique(do.call(rbind, lapply(as.matrix(filtered)[,1],
                                         function(x) read.csv(x,
                                                              stringsAsFactors = FALSE,
                                                              sep = ',',
                                                              na.strings=c("","NA")
                                         ))))

# class(testChangeClass$areaName)
# class(testChangeClass$coastDist)

allFiles <- type_convert(allFiles)

# flip position values for Suriname
# MaxPos <- max(allFiles[allFiles$Country=='Suriname', "pos"])
# allFiles_mutate <- allFiles_mutate %>% 
#   dplyr::mutate(pos = ifelse(Country == "Suriname", abs(pos-MaxPos),
#                              pos))

allFiles_dropPOS <- allFiles %>%
  dplyr::mutate(toFilter = 0) %>%
  dplyr::mutate(toFilter = ifelse((Country == "Suriname" & 
                                     pos %in% posToExcludeSUR),1,toFilter),
                toFilter = ifelse((Country == "FrenchGuiana" & 
                                     pos %in% posToExcludeFG),1,toFilter),
                toFilter = ifelse((Country == "Guyana" & 
                                     pos %in% posToExcludeGUY),1,toFilter)) %>%
  filter(toFilter == 0) %>%
  # also flip positions Suriname
  # dplyr::mutate(pos = ifelse(Country == "Suriname", abs(pos-MaxPos),
                             # pos)) %>%
  dplyr::select(-c(toFilter))

country <- aoi#'FrenchGuiana'
CountryToPlot <- subset(allFiles_dropPOS, Country == country)


all_years <- unique(CountryToPlot$year_col)
rectangles <- data.frame(id = character(),fill =  character(),
                         colour = character,
                         xmin = double(), xmax = double(), 
                         ymin = double(), ymax = double())

# get median position for each year
# only for pos which are considered to have a mudbank
# also exclude outliers and nonsense observations
# how to avoid using the filtered collection ==> you'd want to keep the dataframe intact
# but still calculate the median offshore only on the relevant observations

for(y in 1:length(all_years)){
  # y <- 1
  # selected_year <- '1991-01-01'
  selected_year <- all_years[y]
  annualSubset <- subset(CountryToPlot,as.Date(year_col) == selected_year &
                           coast_outlier == 1)
  idx <- which(CountryToPlot$year_col == selected_year & 
                 CountryToPlot$coast_outlier == 1)
  
  # all posiotions considered a mudbank during given year
  mudbankObs <- subset(annualSubset, !(pos %in% posToExclude) &
                         noMudbank == 0)
  getPos <- unique(mudbankObs$pos)
  
  # sequences
  sequences <- split(getPos, cumsum(c(0, diff(getPos) > 1000)));
  
  # drop sublist with only x amount of consequetive mudbank observations
  filtSequences <- Filter(function(x){length(x)>3}, sequences)
  
  startPos <- vapply(filtSequences, head, n = 1L, FUN.VALUE = numeric(1))
  endPos <- vapply(filtSequences, tail, n = 1L, FUN.VALUE = numeric(1))
  
  if(length(startPos) == 0){
    startPos <- c(0)
    endPos <- c(0)
  }
  
  rectangles <- rbind(rectangles, 
                      data.frame(id = selected_year,fill = 'black',
                                 colour = 'black',
                                 xmin = startPos, 
                                 xmax = endPos, 
                                 ymin = as.Date(selected_year)-184, # the geom_tiles per year have first of januari each year as midpoint  
                                 ymax = as.Date(selected_year)+181)) # so to have years overlapping this needs to be corrected
  
}

# Spatio temporal variation
dfToPlot <- subset(CountryToPlot, !is.na(deltaCoast)) 
                     

range <- round(quantile(dfToPlot$deltaCoast,c(0.05, 0.95), na.rm=T)) # deltaCoast / SmoothedPeakFract

p <-ggplot(dfToPlot,
           aes(x = pos,y = as.Date(year_col), fill=deltaCoast)) + 
  
  geom_tile(color= "white",size=0.1, na.rm = TRUE) +
  
  # resolve here that values larger than the range are indicated in the colourbar 
  # now it reads as if the largest value is -125m/yr where as actually there is locations that
  # have larger values.
  # scale_fill_gradient2(name = 'change [m/yr] \n',limits = c(range[[1]],range[[2]]),
  #                      # breaks = c(range[[1]], range[[1]]/2, 0, range[[2]]/2, range[[2]]),
  # 
  #                      low = '#7b3294', high = "#008837", mid = '#f7f7f7',
  #                      guide = guide_colourbar(nbin=100, draw.ulim = FALSE,
  #                                              draw.llim = FALSE),
  #                      na.value = NA, oob=squish) + # squish clamps all values to be within min & max of limits arguments


  scale_fill_gradientn(name = 'coasltine change \n [m/yr] \n',
                     breaks = c(range[[1]], range[[1]]/2, 0, range[[2]]/2, range[[2]]),
                     limits = c(range[[1]],range[[2]]),
                     colours = c('#7b3294', '#f7f7f7', "#008837"),
                     guide = guide_colourbar(nbin=100, draw.ulim = FALSE, draw.llim = FALSE),
                     oob=squish,
                     values = scales::rescale(c(range[[1]], -50, 0, 50, range[[2]]))
  ) +
  # geom_rect(data = rectangles, inherit.aes = FALSE,
  #           aes(xmin = xmin, xmax = xmax,
  #               ymin = ymin,
  #               ymax = ymax, colour = colour), fill = NA, size = 1) + 
  geom_rect(data = rectangles, inherit.aes = FALSE,
            aes(xmin = xmin, xmax = xmax,
                ymin = ymin, ymax = ymax-125, colour = colour), 
            fill = NA, size = 0.75) + 
  
  scale_colour_manual(name = ' ', values = c("black"),
                      labels = c('mudbank'),
                      guide = guide_legend(ncol = 2))+
  
  labs(y = 'Year', x = 'Alongshore Position [km]') + 
  scale_x_continuous(lim=c(0,max(dfToPlot$pos)+5000), expand = c(0,0),
  labels = unit_format(unit = "", scale = 0.001)) +
  # scale_x_reverse(lim=c(max(CountryToPlot$pos)+4000, 0), expand = c(0,0),
  #                 labels = unit_format(unit = "", scale = 0.001)) + # 
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
legend <- get_legend(p)

p <- p + theme(legend.position = 'none')

file <- '//ad.geo.uu.nl/FG/WOTRO/Research/Site_Suriname_all/Raw_data/GEE/TDOM_cloudscore/Suriname_test_Landsat_TOA_median_hybrid_2013_2015_1_365.tif'
fileFG <- '//ad.geo.uu.nl/FG/WOTRO/Research/Site_Suriname_all/Raw_data/GEE/TDOM_cloudscore/FrenchGuiana_Landsat_TOA_median_hybrid_2017_2019_1_365.tif'

RGBlimits <- matrix(c(430, 700, 900, 
                   4409, 1526, 1400),
                 nrow=3,ncol =2)


ylimit <- c(5.7,6.25)#c(5.8, 4.5)
xlimit <- c(-57.15, -53.95)#c(-54, -52)
brickRGB <- brick(file)

bandNames <- c('blue', 'green', 'red', 'nir', 'swir1',  'swir2', 'unknown', 'TIR', 'BQA')
names(brickRGB) <- bandNames

# create extent to crop image on

UL <- c(-53.9, 6.1)
LL <- c(-54.1772, 5.5664)
UR <- c(-51.3, 4.9)
LR <- c(-51.9, 4.3)


df <- rbind(UL, UR, LR,LL)
pol_for_plot <- SpatialPolygons(list( Polygons(list(Polygon(df[,1:2])),1)),
                                proj4string = CRS("+proj=longlat +datum=WGS84"))

mapview::mapview(pol_for_plot)

test<-st_as_sf(as(extent(pol_for_plot), 'SpatialPolygons' ))
test$CRS <-"+proj=longlat +datum=WGS84"

# mapview::mapview(pol_for_plot)+mapview::mapview(test)


# brickRGB <- fortify(brickRGB)

rgb_plt <- 
  ggRGB(img = brickRGB,  r = 'red', g = 'green', b = 'blue', 
        stretch = "lin", alpha = 0.8, quantiles = c(0.1, 0.95)) +
        # limits = RGBlimits) +
  scale_x_continuous(limits= xlimit,
                     expand = c(0,0)) +
  scale_y_continuous(limits= ylimit,
                     expand = c(0,0),
                     breaks=c(5.8, 6.0, 6.2)) +
  labs(y = 'Lat', x = 'Long') +
  labs(caption='Landsat (USGS): median composite 2014') +
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
        plot.caption = element_text(hjust = 0.5),
        # legend.position = 'none',
        panel.grid.major = element_blank(), # remove grid lines
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        plot.background = element_rect(fill = '#d9d9d9',  colour = '#d9d9d9'))

# rgb_plt

graphicalAbstractLeft <- plot_grid(p,rgb_plt, ncol = 1, align = 'l',
                                   nrow = 2, rel_heights = c(3, 2),
                                   rel_widths = c(0.5, 120))
graphicalAbstract <- plot_grid(graphicalAbstractLeft, legend, ncol = 2,
                               rel_widths = c(2.5, 0.5)) + 
  theme(panel.background = element_rect(fill = '#d9d9d9', colour = '#d9d9d9'))

# graphicalAbstract
ggsave(plot = graphicalAbstract,
       filename = paste0("./results/temp_maps/", 'graphicalAbstract_',country,'_', 
                         format(Sys.Date(), "%Y%m%d"),'.jpeg'),
       width = 13.1, height = 7.25, units = c('in'), dpi = 1200)

ggsave(plot = graphicalAbstract, 
       filename = paste0("D:/WOTRO/Research/Reporting/Publications/",
                         "AlongshoreVariability_mudbanks/submissionFiles/",
                         "figures/Figure_graphicalAbstract.pdf"),
       width = 13.1, height = 7.25, units = c('in'), dpi = 1200)

# https://coderedirect.com/questions/224513/how-to-rotate-an-image-r-raster
