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
# ee_Initialize()
## ---------------------------
source("./src/functions.R")


## ---------------------------

# mapviewOptions(basemaps = c( "Esri.WorldImagery","Esri.WorldShadedRelief", "OpenStreetMap.DE"))
dataFolder <- './data/processed'
aoi <- c('FrenchGuiana', 'Suriname', 'Guyana')  #'FrenchGuiana', 'Suriname', 'Guyana'
years <- seq(from = 1985, to = 2021, by = 1)

outputName <- ifelse(length(aoi) > 1, 
                     'Guianas', aoi)

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

# allFiles = bind_rows(allFilesFG, allFilesSur,allFilesGuy)
allFiles <- unique(do.call(rbind, lapply(as.matrix(filtered)[,1],
                                         function(x) read.csv(x,
                                                              stringsAsFactors = FALSE,
                                                              sep = ',',
                                                              na.strings=c("","NA")
                                         ))))

# names(allFiles)[names(allFiles) == 'pos.x'] <- 'pos'
# names(allFiles)[names(allFiles) == 'year_col.x'] <- 'year_col'

# remove duplicate observations to have accurate stats
# these are the triplicates?
allFiles <- type_convert(allFiles)


#---------------------------
#'
#' Filter 
#' 
#---------------------------

# drop transects near  river mouths
allFiles_dropPOS <- allFiles %>%
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


# normalize coastline changes to reference date (2x)
allFiles_refDate <- allFiles_dropPOS %>%
  # dplyr::mutate(nonOutlier = ifelse(coast_outlier == 1 & coastDist > 0 & 
  #                                     !(is.na(coastDist)), 1, 0)) %>%
  dplyr::group_by(Country, pos) %>%  #, nonOutlier
  
  # nearestDate
  dplyr::mutate(nearestDate =  as.Date(year_col[
    which.min(abs(as.Date(year_col)-reference_date))])) %>%
  
  # corresponding coastDist and coast median values assigned as baseline
  dplyr::mutate(baseline =  coast_median[which.min(abs(as.Date(year_col)-reference_date))]) %>%
  dplyr::mutate(baseline2 =  coast_median[which.min(abs(as.Date(year_col)-normalize_date))]) %>%
  
  #overWrite NA values with most occuring group value
  dplyr::mutate(
    nearestDate = na.aggregate(nearestDate, FUN=Mode),
    baseline = na.aggregate(baseline, FUN=Mode),
    baseline2 = na.aggregate(baseline2, FUN=Mode)) %>%
  # remaining NA values are posToExclude (rivermouths, there is no median computed)
  # unique ID for each combination of ID and Pos  
  dplyr::mutate(countryPos = paste0(Country,pos),
                normalized = coast_median - baseline,
                normalized2 = coast_median - baseline2) %>%
  ungroup()


#################################
#' 
#' test simple 2d plot 
#' 
#################################
twoD_pos <- 29000
subset2d_for_testPlot <- subset(allFiles_refDate, pos == twoD_pos & 
                                  Country == 'Suriname') 


# plot temporal evolution for given transect
ggplot(subset2d_for_testPlot, aes(x= year_col, y = as.numeric(coast_median))) + # color=coast_outlier)
  
  geom_point(size = 3, alpha = 0.6) + # aes(colour = as.factor(coast_outlier)),
  # facet_wrap(~Country) +
  
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


#' 
#' 
#' alongshore variability Guianas 
#' And Suriname
#' 
#' 
allFiles_refDate$alongshore <- NA

allFiles_mutate <- allFiles_refDate %>%
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




# define bins for coastline changes
n_directions = 8
dir_bin_width = 360 / n_directions
dir_bin_cuts = seq(dir_bin_width / 2, 360 - dir_bin_width / 2, dir_bin_width)
dir_bin_cuts_ordered = c(tail(dir_bin_cuts, 1), head(dir_bin_cuts, -1))

factor_labs = paste(dir_bin_cuts_ordered,
                    dir_bin_cuts, sep = ", ")
stepsize <- 22.5


###############################
##'
##'
##' plotting coasltine morphology 
##' - shape (curvature + sinuosity)
##' - shore normal direction
##' 
##' 
##' 

# Centered shore normal direction (& centerOrientatin2) ranges from -180 - 0 - 180, so x_bins 0, 4,5,6,7 are off
#  x_bin==0 (should be -337 - 22.5)   ==> N
#  x_bin==1 (should be 22.5 - 67.6)   ==> NE
#  x_bin==2 (should be 67.5 - 112.5)  ==> E
#  x_bin==3 (should be 112.5 - 157.5) ==> SE
#  x_bin==4 (should be 157.5 - 202.5) ==> S
#  x_bin==5 (should be 202.5 - 247.5) ==> SW
#  x_bin==7 (should be 292.5 - 337.5) ==> NW

allFiles_mutate <- allFiles_mutate %>% 
  dplyr::mutate(recenter = ifelse(centeredOrientation_100 < 0, 
                                  centeredOrientation_100 + 360,
                                  centeredOrientation_100),
                recenter250 = ifelse(centeredOrientation_250 < 0,
                                   centeredOrientation_250 + 360,
                                   centeredOrientation_250)) %>%
  
  # add bins & labels for coastline orientation
  # overwrite original variables 
  dplyr::mutate(x_bins_100 = findInterval(x=recenter, 
                                      vec = dir_bin_cuts),
                x_bins_250 = findInterval(x=recenter250, 
                                         vec = dir_bin_cuts)) %>%
  dplyr::mutate(x_bins_100 = ifelse(x_bins_100 == n_directions,
                                0, x_bins_100),
                x_bins_250 = ifelse(x_bins_250 == n_directions,
                                   0, x_bins_250)) %>%
  dplyr::group_by(Country, x_bins_100) %>%
  dplyr::mutate(mean100 = mean(deltaCoast[which(deltaCoast < 750 & deltaCoast > -750)], 
                               na.rm = T)) %>%
  dplyr::group_by(Country, x_bins_250) %>%
  dplyr::mutate(mean250 = mean(deltaCoast[which(deltaCoast < 750 & deltaCoast > -750)], na.rm = T)) %>%
  rowwise() %>%
  dplyr::mutate(posneg100 = ifelse(mean100<0, 'negative', 'positive'),
                posneg250 = ifelse(mean250<0, 'negative', 'positive'))
  # dplyr::select(Country, year_col,pos, x_bins100,x_bins250,
                # centeredOrientation_100, centeredOrientation_250) 


# # scatterplots
# # coastlineBearing x coastline change
# # meanCurvature x coastline change
# scatterPlots <- 
#   ggplot(allFiles_mutate %>% filter(!is.na(x_bins_100) & deltaCoast < 750 & deltaCoast > -750), 
#          aes(x=meanCurvature_100, y = deltaCoast)) + #  y = normalized2  / deltaCoast
#   geom_point(alpha =0.5)
# 
# 
# orientationStats <- allFiles_mutate %>%
#   filter(!is.na(x_bins_100) & deltaCoast < 350 & deltaCoast > -350) %>%  # ==> which to filter!?
#   dplyr::group_by(Country,x_bins_100) %>%
#   dplyr::summarise(meanDelta = mean(deltaCoast, na.rm = T),
#                    medianDelta = median(deltaCoast, na.rm = T),
#                    bin_size=n()) 
#   # dplyr::mutate(posneg = ifelse(meanDelta<0, 'negative', 'positive'))
# 
# testCategorial <- allFiles_mutate %>% 
#   filter(!is.na(x_bins_100) & deltaCoast < 500 & deltaCoast > -500) %>%
#   dplyr::group_by(Country, x_bins_100, round(deltaCoast)) %>%
#   dplyr::summarise(count = n(),
#                    dcoast = round(deltaCoast[1])) 
# 
# 
# 
# colorFunc <- colorRampPalette(c("red","yellow","springgreen","royalblue"))
# AmountOfClasses <- length(unique(round(testCategorial$dcoast)))
# 
# stackedBar <- ggplot(data = testCategorial, mapping = aes(x = as.factor(x_bins_100))) +
#   geom_bar(aes(fill = as.factor(dcoast)
#                # ,color = dcoast %in% c(-75,75),size = dcoast %in% c(-75,75)
#                ), position = 'fill' ) +
#   
# # somehow add lines that indicate some classes (e.g. 75, 150 and 300 m change?)
#   
#   scale_fill_manual(values= colorFunc(AmountOfClasses),guide = "none") +
#   # scale_colour_manual(name = ' ', values = c("black"), 
#   #                     labels = c('binned classes', NA))+
#   scale_color_manual(values = c(NA, 'black'), guide = "none") +
#   # scale_size_manual(values = c(0,2), guide = "none") +
#   # scale_x_discrete(labels = c('NW', 'N', 'NE', 'E', 'SE','SE', 'S')) +
#   labs(y = 'Relative Occurence', x = 'Shore normal ', ) + #[\u00B0]
#   facet_wrap(~Country) + 
#   theme(
#     axis.line.x = element_line(size = 0.5, colour = "black"),
#     axis.line.y = element_line(size = 0.5, colour = "black"),
#     axis.line = element_line(size=1, colour = "black"),
#     axis.text.x = element_text(color = "grey20", size = 18, hjust = .5, 
#                                angle = 45, vjust = .75, face = "bold"),
#     axis.text.y = element_text(color = "grey20", size = 18, hjust = .5, vjust = .5, face = "bold"),
#     axis.title.y = element_text(size = 20, face = 'bold'),
#     axis.title.x = element_text(size = 20, face = 'bold'),
# 
#     legend.background = element_rect(fill = '#d9d9d9',  colour = '#d9d9d9'),
#     legend.key = element_rect(fill = NA),
#     legend.text = element_text(size = 25),
#     legend.title =  element_text(size = 25),
#     panel.border = element_blank(),
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     panel.background = element_blank(),
#     panel.spacing.x = unit(2, 'lines'),
#     strip.background = element_rect(fill = "#d9d9d9", colour = "#d9d9d9"),
#     strip.text.x = element_text(size = 12, face = 'bold'),
#     plot.background = element_rect(fill = '#d9d9d9',  colour = '#d9d9d9'))
# 
# stackedBar

# ggsave(stackedBar, filename = paste0("./results/temp_maps/Guianas_1985-2020_dcoast_orientation",
#                                       format(Sys.Date(), "%Y%m%d"),'.jpeg'),
#        width = 13.1, height = 7.25, units = c('in'), dpi = 1200)

# Or plot them in vertical boxplots per group of coastline angles
boxplots <- ggplot(allFiles_mutate %>% 
                     filter(!is.na(x_bins_100) & deltaCoast < 350 & deltaCoast > -350), 
                   aes(x=as.factor(x_bins_100), y = as.numeric(deltaCoast))) +
  scale_y_continuous(limits=c(-100,100)) +
  scale_x_discrete(labels = c('NW', 'N', 'NE', 'E', 'SE','SE', 'S')) +
  geom_boxplot(outlier.shape=NA, aes(fill = as.factor(posneg100 ))) +


  facet_wrap(~Country, ncol = 1, nrow = 3) +
  labs(y = 'Coastline change [m/yr]', x = 'Shore normal \u00B0', ) +
  guides(fill=guide_legend(title="average change")) +
  
  theme(
    axis.line.x = element_line(size = 0.5, colour = "black"),
    axis.line.y = element_line(size = 0.5, colour = "black"),
    axis.line = element_line(size=1, colour = "black"),
    axis.text.x = element_text(color = "grey20", size = 18, hjust = .5, vjust = .5, face = "bold"),
    axis.text.y = element_text(color = "grey20", size = 18, hjust = .5, vjust = .5, face = "bold"),
    axis.title.y = element_text(size = 20, face = 'bold'),
    axis.title.x = element_text(size = 20, face = 'bold'),
    
    legend.background = element_rect(fill = '#d9d9d9',  colour = '#d9d9d9'),
    legend.key = element_rect(fill = NA),
    legend.text = element_text(size = 25),
    legend.title =  element_text(size = 25),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.spacing.x = unit(2, 'lines'),
    strip.background = element_rect(fill = "#d9d9d9", colour = "#d9d9d9"),
    strip.text.x = element_text(size = 12, face = 'bold'), 
    plot.background = element_rect(fill = '#d9d9d9',  colour = '#d9d9d9'))

boxplots

# ggsave(boxplots, filename = paste0("./results/temp_maps/",
#                                              'orientation_dCoast_Guianas_1985-2020',
#                                              '_',  format(Sys.Date(), "%Y%m%d"),'.jpeg'),
#        width = 13.1, height = 7.25, units = c('in'), dpi = 1200)


###############################
##'
##' Mudbank statistics
##' Figure 5: Shore normal orientation Guianas
##' 
##' 

# temporal variability per transect
tempVar <- allFiles_mutate %>%

  dplyr::group_by(Country, pos) %>%
  dplyr::mutate(meanOrient = median(centeredOrientation2_250, na.rm = T),
                sdOrient = sd(centeredOrientation2_250, na.rm = T),
                meanCurv = mean(meanCurvature_250, na.rm = T),
                sdCurv = sd(meanCurvature_250, na.rm = T)) %>%
  dplyr::select(c(Country, pos, year_col, centeredOrientation2_250,
                  centeredOrientation_250, centeredOrientation2_100,
                  centeredOrientation_100, meanOrient,sdOrient,
                  meanCurv,sdCurv))

tempVar <- transform(tempVar,
                        Country=factor(Country, levels=c("FrenchGuiana","Suriname","Guyana")))

# calculate means
meansOI <- tempVar %>%
  # filter(Country == countryOI) %>%
  dplyr::group_by(Country) %>%
  dplyr::summarize(mean=mean(centeredOrientation2_250, na.rm = T)) %>%
  ungroup() #%>%
  rename_at(1, ~grouping)


plotTempVar <- ggplot(data=tempVar, aes(x=meanOrient, y = sdOrient, 
                                        color = Country)) +
  guides(fill=guide_legend(title=" "), 
         color=guide_legend(title=" "))  +
  geom_vline(data = meansOI, aes(xintercept= mean), size = 3.5, colour = 'black',
             linetype="solid") +
  geom_vline(data = meansOI, aes(xintercept= mean, color=Country), size = 2,
             linetype="solid") +
  geom_point() +
  scale_y_continuous() + # expand = c(0,0)
  scale_x_continuous( breaks = round(sort(c(-50,meansOI$mean,0,50,100)))) +
  labs(x = 'Shore normal [\u00B0]', y = 'temporal variability [\u00B0]' ) +
  
  theme(
    axis.line.x = element_line(size = 0.5, colour = "black"),
    axis.line.y = element_line(size = 0.5, colour = "black"),
    axis.line = element_line(size=1, colour = "black"),
    axis.text.x = element_text(color = "grey20", size = 18, hjust = .5, 
                               vjust = 0.8, face = "bold", angle = 45),
    axis.text.y = element_text(color = "grey20", size = 18, hjust = .5, vjust = .5, face = "bold"),
    axis.title.y = element_text(size = 20, face = 'bold'),
    axis.title.x = element_text(size = 20, face = 'bold',  vjust = 5),
    # legend.position = "none",
    legend.background = element_rect(fill = '#d9d9d9',  colour = '#d9d9d9'),
    legend.key = element_rect(fill = NA),
    legend.text = element_text(size = 18),
    
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.spacing.x = unit(2, 'lines'),
    strip.background = element_rect(fill = "#d9d9d9", colour = "#d9d9d9"),
    strip.text.x = element_text(size = 12, face = 'bold'), 
    plot.background = element_rect(fill = '#d9d9d9',  colour = '#d9d9d9'))
plotTempVar

legend <- get_legend(plotTempVar) 

plotTempVar <- plotTempVar + theme(legend.position = 'none')

dens1 <- ggplot(tempVar, aes(x = centeredOrientation2_250, fill = Country)) + 
  geom_density(alpha = 0.4) + 
  theme(
    axis.line.x = element_line(size = 0.5, colour = "black"),
    axis.line.y = element_line(size = 0.5, colour = "black"),
    axis.line = element_line(size=1, colour = "black"),
    axis.text.x = element_blank(),
    axis.text.y = element_text(color = "grey20", size = 18, hjust = .5, vjust = .5, face = "bold"),
    axis.title.y = element_text(size = 20, face = 'bold'),
    axis.title.x = element_blank(),
    legend.position = "none",

    legend.key = element_rect(fill = NA),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.spacing.x = unit(2, 'lines'),
    strip.background = element_rect(fill = "#d9d9d9", colour = "#d9d9d9"),
    plot.background = element_rect(fill = '#d9d9d9',  colour = '#d9d9d9'))
  

dens2 <- ggplot(tempVar, aes(x = sdOrient)) + 
  geom_density(alpha = 0.4) +
  scale_y_continuous(breaks = c(20,40,60)) +
  theme(legend.position = "none",
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.background = element_rect(fill = '#d9d9d9',  colour = '#d9d9d9'),
        axis.line.x = element_line(size = 0.5, colour = "black"),
        axis.line.y = element_line(size = 0.5, colour = "black"),
        axis.line = element_line(size=1, colour = "black"),
        axis.text.x = element_text(color = "grey20", size = 18, hjust = .5, 
                                   vjust = 0.8, face = "bold", angle = 45),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 20, face = 'bold',  vjust = 5)) + 
  coord_flip()


combined <- dens1 +legend + plotTempVar + dens2 + 
  plot_layout(ncol = 2, nrow = 2, widths = c(4, 1), heights = c(1, 4))+
  plot_annotation(tag_levels = list(c('A', ' ', 'B', 'C'), '1')) &
  theme(plot.background = element_rect(fill = '#d9d9d9', colour = '#d9d9d9'),
        # plot.tag.position = c(0, 1),
        plot.tag = element_text(size = 12, hjust = 0, vjust = 0, face = "bold")) 

# ggsave(combined, filename = paste0("./results/temp_maps/",
#                                              'variabilityOrientation_Guianas_1985-2020',
#                                              '_',  format(Sys.Date(), "%Y%m%d"),'.jpeg'),
#        width = 13.1, height = 7.25, units = c('in'), dpi = 1200)


###############################
##'
##' Stability of coastline positions
##' Figure 9: coastline stability
##' 
##' 

allFiles_posMudbank <- allFiles_mutate %>%
  filter(alongshore != 'NA') %>%
  dplyr::group_by(Country, pos) %>%
  dplyr::mutate(
    firstCoastline = coast_median[
      which.min(abs(as.Date(year_col)-as.Date("1986-01-01")))],
    lastCoastline = coast_median[
      which.min(abs(as.Date(year_col)-as.Date("2021-01-01")))],
    endPointDiff = lastCoastline - firstCoastline
  ) %>%
  
  
  dplyr::mutate(
    # amount of years there is between first and last observations at each transect
    yrs = length(year(max(as.Date(year_col))):year(min(as.Date(year_col)))), # incl. end
    noDataYRS = yrs-length(as.Date(unique(year_col))), # years without observation
    
    # Amount of years mudbank & no mudbank
    mudbankYRS = sum(noMudbank == 0, na.rm = T),
    noMudbankYRS = sum(noMudbank == 1, na.rm = T),
    
    # startPos & endPos
    startPos = normalized2[which.min(as.Date(year_col))],
    endPos = normalized2[year(max(as.Date(year_col)))],
    freqOcc = mudbankYRS/(yrs-noDataYRS), # amount of mudbanks in the years there is data
    
    meanCurve = mean(medianC_250,na.rm = T),
    meanOrient = mean(centeredOrientation2_250,na.rm = T),
    concavity = ifelse(meanCurve > 0, 'convex', 'concave'),
  ) %>%
  
  dplyr::group_by(Country, year_col, pos, noMudbank) %>% #normalized2 / Country
  dplyr::mutate(
    mean_deltaCoast = mean(deltaCoast, na.rm = T)#,
  ) %>%
  # don't change these two! 
  dplyr::group_by(Country, pos, noMudbank) %>%
  dplyr::summarize(
    endPointDiff = endPointDiff[1],
    sum_deltaCoast = sum(mean_deltaCoast, na.rm = T),
    alongshore =  alongshore[1],
    yrs =  yrs[1],
    firstCoastline = firstCoastline[1],
    lastCoastline = lastCoastline[1],
    noDataYRS = noDataYRS[1],
    mudbankYRS = mudbankYRS[1],
    noMudbankYRS = noMudbankYRS[1],
    freqOcc = freqOcc[1],
    meanCurve = meanCurve[1],concavity = concavity[1], 
    meanOrient = meanOrient[1]
    ) %>%
  ungroup() %>%
  spread(noMudbank, sum_deltaCoast) %>%
  # noMudbank == `1` ==> sum coastline changes without mudbank
  # noMudbank == `0` ==> sum coastline changes WITH mudbank
  dplyr::rename(withoutMudbank = `1`,
                withMudbank = `0` )  %>%
  dplyr::mutate(
    withoutMudbank = if_else(is.na(withoutMudbank), 0, withoutMudbank),
    withMudbank = if_else(is.na(withMudbank), 0, withMudbank)) %>%
  # amount of meters per year
  dplyr::mutate(
    # divide by the amount of years a mudbank is observed or not (instead of total years)
    withMudbank_myr = withMudbank/mudbankYRS,  # yrs,#
    withoutMudbank_myr = withoutMudbank/noMudbankYRS,
    netChange_myr = withMudbank_myr + withoutMudbank_myr,
    endpointRate = endPointDiff/yrs) %>% # yrs)%>% #
  dplyr::mutate(orientClass = ifelse(meanOrient<22.5 & meanOrient > -22.5,
                                     'N', 'other'),
                orientClass = ifelse(meanOrient<67.5 & meanOrient > 22.5,
                                     'NE', orientClass),
                orientClass = ifelse(meanOrient<112.5 & meanOrient > 67.5,
                                     'E', orientClass))

allFiles_posMudbank <- transform(allFiles_posMudbank,
                                 Country=factor(Country, levels=c("FrenchGuiana","Suriname","Guyana")))

coastlineChanges <- allFiles_posMudbank %>% 
  # dplyr::group_by(stability) %>% 
  dplyr::group_by(Country) %>% 
  dplyr::summarize(
    groupSize =  n(), 
    freqOcc = mean(freqOcc, na.rm = T),
    withMudbank_myr = mean(withMudbank_myr, na.rm = T),
    withoutMudbank_myr = mean(withoutMudbank_myr, na.rm = T),
    netChange_myr =mean(netChange_myr, na.rm = T),
    endpointRate = mean(endpointRate, na.rm = T)) %>%
  ungroup()

# not all end-point rate values are equal to the net change myr approach (outliers are more prominent due to summing of values)
plot(allFiles_posMudbank$netChange_myr, allFiles_posMudbank$endpointRate)

histCoastalChange <-
  ggplot(data = allFiles_posMudbank,
         aes(x=endpointRate, fill = Country) ) +
  # geom_bar(position="stack", stat="identity")
  geom_histogram( binwidth = 25, alpha = 1, position = 'stack') +
  # scale_x_continuous(limits=c(-1500, 500), breaks = c(-200, 0, 200)) +
  scale_y_continuous(expand = c(0,0)) +
  # scale_y_log10(name = 'log(obs. count)')+

  geom_vline(data = coastlineChanges, aes(xintercept= endpointRate), size = 1,
             colour = 'black', linetype="solid") +
  # geom_vline(data = coastlineChanges, aes(xintercept= mean), size = 1,
  #            linetype="solid") +
  facet_wrap(~Country) +
  labs(y = 'Observations [n]', x = 'Coastline change [m/yr]') +

  theme(
    axis.line.x = element_line(size = 0.5, colour = "black"),
    axis.line.y = element_line(size = 0.5, colour = "black"),
    axis.line = element_line(size=1, colour = "black"),
    axis.text.x = element_text(color = "grey20", size = 18, hjust = .5, vjust = .5, face = "bold"),
    axis.text.y = element_text(color = "grey20", size = 18, hjust = .5, vjust = .5, face = "bold"),
    axis.title.y = element_text(size = 20, face = 'bold'),
    axis.title.x = element_text(size = 20, face = 'bold'),

    legend.background = element_rect(fill = '#d9d9d9',  colour = '#d9d9d9'),
    legend.key = element_rect(fill = NA),
    legend.text = element_text(size = 25),#element_blank(),
    legend.title =  element_blank(),
    # legend.position = c(.8, .4),

    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.spacing.x = unit(2, 'lines'),
    strip.background = element_rect(fill = "#d9d9d9", colour = "#d9d9d9"),
    strip.text.x = element_blank(), # Facet titles
    plot.background = element_rect(fill = '#d9d9d9',  colour = '#d9d9d9'))


histCoastalChange

# ggsave(resilience, filename = paste0("./results/temp_maps/", outputName,
#                             '_endPointRateChanges','_',
#                             format(Sys.Date(), "%Y%m%d"),'.jpeg'),
#         width = 13.1, height = 7.25, units = c('in'), dpi = 1200)

# count per region the amount of stable and unstable positions
allFiles_sumChanges <- allFiles_posMudbank %>%
  filter(alongshore != 'NA') %>%

  dplyr::mutate(sumChanges = withMudbank_myr + withoutMudbank_myr
                ) %>%
  dplyr::group_by(Country) %>%
  dplyr::mutate(
    groupSize = n(),
    # endpointRate = mean(endpointRate,na.rm=T),
    # stability = ifelse(sumChanges > 0, 'stable', 'unstable'),
    # stability2 = ifelse(endpointRate > 0, 'stable', 'unstable'),
    convex = sum(concavity == 'convex', na.rm = T),
    concave = sum(concavity == 'concave', na.rm = T),
    withMudbank_myr = sum(sumChanges >= 0, na.rm = T),
    withoutMudbank_myr = sum(sumChanges < 0, na.rm = T),
    endpointRateStable = sum(endpointRate >= 0 ,na.rm=T),
    endpointRateUnstable = sum(endpointRate < 0 ,na.rm=T)
    ) %>%
  # percentage stable <> unstable
  dplyr::mutate(withMudbank_myr = withMudbank_myr[1]/groupSize*100,
                   withoutMudbank_myr = withoutMudbank_myr[1]/groupSize*100,
                stable = endpointRateStable[1]/groupSize*100,
                unstable = endpointRateUnstable[1]/groupSize*100,
                   )

stableGroup <- allFiles_sumChanges %>% 
  # dplyr::group_by(stability) %>% 
  dplyr::group_by(Country) %>% 
  dplyr::summarize(
    endpointRate = mean(endpointRate,na.rm=T),
    groupSize =  n(), 
    concavity = concavity[1],
    withMudbank_myr = withMudbank_myr[1],
    withoutMudbank_myr = withoutMudbank_myr[1],
    stable = stable[1],
    unstable = unstable[1])

# consider adding a group for plotting a fourth 
# facet wrap with explanation of zones in graph
resilience <- ggplot(allFiles_posMudbank 
                     ,aes(x=withMudbank_myr, y= withoutMudbank_myr ,
                       colour=endpointRate <0,
                       ))+ # shape= endpointRate <0
                         # shape= withMudbank_myr + withoutMudbank_myr < 0))+
  geom_point(alpha = 0.8) + 
  geom_abline(intercept = 1, slope = -1) +
  facet_wrap(~Country) +
  geom_text(data = stableGroup #%>% filter(orientClass!='other') ,
            ,aes(label=sprintf("prograding: %i%%", round(stable)),
                x = c(-100), y=c(140)),
            colour = 'Black', angle =-64, size =7) + #
  geom_text(data = stableGroup #%>% filter(orientClass!='other'),
            ,aes(label=sprintf("retrograding: %i%%", round(unstable)),
                x = 120, y=-140),
            colour = 'Black', angle =-64, size = 7) +

  scale_y_continuous(limits = c(-200, 200)) +
  scale_x_continuous(limits = c(-200, 200)) +
  # scale_shape_manual(name = "",
  #                    labels = c("prograding", "retrograding", ''),
  #                    values = c(16, 17)) +
  scale_color_manual(name = "",
                     labels = c("prograding", "retrograding", ''),
                     values = c('#018571', '#a6611a')) +
  
  labs(y="Without mudbank [m/yr]", x = "With mudbank [m/yr]") + 
  theme(axis.line.x = element_line(size = 0.5, colour = "black"),
        axis.line.y = element_line(size = 0.5, colour = "black"),
        axis.line = element_line(size= 1, colour = "black"),
        axis.title.y = element_text(size = 20, face = 'bold'),
        axis.title.x = element_text(size = 20, face = 'bold'),
        axis.text.x = element_text(size = 18,  hjust = .5, vjust = .5),
        axis.text.y = element_text(size = 18, hjust = .5, vjust = .5),
        legend.title = element_blank(),
        legend.background = element_rect(fill = '#d9d9d9',  colour = '#d9d9d9'),
        legend.key=element_blank(),
        legend.text = element_text(size = 20),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        strip.text.x =element_text(size = 20),
        plot.background = element_rect(fill = '#d9d9d9'))
resilience  

# ggsave(resilience, filename = paste0("./results/temp_maps/", outputName,
#                             '_resilience_meanOrient_mudbanks','_',
#                             format(Sys.Date(), "%Y%m%d"),'.jpeg'),
#         width = 13.1, height = 7.25, units = c('in'), dpi = 1200)


#################################
#'   
#'   spatial perspective of coastline 'stability'
#'   Part of Figure 9 (each country seperately)
#'

# region <- c('cayenne')
Cntry <- c('Suriname') # Guyana / Suriname / FrenchGuiana

toPlotSpatial <- allFiles_posMudbank %>%
  filter(Country == Cntry) #%>%
  # dplyr::mutate(sign = as.factor(sign(withMudbank_myr + withoutMudbank_myr)))

spatialVariability <- ggplot(toPlotSpatial, 
                             aes(x=pos/1000, y=endpointRate #withMudbank_myr + withoutMudbank_myr, 
                             )) +
  geom_hline(yintercept = 0) +
  geom_linerange(data = toPlotSpatial, 
                 aes(ymin = 0, ymax = endpointRate, #withMudbank_myr+ withoutMudbank_myr, 
                     colour = ifelse(endpointRate <0, #withMudbank_myr + withoutMudbank_myr <0, 
                                     "blue", "red")),
                 stat = "identity",
                 position = "identity",size=2) + 
  scale_color_manual(values = c("#f4a582", "#92c5de")) +
  scale_y_continuous(position = "right") +
  labs(y="m/yr", x = "alongshore position [km]") + 
  theme(axis.line.x = element_blank(),
        axis.line.y = element_line(size = 0.5, colour = "black"),
        axis.line = element_line(size= 1, colour = "black"),
        axis.title.y = element_text(size = 20, face = 'bold'),
        axis.title.x =element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 18, hjust = .5, vjust = .5),
        legend.title = element_blank(), 
        axis.ticks.x = element_blank(),
        
        legend.key=element_blank(),
        legend.position = 'none',
        legend.text = element_text(size = 20),
        panel.grid.major = element_blank(), # remove grid lines
        panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill = "transparent"), #element_blank(),
        strip.text.x = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA),
        
  )

spatialVariability

# ggsave(spatialVariability, filename = paste0("./results/temp_maps/", Cntry,'spatialVariability_endpointRate',
#                                         '_',  format(Sys.Date(), "%Y%m%d"),'.jpeg'),
#        width = 13.1, height = 7.25, units = c('in'), dpi = 1200)




















###############################
##'
##' 
##' No figure in manuscript
##' Coastline resilience plot 
##' 
# # to compare the effect of different buffersizes go from a wide format to long
# # probably use a dplyr function here to ensure you only do that for a specific variable that we define.
# propertyToCompare <- c('medianC') # 'meanCurvature' 'sinuositySmoothed' 'x_bins' 'coastlineLengthOrdered'
# # 'coastlineLength' 'centeredOrientation' 'centeredOrientation2' 'bearing' 
# # 'medianC' 'maxC' 'minC' 'meanCurvature'
# 
# 
# 
# # testVals = allFiles_mutate %>%
# #   filter(centeredOrientation_250 > 22.5 & centeredOrientation_250 < 67.5) %>%
# #   # filter(x_bins_250 == 7) %>%
# #   dplyr::select(Country, year_col,pos, x_bins_250,centeredOrientation_250)
# 
# allfiles_long <- allFiles_mutate %>%
#   filter(!is.na(meanCurvature_250)& 
#            meanCurvature_250 != -1.0 & 
#            meanCurvature_250 != 0.0 ) %>%  # ==> which to filter!?
#   group_by(Country, pos, year_col) %>%
#   dplyr::select(matches(propertyToCompare), noMudbank) %>%
#   ungroup() %>%
#   pivot_longer(!c(Country, pos, year_col, noMudbank), 
#                names_to = c(propertyToCompare, 'transectBuffer'), 
#                values_to = c("value"), names_sep = '_') %>%
#   ungroup() 
# 
# 
# meansPerbuffer <- allfiles_long %>%
#   # filter(Country == countryOI) %>%
#   # filter(value < 0.003 & value > -0.003) %>%
#   filter(transectBuffer == 250) %>%
#   filter(as.numeric(value) != 0) %>%
#   dplyr::group_by(Country, as.factor(noMudbank)) %>%
#   dplyr::summarize(mean=mean(value, na.rm = T),
#                    sdev = sd(value, na.rm = T),
#                    median= median(value, na.rm = T),
#                    
#                    n=n()) %>%
#   ungroup() 
# 
# # Variability/distribution occurence of the selected properties. 
# variability <- ggplot(data = allfiles_long %>% filter(transectBuffer == 250)
#                       , aes(x=value, fill = as.factor(noMudbank)),  # fill = as.factor(noMudbank)
#                       alpha = 1) +
#   # scale_y_log10(expand = c(0,0), name = 'log(obs. count)')+
#   facet_wrap(paste0('~Country'), ncol = 1, nrow = 3) +
#   
#   geom_histogram(binwidth = 0.0005, alpha = 0.8) +
#   # geom_histogram(binwidth = 0.0005) + 
#   # geom_histogram(binwidth = 0.00005*10^2.1, position = 'identity') +
#   # geom_density( alpha = 0.5) +
#   geom_vline(data = meansPerbuffer, aes(xintercept= median),color = 'Black', size = 2,
#              linetype="dashed") +
#   
#   # guides(fill = guide_legend(override.aes = list(size = 10, alpha =1,
#   #                                                linetype = 0),
#   #                            title="buffersize [m]"),colour = F) +
#   # labs(y = 'count', x = paste0(propertyToCompare)) +
#   labs(y = 'Observations [n]', x = 'curvature κ [deg*m-1]') + # 'curvature κ [deg*m-1]' , 'Coastline orientation (°)'
#   scale_x_continuous(limits = c(-0.004,0.004)) +
#   # scale_x_continuous(limits = c(-67.5,180),
#   #   breaks = c(seq(from = -67.5, to = -1,by = stepsize),
#   #               seq(from = 0, to = 180,by = stepsize)),
#   #                    expand = c(0,0)) +
#   
#   theme(axis.line.x = element_line(size = 0.5, colour = "black"),
#         axis.line.y = element_line(size = 0.5, colour = "black"),
#         axis.line = element_line(size= 1, colour = "black"),
#         axis.title.y = element_text(size = 20, face = 'bold'),
#         axis.title.x = element_text(size = 20, face = 'bold'),
#         axis.text.x = element_text(size = 18,  hjust = .5, vjust = .5),
#         axis.text.y = element_text(size = 18, hjust = .5, vjust = .5),
#         legend.title = element_blank(), #element_text(colour = 'black', size = 14, face = "bold"),
#         legend.background = element_rect(fill = '#d9d9d9',  colour = '#d9d9d9'),
#         legend.position = c(0.9,0.8),
#         legend.text = element_text(size = 20),
#         panel.grid.major = element_blank(), # remove grid lines
#         panel.grid.minor = element_blank(), 
#         panel.background = element_blank(),
#         strip.text.x =element_text(size = 20, face = 'bold'),
#         plot.background = element_rect(fill = '#d9d9d9'))
# 
# variability
# 
# # ggsave(variability, filename = paste0("./results/temp_maps/",propertyToCompare,
# #                                              '_Guianas_1985-2020',
# #                                              '_',  format(Sys.Date(), "%Y%m%d"),'.jpeg'),
# #        width = 13.1, height = 7.25, units = c('in'), dpi = 1200)
# 


# # plot coastline position (normalized for given year and compare to curvature)
# temporalSubset <- allFiles_mutate %>% filter(year_col == as.Date('2016-01-01') &
#                                                Country == 'Suriname')
# temporalSubset2 <- allFiles_mutate %>% filter(year_col == as.Date('2020-01-01') &
#                                                 Country == 'Suriname')
# 
# # spline interpolation
# # spline_int <- as.data.frame(spline(temporalSubset$pos, 
# # temporalSubset$meanCurvature_250))
# 
# # add geom_rect with mudbanks
# planviewPos <- ggplot(data=temporalSubset, mapping = aes(x=pos,y = normalized2)) +
#   geom_point(alpha = 0.5) +
#   # geom_line(data = spline_int, aes(x = x, y = y))
#   stat_smooth(method = "lm",
#               formula = y ~ poly(x, 25), se = FALSE) +
#   labs(y = 'Cross-shore \n position [m]') +
#   theme(axis.line.x = element_line(size = 0.5, colour = "black"),
#         axis.line.y = element_line(size = 0.5, colour = "black"),
#         axis.line = element_line(size= 1, colour = "black"),
#         axis.title.y = element_text(size = 12, face = 'bold'),
#         axis.title.x = element_blank(),
#         axis.text.x = element_blank(),
#         axis.text.y = element_text(size = 12, hjust = .5, vjust = .5),
#         legend.background = element_rect(fill = '#d9d9d9',  colour = '#d9d9d9'),
#         legend.title = element_text(colour = 'black', size = 14, face = "bold"),
#         # legend.key = element_rect(fill = NA),
#         legend.text = element_text(size = 12),
#         # legend.position = 'none',
#         panel.grid.major = element_blank(), # remove grid lines
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         plot.background = element_rect(fill = '#d9d9d9',  colour = '#d9d9d9'))
# 
# 
# 
# planviewCurv <- ggplot(data=temporalSubset, mapping = aes(x=pos,y = meanCurvature_250)) +
#   # geom_point(alpha = 0.5) +
#   stat_smooth(method = "glm",
#               formula = y ~ poly(x, 25), se = FALSE) +
#   stat_smooth(data=temporalSubset2, color = 'red', method = "glm",
#               formula = y ~ poly(x, 25), se = FALSE,
#               mapping = aes(x=pos,y = meanCurvature_250)) +
#   scale_y_continuous(limits = c(-0.002,0.002), breaks = c(-0.002,0,0.002)) +
#   labs(y = 'curvature') +
#   theme(axis.line.x = element_line(size = 0.5, colour = "black"),
#         axis.line.y = element_line(size = 0.5, colour = "black"),
#         axis.line = element_line(size= 1, colour = "black"),
#         axis.title.y = element_text(size = 12, face = 'bold'),
#         axis.title.x = element_blank(),
#         axis.text.x = element_blank(),
#         axis.text.y = element_text(size = 12, hjust = .5, vjust = .5),
#         legend.background = element_rect(fill = '#d9d9d9',  colour = '#d9d9d9'),
#         legend.title = element_text(colour = 'black', size = 14, face = "bold"),
#         # legend.key = element_rect(fill = NA),
#         legend.text = element_text(size = 12),
#         # legend.position = 'none',
#         panel.grid.major = element_blank(), # remove grid lines
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         plot.background = element_rect(fill = '#d9d9d9',  colour = '#d9d9d9'))
# 
# differencePos <- ggplot(data=temporalSubset, mapping = 
#                           aes(x=pos,
#                               y = temporalSubset2$normalized2-temporalSubset$normalized2)) +
#   geom_point(alpha = 0.5) +
#   scale_y_continuous(limits = c(-1000,1000), breaks = c(-1000,0,1000)) +
#   labs(y = 'Change \n position [m]') +
#   theme(axis.line.x = element_line(size = 0.5, colour = "black"),
#         axis.line.y = element_line(size = 0.5, colour = "black"),
#         axis.line = element_line(size= 1, colour = "black"),
#         axis.title.y = element_text(size = 12, face = 'bold'),
#         axis.title.x = element_text(size = 12, face = 'bold'),
#         axis.text.x = element_text(size = 12, hjust = .5, vjust = .5),
#         axis.text.y = element_text(size = 12, hjust = .5, vjust = .5),
#         legend.background = element_rect(fill = '#d9d9d9',  colour = '#d9d9d9'),
#         legend.title = element_text(colour = 'black', size = 14, face = "bold"),
#         # legend.key = element_rect(fill = NA),
#         legend.text = element_text(size = 12),
#         # legend.position = 'none',
#         panel.grid.major = element_blank(), # remove grid lines
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         plot.background = element_rect(fill = '#d9d9d9',  colour = '#d9d9d9'))
# 
# differenceCurv <- ggplot(data=temporalSubset, mapping = 
#                            aes(x=pos,
#                                y = temporalSubset2$meanCurvature_250-temporalSubset$meanCurvature_250)) +
#   geom_point(alpha = 0.5, ) +
#   labs(y = 'change curvature') +
#   scale_y_continuous(limits = c(-0.005,0.005), breaks = c(-0.005,0,0.005)) +
#   theme(axis.line.x = element_line(size = 0.5, colour = "black"),
#         axis.line.y = element_line(size = 0.5, colour = "black"),
#         axis.line = element_line(size= 1, colour = "black"),
#         axis.title.y = element_text(size = 12, face = 'bold'),
#         axis.title.x = element_blank(),
#         axis.text.x = element_blank(),
#         axis.text.y = element_text(size = 12, hjust = .5, vjust = .5),
#         legend.background = element_rect(fill = '#d9d9d9',  colour = '#d9d9d9'),
#         legend.title = element_text(colour = 'black', size = 14, face = "bold"),
#         # legend.key = element_rect(fill = NA),
#         legend.text = element_text(size = 12),
#         # legend.position = 'none',
#         panel.grid.major = element_blank(), # remove grid lines
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         plot.background = element_rect(fill = '#d9d9d9',  colour = '#d9d9d9'))
# 
# plot_grid(planviewPos, planviewCurv,differenceCurv,differencePos, ncol = 1, align = 'v', #, mapped
#           nrow = 4, rel_heights = c(2.5, 2.5, 2.5, 2.5),         # adjust lay out 
#           labels = c('A', 'B', 'C', 'D'), vjust = 1, hjust =-7.2) 


# #Hovmoller plot for the spatio temporal distribytion of coastline property
# p <-ggplot(allfiles_long %>% 
#              filter(transectBuffer == 250), 
#            aes(x =pos,y = as.Date(year_col), fill=value)) + 
#   
#   geom_tile(size=0.1, na.rm = TRUE) +
#   facet_wrap(paste0('~Country'), ncol = 1, nrow = 3) + 
#   # facet_wrap(~transectBuffer) +
#   
#   scale_fill_gradientn(name = 'Curvature',
#                        colours = c('#7b3294', '#f7f7f7', "#008837"),
#                        guide = guide_colourbar(nbin=100, draw.ulim = FALSE,
#                                                draw.llim = FALSE),
#                        oob=squish,
#                        values = scales::rescale(c(-0.2 ,-0.02,0,0.02, 0.2))) +
#   # scale_fill_gradientn(name = 'Shore normal',
#   #                    colours = c('#7b3294', '#f7f7f7', "#008837"),
#   #                    guide = guide_colourbar(nbin=100, draw.ulim = FALSE,
#   #                                            draw.llim = FALSE),
#   #                    oob=squish,
#   #                    limits = c(-22.5,45),
#   #                    values = scales::rescale(c(-22.5 ,0,11.75,22.5, 45))) +
#   
#   labs(y = 'Year', x = 'Alongshore Position [km]') +
#   
#   theme(axis.line.x = element_line(size = 0.5, colour = "black"),
#         axis.line.y = element_line(size = 0.5, colour = "black"),
#         axis.line = element_line(size= 1, colour = "black"),
#         axis.title.y = element_text(size = 12, face = 'bold'),
#         axis.title.x = element_text(size = 12, face = 'bold'),
#         axis.text.x = element_text(size = 12,  hjust = .5, vjust = .5),
#         axis.text.y = element_text(size = 12, hjust = .5, vjust = .5),
#         legend.background = element_rect(fill = '#d9d9d9',  colour = '#d9d9d9'),
#         legend.title = element_text(colour = 'black', size = 14, face = "bold"),
#         # legend.key = element_rect(fill = NA),
#         legend.text = element_text(size = 12),
#         # legend.position = 'none',
#         panel.grid.major = element_blank(), # remove grid lines
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         plot.background = element_rect(fill = '#d9d9d9',  colour = '#d9d9d9'))
# p


# concGroup <- allFiles_sumChanges %>% 
#   # dplyr::group_by(stability) %>% 
#   dplyr::group_by(concavity) %>% 
#   dplyr::summarize(
#     groupSize =  sum(stability == 'stable' | stability == 'unstable' , na.rm=T),
#     stable = sum(stability == 'stable', na.rm=T),
#     unstable =  sum(stability == 'unstable', na.rm=T),
#     stableRel = (stable/groupSize)*100,
#     unstableRel = (unstable/groupSize)*100,
#     # concave2 = sum(concavity == 'concave', na.rm = T),
#     # convex2 = sum(concavity == 'convex', na.rm = T),
#     # convexRel = convex2/(convex2+concave2)*100,
#     # concaveRel = concave2/(convex2+concave2)*100,
#     concavity = concavity[1],
#     withMudbank_myr = withMudbank_myr[1],
#     withoutMudbank_myr = withoutMudbank_myr[1])
# 
# 
# orientationGroup <- allFiles_sumChanges %>% 
# 
#   dplyr::group_by(orientClass) %>% 
#   dplyr::summarize(
#     groupSize =  sum(stability == 'stable' |stability == 'unstable' , na.rm=T),
#     stable = sum(stability == 'stable', na.rm=T),
#     unstable =  sum(stability == 'unstable', na.rm=T),
#     stableRel = (stable/groupSize)*100,
#     unstableRel = (unstable/groupSize)*100,
#     meanOrient = median(meanOrient, na.rm=T),
#     N = sum(orientClass == 'N', na.rm = T),
#     NE = sum(orientClass == 'NE', na.rm = T),
#     E = sum(orientClass == 'E', na.rm = T),
#     other = sum(orientClass == 'other', na.rm = T),
# 
#     withMudbank_myr = withMudbank_myr[1],
#     withoutMudbank_myr = withoutMudbank_myr[1],
#     concavity = concavity[1])
# 
# stabilityGroup <- allFiles_sumChanges %>% 
#   dplyr::mutate(orientClass = ifelse(meanOrient<22.5 & meanOrient > -22.5,
#                 'N', 'other'),
#                 orientClass = ifelse(meanOrient<67.5 & meanOrient > 22.5,
#                 'NE', orientClass),
#                 orientClass = ifelse(meanOrient<112.5 & meanOrient > 67.5,
#                                      'E', orientClass)) %>%
#   dplyr::group_by(stability) %>% 
#   dplyr::summarize(
#     meanOrient = median(meanOrient, na.rm=T),
#     N = sum(orientClass == 'N', na.rm = T),
#     NE = sum(orientClass == 'NE', na.rm = T),
#     E = sum(orientClass == 'E', na.rm = T),
#     other = sum(orientClass == 'other', na.rm = T),
#     Nrel = N/(N+NE+E+other)*100,
#     NErel = NE/(N+NE+E+other)*100,
#     Erel = E/(N+NE+E+other)*100,
#     otherrel = other/(N+NE+E+other)*100,
#     withMudbank_myr = withMudbank_myr[1],
#     withoutMudbank_myr = withoutMudbank_myr[1],
#     concavity = concavity[1])
# 
# presilience <- ggplot(allFiles_posMudbank, aes(x=meanOrient, 
#                            y= withMudbank_myr + withoutMudbank_myr,
#                            colour=Country,
#                            shape=concavity))+ #withMudbank_myr + withoutMudbank_myr < 0))+ # concavity
#   # geom_abline(intercept = 1, slope = -1) +
#   geom_abline(intercept = 0, slope = 0) +
# 
#   geom_point(alpha = 0.8) + 
# 
#   
#   # facet_wrap(~Country) +
#   # geom_text(data = concGroup,
#   #           aes(label=sprintf("stable: %i%%", round(withMudbank_myr)),
#   #               x = c(-50), y=c(70)),
#   #           colour = 'Black', angle =-64, size =7) + #
#   # geom_text(data = unique(allFiles_sumChanges),
#   #           aes(label=sprintf("unstable: %i%%", round(withoutMudbank_myr)),
#   #               x = 60, y=-70),
#   #           colour = 'Black', angle =-64, size = 7) +
#   geom_text(#data = concGroup %>% filter(stability == 'stable'),
#           aes(label=sprintf("concave: %i%% \n convex: %i%%",
#                             round(as.numeric(concGroup[concGroup$concavity =='concave','stableRel'])),
#                             round(as.numeric(concGroup[concGroup$concavity =='convex','stableRel']))),
#               x = c(-25), y=c(150)),
#           colour = 'Black', angle =0, size =7) + #
#   geom_text(#data = concGroup %>% filter(stability == 'unstable'),
#             aes(label=sprintf("concave: %i%% \n convex: %i%%",
#                               round(as.numeric(concGroup[concGroup$concavity =='concave','unstableRel'])),
#                               round(as.numeric(concGroup[concGroup$concavity =='convex','unstableRel']))),
#                 x = -25, y=-130),
#             colour = 'Black', angle =0, size = 7) +
#   
#   # # add info on orientation
#   # geom_text(data = stabilityGroup %>% filter(stability == 'stable'),
#   #           aes(label=sprintf("N: %i%% \n NE: %i%% \n E: %i%%",
#   #                             round(Nrel), round(NErel), round(Erel)),
#   #               x = c(120), y=c(150)),
#   #           colour = 'Black', angle =0, size =7) + #
#   # geom_text(data = stabilityGroup %>% filter(stability == 'unstable'),
#   #           aes(label=sprintf("N: %i%% \n NE: %i%% \n E: %i%%",
#   #                             round(Nrel), round(NErel), round(Erel)),
#   #               x = 120, y=-110),
#   #           colour = 'Black', angle =0, size = 7) +
#   
#   geom_text(
#             aes(label=sprintf("N: %i%% \n NE: %i%% \n E: %i%%",
#                               round(as.numeric(orientationGroup[orientationGroup$orientClass =='N','stableRel'])), 
#                               round(as.numeric(orientationGroup[orientationGroup$orientClass =='NE','stableRel'])), 
#                               round(as.numeric(orientationGroup[orientationGroup$orientClass =='E','stableRel']))),
#                 x = c(120), y=c(150)),
#             colour = 'Black', angle =0, size =7) + #
#   geom_text(
#             aes(label=sprintf("N: %i%% \n NE: %i%% \n E: %i%%",
#                               round(as.numeric(orientationGroup[orientationGroup$orientClass =='N','unstableRel'])), 
#                               round(as.numeric(orientationGroup[orientationGroup$orientClass =='NE','unstableRel'])), 
#                               round(as.numeric(orientationGroup[orientationGroup$orientClass =='E','unstableRel']))),
#                 x = 120, y=-110),
#             colour = 'Black', angle =0, size = 7) +
#   
#   
#   # scale_y_continuous(limits = c(-100, 100)) +
#   # scale_x_continuous(limits = c(-100, 100)) +
#   scale_x_continuous(breaks = c(-22.5, 22.5,67.5,112.5)) +
#   scale_shape_manual(name = "",
#                      # labels = c("Stable", "Unstable"),
#                      values = c(16, 17)) +
#   
#   labs(y="Resilience [m/yr]", x = "Coastline orientation") + 
#   theme(axis.line.x = element_line(size = 0.5, colour = "black"),
#         axis.line.y = element_line(size = 0.5, colour = "black"),
#         axis.line = element_line(size= 1, colour = "black"),
#         axis.title.y = element_text(size = 20, face = 'bold'),
#         axis.title.x = element_text(size = 20, face = 'bold'),
#         axis.text.x = element_text(size = 18,  hjust = .5, vjust = .5),
#         axis.text.y = element_text(size = 18, hjust = .5, vjust = .5),
#         legend.title = element_blank(),
#         legend.background = element_rect(fill = '#d9d9d9',  colour = '#d9d9d9'),
#         legend.key=element_blank(),
#         legend.text = element_text(size = 20),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(), 
#         panel.background = element_blank(),
#         strip.text.x = element_blank(),
#         plot.background = element_rect(fill = '#d9d9d9'))
# presilience  


# ggsave(presilience, filename = paste0("./results/temp_maps/", outputName,
#                             'resilience_coastlines_medianconcavity250_meanOrient2','_',
#                             format(Sys.Date(), "%Y%m%d"),'.jpeg'),
# width = 13.1, height = 7.25, units = c('in'), dpi = 1200)

# Alternative to the boxplots: create groups of pos (e.g. every 10 or per region)
# and compute their average shore-normal angle for the x-axis and the distribution of
# coastline responses (Wiggins et al., 2019 ==> Figure 9)

# # aggregated pos gos per x Meters:
# summarisePos <- 10000
# breaks <- seq(0, max(allFiles_mutate$pos), summarisePos)
# 
# # position label for plotting x-axis
# poslabel <- seq(0, max(allFiles_mutate$pos), 
#                 summarisePos)
# # noMudbank: when frequently identified as mudbank in a year: give 0
# groupedBoxes <- allFiles_mutate %>%
#   filter(!is.na(centeredOrientation2_250) & 
#            # noMudbank == 1 & 
#            deltaCoast < 250 & deltaCoast > -250) %>%
#   group_by(Country) %>%
#   dplyr::mutate(
#     newPos = cut(pos,breaks, include.lowest = T, right = T)) %>%
#   group_by(Country,newPos) %>%
#   
#   # probably change this because now the range of values is not suitable to take an average?
#   dplyr::mutate(meanOrient = mean(centeredOrientation2_100, na.rm = T),
#                 meanCurv = mean(medianC_250, na.rm = T),
#                 meanChange = mean(deltaCoast, na.rm = T),
#                 n = n()) %>%
#   dplyr::select(Country,alongshore,pos,deltaCoast, meanOrient,newPos,
#                 n, normalized2,meanCurv,meanChange)
# 
# 
# plotGroupedBoxes <- ggplot(groupedBoxes, 
#                 aes(x=round(meanOrient), color = as.factor(meanOrient),
#                     fill = as.factor(Country), y = as.numeric(deltaCoast))) +
#   scale_color_manual(values = rep(c("black"),223), guide = 'none') +
#   geom_boxplot(outlier.shape=NA, ) + # aes(fill = as.factor(Country))
#   # stat_smooth(data = groupedBoxes,  method = "glm", inherit.aes = F,
#   #             mapping = aes(#linetype = Country,
#   #                           fill = as.factor(Country),
#   #                           x=round(meanOrient),
#   #                           y = as.numeric(meanChange)),
#   #             formula = y ~ poly(x, 5), se = FALSE) +
# 
# 
#   # facet_wrap(~Country, ncol = 1, nrow = 3) +
#   labs(y = 'Coastline change [m/yr]', x = 'Orientation [\u00B0]' ) +
#   guides(fill=guide_legend(title=" "))  +
#   theme(
#     axis.line.x = element_line(size = 0.5, colour = "black"),
#     axis.line.y = element_line(size = 0.5, colour = "black"),
#     axis.line = element_line(size=1, colour = "black"),
#     axis.text.x = element_text(color = "grey20", size = 18, hjust = .5, vjust = .5, face = "bold"),
#     axis.text.y = element_text(color = "grey20", size = 18, hjust = .5, vjust = .5, face = "bold"),
#     axis.title.y = element_text(size = 20, face = 'bold'),
#     axis.title.x = element_text(size = 20, face = 'bold'),
#     legend.position = c(.8, .8),
#     legend.background = element_rect(fill = '#d9d9d9',  colour = '#d9d9d9'),
#     legend.key = element_rect(fill = NA),
#     legend.text = element_text(size = 25),
#     legend.title =  element_text(size = 25),
#     panel.border = element_blank(),
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     panel.background = element_blank(),
#     panel.spacing.x = unit(2, 'lines'),
#     strip.background = element_rect(fill = "#d9d9d9", colour = "#d9d9d9"),
#     strip.text.x = element_text(size = 12, face = 'bold'), 
#     plot.background = element_rect(fill = '#d9d9d9',  colour = '#d9d9d9'))
# 
# 
# plotGroupedBoxes

# ggsave(plotGroupedBoxes, filename = paste0("./results/temp_maps/",
#                                              'groupedOrientation_normalized2_Guianas_1985-2020',
#                                              '_',  format(Sys.Date(), "%Y%m%d"),'.jpeg'),
#        width = 13.1, height = 7.25, units = c('in'), dpi = 1200)




##'
##' Explore properties of coastline positions 
##' No figure in manuscript
##'

# countryOI <- aoi[1]# c('Suriname')
# grouping <- c('Country')
# propertyToPlot <- c('centeredOrientation_250')  # 'meanCurvature_250' 'sinuositySmoothed_100', 'centeredOrientation_100', 'centeredOrientation2_100'
# colnames(allFiles_mutate)
# 
# # hist(allFiles_mutate$sinuositySmoothed_100)
# 
# # calculate means
# meansOI <- allFiles_mutate %>%
#   # filter(Country == countryOI) %>%
#   dplyr::group_by(eval(as.name(paste(grouping)))) %>%
#   dplyr::summarize(mean=mean(eval(as.name(paste(propertyToPlot))), na.rm = T)) %>%
#   ungroup() %>%
#   rename_at(1, ~grouping)
# 
# range <- round(quantile(allFiles_dropPOS[[propertyToPlot]],c(0.01, 0.99), na.rm=T))
# 
# # overallMean <- mean(subsetVariability$normalized, na.rm = T)
# overallMean <- allFiles_mutate %>%
#   filter(Country == countryOI) %>%
#   group_by(eval(as.name(paste(grouping)))) %>%
#   dplyr::summarize(mean=mean(eval(as.name(paste(propertyToPlot))), na.rm = T)) %>%
#   rename_at(1, ~grouping)
# 
# variability <- ggplot(data = allFiles_mutate, #%>% filter(Country == countryOI),
#                       alpha = 1,
#                       aes(x=eval(as.name(paste(propertyToPlot))), 
#                           fill = eval(as.name(paste(grouping))))) +
#   facet_wrap(~Country, ncol = 1, nrow = nrow(meansOI)) +
#   scale_y_log10(expand = c(0,0), name = 'log(obs. count)')+
#   # geom_vline(data = meansOI, aes(xintercept= mean), size = 3.5, colour = 'black',
#   # linetype="solid") +
#   # geom_vline(data = meansOI, aes(xintercept= mean, color=eval(as.name(paste(grouping)))), size = 2,
#   # linetype="solid") +
#   # geom_vline(data = overallMean, aes(xintercept= mean),color = 'Black', size = 2,
#   # linetype="dashed") +
#   geom_histogram(position = 'identity', bins = 75, alpha = 0.8) +
#   
#   # 
#   # scale_x_continuous(breaks = c(range[[1]], range[[1]]/2, 0, range[[2]]/2, range[[2]]),
#   #                    limits = c(range[[1]],range[[2]])) +
#   # guides(fill = guide_legend(override.aes = list(size = 10, alpha =1,
#   # linetype = 0)),
#   # colour = F) +
#   labs(y = 'count', x = paste0(propertyToPlot)) +
#   
#   theme(axis.line.x = element_line(size = 0.5, colour = "black"),
#         axis.line.y = element_line(size = 0.5, colour = "black"),
#         axis.line = element_line(size= 1, colour = "black"),
#         axis.title.y = element_text(size = 20, face = 'bold'),
#         axis.title.x = element_text(size = 20, face = 'bold'),
#         axis.text.x = element_text(size = 18,  hjust = .5, vjust = .5),
#         axis.text.y = element_text(size = 18, hjust = .5, vjust = .5),
#         legend.title = element_blank(), #element_text(colour = 'black', size = 14, face = "bold"),
#         legend.background = element_rect(fill = '#d9d9d9',  colour = '#d9d9d9'),
#         legend.position = c(0.9,0.8),
#         legend.text = element_text(size = 20),
#         panel.grid.major = element_blank(), # remove grid lines
#         panel.grid.minor = element_blank(), 
#         panel.background = element_blank(),
#         strip.text.x = element_blank(),
#         plot.background = element_rect(fill = '#d9d9d9'))
# 
# variability



# df5 <- allFiles_mutate %>%
#   group_by(Country, x_bins_100) %>%
#   dplyr::mutate(bin_size=n()) %>%
#   ungroup() %>%
#   dplyr::mutate(concavity = ifelse(medianC_100 > 0, 'convex', 'concave')) %>%
#   dplyr::mutate(concavityClasses = cut(medianC_100,
#                                        breaks=c(-Inf, -0.002, 0, 0.002, Inf),
#                                        labels=c("strongConcave","concave","convex", 'strongConvex')))
# # summary statistics
# deltaCoastXbins<- df5 %>%
#   filter(!is.na(x_bins_100) &
#            bin_size >10) %>%
#   dplyr::group_by(Country, x_bins_100) %>%
#   dplyr::summarize(mean=mean(deltaCoast, na.rm = T),
#                    sd=sd(deltaCoast, na.rm = T),
#                    median=median(deltaCoast,na.rm=T),
#                    n = n()) %>%
#   dplyr::mutate(col = ifelse(mean>0 , 1, 
#                              ifelse(mean < 0, 0,
#                                     NA)))
# 
# 
# df5 <- df5 %>%
#   dplyr::group_by(Country, x_bins_100) %>%
#   dplyr::mutate(mean=mean(deltaCoast, na.rm = T),
#                 median=median(deltaCoast,na.rm=T),
#                 n = n()) %>%
#   dplyr::mutate(col = ifelse(mean>0 , 1, 
#                              ifelse(mean < 0, 0,
#                                     NA))) %>%
#   ungroup()
# 
# # stacked histograms
# # x-axis (coastline change bins)
# # Y-axis (count), stacked -> 2 classes (convex <> concave)
# 
# averageDeltaCoasts <- df5 %>%
#   filter(!is.na(concavity)) %>%
#   dplyr::group_by(Country, concavity) %>%
#   dplyr::summarize(mean=mean(deltaCoast, na.rm = T),
#                    median=median(deltaCoast,na.rm=T),
#                    n = n())
# 
# averageDeltaCoastsAll <- df5 %>%
#   filter(!is.na(concavity)) %>%
#   dplyr::group_by(concavity) %>%
#   dplyr::summarize(mean=mean(deltaCoast, na.rm = T),
#                    median=median(deltaCoast,na.rm=T),
#                    n = n())
# 
# histCoastalChange <- 
#   ggplot(data = df5 %>% 
#            filter(!is.na(concavity)), 
#          aes(x=deltaCoast, fill = concavity) ) +
#   # geom_bar(position="stack", stat="identity")
#   geom_histogram( binwidth = 25, alpha = 1, position = 'stack') +
#   scale_x_continuous(limits=c(-500, 500), breaks = c(-200, 0, 200)) +
#   # scale_y_continuous(expand = c(0,0)) +
#   scale_y_log10(name = 'log(obs. count)')+
#   
#   # geom_vline(data = averageDeltaCoasts, aes(xintercept= mean), size = 1, 
#   #            colour = 'black', linetype="solid") +
#   # geom_vline(data = averageDeltaCoastsAll, aes(xintercept= mean), size = 1,
#   #            linetype="solid") +
#   facet_wrap(~Country) +
#   labs(y = 'Observations [n]', x = 'Coastline change [m/yr]') +
#   
#   theme(
#     axis.line.x = element_line(size = 0.5, colour = "black"),
#     axis.line.y = element_line(size = 0.5, colour = "black"),
#     axis.line = element_line(size=1, colour = "black"),
#     axis.text.x = element_text(color = "grey20", size = 18, hjust = .5, vjust = .5, face = "bold"),
#     axis.text.y = element_text(color = "grey20", size = 18, hjust = .5, vjust = .5, face = "bold"),
#     axis.title.y = element_text(size = 20, face = 'bold'),
#     axis.title.x = element_text(size = 20, face = 'bold'),
#     
#     legend.background = element_rect(fill = '#d9d9d9',  colour = '#d9d9d9'),
#     legend.key = element_rect(fill = NA),
#     legend.text = element_text(size = 25),#element_blank(),
#     legend.title =  element_blank(),
#     # legend.position = c(.8, .4),
#     
#     panel.border = element_blank(),
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     panel.background = element_blank(),
#     panel.spacing.x = unit(2, 'lines'),
#     strip.background = element_rect(fill = "#d9d9d9", colour = "#d9d9d9"),
#     # strip.text.x = element_text(size = 12), # Facet titles
#     plot.background = element_rect(fill = '#d9d9d9',  colour = '#d9d9d9'))
# 
# 
# histCoastalChange
#

# ggsave(histCoastalChange, filename = paste0("./results/temp_maps/",
#                                              'concavity_dCoast_Guianas_1985-2020',
#                                              '_',  format(Sys.Date(), "%Y%m%d"),'.jpeg'),
#        width = 13.1, height = 7.25, units = c('in'), dpi = 1200)



# histCoastalx_bins <- 
#   ggplot(data = df5 %>% filter(!is.na(x_bins_100) ), 
#          aes(x=deltaCoast, fill = as.factor(x_bins_100))) + 
#   geom_histogram( binwidth = 25, alpha = 1, position = 'fill') +
#   scale_x_continuous(limits=c(-750, 750), breaks = c(-400, 0, 400)) +
#   scale_y_continuous(expand = c(0,0)) +
#   scale_fill_discrete(labels = c('NW', 'N', 'NE', 'E','SE', 'SW', 'W')) +
#   # geom_vline(data = averageDeltaCoasts, aes(xintercept= mean), size = 1, colour = 'black',
#   # linetype="solid") +
#   # geom_vline(data = averageDeltaCoastsAll, aes(xintercept= mean), size = 1,
#   #            linetype="solid") +
#   facet_wrap(~Country) +
#   labs(y = 'Observations [n]', x = 'Coastline change [m/yr]') +
#   
#   theme(
#     axis.line.x = element_line(size = 0.5, colour = "black"),
#     axis.line.y = element_line(size = 0.5, colour = "black"),
#     axis.line = element_line(size=1, colour = "black"),
#     axis.text.x = element_text(color = "grey20", size = 18, hjust = .5, vjust = .5, face = "bold"),
#     axis.text.y = element_text(color = "grey20", size = 18, hjust = .5, vjust = .5, face = "bold"),
#     axis.title.y = element_text(size = 20, face = 'bold'),
#     axis.title.x = element_text(size = 20, face = 'bold'),
#     
#     legend.background = element_rect(fill = '#d9d9d9',  colour = '#d9d9d9'),
#     legend.key = element_rect(fill = NA),
#     legend.text = element_text(size = 25),#element_blank(),
#     legend.title =  element_blank(),
#     # legend.position = c(.8, .4),
#     
#     panel.border = element_blank(),
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     panel.background = element_blank(),
#     panel.spacing.x = unit(2, 'lines'),
#     strip.background = element_rect(fill = "#d9d9d9", colour = "#d9d9d9"),
#     # strip.text.x = element_text(size = 12), # Facet titles
#     plot.background = element_rect(fill = '#d9d9d9',  colour = '#d9d9d9'))
# histCoastalx_bins

# ggsave(histCoastalx_bins, filename = paste0("./results/temp_maps/",
#                                              'orientation_dCoast_GuianaCountries_1985-2020',
#                                              '_',  format(Sys.Date(), "%Y%m%d"),'.jpeg'),
#        width = 13.1, height = 7.25, units = c('in'), dpi = 1200)


# histCoastalConcavity<- 
#   ggplot(data = df5 %>% 
#            filter(!is.na(concavityClasses)), 
#          aes(x=deltaCoast, fill = concavityClasses) ) +  # x = deltaCoast / x = normalized2
#   
#   geom_histogram(binwidth = 25, alpha = 1, position = 'fill') +
#   scale_x_continuous(limits=c(-500, 500), breaks = c(-400, 0, 400)) +
#   scale_y_continuous(expand = c(0,0)) +
#   facet_wrap(~Country) +
#   labs(y = 'Observations [n]', x = 'Coastline change [m/yr]') +
#   
#   theme(
#     axis.line.x = element_line(size = 0.5, colour = "black"),
#     axis.line.y = element_line(size = 0.5, colour = "black"),
#     axis.line = element_line(size=1, colour = "black"),
#     axis.text.x = element_text(color = "grey20", size = 18, hjust = .5, vjust = .5, face = "bold"),
#     axis.text.y = element_text(color = "grey20", size = 18, hjust = .5, vjust = .5, face = "bold"),
#     axis.title.y = element_text(size = 20, face = 'bold'),
#     axis.title.x = element_text(size = 20, face = 'bold'),
#     
#     legend.background = element_rect(fill = '#d9d9d9',  colour = '#d9d9d9'),
#     legend.key = element_rect(fill = NA),
#     legend.text = element_text(size = 25),#element_blank(),
#     legend.title =  element_blank(),
#     # legend.position = c(.8, .4),
#     
#     panel.border = element_blank(),
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     panel.background = element_blank(),
#     panel.spacing.x = unit(2, 'lines'),
#     strip.background = element_rect(fill = "#d9d9d9", colour = "#d9d9d9"),
#     # strip.text.x = element_text(size = 12), # Facet titles
#     plot.background = element_rect(fill = '#d9d9d9',  colour = '#d9d9d9'))
# histCoastalConcavity

# ggsave(histCoastalConcavity, filename = paste0("./results/temp_maps/",
#                                             'stackedHist_concavity_dCoast_Guianas_1985-2020',
#                                             '_',  format(Sys.Date(), "%Y%m%d"),'.jpeg'),
#        width = 13.1, height = 7.25, units = c('in'), dpi = 1200)

##### 
##' Not a figure in manuscript
##' 
##' Compute significance according to lauzon et al .,2019 
##' correlation coefficient (zero‐lag) 
##' 

# 
# significance <- allFiles_mutate %>%
#   dplyr::group_by(Country, pos) %>%
#   dplyr::mutate(meanCurv = mean(medianC_250, na.rm=T),
#                 sdCurv = sd(medianC_250, na.rm = T),
#                 meanOrient = mean(centeredOrientation_250, na.rm=T),
#                 sdOrient = sd(centeredOrientation_250, na.rm = T),
#                 meanChange = mean(deltaCoast, na.rm=T), # or total change between start & end divided by amount of years? 
#                 sdChange = sd(deltaCoast, na.rm=T),
#                 groupSize = n()) %>%
#   dplyr::select(Country, pos, Country,medianC_250,centeredOrientation_250, deltaCoast,
#          meanCurv, sdCurv, meanChange, sdChange, meanOrient,sdOrient, groupSize) %>%
#   rowwise() %>%
#   dplyr::mutate(part1 = ((medianC_250-meanCurv)/sdCurv)*
#                   ((deltaCoast-meanChange)/sdChange)) %>%
#   dplyr::mutate(part1_orient = ((centeredOrientation_250-meanOrient)/sdOrient)*
#                   ((deltaCoast-meanChange)/sdChange)) %>%
#   group_by(Country, pos) %>%
#   dplyr::mutate(corcoef = sum(part1, na.rm = T) * (1/(groupSize-1)) ) %>%
#   dplyr::mutate(corcoef2 = sum(part1_orient, na.rm = T) * (1/(groupSize-1)) ) %>% 
# # 
# # test<-cor.test(significance$deltaCoast,significance$medianC_250, method = 'spearman',
# #           exact = F, na.action = 'na.exclude')$p.value
# 
# 
#   dplyr::mutate(pval = stats::cor.test(deltaCoast,medianC_250, method = 'spearman',
#                                    exact = F, na.action = 'na.exclude')$p.value) %>%
#   # now also for orientation
#   dplyr::mutate(pval2 = cor.test(deltaCoast,centeredOrientation_250, method = 'spearman',
#                                 exact = F)$p.value) %>%
#   
#   left_join(allFiles_sumChanges %>% 
#               dplyr::select(Country,pos, stability, withMudbank_myr, withoutMudbank_myr,
#                      sumChanges), 
#           c("Country" = "Country",
#             "pos" = "pos"), keep = F
#   )
# 
# # select significan positions and corresponding statistics
# test <- subset(significance, pval <= 0.05 | pval2 <= 0.05 ) %>%
#   dplyr::group_by(Country, pos) %>%
#   dplyr::summarise(
#     meanOrient = mean(meanOrient), meanChange = mean(meanChange),
#     meanCurv = mean(meanCurv),
#     corcoef = mean(corcoef), corcoef2 = mean(corcoef2),
#     pval = mean(pval), pval2 = mean(pval2),groupSize = mean(groupSize),
#     sumChanges = mean(sumChanges),
#     stability = stability[1]) %>%
#   dplyr::mutate(negPos = ifelse(corcoef >0, 'positive', 'negative'),
#                 scaleSignificance = ifelse(pval < 0.05, 0.95, 
#                                          0.09),
#                 scaleSignificance2 = scale(pval))
# 
# 
# # what coasltine orientations result in significant coastline changes?
# significantOrientation <- subset(test, pval <= 0.05 ) 
# 
# variability <- ggplot(data = significantOrientation, 
#                       alpha = 1,
#                       aes(x=meanCurv,y=corcoef, color = Country)) +
#   geom_point()
# 
# 
# spatialSignificance <- 
#   ggplot(data = test, alpha = 1,
#           aes(x=pos/1000,y=sumChanges, color = negPos)) +
#   geom_point(aes(size = scaleSignificance)) + 
#   scale_size_continuous(breaks = c(0, 0.95), labels = c('p = >0.05',
#                                                          'p = <0.05'), 
#                         guide_legend(title = '')) +
#   # scale_color_manual(guide_legend(title = 'Correlation Coeff')) +
#   guides(color=guide_legend(title="Correlation Coeff")) +
#   facet_wrap(~Country, ncol = 1, nrow =3) +
#   # fill = guide_legend(override.aes = list(title=""))
#   labs(x = 'Alongshore position [km]', y = 'Coastline Stability [m/yr]') +
#   theme(
#     axis.line.x = element_line(size = 0.5, colour = "black"),
#     axis.line.y = element_line(size = 0.5, colour = "black"),
#     axis.line = element_line(size=1, colour = "black"),
#     axis.text.x = element_text(color = "grey20", size = 18, hjust = .5, vjust = .5, face = "bold"),
#     axis.text.y = element_text(color = "grey20", size = 18, hjust = .5, vjust = .5, face = "bold"),
#     axis.title.y = element_text(size = 20, face = 'bold'),
#     axis.title.x = element_text(size = 20, face = 'bold'),
#     
#     legend.background = element_rect(fill = '#d9d9d9',  colour = '#d9d9d9'),
#     legend.key = element_rect(fill = NA),
#     legend.text = element_text(size = 18),#element_blank(),
#     legend.title =  element_text(size = 25),
#     # legend.position = c(.8, .4),
#     
#     panel.border = element_blank(),
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     panel.background = element_blank(),
#     panel.spacing.x = unit(2, 'lines'),
#     strip.background = element_rect(fill = "#d9d9d9", colour = "#d9d9d9"),
#     plot.background = element_rect(fill = '#d9d9d9',  colour = '#d9d9d9'))
# 
# spatialSignificance
# ggsave(spatialSignificance, filename = paste0("./results/temp_maps/",
#                                             'significance_dcoast_curvature_Guianas_1985-2020',
#                                             '_',  format(Sys.Date(), "%Y%m%d"),'.jpeg'),
#        width = 13.1, height = 7.25, units = c('in'), dpi = 1200)
