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
# library(rgee)
# Sys.setenv("RETICULATE_PYTHON" = "C:/Users/5600944/AppData/Local/r-miniconda/envs/rgee/python.exe")
ee_Initialize()

## ---------------------------
source("./src/functions.R")

## ---------------------------

mapviewOptions(basemaps = c( "Esri.WorldImagery","Esri.WorldShadedRelief", "OpenStreetMap.DE"))
dataFolder <- './data/processed/offshore_points'
# years <- c('2005', '2006','2007', '2008','2009')
years <- seq(from = 1985, to = 2021, by = 1)
aoi <- c("FrenchGuiana",'Suriname','Guyana') # "FrenchGuiana",'Suriname','Guyana'

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
# posToExclude <- allPos[[aoi]]

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

# select folders
folderSelect <- as.matrix(list.files(paste0(dataFolder), full.names = T)) # , '/offshore_points'
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
allFiles <- unique(do.call(rbind, lapply(as.matrix(filtered)[,1], 
                 function(x) read.csv(x, stringsAsFactors = FALSE,
                                      sep = ',', na.strings=c("","NA")
                                      ))))
# names(allFiles)[names(allFiles) == 'pos.x'] <- 'pos'
# names(allFiles)[names(allFiles) == 'year_col.x'] <- 'year_col'


#'
#' create an image collection
#' 

pol <- ee$Geometry$Polygon(
  coords = list(
    c(-56.856912, 5.836168),
    c(-56.821485, 6.120976),
    c(-54.262531, 6.009777),
    c(-54.255509, 5.772303)
  ),
  proj = "EPSG:4326",
  geodesic = FALSE
)


collectionL4 <- ee$ImageCollection("LANDSAT/LT04/C01/T1_TOA")$ #T1_TOA
  filterBounds(pol)

collectionL5 <- ee$ImageCollection("LANDSAT/LT05/C01/T1_TOA")$
  filterBounds(pol)

collectionL7 <- ee$ImageCollection("LANDSAT/LE07/C01/T1_TOA")$
  filterBounds(pol)
  # merge(collection)
# LANDSAT/LC08/C01/T1_RT
collectionL8 <- ee$ImageCollection("LANDSAT/LC08/C01/T1_TOA")$
  filterBounds(pol)

collection <- collectionL8$merge(collectionL5)$merge(collectionL7)
  # merge(collectionL4)


visParams <- list(bands = c('B4', 'B3', 'B2'), min = 0, max = 72)

visParamsToa = list(
    bands = c("B5", "B4", "B3"),
    min = 0.05, max = 0.4, gamma = 1.4
  )

#' implement workflow
#' 1) filter outliers & transects with NO mudbank(see pre-processing)
#' 2) create annual estimates of mudbank position or mudbank 
#' 3) alternatively apply douglas pecker algorithm
#'      - Requires to define subsections (see https://www.tandfonline.com/doi/pdf/10.1559/152304099782424901?casa_token=9wn9uSUp3zYAAAAA:XYDB0pKcZcH69STl6eOAlKoMPEwIbvxtlwUwzZ00q4V-z8yOfAREUCePnd4fiZbS9H2A-woJqt0mIg  )
#'      to ensure separate mudbanks are recognized
#'      

allFiles_dropPOS <- allFiles %>%
  dplyr::mutate(toFilter = 0) %>%
  dplyr::mutate(alongshore = 'NA') %>%
  dplyr::mutate(toFilter = ifelse((Country == "Suriname" & 
                                     pos %in% allPos[["Suriname"]]),1,toFilter),
                toFilter = ifelse((Country == "FrenchGuiana" & 
                                     pos %in% allPos[["FrenchGuiana"]]),1,toFilter),
                toFilter = ifelse((Country == "Guyana" & 
                                     pos %in% allPos[["Guyana"]]),1,toFilter)) %>%
  filter(toFilter == 0) %>%
  dplyr::select(-c(toFilter)) %>%
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
                        alongshore))

# # collection for testing
# filtCollect <- collection$filterDate(as.character(as.Date(nearestDate)-0), 
#                                      as.character(as.Date(nearestDate)+1))
# dates <- ee_get_date_ic(filtCollect, time_end = FALSE)
# first <- Map$addLayer(filtCollect$first(), visParams, paste0('landsat: ',nearestDate))
# Map$centerObject(filtCollect$first(), 14)
# 
# 
# # one image - all observations
# subset_for_testPlot <- subset(allFiles, DATE_ACQUIRED == reference_date &
#                                 !(pos %in% posToExclude))
# mudbanks_selection <-subset(subset_for_testPlot, mudbank_outlier < 1 &
#                               mudbank_extent > 0)
# 
# mudbank_selection_Outlier <- subset(subset_for_testPlot,
#                                     mudbank_outlier >= 1 |
#                                       mudbank_extent < 0)
# 
# mudbankPos <- sp_pnt_ee(mudbanks_selection$x,
#                         mudbanks_selection$y,  
#                         'non outlier abs', "#ece7f2")
# 
# outlierPos <- sp_pnt_ee(mudbank_selection_Outlier$x,
#                          mudbank_selection_Outlier$y,  'outlier',
#                         "orange")
# 
# # first + mudbankPos + outlierPos


####
#'
#' Plots for chapter 2 - coastline changes Suriname 
#'
#####
outputName <- c('FrenchGuiana')
suriData <- subset(allFiles_dropPOS, Country ==outputName )

ref_date <- as.Date(c("2009-09-12"))# as.Date('2006-08-22') # 
subsetSelectedDates <- subset(suriData, 
                              as.Date(DATE_ACQUIRED) %in% ref_date)
facet <- 'DATE_ACQUIRED'

# method figure: alongshor edistribution of mud fractions
alongshoreFracts <- ggplot(subsetSelectedDates, aes(x= pos, y = SmoothedPeakFract,  #meanMud
                               colour = as.factor(mudbank_outlier))) + 
  
  geom_point(size = 6) + # ,
  facet_wrap(paste0('~', facet), ncol = 1) + 
  scale_color_manual(labels = c("Mudbank","No mudbank"), values = c("#8c510a", "#35978f")) +
  # scale_x_reverse(lim=c(max(subsetSelectedDates$pos)+4000, 
  #                       min(subsetSelectedDates$pos)-4000), expand = c(0,0)) +
  scale_y_continuous(lim=c(0,1)) +
  labs(x = "Position", y = "Fractions", col = ' ') +
  theme(axis.line.x = element_line(size = 0.5, colour = "black"),
        axis.line.y = element_line(size = 0.5, colour = "black"),
        axis.line = element_line(size= 1, colour = "black"),
        axis.title.y = element_text(size = 30, face = 'bold'),
        axis.title.x = element_text(size = 30, face = 'bold'),
        axis.text.x = element_text(size = 18,  hjust = .5, vjust = .5),
        axis.text.y = element_text(size = 18, hjust = .5, vjust = .5),
        
        legend.key = element_rect(fill = NA),
        legend.text = element_text(size = 50),
        legend.position = c(.2, .8),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        strip.text.x = element_blank(),#element_text(size = 14, face = 'bold'),
        strip.background = element_rect(fill = NA, colour = NA)) # "#d9d9d9"

# alongshoreFracts

# ggsave(filename = paste0("./results/methodology_figures/",'alongshore_fractions',
#                          '_',ref_date[1], '_',  format(Sys.Date(), "%Y%m%d"),'.jpeg'),
#        width = 20.1, height = 7.25, units = c('in'), dpi = 1200)

# get only 1 observation per year per pos here
SuriAnnual <- suriData %>% 
  dplyr::group_by(year_col, pos) %>% 
  dplyr::distinct(deltaCoast, .keep_all = T)

# Significant values only
coastalChangeSur <- subset(SuriAnnual, mudbank_outlier == 0) # & mudbank_outlier == 0 

meansSur <- coastalChangeSur %>%
  dplyr::group_by(noMudbank) %>%
  dplyr::summarize(mean=mean(deltaCoast, na.rm = T)) %>%
  ungroup()

# Figure 9
density_mudbank <- 
  ggplot(data = coastalChangeSur,  aes(x=deltaCoast, fill=as.factor(noMudbank))) +
  geom_density(adjust = 2, alpha = 0.5) +
  
  scale_x_continuous(limits=c(-750, 750), name = 'Coastline change [m/yr]') +
  scale_y_continuous(expand = c(0,0)) +
  guides(fill=guide_legend(override.aes=list(alpha=1), 
                           ncol = 1, reverse = TRUE,
                           keywidth=1))+
  geom_text(data = meansSur %>% filter(noMudbank == 0),
            aes(label=paste0('mean: ', round(mean), ' m/yr'),
                x = 400,
                y =0.01),
            size = 8, colour = "black") +
  geom_text(data = meansSur %>% filter(noMudbank == 1),
            aes(label=paste0('mean: ', round(mean), ' m/yr'),
                x = -400,
                y =0.01),
            size = 8, colour = "black") +
  
  scale_fill_manual(labels = c('Mudbank', 'No mudbank'), 
                    values = c('#F8766D', '#00BFC4')) +
  theme(
    axis.line.x = element_line(size = 0.5, colour = "black"),
    axis.line.y = element_line(size = 0.5, colour = "black"),
    axis.line = element_line(size=1, colour = "black"),
    axis.text.x =element_blank(),
    axis.text.y = element_text(color = "grey20", size = 18, hjust = .5, vjust = .5, face = "bold"),
    axis.title.y = element_text(size = 20, face = 'bold'),
    axis.title.x = element_blank(),
    
    legend.background = element_rect(fill = '#d9d9d9',  colour = '#d9d9d9'),
    legend.key = element_rect(fill = NA),
    legend.text = element_text(size = 25),#element_blank(),
    legend.title =  element_blank(),
    legend.position = c(.8, .3),
    
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.spacing.x = unit(2, 'lines'),
    strip.background = element_rect(fill = "#d9d9d9", colour = "#d9d9d9"),
    strip.text.x = element_blank(), #element_text(size = 18, face = "bold"), # Facet titles
    plot.background = element_rect(fill = '#d9d9d9',  colour = '#d9d9d9'))

density_mudbank

# ggsave(density_mudbank, filename = paste0("./results/temp_maps/", outputName,
#                                   '_coastlineChange_histogram_','_',
#                                   format(Sys.Date(), "%Y%m%d"),'.jpeg'),
#       width = 13.1, height = 7.25, units = c('in'), dpi = 1200)

boxplot <-  ggplot(data = coastalChangeSur) +
  geom_boxplot(aes(x=deltaCoast, y = noMudbank, fill = as.factor(noMudbank))) +
  facet_wrap(~Country, nrow = 3)+
  scale_fill_manual(labels = c('Mudbank', 'No mudbank'),
                    values = c('#F8766D', '#00BFC4')) +
  scale_x_continuous(limits=c(-750, 750), name = 'Coastline change [m/yr]') +

  theme(
    axis.line.x = element_line(size = 0.5, colour = "black"),
    axis.line.y = element_line(size = 0.5, colour = "black"),
    axis.line = element_line(size=1, colour = "black"),
    axis.text.x = element_text(color = "grey20", size = 18, hjust = .5, vjust = .5, face = "bold"),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.x = element_text(size = 20, face = 'bold'),

    legend.position = 'none',#c(.8, .5),

    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.spacing.x = unit(2, 'lines'),
    strip.background = element_rect(fill = "#d9d9d9", colour = "#d9d9d9"),
    strip.text.x = element_blank(),

    plot.background = element_rect(fill = '#d9d9d9',  colour = '#d9d9d9'))

# boxplot

final <- plot_grid(density_mudbank, boxplot, align = 'v', ncol = 1, nrow =2,
                   rel_heights = c(2.5, 1.5))
# final
# ggsave(final, filename = paste0("./results/temp_maps/", outputName,'_coastlineChange_histogram_',
#                                 '_',  format(Sys.Date(), "%Y%m%d"),'.jpeg'),
#       width = 13.1, height = 7.25, units = c('in'), dpi = 1200)

# ggsave(plot = final, filename = paste0("D:/WOTRO/Research/Reporting/Publications/",
#                                        "AlongshoreVariability_mudbanks/submissionFiles/",
#                                        "figures/Figure_9.pdf"),
#        width = 13.1, height = 7.25, units = c('in'), dpi = 1200)

##################
### test 
#################

# remove duplicate observations to have accurate stats
duplicateRemove <- coastalChangeSur %>%
  filter(alongshore != 'NA') %>%
  group_by(Country, as.Date(DATE_ACQUIRED), pos) %>% 
  filter(row_number(pos) == 1) %>%
  ungroup()
# 
# allFiles3 <- get_dists2(allFiles3, allFiles3$originX, allFiles3$originY, 
#                         allFiles3$bearing, 
#                         c('coast_median'))

sampleDelta <- sample(allFiles3$deltaCoast, 2500)
shapiroTest<- shapiro.test(sampleDelta) # P<0.05 ==> not normally distributed

# thus apply wilcox test (non-parametric test)
# http://www.sthda.com/english/wiki/unpaired-two-samples-wilcoxon-test-in-r
wilcoxTest <- wilcox.test(coastalChangeSur$deltaCoast ~ coastalChangeSur$noMudbank, 
                          alternative = "two.sided", mu = 0, conf.int=T, paired = F, exact = F,
                          correct = T)
# P value < 0.05 thus the median is significantly different with a P ~ 0.00000 






####
#'
#' Plots for chapter 3 - coastline changes Guianas 
#'
#####


#################################
#'   
#'   Estimate resilient coastlines
#'   - mudbank <> noMudank
#'   
#'

sequenceLength <- 6
rectangles <- data.frame(id = character(),fill =  character(),
                         colour = character(),Country = character(),
                         xmin = double(), xmax = double(),
                         ymin = double(), ymax = double(),
                         xminTmin1 = double(), # pos previous year
                         xmaxTmin1 = double(),  ## pos previous year
                         lengthMudbank = double(), #length current year
                         lengthMudbankTmin1 = double(), # length previous year  
                         nrMudbanks = double(),
                         area = double(),
                         mudbankMaxWidth = double(),
                         mudbankMeanWidth = double(),
                         mudbankMedianWidth = double(),
                         mudbankFract = double(),
                         transectFract = double(),
                         migrationSpeed = double(), # speed based on front migration speed
                         speedMeanPos = double(), # speed based on mean posiotns
                         overlapMudbanks = double())     # amount of overlap [meters]

countries <- unique(allFiles_dropPOS$Country)
allFiles_refDate$noMudbank2 <- 1

for (cntr in countries){
  # cntr <- countries[1]
  # print(cntr)
  
  all_years <- sort(unique(pull(allFiles_dropPOS, year_col)))
  
  # get median position for each year & mudbank indicators (rectangles)
  for(y in 1:length(all_years)){
    # y <- 16
    # selected_year <- '2000-01-01'
    selected_year <- all_years[y]
    
    annualSubset <- subset(allFiles_dropPOS,
                           Country == cntr &
                             as.Date(year_col) == as.Date(selected_year) &
                             coast_outlier == 1) %>%
      filter(dropClass == 'rel') # probably only needed here
    # filter(alongshore != 'NA') %>%
    
      # filter(mudbank_outlier == 0) %>%   # outliers
      # filter(axisDist != -1)    %>%         # nonsens observations
    
    # all posiotions considered a mudbank during given year
    mudbankObs <- subset(annualSubset, #!(pos %in% posToExclude) &
                         noMudbank == 0 & axisDist != -1 & mudbank_outlier == 0)

    if(nrow(mudbankObs) == 0){next}
    
    getPos <- unique(mudbankObs$pos)
    
    # sequences
    sequences <- split(getPos, cumsum(c(0, diff(getPos) > 1000)));
    
    # drop sublist with only x amount of consequetive mudbank observations
    filtSequences <- Filter(function(x){length(x)>sequenceLength}, sequences)
    
    # in original data frame select which ranges are used as mudbank
    idx <- which(as.Date(allFiles_dropPOS$year_col) == as.Date(selected_year) & 
                   allFiles_dropPOS$Country == cntr &
                   allFiles_dropPOS$pos %in% unlist(filtSequences))
    allFiles_dropPOS$noMudbank2[idx] <- 0 # update indication of mudbank
    # length(which(allFiles_dropPOS$noMudbank2 == 0))
    
    # now compute stats of mudbanks
    startPos <- vapply(filtSequences, head, n = 1L, FUN.VALUE = numeric(1))
    endPos <- vapply(filtSequences, tail, n = 1L, FUN.VALUE = numeric(1))
    lengthMudbank <- endPos - startPos
    mudbankCount <- length(startPos)

    mudbankID <- as.numeric(names(filtSequences))
    
    yearStat <- data.frame(maxOffshoreDist = double(),
                           mudbankFraction = double(),
                           transectFraction= double(),
                           meanOffshoreDist = double(),
                           medianOffshoreDist = double(),
                           area = double())
    
    if(length(startPos) != 0){
      for(q in 1:length(mudbankID)){
        # q <- 1

        # for each sequence get mean fraction and max extent (medianOffshore)
        testFilter <- annualSubset %>% filter(pos %in% filtSequences[[q]])
        d <- max(testFilter$medianOffshore) #maximum distance corresponding to mudbank extent
        dmean <- mean(testFilter$medianOffshore, na.rm = T)   # mean offshore distance 
        dmed <- median(testFilter$medianOffshore, na.rm = T)  # median offshore distance
        f <- mean(testFilter$meanFraction)                    # mean fraction
        transectMean <-  mean(testFilter$meanMud[testFilter$meanMud>0]) #  

        # compute area
        area <- testFilter %>%
          group_by(year_col,pos) %>%
          # compute the mudbank area (m^2) at each position
          dplyr::summarise(medianOffshore = mean(medianOffshore, na.rm=T)* 1000,
                           .groups = 'drop') %>%
          ungroup() %>%
          # sum for all positions and change to km^2 
          dplyr::summarise(area = sum(medianOffshore, na.rm = T)/ 1000000) # 
        
        yearStat<-rbind(yearStat, data.frame(maxOffshoreDist = d,
                                             meanOffshoreDist = dmean,
                                             medianOffshoreDist = dmed,
                                             mudbankFraction =f,
                                             transectFraction = transectMean,
                                             area = area))
        
      }
    }
    
    if(length(startPos) == 0){
      startPos <- c(0)
      endPos <- c(0)
      lengthMudbank <- c(0)
      mudbankCount <- c(0)
      yearStat<-rbind(yearStat, data.frame(maxOffshoreDist = -1,
                                           mudbankFraction = -1,
                                           meanOffshoreDist = -1,
                                           medianOffshoreDist = -1,
                                           transectFraction = -1,
                                           area = -1
      ))
    }
    
    
    # get migration speed: look for overlapping sequences 
    # prevYear <- rectangles %>% filter(Country == cntr, 
    #                           as.Date(id) == as.Date(selected_year) - years(1))
    # if previous year migration speed is negative: look for year before?
    prevYears <- rectangles %>% filter(Country == cntr, 
                                      as.Date(id) == as.Date(selected_year) - years(1) 
                                      | as.Date(id) == as.Date(selected_year) - years(2)
                                      )
    
    if(nrow(prevYears)>1){
      
      df <- data.frame(start1 = startPos, end1= endPos) # t
      df2 <-data.frame(start2 = prevYears$xmin, end2 = prevYears$xmax,
                       year = prevYears$id) #%>% # t - 1
        
      
      data.table::setkey(data.table::setDT(df), start1, end1)
      data.table::setkey(data.table::setDT(df2), start2, end2)
      
      overlap <- data.table::foverlaps(df,df2, which=TRUE, type = 'any')
      # type = any, also test for 'within'
    
      
      compareYears <- data.frame(df[overlap$xid], # current mudbank position
                                 df2[overlap$yid], # previous mudbank pos (t-1 and t-2)
                                 length = lengthMudbank[overlap$xid],
                                 lengthtMin1 = prevYears$lengthMudbank[overlap$yid]) %>%
        dplyr::mutate(tmean = ((end1-start1)/2)+start1,           # mean position
                      tmin1Mean = ((end2-start2)/2)+start2) %>%   # mean position    
        
        dplyr::mutate(
          year = ifelse(!is.na(start2), as.character(year), as.character(selected_year)),
          years = round(time_length(difftime(as.Date(selected_year), as.Date(year)), "years")),
          
          speed = ifelse(!is.na(start2), (start2-start1)/years,0),
          speedMeanPos =ifelse(!is.na(start2), (tmin1Mean-tmean)/years, 0)) %>% # m/yr
        
        rowwise()%>%
        dplyr::mutate(overlap = sequenceOverlap(start1, end1,start2,end2)) %>% # overlap with previous year
        dplyr::group_by(start1, end1) %>%
        dplyr::mutate(obs = n(),
                      positive = any(speed >0, na.rm = T)) %>% # Amount of observations for given position
        # only groups with true (at least 1 positive) can be filtered on the speed   
        dplyr::filter(ifelse(positive == TRUE & obs>1,
                      speed>=0,speed >= min(speed))) %>%
        
        # dplyr::filter(obs>1 & speed >=0 , .preserve = T) #%>%
        
        # take most recent observation
        dplyr::filter(as.Date(year) == max(as.Date(year)), 
                      .preserve = T) %>% 
        # duplicates indicate two mudbanks in same year (t-1): select one with most overlap
        dplyr::filter(overlap == max(overlap), .preserve = T) %>%        #
        
        # if overlap is equal: select fastest migration speed
        dplyr::mutate(the_rank = rank(-speed, ties.method = "first"))%>% 
        dplyr::filter(the_rank == 1, .preserve = T) %>%
        dplyr::ungroup()
      
      
      
    }else{
      compareYears <- data.frame(start2 = rep(NA, length(startPos)),
                                 end2 = rep(NA, length(startPos)),
                                 lengthtMin1= rep(NA, length(startPos)),
                                 speed = rep(NA, length(startPos)),
                                 speedMeanPos = rep(NA, length(startPos)),
                                 overlap = rep(NA, length(startPos)))
    }
    
    
    
    # get average mud fraction for each mudbank detected?
    # for each start position subset
    rectangles <- rbind(rectangles,
                        data.frame(
          id = selected_year,fill = 'black', colour = 'black',
         Country = cntr,
         xmin = startPos,
         xmax = endPos,
         xminTmin1 = compareYears$start2, # pos previous year
         xmaxTmin1 = compareYears$end2,  ## pos previous year
         lengthMudbank = lengthMudbank, #length current year
         lengthMudbankTmin1 = compareYears$lengthtMin1, # length previous year  
         nrMudbanks = mudbankCount,
         ymin = as.Date(selected_year)-184, # the geom_tiles per year have first of januari each year as midpoint
         ymax = as.Date(selected_year)+181,
         area = yearStat$area,
         mudbankMaxWidth = yearStat$maxOffshoreDist,
         mudbankMeanWidth = yearStat$meanOffshoreDist,
         mudbankMedianWidth = yearStat$medianOffshoreDist,
         mudbankFract = yearStat$mudbankFraction,
         transectFract = yearStat$transectFraction, # annual mean transect fraction
         migrationSpeed = compareYears$speed, # speed based on front migration speed
         speedMeanPos  = compareYears$speedMeanPos, # speed based on mean posiotns
         overlapMudbanks  = compareYears$overlap*1000     # amount of overlap 
                        )) 
    
    meanVal <- ifelse(nrow(annualSubset) > 0, 
                      mean(annualSubset$deltaCoast, na.rm =T),
                      0)
    
    # medianVal <- median(annualSubset$deltaCoast, na.rm =T)
    remove(startPos, compareYears,endPos,yearStat )
    # myColors <- rbind(myColors, data.frame(id = 'annualChange', year =  selected_year,
    #                                        positionGroup = 'all', mean = meanVal,
    #                                        col =  ifelse(meanVal>0 , '#2166ac', # purple if positive
    #                                                      ifelse(meanVal < 0, '#b2182b', # green if negative
    #                                                             "grey90"))))
    print(paste0(selected_year, ': ', round(meanVal,2)))
    # if(meanVal < 0){
    #   
    #   allFiles_dropPOS$negPos[idx] <- 0}
  }
}

#################################
#'   
#'   Figure 4: annual coastline changes
#'   
#'   
#'
reference_date <-  as.Date(c("2006-01-01")) # to make it comparable with Gratiot et al., 2008
normalize_date <-  as.Date(c("1986-01-01")) # normalize to start of timeseries

allFiles3 <- allFiles_dropPOS %>%
  filter(alongshore != 'NA') %>%
  filter(mudbank_outlier == 0) %>%   # outliers
  filter(axisDist != -1)    %>%         # nonsens observations
  dplyr::group_by(Country, pos, year_col) %>% 
  
  dplyr::group_by(Country, year_col, pos) %>% 
  dplyr::distinct(deltaCoast, .keep_all = T) %>%
  ungroup %>%
  transform(Country=factor(Country,
                           levels=c("FrenchGuiana","Suriname","Guyana")))

# normalize coastline changes to reference date (2x)
allFiles_refDate <- allFiles3 %>%
  dplyr::mutate(nonOutlier = ifelse(coast_outlier == 1 & coastDist > 0 & 
                                      !(is.na(coastDist)), 1, 0)) %>%
  dplyr::group_by(Country, pos, nonOutlier) %>% 
  
  # nearestDate
  dplyr::mutate(nearestDate =  as.Date(DATE_ACQUIRED[
    which.min(abs(as.Date(DATE_ACQUIRED)-reference_date))])) %>%
  
  # corresponding coastDist and coast median values assigned as baseline
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
  ungroup() %>%
  dplyr::select(c(DATE_ACQUIRED, Country, pos, areaName, coastDist, coast_median,nonOutlier, 
                  baseline, baseline2, year_col, coast_outlier,mudbank_outlier,
                  medianOffshore, mudbankObs, noMudbank,noMudbank2, deltaCoast,
                  distX, distY, alongshore 
  ))

# Baseline is either the median annual observation close to a reference date (baseline)
# Or the annual median observation close to the first observation (baseline2)
allFiles_refDate$normalized <- as.numeric(allFiles_refDate$coastDist) - 
  as.numeric(allFiles_refDate$baseline)
allFiles_refDate$normalized2 <- as.numeric(allFiles_refDate$coastDist) - 
  as.numeric(allFiles_refDate$baseline2)
# median position per year & position compared to the median baseline value for each year 
# allFiles_refDate$normalized3 <- as.numeric(allFiles_refDate$coast_median) - 
#   as.numeric(allFiles_refDate$baseline_median)

# noMudbank == 0 ==> mudbank observation
# noMudbank2 == 0 ==> noMudbank observation after sequencelenght Filter
# coast_outlier == 0 ==> outlier
coastalChange <- allFiles_refDate %>%    #allFiles_dropPOS %>% 
  
  dplyr::group_by(Country, pos, year_col) %>% 
  dplyr::distinct(deltaCoast, .keep_all = T) %>%
  ungroup() %>%
  group_by(Country, pos) %>%
  # exlude first year for each pos (over representation of 0)
  dplyr::arrange(as.Date(year_col), by_group = T) %>%
  filter(row_number(as.Date(year_col)) > 1) %>%
  dplyr::select(Country, year_col,pos,alongshore, deltaCoast,noMudbank, noMudbank2)%>%
  transform(Country=factor(Country,
                           levels=c("FrenchGuiana","Suriname","Guyana")))
# already filtered: nonsense obserations (axistDist -1)
#                   mudbank outliers
outputName <- ifelse(length(aoi) > 1, 
                     'Guianas', aoi)

# mean values: noMudbank / noMudbank2 (updated: incl. sequence length filter)
meanMudbank <- mean(coastalChange$deltaCoast[which(coastalChange$noMudbank2 == 0)], na.rm = T)
meanNoMudbank <- mean(coastalChange$deltaCoast[which(coastalChange$noMudbank2 == 1)], na.rm = T)

# calculate means per country and mudbank <> no mudbank
meansOI <- coastalChange %>%
  dplyr::group_by(Country, noMudbank2) %>% # noMudbank2
  dplyr::summarize(mean=mean(deltaCoast, na.rm = T)) %>%
  ungroup()

annual_coastlineC <- 
  ggplot(data = coastalChange,  aes(x=deltaCoast, fill=as.factor(noMudbank2))) +
  geom_histogram(position = 'identity', binwidth = 25, alpha = 0.7)  +
  facet_wrap(~Country, nrow = 3)+
  scale_x_continuous(limits=c(-750, 750), name = 'Coastline change [m/yr]') +
  scale_y_log10(expand = c(0,0), name = 'log(obs. count)')+
  guides(fill=guide_legend(override.aes=list(alpha=1), 
                           ncol = 2, reverse = TRUE,keywidth=1))+
  geom_text(data = meansOI %>% filter(noMudbank2 == 0), 
            aes(label=paste0('mean: ', round(mean), ' m/yr'),
                x = 400,y =150),size = 8, colour = "black") +
  geom_text(data = meansOI %>% filter(noMudbank2 == 1), 
            aes(label=paste0('mean: ', round(mean), ' m/yr'),
                x = -400,y =150),size = 8, colour = "black") +
  scale_fill_manual(labels = c('Mudbank', 'No mudbank'), 
                    values = c('#F8766D', '#00BFC4')) +
  theme(
    axis.line.x = element_line(size = 0.5, colour = "black"),
    axis.line.y = element_line(size = 0.5, colour = "black"),
    axis.line = element_line(size=1, colour = "black"),
    axis.text.x =element_text(color = "grey20", size = 18, hjust = .5, vjust = .5, face = "bold"),
    axis.text.y = element_text(color = "grey20", size = 18, hjust = .5, vjust = .5, face = "bold"),
    axis.title.y = element_text(size = 20, face = 'bold'),
    axis.title.x = element_text(size = 20, face = 'bold'),
    
    legend.background = element_rect(fill = '#d9d9d9',  colour = '#d9d9d9'),
    legend.key = element_rect(fill = NA),
    legend.text = element_text(size = 25),#element_blank(),
    legend.title =  element_blank(),
    legend.position = 'bottom',#c(.2, -.07),
    
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.spacing.x = unit(2, 'lines'),
    strip.background = element_rect(fill = "#d9d9d9", colour = "#d9d9d9"),
    strip.text.x = element_text(size = 18, face = "bold"), # Facet titles
    plot.background = element_rect(fill = '#d9d9d9',  colour = '#d9d9d9'))

annual_coastlineC

# ggsave(annual_coastlineC, filename = paste0("./results/temp_maps/", outputName,
#                                   '_coastlineChange_histogram_','_',
#                                   format(Sys.Date(), "%Y%m%d"),'.jpeg'),
#       width = 13.1, height = 7.25, units = c('in'), dpi = 1200)






###############################
##'
##' Mudbank statistics
##' Figure 6: mudbank dimensions (boxplot)
##' 
##' 
rectangles <- transform(rectangles,
                        Country=factor(Country, levels=c("FrenchGuiana","Suriname","Guyana")))

rectangles <- rectangles %>%
  rowwise() %>%
  dplyr::mutate(LWratio = lengthMudbank/mudbankMeanWidth)

# write_csv(rectangles, paste0(wd,"/data/processed/mudbanks/mudbanks.csv"))
annualStats <- rectangles %>%
  dplyr::group_by(Country, id) %>%  # group per year & country
  dplyr::mutate(meanArea = mean(area, na.rm = T),
                meanLength = mean(lengthMudbank, na.rm = T),
                meanMudbankCount = mean(nrMudbanks, na.rm = T),
                mudbankMeanWidth = mean(mudbankMeanWidth, na.rm = T),
                meanLWratio =mean(LWratio, na.rm = T) 
  ) %>%

  dplyr::summarize(meanSpeed = mean(migrationSpeed, na.rm = T),
                   maxSpeed = max(migrationSpeed, na.rm = T),
                   sdSpeed = sd(migrationSpeed, na.rm = T),
                   meanSpeed = mean(migrationSpeed, na.rm = T),
                   meanArea = meanArea[1],
                   meanLength = meanLength[1],
                   meanMudbankCount = meanMudbankCount[1],
                   mudbankMeanWidth = mudbankMeanWidth[1],
                   meanLWratio = meanLWratio[1]
  )

statToPlot <- 'lengthMudbank'#'lengthMudbank' / 'mudbankFract' / 'nrMudbanks' / 'transectFract'
# 'mudbankMaxWidth' / 'mudbankMeanWidth' / 'mudbankMedianWidth' / 'migrationSpeed' / 'speedMeanPos' / 'LWratio' / 'area'
# hist(rectangles[[statToPlot]])

# plot mudbank stats
mudbankStats <-  ggplot(rectangles #%>% 
      # filter(
        # eval(as.name(paste(statToPlot))) >= 0,
          # abs(eval(as.name(paste(statToPlot)))) < 10000)
          , aes(x= mudbankMeanWidth/1000,#as.Date(id), 
                y = eval(as.name(paste(statToPlot)))/1000, 
                group = id,color = Country)) +  
  geom_point()+
  # geom_boxplot(outlier.shape=NA)+ 
  # facet_wrap(~Country) +
  
  # labs(y =  statToPlot, x = "year") +
  # scale_y_continuous(limits=c(-150,150)) +
  # scale_x_date(date_breaks = "5 years", date_labels = "%Y") +

  theme(
    axis.line.x = element_line(size = 0.5, colour = "black"),
    axis.line.y = element_line(size = 0.5, colour = "black"),#element_blank(),
    axis.text.x = element_text(color = "grey20", size = 12, hjust = .5, vjust = .5),
    axis.title.x = element_text(size = 12, face = 'bold'),
    axis.text.y = element_text(color = "grey20", size = 12, hjust = .5, vjust = .5),
    axis.title.y = element_text(size = 12, face = 'bold'),
    legend.background = element_rect(fill = '#d9d9d9',  colour = '#d9d9d9'),
    legend.key = element_rect(fill = NA),
    legend.position = c(.8, .8),
    legend.title = element_blank(),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.spacing.x = unit(2, 'lines'),
    plot.background = element_rect(fill = '#d9d9d9',  colour = '#d9d9d9'),
    strip.text.x = element_text(size = 16, face = 'bold') # Facet titles
  )
mudbankStats

# ggsave(mudbankStats,
#         filename = paste0("./results/temp_maps/",'mudbankStat_',statToPlot,
#                          '_', format(Sys.Date(), "%Y%m%d"),'.jpeg'),
#        width = 13.1, height = 7.25, units = c('in'), dpi = 1200)


###############################
##'
##' plot Migration speeds
##' not included as figure in publication
##'
##' 

speedToPlot <- 'migrationSpeed' 
# speedMeanPos / migrationSpeed / mudbankMeanWidth / area / mudbankFract / mudbankMedianWidth

migrationStats <- rectangles %>%
  dplyr::group_by(Country, id) %>%  # group per year & country
  filter( #eval(as.name(paste(speedToPlot))) >= 0 &
           abs(eval(as.name(paste(speedToPlot)))) < 10000 ) %>%
  dplyr::summarize(mean=mean(eval(as.name(paste(speedToPlot))), na.rm = T),
                   max = max(eval(as.name(paste(speedToPlot))), na.rm = T),
                   stdv = sd(eval(as.name(paste(speedToPlot))), na.rm = T),
                   median = median(eval(as.name(paste(speedToPlot))), na.rm = T),
                   count = nrMudbanks[1]) 

CountryAverage <- rectangles %>%
  dplyr::filter(as.Date(id) > as.Date('2000-01-01') ) %>% # only when older than 2000
  dplyr::group_by(Country) %>%  # group per year & country
  filter(#eval(as.name(paste(speedToPlot))) >=0 &
           abs(eval(as.name(paste(speedToPlot)))) < 10000 ) %>%
  dplyr::summarize(mean=mean(eval(as.name(paste(speedToPlot))), na.rm = T))


migration_speed <- ggplot(migrationStats, aes(x=as.Date(id), y=mean/1000, 
                                              color = Country)) +
  facet_wrap(~Country, ncol = 1, nrow = 3, scales = "free_x") +
  annotate("rect", xmin = min(as.Date(migrationStats$id)), 
           xmax = max(as.Date(migrationStats$id))+90, fill = '#f7fcb9',
           ymin = min(migrationStats$mean)/1000, ymax = 0, alpha = 0.5) +

  geom_point(size = 3, alpha = 0.6) +
  
  guides(color=guide_legend(override.aes=list(alpha=1), 
                           ncol = 3, 
                           keywidth=1))+
  
  labs(y =  'Migration Speed [km/yr]', x = "Year") +
  # geom_hline(data = CountryAverage,  aes(yintercept = mean/1000,
  #                                        color = Country)) +
  geom_segment(data = CountryAverage, 
               aes(x=as.Date('2000-01-01'), xend = as.Date('2021-01-01'),
                   y=mean/1000,yend = mean/1000, color = Country)) +
  geom_text(data = CountryAverage,
            aes(label=paste0('mean: ', round(mean/1000,2), ' km/yr'),
                x = as.Date('2014-01-01'),
                y = 4),
            size = 8, colour = "black") +
  scale_y_continuous(limits =  c(min(migrationStats$mean)/1000, 
                                 max(migrationStats$mean)/1000)
                     ,expand = c(0,0))+ # breaks = round(c(-2.5, CountryAverage$mean/1000, 2.5, 5),2)
  scale_x_date(expand = c(0,0),date_breaks = "3 years", date_labels = "%Y",
               limits = c(min(as.Date(migrationStats$id)),
                          max(as.Date(migrationStats$id))+90)) +
  
  theme(
    axis.line.x = element_line(size = 0.5, colour = "black"),
    axis.line.y = element_line(size = 0.5, colour = "black"),#element_blank(),
    axis.text.x = element_text(color = "grey20", size = 14, hjust = .5, vjust = .5),
    axis.title.x = element_text(size = 18, face = 'bold'),
    axis.text.y = element_text(color = "grey20", size = 14, hjust = .5, vjust = .5),
    axis.title.y = element_text(size = 18, face = 'bold'),
    legend.background = element_rect(fill = '#d9d9d9',  colour = '#d9d9d9'),
    legend.key = element_rect(fill = NA),
    legend.position = c(.25, .95),
    legend.title = element_blank(),
    legend.text =  element_text(size = 18, face = 'bold'),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.spacing.x = unit(2, 'lines'),
    
    plot.background = element_rect(fill = '#d9d9d9',  colour = '#d9d9d9'),
    strip.text.x =element_blank() #  element_text(size = 16, face = 'bold'
  )

# ggsave(migration_speed,
#         filename = paste0("./results/temp_maps/",speedToPlot,'_Guianas',
#                          '_', format(Sys.Date(), "%Y%m%d"),'.jpeg'),
#        width = 13.1, height = 7.25, units = c('in'), dpi = 1200)

rectangles_reformatPos <- rectangles %>%
  dplyr::mutate(xmin = ifelse(Country == 'Guyana',
                              xmin - 39000,
                              xmin),
                xmax = ifelse(Country == 'Guyana',
                              xmax - 39000,
                              xmax)) %>%
  dplyr::filter(xmin > -1)

pSpeed <-ggplot(rectangles_reformatPos, #& !(pos %in% posToExclude)),
                aes(x = xmin, y = as.Date(id))) + 
  
  geom_rect(data = rectangles_reformatPos, inherit.aes = FALSE,
            aes(xmin = xmin, xmax = xmax,
                ymin = ymin, ymax = ymax-125), 
            fill = 'black',color='black', size = 0.7) + 
  
  # these are swaped around: first the empty box, then the filled rectangles: easier to visualize
  geom_rect(data = subset(rectangles_reformatPos,  
                          # eval(as.name(paste(speedToPlot)))  >= 0 &
                            abs(eval(as.name(paste(speedToPlot))))  < 10000), 
            inherit.aes = FALSE,
            aes(xmin = xmin, xmax = xmax,
                ymin = ymin, ymax = ymax-125, 
                fill =  eval(as.name(paste(speedToPlot)))/1000), # 
            size = 0.8) +
  facet_wrap(~Country, ncol = 1, nrow = 3) +
  labs(x =  'Alongshore position [km]', y = "Year") +
  scale_fill_gradientn(
    # name = 'migration \n [km/yr] \n',
    name = 'median \n extent [km] \n',
    # name = 'area mudbank \n extent [km2] \n',
    colours = c( "#4575b4",'#ffffbf','#d73027' )) +
  theme(axis.line.x = element_line(size = 0.5, colour = "black"),
        axis.line.y = element_line(size = 0.5, colour = "black"),
        axis.line = element_line(size= 1, colour = "black"),
        axis.title.y = element_text(size = 12, face = 'bold'),
        axis.title.x = element_text(size = 12, face = 'bold'),
        axis.text.x = element_text(size = 12,  hjust = .5, vjust = .5),
        axis.text.y = element_text(size = 12, hjust = .5, vjust = .5),
        legend.background = element_rect(fill = '#f0f0f0',  colour = '#f0f0f0'),
        legend.spacing.y = unit(0.1, 'cm'),
        legend.title = element_text(colour = 'black', size = 14, face = "bold"),
        strip.text.x = element_text(size = 16, face = 'bold'),
        strip.background = element_rect(fill = '#f0f0f0',  colour = '#f0f0f0'),
        legend.text = element_text(size = 12),
        legend.position = c(0.9,0.8),
        panel.grid.major = element_blank(), # remove grid lines
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        plot.background = element_rect(fill = '#f0f0f0',  colour = '#f0f0f0'))

pSpeed

# ggsave(pSpeed,
#         filename = paste0("./results/temp_maps/","Hovmoller_", speedToPlot,
#                          '_', format(Sys.Date(), "%Y%m%d"),'.jpeg'),
#        width = 13.1, height = 7.25, units = c('in'), dpi = 1200)

# get only 1 observation per year per pos here
# transformed <- allFiles_dropPOS %>% 
#   dplyr::group_by(Country, year_col, pos) %>% 
#   dplyr::distinct(deltaCoast, .keep_all = T) %>%
#   # filter(n()>1) %>% 
#   ungroup() #%>%
# # dplyr::select(DATE_ACQUIRED, pos, year_col, deltaCoast, noMudbank,coastDist, 
# #               mudbank_outlier,coast_median, Country,alongshore, coast_outlier,
# #               medianOffshore,areaName)

transformed <-
  transform(allFiles_refDate,
            Country=factor(Country, levels=c("FrenchGuiana","Suriname","Guyana")))

simplify <- transformed %>%
  filter(noMudbank2 == 0 & medianOffshore >0) %>% # only for mudbanks plot 
  dplyr::mutate(pos = ifelse(Country == 'Guyana', # start at 0 for Guyana 
                              pos - 39000,
                              pos)) %>%
  dplyr::filter(pos > -1)               

range <- round(quantile(simplify$medianOffshore,c(0.1,0.5, 0.9), na.rm=T))
  

pWidth <-ggplot(simplify, #& !(pos %in% posToExclude)),
                aes(x = pos/1000,y = as.Date(year_col), fill=medianOffshore)) + 
  geom_tile(color= "white",size=0.1, na.rm = TRUE) +

  scale_fill_gradientn(name = 'mudbank boundary \n offshore [m] \n',
                       breaks = c(range[[1]], range[[2]], range[[3]]),
                       limits = c(range[[1]],range[[3]]),
                       colours = c('#ffffd9', '#41b6c4', "#081d58"),
                       guide = guide_colourbar(nbin=100, draw.ulim = FALSE, draw.llim = FALSE),
                       oob=squish,
                       values = scales::rescale(c(range[[1]],range[[2]], range[[3]]))
  ) +
  scale_x_continuous(expand = c(0,0)) +

  # geom_rect(data = rectangles_reformatPos, inherit.aes = FALSE,
  #           aes(xmin = xmin, xmax = xmax,
  #               ymin = ymin, ymax = ymax-125),
  #           fill = NA,color='black', size = 0.7) +
  facet_wrap(~Country, ncol = 1, nrow = 3) +
  labs(x =  'Alongshore position [km]', y = "Year") +

  theme(axis.line.x = element_line(size = 0.5, colour = "black"),
        axis.line.y = element_line(size = 0.5, colour = "black"),
        axis.line = element_line(size= 1, colour = "black"),
        axis.title.y = element_text(size = 12, face = 'bold'),
        axis.title.x = element_text(size = 12, face = 'bold'),
        axis.text.x = element_text(size = 12,  hjust = .5, vjust = .5),
        axis.text.y = element_text(size = 12, hjust = .5, vjust = .5),
        legend.background = element_rect(fill = '#f0f0f0',  colour = '#f0f0f0'),
        legend.spacing.y = unit(0.1, 'cm'),
        legend.title = element_text(colour = 'black', size = 14, face = "bold"),
        strip.text.x = element_text(size = 16, face = 'bold'),
        strip.background = element_rect(fill = '#f0f0f0',  colour = '#f0f0f0'),
        legend.text = element_text(size = 12),
        legend.position = c(0.8,0.9),
        panel.grid.major = element_blank(), # remove grid lines
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        plot.background = element_rect(fill = '#f0f0f0',  colour = '#f0f0f0'))

ggsave(pWidth,
        filename = paste0("./results/temp_maps/",'Hovmoller_mudbankWidth',
                         '_', format(Sys.Date(), "%Y%m%d"),'.jpeg'),
       width = 13.1, height = 7.25, units = c('in'), dpi = 1200)

# # prepare export TO GEE
# # containing only 1 observation per year, per pos.
# testForExport <- annualStats %>%
#   dplyr::select(c(pos,year_col, validMudbankObs,
#                   distX, distY, medianOffshore,
#                   deltaCoast )) %>%
#   dplyr::group_by(year_col, pos) %>%
#   dplyr::distinct(deltaCoast, .keep_all = T)
# # summarize()
# 
# # remove NA values in export
# dplyr::mutate(coastDist = ifelse(is.na(coastDist), -1, coastDist)) %>%
#   dplyr::mutate(validMudbankObs = ifelse(is.na(validMudbankObs), 0,
#                                          validMudbankObs))
# 
# test<-st_as_sf(testForExport, coords = c("distX", "distY"),remove = F, crs = 4326)
# 
# test$validMudbankObs <- ifelse(is.na(test$validMudbankObs), 0,
#                                test$validMudbankObs)
# 
# 
# sf_to_ee <- ee$FeatureCollection(sf_as_ee(st_as_sf(testForExport,
#                                                    coords = c("distX", "distY"),
#                                                    crs = 4326)))
# 
# 
# fileN <- paste0(aoi,'_',year,'_offshore')
# assetid <- paste0(ee_get_assethome(), '/',aoi,'_mudbanks/',fileN)
# 
# task_vect <- ee_table_to_asset(
#   collection = sf_to_ee,
#   description = fileN,
#   assetId = assetid,
#   overwrite = TRUE
# )
# task_vect$start()


# highest density
# apply only to valid points.
# test if it helps to do this per transect.
# as alternative to using the median position

# # now all obs within the selected year
# dateForTest <- as.Date("2002-01-01")
# annual_obs <- subset(allFiles,
#                      as.Date(year_col) == dateForTest &
#                        !(pos %in% posToExclude))
# 
# annual_obs_filter <- annual_obs %>%
#   filter(noMudbank == 0) %>%         # positions that are indicated as no mudbank
#   filter(mudbank_outlier == 0) %>%   # outliers
#   filter(axisDist != -1)             # nonsens observations
# 
# 
# selection_density <- pointdensity(df = annual_obs_filter, lat_col = "x", 
#                                   lon_col = "y", date_col = NULL, 
#                                   grid_size = 0.1, radius = 1)
# 
# countRange <- round(quantile(selection_density$count,c(0.01, 0.99), na.rm=T))
# 
# # now it is the trick to find the max count for the selected transect
# Tempcount <- merge(annual_obs_filter, selection_density, by.x=c('x', 'y'), 
#                    by.y=c('lat', 'lon'))
# 
# # find max count for each position
# # e.g. the location where the density is largest
# density <- unique(Tempcount) %>% 
#   dplyr::group_by(pos) %>%
#   dplyr::filter(count > countRange[1]) %>% # not yet  very succesfull
#   top_n(1, count) %>%
#   ungroup()  %>%
#   dplyr::select(!c(CLOUD_COVER, SmoothedPeak, SmoothedPeakFract,areaName, coastDist,
#              coastX, coastY, ndwi_threshold, offsetLast, originX, originY,
#              bearing, geometry, quarterly_col, date_col, five_year_col,
#              year_col, coast_outlier, locf, deltaCoast,dateavg))
# 
# # make it spatial
# maxCount <- SpatialPoints(data.frame(x = density$x,y = density$y),
#                                   CRS("+proj=longlat +datum=WGS84"))
# 
# maxCount_spatial <- sp_pnt_ee(maxCount$x,maxCount$y,
#                               'density', "orange")
# # addComposite + meanPos_sp + maxCount_spatial
# 
# # ggplot idea: https://www.earthdatascience.org/tutorials/visualize-2d-point-density-ggmap/
# pointDensity <- ggplot(subset(annual_obs,   # mudbank_outlier == 0 &
#                                 x != -1), 
#                        aes(x = x, y = y, colour = SmoothedPeakFract)) +
#   geom_point(size = 1.6, alpha = 0.3) +
#   
#   scale_colour_gradient2(low = "#2166ac",
#                          high = "#b2182b",
#                          mid = '#fddbc7', midpoint = 0.4,
#                          na.value = NA,
#                          guide = guide_colourbar(direction = 'horizontal', order = 1)) +
#   geom_point(data = subset(annual_obs,  mudbank_outlier == 0 &
#                            x != -1 & noMudbank == 0),
#              aes(x = distX, y = distY, fill = 'black'), colour = 'black', 
#              size = 4, alpha = 1) + 
#   scale_fill_manual(name = ' ', values = c('black'= 'black'), 
#                     labels = c('mudbank'), 
#                     guide = guide_legend(order =2, label.position = 'left',
#                                          label.theme = element_text(size = 40))) +
#   # coord_equal(xlim = c(-57,-54), ylim = c(5.8, 6.2), ratio = 1.5) +
#   labs(x = "Longitude", y = "Latitude", col = 'Fraction', 
#        title = paste0(year(dateForTest))) +
#   theme(axis.line.x = element_line(size = 0.5, colour = "black"),
#         axis.line.y = element_line(size = 0.5, colour = "black"),
#         axis.line = element_line(size= 1, colour = "black"),
#         axis.text.x = element_text(size = 18,  hjust = .5, vjust = .5),
#         axis.text.y = element_text(size = 18, hjust = .5, vjust = .5),
#         axis.title.x = element_text(size = 40, face = "bold"), 
#         axis.title.y = element_text(size = 40, face = "bold"), 
#         legend.title = element_text(colour = 'black', size = 40),
#         legend.key = element_rect(fill = NA),
#         legend.text = element_text(size = 18),
#         legend.position = c(.5, -.4),
#         legend.direction = "horizontal",
#         legend.box = "horizontal",
#         plot.title = element_blank(),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         plot.background = element_rect(fill = NA), # '#d9d9d9'
#         strip.text.x = element_blank())
# pointDensity


# ggsave(filename = paste0("./results/methodology_figures/",'annual_fractions',
#                          '_',dateForTest[1], '_',  format(Sys.Date(), "%Y%m%d"),'.jpeg'),
#        width = 21.1, height = 7.25, units = c('in'), dpi = 1200)


#' #'
#' #' select 1 year as example
#' #'  for plotting purposes
#' #'  
#' 

#' 
#' # build image collection around that year
#' filtCollectAnnual <- collection$filterDate(as.character(dateForTest), 
#'                                            as.character(dateForTest + 365))
#' dates <- ee_get_date_ic(filtCollectAnnual, time_end = FALSE)
#' 
#' medianComp <- filtCollectAnnual$median()
#' # composite <- ee$Algorithms$Landsat$simpleComposite(filtCollectAnnual)
#' # print(medianComp$bandNames()$getInfo())
#' 
#' Map$centerObject(filtCollectAnnual$first(), 11)
#' addComposite <- Map$addLayer(medianComp, visParamsToa, paste0('landsat: ',dateForTest))
#' 

#' 
#' annual_obs_nonOutlier <- subset(allFiles,  
#'                                 as.Date(year_col) == dateForTest &
#'                                   !(pos %in% posToExclude) &
#'                                   mudbank_outlier == 0)
#' 
#' annual_obs_outlier <- subset(allFiles,  
#'                              as.Date(year_col) == dateForTest &
#'                                !(pos %in% posToExclude) &
#'                                mudbank_outlier > 0)
#' 
#' # make it spatial.  
#' annual_mudbankPos <- sp_pnt_ee(annual_obs_nonOutlier$x,
#'                                annual_obs_nonOutlier$y,  
#'                                'non outlier abs', "#ece7f2")
#' 
#' annual_outlierPos <- sp_pnt_ee(annual_obs_outlier$x,
#'                                annual_obs_outlier$y,  'outlier',
#'                                "#fee0d2")
#' 
#' # calculated distance to spatial for plotting
#' meanPos <- SpatialPoints(data.frame(x = annual_obs$distX[complete.cases(annual_obs$distX)],
#'                                     y =  annual_obs$distY[complete.cases(annual_obs$distY)]),
#'                          CRS("+proj=longlat +datum=WGS84"))
#' meanPos_sp <- sp_pnt_ee(meanPos$x,meanPos$y, 'meanPos', "yellow")
#' 
#' 
#' # for plotting purpose, filter them

#' 
#' # also make it spatial for plotting
#' annual_obs_filter_sp <- sp_pnt_ee(annual_obs_filter$x,
#'                                   annual_obs_filter$y,  
#'                                   'filtered', "red")
#' 
#' # addComposite + annual_mudbankPos + annual_outlierPos + annual_obs_filter_sp + meanPos_sp
#' 

#' 
#' #'
#' #' Test Mas observations versus fractions
#' #' 
#' #' 
#' 
#' mas_folder <- 'D:/BackUp_D_mangroMud_202001/Research/External_Data/Spatial/Kustmetingen_MAS/surveys'
#' # relevant files: 
#' # 4-6-2002 (near Suriname river)
#' # 31-12-2008 (entire coast of Suriname)
#' 
#' # mas_folder2 <- 'D:/BackUp_D_mangroMud_202001/Research/External_Data/Source/KustmetingMAS_source/shp'
#' mas_folder2 <- 'D:/WOTRO/Research/External_Data/Spatial/Kustmetingen_MAS/surveys'
#' # D:\WOTRO\Research\External_Data\Spatial\Kustmetingen_MAS\surveys
#' # relevant file: 2016_west 
#' # to lesser extent 2014_oost
#' 
#' folderSelect <- as.matrix(list.files(paste0(mas_folder2), full.names = T))
#' df <- rewrite(folderSelect);
#' df <- df[grep('.shp$', folderSelect, ignore.case = T),]
#' 
#' 
#' 
#' # yearsMas <- c('2008')
#' # filteredMas <- vector('list', 100)
#' # for (q in seq_along(yearsMas)) {
#' #   # q <- 1
#' #   year = yearsMas[q]
#' #   
#' #   filters = c(year)
#' #   
#' #   filteredMas = rbind(filteredMas, df %>% 
#' #                      dplyr::filter(
#' #                        filters %>%
#' #                          # apply the filter of all the text rows for each pattern
#' #                          # you'll get one list of logical by pattern ignored_string
#' #                          purrr::map(~ to_keep(.x, text = text)) %>%
#' #                          # get a logical vector of rows to keep
#' #                          purrr::pmap_lgl(all)
#' #                      ))
#' # }
#' # filteredMas <- unique(filteredMas)
#' # allMasPoints <- do.call(rbind, lapply(as.matrix(filteredMas)[,1], function(x) shapefile(x)))
#' 
#' # or a single file
#' 
#' # df <- df[grep('2008_', df, ignore.case = T),]
#' allMasPoints <- shapefile(paste0(df[24,])) # 23 == 2008 / 24 = 2016
#' 
#' allMasPoints <- spTransform(allMasPoints, CRS = CRS("+init=epsg:32621"))
#' coordinatesMasUtm <- coordinates(allMasPoints)
#' 
#' allMasPointsWGS84 <- spTransform(allMasPoints, CRS = CRS("+proj=longlat +datum=WGS84"))
#' coordinatesMAS <- coordinates(allMasPointsWGS84)
#' coordinatesMAS_sp <- sp_pnt_ee(coordinatesMAS[,1], coordinatesMAS[,2],
#'                               'masPoints', "#e5f5e0")
#' 
#' addComposite + coordinatesMAS_sp + annual_mudbankPos +
#'   annual_outlierPos + annual_obs_filter_sp + meanPos_sp
#' 
#' 
#' hist(allMasPoints$z)
#' 
#' 
#' # library(geosphere)
#' library(rgeos)
#' # library(spatialEco)
#' # library(sp)
#' 
#' # get nearest observation for 2005
#' annual_obs_nonOutlier_sp <- SpatialPoints(data.frame(x = annual_obs_nonOutlier$x,
#'                                                      y = annual_obs_nonOutlier$y),
#'                                         CRS("+proj=longlat +datum=WGS84"))
#' # do in UTM 21 to get distance in meters
#' annual_obs_nonOutlier_sp_utm <- spTransform(annual_obs_nonOutlier_sp, CRS = CRS("+init=epsg:32621"))
#' 
#' pnts <- data.frame(DATE_ACQUIRED = as.Date(character()),
#'                    pos = character(),dropClass = character(),
#'                    axisDist = double(),mudFract = double(),
#'                    x = double(), y = double(),
#'                    xMas = double(), yMax = double(),
#'                    zMax = double(), distance = double())
#' # for all relevant points 
#' for(pnt in 1:length(annual_obs_nonOutlier_sp_utm)){
#'   # pnt <- 1
#'   # print(pnt)
#'   
#'   dist_with_markers <- gDistance(annual_obs_nonOutlier_sp_utm[pnt], allMasPoints, byid=T)
#'   nearest <- apply(dist_with_markers,2, which.min)
#'   nearest_dist <- apply(dist_with_markers, 2, min) 
#'   
#'   
#'   # get the details of nearest points in a data.frame together
#'   pnts<- rbind(pnts, data.frame(DATE_ACQUIRED = annual_obs_nonOutlier$DATE_ACQUIRED[pnt],
#'              pos = annual_obs_nonOutlier$pos[pnt],
#'              dropClass = annual_obs_nonOutlier$dropClass[pnt],
#'              axisDist = annual_obs_nonOutlier$axisDist[pnt],
#'              mudFract = annual_obs_nonOutlier$mudFract[pnt],
#'              x = annual_obs_nonOutlier$x[pnt],
#'              y = annual_obs_nonOutlier$y[pnt],
#'              xMas = coordinatesMAS[nearest,1],# coordinates remain in WGS84 for consistency
#'              yMax = coordinatesMAS[nearest,2],
#'              zMax = coordinatesMAS[nearest,3],
#'              distance = nearest_dist))
#' }
#' 
#' filtered <- annual_obs %>% distinct(distX, distY, .keep_all = TRUE)
#' 
#' annual_obs_meanPos_sp <- SpatialPoints(data.frame(x = filtered$distX[complete.cases(filtered$distX)],
#'                                                   y = filtered$distY[complete.cases(filtered$distX)]),
#'                                        CRS("+proj=longlat +datum=WGS84"))
#' annual_obs_meanPos_sp_utm <- spTransform(annual_obs_meanPos_sp, CRS = CRS("+init=epsg:32621"))
#' 
#' pnts_boundary <- data.frame(DATE_ACQUIRED = as.Date(character()),
#'                    pos = character(),dropClass = character(),
#'                    axisDist = double(),mudFract = double(),
#'                    x = double(), y = double(),
#'                    xMas = double(), yMax = double(),
#'                    zMax = double(), distance = double())
#' 
#' 
#' for(pnt in 1:length(annual_obs_meanPos_sp_utm)){
#'   # pnt <- 1
#'   # print(pnt)
#'   
#'   dist_with_markers <- gDistance(annual_obs_meanPos_sp_utm[pnt], allMasPoints, byid=T)
#'   nearest <- apply(dist_with_markers,2, which.min)
#'   nearest_dist <- apply(dist_with_markers, 2, min) 
#'   
#'   
#'   # get the details of nearest points in a data.frame together
#'   pnts_boundary<- rbind(pnts_boundary, data.frame(DATE_ACQUIRED = filtered$DATE_ACQUIRED[pnt],
#'                                 pos = filtered$pos[pnt],
#'                                 dropClass = filtered$dropClass[pnt],
#'                                 axisDist = filtered$axisDist[pnt],
#'                                 mudFract = filtered$mudFract[pnt],
#'                                 x = filtered$distX[pnt],
#'                                 y = filtered$distY[pnt],
#'                                 xMas = coordinatesMAS[nearest,1],# coordinates remain in WGS84 for consistency
#'                                 yMax = coordinatesMAS[nearest,2],
#'                                 zMax = coordinatesMAS[nearest,3],
#'                                 distance = nearest_dist))
#' }
#' 
#' # limit allowed distance
#' pnts_limitDist <- subset(pnts, distance<1000)
#' pnts_boundary_limitDist <- subset(pnts_boundary, distance<2000 & mudFract != -1)
#' plot(pnts_limitDist$zMax, pnts_limitDist$mudFract,
#'      main = paste0('2008'), xlab = paste0('depth'), ylab = 'fraction')           
#' points(pnts_boundary$zMax, pnts_boundary$mudFract, 
#'        pch = 16, col = 'red')           
#' 
#' 
#' mapView(allMasPoints[nearest,]) + mapView(annual_obs_nonOutlier_sp[5000])
#' 
