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
library(leaps)
library(nlme) # for gls model
library(relaimpo) # for relative contribution
library(gridExtra)# for table plotting
library(broom) # for glance
library(Hmisc)
library(corrplot)
library(PerformanceAnalytics)
# library(vegan) # for PCA / ord() == ordination function, more for ecology
# perform model selection procedure based on AICc selection on the full model to select the best predictors using the dredge function 
library(MuMIn)
# Interaction plottnig with interact_plot from interactions library
library(interactions)
library(pscl) # for applying poissoin: https://stats.oarc.ucla.edu/r/dae/zip/#:~:text=Zero%2Dinflated%20poisson%20regression%20is,zeros%20can%20be%20modeled%20independently.
library(lmtest)
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
maxPosFG <- max(allFiles$pos[which(allFiles$Country == "FrenchGuiana")])
maxPosSur <- max(allFiles$pos[which(allFiles$Country == "Suriname")])
maxPosGuy <- max(allFiles$pos[which(allFiles$Country == "Guyana")])

# drop transects near  river mouths
allFiles_dropPOS <- allFiles %>%
  # accumulate all coastline positions to compute alongshore distance guianas
  dplyr::mutate(alongshorePos =  ifelse(Country == "FrenchGuiana",
                                        maxPosFG-pos,0),
                alongshorePos = ifelse(Country == "Suriname",
                                       (maxPosSur-pos)+maxPosFG+1000,
                                       alongshorePos),
                alongshorePos = ifelse(Country == "Guyana",
                                       (maxPosGuy-pos)+maxPosFG+maxPosSur+1000,
                                       alongshorePos)
                ) %>%
  
  dplyr::mutate(toFilter = 0) %>%
  dplyr::mutate(toFilter = ifelse((Country == "Suriname" & 
                                    pos %in% posToExcludeSUR),1,toFilter),
                toFilter = ifelse((Country == "FrenchGuiana" & 
                                    pos %in% posToExcludeFG),1,toFilter),
                toFilter = ifelse((Country == "Guyana" & 
                                    pos %in% posToExcludeGUY),1,toFilter)) %>%
  filter(toFilter == 0) %>%
  dplyr::select(-c(toFilter))

group_dates<-unique(allFiles_dropPOS$year_col)
group_pos <- unique(allFiles_dropPOS$pos)

# # get for all transects an coastline observation near
## reference date as baseline
allFiles_dropPOS$baseline <- 0
allFiles_dropPOS$baseline2 <- 0
allFiles_dropPOS$mudbankObs[which(is.na(allFiles_dropPOS$mudbankObs))] <- 0 # replace NA for 0
allFiles_dropPOS$validMudbankObs[which(is.na(allFiles_dropPOS$validMudbankObs))] <- 0 # replace NA for 0


allFiles_refDate <- allFiles_dropPOS %>%
  # dplyr::mutate(nonOutlier = ifelse(coast_outlier == 1 & coastDist > 0 & 
  #                                     !(is.na(coastDist)), 1, 0)) %>%
  dplyr::group_by(Country, pos) %>%  # , nonOutlier
  
  # # nearestDate
  # dplyr::mutate(nearestDate =  as.Date(year_col[
  #   which.min(abs(as.Date(year_col)-reference_date))])) %>%
  
  # corresponding coastDist and coast median values assigned as baseline
  dplyr::mutate(baseline =  coast_median[which.min(abs(as.Date(year_col)-reference_date))]) %>%
  dplyr::mutate(baseline2 =  coast_median[which.min(abs(as.Date(year_col)-normalize_date))]) %>%
  
  # Compute variables that are NA for certain years
  # either fill with most occuring observation in time series or first one
  dplyr::mutate(
    meanOrient = mean(centeredOrientation2_250,na.rm = T), # mean orientation before replacing NA's!
    # firstOrient = centeredOrientation2_250[
    #   which.min(abs(as.Date(year_col)-as.Date("1986-01-01")))],
    firstOrient = first(na.omit(centeredOrientation2_250)), # Make sure the date order is working
    lastOrient = last(na.omit(centeredOrientation2_250)),
    # lastOrient = centeredOrientation2_250[
    #   which.min(abs(as.Date(year_col)-as.Date("2020-01-01")))]
    ) %>%
  
  # overwrite for entire group (country, pos) the baseline values
  dplyr::mutate(
    # nearestDate = na.aggregate(nearestDate, FUN=Mode),
    baseline = na.aggregate(baseline, FUN=Mode),
    baseline2 = na.aggregate(baseline2, FUN=Mode),
    firstOrient = na.aggregate(firstOrient, FUN=Mode),
    lastOrient = na.aggregate(lastOrient, FUN=Mode)) %>%
  
  # the orientation parameters: use mean
  dplyr::mutate(
    centeredOrientation_100 = replace_na(centeredOrientation_100,mean(centeredOrientation_100, na.rm = TRUE)),
    centeredOrientation2_100 = replace_na(centeredOrientation2_100,mean(centeredOrientation2_100, na.rm = TRUE)),
    centeredOrientation_250 = replace_na(centeredOrientation_250,mean(centeredOrientation_250, na.rm = TRUE)),
    centeredOrientation2_250 = replace_na(centeredOrientation2_250,mean(centeredOrientation2_250, na.rm = TRUE)),
    sinuositySmoothed_100 = replace_na(sinuositySmoothed_100,mean(sinuositySmoothed_100, na.rm = TRUE)),
    sinuositySmoothed_250 = replace_na(sinuositySmoothed_250,mean(sinuositySmoothed_250, na.rm = TRUE)),
  )%>%
  
  # dplyr::mutate(across(contains("centeredOrientation")), .fns = list(cat = ~mean(., na.rm = TRUE))) %>%
  
  # remaining NA values are posToExclude (rivermouths, there is no median computed
  dplyr::group_by(Country, pos,year_col) %>%
  
  # subtract median from value in nearest date to normalize
  dplyr::mutate(
    countryPos = paste0(Country,pos),
    normalized = coast_median - baseline,
    normalized2 = coast_median - baseline2
    ) %>%
  ungroup()

  
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
##' - Orientation
##' 
##' 
##' 

# Centered orientation (& centerOrientatin2) ranges from -180 - 0 - 180, so x_bins 0, 4,5,6,7 are off
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
                                   centeredOrientation_250),
                mudbank_medianExtent = max((medianOffshore - coast_median),0) # set to 0 if extent is negative 
                ) %>%
  
  # add bins & labels for coastline orientation
  # overwrite original variables 
  dplyr::mutate(x_bins_100 = findInterval(x=recenter, 
                                      vec = dir_bin_cuts),
                x_bins_250 = findInterval(x=recenter250, 
                                         vec = dir_bin_cuts)) %>%
  dplyr::mutate(x_bins_100 = ifelse(x_bins_100 == n_directions,
                                0, x_bins_100),
                x_bins_250 = ifelse(x_bins_250 == n_directions,
                                   0, x_bins_250)) 

# to compare the effect of different buffersizes go from a wide format to long
# probably use a dplyr function here to ensure you only do that for a specific variable that we define.
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

allFiles_posMudbank <- allFiles_mutate %>%
  filter(alongshore != 'NA') %>%
  dplyr::group_by(Country, pos) %>%
  dplyr::mutate(

    firstCoastline = coast_median[
      which.min(abs(as.Date(year_col)-as.Date("1986-01-01")))],
    lastCoastline = coast_median[
      which.min(abs(as.Date(year_col)-as.Date("2021-01-01")))],
    endPointDiff = lastCoastline - firstCoastline,

    # amount of years in series ==> not the same as the amount of years with observation!
    yrs = length(year(max(as.Date(year_col))):year(min(as.Date(year_col)))), # incl. end
    noDataYRS = yrs-length(as.Date(unique(year_col))), # years without observation
    
    # Amount of years mudbank & no mudbank
    mudbankYRS = sum(noMudbank == 0, na.rm = T),
    noMudbankYRS = sum(noMudbank == 1, na.rm = T),
    
    # frequency of mudbank occurence 
    freqOcc = mudbankYRS/(yrs-noDataYRS), # amount of mudbanks in the years there is data
    
    meanCurve = mean(medianC_250,na.rm = T),
    concavity = ifelse(meanCurve > 0, 'convex', 'concave'),
  ) %>%
  
  dplyr::group_by(Country, year_col, pos, noMudbank) %>% #normalized2 / Country
  dplyr::mutate(
    mean_deltaCoast = mean(deltaCoast, na.rm = T),
    mean_smoothedMeanFract = mean(mean_smoothedMeanFract),
    meanFraction = mean(meanFraction)
  ) %>%
  # don't change these two! 
  dplyr::group_by(Country, pos, noMudbank) %>%
  dplyr::summarize(
    endPointDiff = endPointDiff[1],
    sum_deltaCoast = sum(mean_deltaCoast, na.rm = T),
    alongshore =  alongshore[1],
    yrs =  yrs[1],
    noDataYRS = noDataYRS[1],
    mudbankYRS = mudbankYRS[1],
    noMudbankYRS = noMudbankYRS[1],
    freqOcc = freqOcc[1],meanOrient = meanOrient[1],
    meanCurve = meanCurve[1],concavity = concavity[1],
    alongshorePos = alongshorePos[1]
    ) %>%
  ungroup() %>%
  spread(noMudbank, sum_deltaCoast) %>%
  # noMudbank == `1` ==> sum coastline changes without mudbank
  # noMudbank == `0` ==> sum coastline changes WITH mudbank
  dplyr::rename(withoutMudbank = `1`,
                withMudbank = `0` )  %>%
  dplyr::mutate(
    withoutMudbank = round(if_else(is.na(withoutMudbank), 0, withoutMudbank)),
    withMudbank = round(if_else(is.na(withMudbank), 0, withMudbank))) %>%
  # amount of meters per year
  dplyr::mutate(
    withMudbank_myr = withMudbank/mudbankYRS,
    withoutMudbank_myr = ifelse(is.nan(withoutMudbank/noMudbankYRS),
                                0,withoutMudbank/noMudbankYRS),
    netChange_myr = withMudbank_myr + withoutMudbank_myr,
    endpointRate = endPointDiff/yrs) %>%
  dplyr::mutate(orientClass = ifelse(meanOrient<22.5 & meanOrient > -22.5,
                                     'N', 'other'),
                orientClass = ifelse(meanOrient<67.5 & meanOrient > 22.5,
                                     'NE', orientClass),
                orientClass = ifelse(meanOrient<112.5 & meanOrient > 67.5,
                                     'E', orientClass))

allFiles_posMudbank <- transform(allFiles_posMudbank,
                                 Country=factor(Country, levels=c("FrenchGuiana","Suriname","Guyana")))

# ggplot(allFiles_posMudbank,aes(x = endpointRate, y = netChange_myr)) +
#   geom_point() + 
#   geom_smooth(method = "lm", se = FALSE) + 
#   geom_text(x = 25, y = 300, label = lm_eqn(allFiles_posMudbank), parse = TRUE)

dens1 <- ggplot(allFiles_posMudbank, aes(x = freqOcc, fill = Country)) + 
  geom_density(alpha = 0.4) + 
  theme(legend.position = "none",
        plot.background = element_rect(fill = '#d9d9d9',  colour = '#d9d9d9'),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        
        axis.line.x = element_line(size = 0.5, colour = "black"),
        axis.line.y = element_line(size = 0.5, colour = "black"),
        axis.line = element_line(size=1, colour = "black"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank())
# dens1

dens2 <- ggplot(allFiles_posMudbank, aes(x = endpointRate, fill = Country)) + 
  geom_density(alpha = 0.4) +
  scale_x_continuous(limits = c(-150, 150)) +
  
  theme(legend.position = "none",
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.background = element_rect(fill = '#d9d9d9',  colour = '#d9d9d9'),
        axis.line.x = element_line(size = 0.5, colour = "black"),
        axis.line.y = element_line(size = 0.5, colour = "black"),
        axis.line = element_line(size=1, colour = "black"),
        axis.text.x =element_text(size = 18,  hjust = .5, vjust = .5),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 20, face = 'bold')) +
  coord_flip()
# dens2

cols <- rev(RColorBrewer::brewer.pal(11, 'RdYlBu'))

ggplot(allFiles_posMudbank, 
      aes(y=freqOcc, x=alongshorePos/1000))+
  geom_point(alpha = 1) +
  geom_smooth(method = "lm", se = FALSE) 

# plot frequency of occurence & netto result for each pos
presilience <- ggplot(allFiles_posMudbank, 
                      aes(x=freqOcc, y= endpointRate,colour= Country#alongshorePos/1000,
                          # shape = Country
                          ))+
  # facet_wrap(~Country, nrow = 3) +
  geom_point(alpha = 1) + 
  # scale_colour_gradientn(colours = cols) +
  geom_smooth(method = "lm", se = FALSE) +

  scale_y_continuous(limits = c(-150, 150)) +
  labs(y="net coastline change [m/yr]", x = "frequency of occurence [-]") + 
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
        # legend.position = c(0.15, 0.65),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        strip.text.x = element_blank(),
        plot.background = element_rect(fill = '#d9d9d9'))
presilience
legend <- get_legend(presilience) # plot_spacer()
presilience <- presilience + theme(legend.position = 'none')

combined <- dens1 + legend + presilience + dens2 + 
  plot_layout(ncol = 2, nrow = 2, widths = c(4, 1), heights = c(1, 4), ) &
  theme(plot.background = element_rect(fill = '#d9d9d9', colour = '#d9d9d9'))

# # combined
# ggsave(combined, filename = paste0("./results/temp_maps/",
#                                              'frequencyOccurence_Guianas_1985-2020',
#                                              '_',  format(Sys.Date(), "%Y%m%d"),'.jpeg'),
#        width = 13.1, height = 7.25, units = c('in'), dpi = 1200)


######################
### 
### Prepare for multi variable statistics
###' https://statisticsbyjim.com/regression/multicollinearity-in-regression-analysis/#:~:text=Multicollinearity%20occurs%20when%20independent%20variables,model%20and%20interpret%20the%20results.
###' https://bookdown.org/ndphillips/YaRrr/anova.html
###' https://statisticsbyjim.com/regression/check-residual-plots-regression-analysis/
###' 
###' dependend/ variable: net coastline change (deltaCoast or netChange_myr, netChange_myr is not unique per year whereass deltacoast is)
###' 
###' independent / explanatory variables: 
###' Start orientation, end orientation, 
###' mudbank width, mudbank occurance
###' Alongshore position (?), distance to upcoast river mouth or xy coordinates to account for spatial structure? If so use sine/cosine to avoid bias due to circularity of longitude
###' 
###' 
###' - interaction between variables?
###' - F-test on models: variable selection: https://www.youtube.com/watch?v=G_obrpV70QQ&t=435s (see also ANOVA: )
###' - 
###' 
###' Determine variable importance!
###' 
###' What technique to use:
###' Generalized least squares (supposedly when there is a degree of correlation between residuals)
###' Ordinary least squares 
###' Weighted least squares
###' 

####################### 
#' 
#'  
#' link coastline changes to climate data
#' 
#' Not produced as figure


NAOMEI <- read.csv('D:/WOTRO/Research/Software/Projects/offshore_boundary/data/raw/MEI_NAO_winter_yrMei.csv',
                            stringsAsFactors = FALSE,
                            sep = ',',
                            na.strings=c("","NA"))
NAOMEI <- type_convert(NAOMEI)

# wave power database
folderWavePower <- './data/processed/wavePower/'
  # select folders
folderSelectWP <- as.matrix(list.files(paste0(folderWavePower), full.names = T))
dfWP <- rewrite(folderSelectWP);
# only csv's
dfWP <- dfWP[grep('.csv', folderSelectWP, ignore.case = T),]


filteredWP <- vector('list', 100)
for (q in seq_along(years)) {
  for (x in seq_along(aoi)){
    year = years[q]
    region = aoi[x]
    
    filters = c(year, region)
    
    filteredWP = rbind(filteredWP, dfWP %>% 
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
filteredWP <- unique(filteredWP)

wavePower <- unique(do.call(rbind, lapply(as.matrix(filteredWP)[,1],
                                         function(x) read.csv(x,
                                                              stringsAsFactors = FALSE,
                                                              sep = ',',
                                                              na.strings=c("","NA")
                                         ))))

wavePower <- type_convert(wavePower) %>%
  dplyr::mutate(alongshorePos = as.numeric(alongshorePos))


# based on nearest ID you can select from original ERA5 data an appropriate join.
allFiles_merge <- allFiles_mutate %>%
  dplyr::group_by(alongshorePos, year_col) %>%
  left_join(wavePower, #%>%
              # dplyr::mutate(time = as.Date(paste0(time))) %>%
              # dplyr::select(!c(Country, longitude,latitude)),
            c('year_col' = 'year_col',
              "Country" = 'Country',
              "alongshorePos" = "alongshorePos"), keep = F) %>% ## 

  dplyr::select(c( # from coastline data
    "Country", "year_col", "pos","alongshorePos","alongshore","coast_median","deltaCoast", # 'Country.y', "year_col.y",
    "medianOffshore","normalized2", "meanFraction", "distX", "distY","noMudbank",
    "meanCurvature_100","meanCurvature_250","medianC_100", "medianC_250", 
    "mean_smoothedMeanFract", "mudbank_medianExtent", "firstOrient","lastOrient","meanOrient", 'centeredOrientation2_250',
              
    # From wavePower processed data:
    'wave_power_calm', 'wave_power_storm', # wave power (mwp * swh^2) * sin(angleDiff) * cos(angleDiff) (angle dif is based on country average)
    'P_swh_calm', 'P_swh_storm',           # wave energy (mwp * swh^2)
    'mwp_calm', "mwp_storm", # seaonal mean wave period
    "swh_calm", "swh_storm", # seasonal mean swignificant wave heights
    "mwd_calm", "mwd_storm"  # Seasonal mean wave direction
    ))



# corrTest <- allFiles_merge %>%
#   # dplyr::group_by(Country, year_col) %>%
#   # dplyr::summarize(mean=mean(deltaCoast, na.rm = T)) %>%
#   left_join(NAOMEI,
#             c("year_col" = "date"), keep = T
#   )


waterDens <- 1025 # g / m3
acceleration <- 9.81 # m/s
# all constants so doesn't change the relative difference

deg2rad <- function(deg) {(deg * pi) / (180)}
rad2deg <- function(rad) {(rad * 180) / (pi)}


# TO DO: 
# filter coastline pos with angleDiff < -90 or > 90
# interpolate 

toJoin <- allFiles_merge %>%
  dplyr::mutate(
    angleDiff = mwd_storm-centeredOrientation2_250, 
    
    # according to mota et al., 2014 it should include: ((waterDens * acceleration^2) / (64*pi)) * wave engery (SWH^2 * mwp)
    # the wave energy flux per unit of wave-crest length
    p_swh = P_swh_storm * ((waterDens * acceleration^2) / (64*pi)) *
      sin(deg2rad(angleDiff)) * 
      cos(deg2rad(angleDiff)),
    
    # original wavepower is on hourly data (non-linear relation) check the difference
    # post_WP_Storm = mwp_storm *(swh_storm^2)
    ) 
toJoin <- transform(toJoin,
                    Country=factor(Country, levels=c("FrenchGuiana","Suriname","Guyana")))


countryMean <- toJoin %>%
  dplyr::group_by(Country) %>%
  dplyr::summarize(meanAngleDiff = mean(angleDiff,na.rm=T),
                   wave_power_storm = mean(wave_power_storm, na.rm=T),
                   meanWave = mean(p_swh,na.rm=T))

# plots for wave-power: time series and spatial distribution
dens_pSWH <- ggplot(toJoin, aes(x = as.numeric(P_swh_storm), fill = Country)) + 
  # geom_density(alpha = 0.4) + 
  geom_histogram(position = 'identity', bins = 75, alpha = 0.8) +
  facet_wrap(~Country, nrow = 3) +
  theme(
        plot.background = element_rect(fill = '#d9d9d9',  colour = '#d9d9d9'),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        
        axis.line.x = element_line(size = 0.5, colour = "black"),
        axis.line.y = element_line(size = 0.5, colour = "black"),
        axis.line = element_line(size=1, colour = "black")
        )
dens_pSWH

# ggsave(dens_pSWH, filename =paste0("./results/temp_maps/",
#                                         'Guianas_P_swh_storm',
#                                         '.jpeg'),
#        width = 13.1, height = 7.25, units = c('in'), dpi = 1200)


pSWH_timeSeries <- ggplot(toJoin, aes(x = as.Date(year_col), y = as.numeric(p_swh),
                                color = Country)) + 
  geom_point(alpha = 0.4, position = position_jitter(width = 150)) + 
  facet_wrap(~Country)+
  theme(
    # legend.position = "none",
    plot.background = element_rect(fill = '#d9d9d9',  colour = '#d9d9d9'),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    
    axis.line.x = element_line(size = 0.5, colour = "black"),
    axis.line.y = element_line(size = 0.5, colour = "black"),
    axis.line = element_line(size=1, colour = "black")
  )
# pSWH_timeSeries

# 
# ggplot(toJoin %>% filter(abs(deltaCoast) < 1000), 
#        aes(x = deltaCoast, y = as.numeric(p_swh))) + 
#   geom_point() + 
#   geom_smooth(method = "lm", se =TRUE, color = 'red')

########################### variable selection
#' 
#' https://iqss.github.io/dss-workshops/Rmodels.html
#' Before analysis, https://rpubs.com/iabrady/residual-analysis
#' 
#' 0) pre-process: NA / missing data?
#' 1) data test (distributions) for normality (Q-Q plot): 
#' 2) data test homogeneity of variance (residual plot) to test the linear regression assumption of equal variance (homoscedasticity)
#'                    
#' 3) influential cases in the dataset (outiers): cook's distances / Dixon's Q
#' 4) Data log transformed and significance levels set at α = 0.05 (To improve normality log-transform before Z-score transformation)
#' 5) standardize (z-score?) = Scaling + centering the data!  
#' - Center to reduce multicolinearity? (subtract the mean: interpretation remains the same) 
#' - scaling data: ensure empirical standard deviations
#' 
#' 6) explanatory analysis
#'     Correlation, distributions and regression values
#'     Variance Inflation Factors?  ==> test collinearity among variables
#'     Spatial autoCorrelation?
#'     
#' 7) stepwise multiple regression ?????? ==> which independepnd variable to select?
#'     Also referred to as forward selection of variables (??? See Michaud 2022)
#' - interaction between variables ==> comparing regression models / F-test (https://interactions.jacob-long.com/articles/interactions.html)
#' - PCA (on the correlation matrix (Michaud 2022), or different?): to calculate ordination
#'     See vegan package  
#' - 
#' - Or ocnsider one-way / two way anova's?
#' 
#' 
#' FOR multiple variate statistics: which independend variables to select? Should they 
#' be unique per year (like mudbank width, deltaCoast) or 
#' only onique per position (like mudbank Occurence? Allthough occurence is normalized to the amount per year?)
#'  

# allFiles_posMudbank ==> unique per pos
# toJoin ==> unique per pos and per year.
dbToUse1 <- 'toJoin'

#http://www.sthda.com/english/wiki/correlation-matrix-a-quick-start-guide-to-analyze-format-and-visualize-a-correlation-matrix-using-r-software
dependentVar <- c('normalized2', "deltaCoast") # unique per pos and year
dependentVar <- ifelse(dbToUse1 == 'allFiles_posMudbank',
                       'endpointRate', # unique per pos
                       dependentVar)

# columns to consider for linear model: manual selection
varToInclude <- c('meanFraction',
                   'mudbank_medianExtent', # ==> overlap with freqOcc? 
                  #  'mean_smoothedMeanFract', 
                  # 'firstOrient', 'lastOrient','meanOrient', 'noMudbank','alongshorePos', 'deltaCoast', 'meanCurve',
                  'freqOcc', 
                  
                  'p_swh', 'swh_storm','mwd_storm','mwp-storm')

# 1) preproces: remove NA + to numeric values
filterNA <- na.omit(eval(as.name(paste(dbToUse1)))) #%>%
  #filter(abs(deltaCoast) < 500) # unrealistic values
columnClasses <- flatten(data.frame(lapply(filterNA,class))) # class of columns
numericClasses <- which(columnClasses != 'character' &
                          columnClasses != 'factor') #names(columnClasses)[which(columnClasses == 'character')]


filterNA[numericClasses] <- sapply(filterNA[numericClasses], as.numeric) # apply here which ones to include (and thus convert to numeric)

# col numbers
dependentCol <- which(names(filterNA) %in% dependentVar)
varToIncludeNR <- which(names(filterNA) %in% varToInclude)

# 2) + 3) test for normality and homoscadcity
# example to check for normality: mudbank_medianExtent 
# (0 should be handled, can't set them to NA but the zero indicates no mudbank)

for (i in varToIncludeNR){
  # i <- varToIncludeNR[4]
  
  cexVal <- 2
  
  # file with the summary of linear model
  tableFile <- paste0("./results/temp_maps/stats/",
                      'lm_',dbToUse1,'_', names(columnClasses[i]), '.txt')
  
  # save file
  sink(tableFile)

  writeLines('Initialize raw data')
  sink()  # returns output to the console
  for(di in dependentVar){
    # di <- dependentVar[1]
    
    form <- paste(di," ~",  paste0(names(columnClasses[i])))
    
    # test linear fits for each variable with netchange per year
    fit <- lm(form, data = filterNA,
              na.action = na.exclude) # fit the model
    
    sink(tableFile, 
         append = T)
    

    print(paste0('db used to create filterNA: ', dbToUse1))
    print(paste0(form))
    print(summary(fit))
    print(paste0('date created: ', Sys.Date()))
    print('------------------------------------------------------------------')
    sink()  # returns output to the console

    outName <- paste0("./results/temp_maps/stats/",
                      'linearRegress_', dbToUse1, '_',di,"~" ,names(columnClasses[i]),
                      '.jpeg')
    
    if(!file.exists(outName)){
      jpeg(outName,
           width = 2143, height = 894)
      
      par(mfrow=c(3,2))
      plot(fit, which=1, col=c("blue"), cex.lab=cexVal, cex.axis=cexVal, cex.main=cexVal, cex.sub=cexVal) # Residuals vs Fitted Plot
      plot(fit, which=2, col=c("red"), cex.lab=cexVal, cex.axis=cexVal, cex.main=cexVal, cex.sub=cexVal)  # Q-Q Plot
      plot(fit, which=3, col=c("blue"), cex.lab=cexVal, cex.axis=cexVal, cex.main=cexVal, cex.sub=cexVal)  # Scale-Location Plot: The residuals have equal variance(occupy equal space) above and below the line and along the length of the line
      plot(fit, which=4, col=c("orange"), cex.lab=cexVal, cex.axis=cexVal, cex.main=cexVal, cex.sub=cexVal)  # Cook's distance
      plot(fit, which=5, col=c("purple"), cex.lab=cexVal, cex.axis=cexVal, cex.main=cexVal, cex.sub=cexVal)  # leverage
      plot(fit, which=6, col=c("purple"), cex.lab=cexVal, cex.axis=cexVal, cex.main=cexVal, cex.sub=cexVal)  # leverage vs cooks distance
      mtext(paste0(names(columnClasses[i])), side = 3, line = -2, outer = TRUE, cex = 2)
      
      
      # predicted <- predict(fit)   # Save the predicted values
      # residuals <- residuals(fit) # Save the residual values
      
      dev.off()
      par(mfrow=c(1,1))}

  }
 
  # signif(cooks.distance(fit),3)
  # influenceSR <- influence.measures(fit)
  # infl <- which(apply(influenceSR$is.inf, 1, any))  # which are influencial
  # summary(influenceSR)
  # plot(rstudent(influenceSR) ~ hatvalues(influenceSR)) # recommended by some
  # plot(influenceSR, which = 5) # an enhanced version of that via plot(<lm>)
}


# sds <- sapply(filterNA[numericClasses], sd)
# means <- sapply(filterNA[numericClasses], mean)

mosthighlycorrelated <- function(mydataframe,numtoreport)
{
  # mydataframe <- filterNA[varToIncludeNR]
  # find the correlations
  cormatrix <- cor(mydataframe)
  # set the correlations on the diagonal or lower triangle to zero,
  # so they will not be reported as the highest ones:
  diag(cormatrix) <- 0
  cormatrix[lower.tri(cormatrix)] <- 0
  # flatten the matrix into a dataframe for easy sorting
  fm <- as.data.frame(as.table(cormatrix))
  # assign human-friendly names
  names(fm) <- c("First.Variable", "Second.Variable","Correlation")
  # sort and print the top n correlations
  head(fm[order(abs(fm$Correlation),decreasing=T),],n=numtoreport)
}


mosthighlycorrelated(filterNA[varToIncludeNR], length(varToIncludeNR)) # cor coefs

# correlation with significance: 
Hmisc::rcorr(as.matrix(filterNA[varToIncludeNR]), type="pearson") # type can be pearson (continuous) or spearman (ordinal/non-normal?)
# do variables have a significant effect on each other: 


# 4) log transform: e.g. mudbank median extent
# which ones to log transform: based on skewness?
skew <- mostSkewed(filterNA, varToIncludeNR)
toskewed <- row.names(skew)[which(abs(skew) >0.5)]
toSkewedNr <- which(names(filterNA) %in% toskewed)

# candidates for log transforming:
# coast_median, deltaCoast
log10t <- transform(filterNA,
                    Country=factor(Country, levels=c("FrenchGuiana","Suriname","Guyana")))
# log10t[c(varToIncludeNR)] <- sapply(filterNA[c(varToIncludeNR)], logTransformNegatives) 

log10t[unique(c(varToIncludeNR,dependentCol))] <-
  apply(log10t[unique(c(varToIncludeNR,dependentCol))], 2,
        logTransformNegatives)

Hmisc::rcorr(as.matrix(log10t[c(dependentCol,varToIncludeNR)]), type="pearson") # type can be pearson (continuous) or spearman (ordinal/non-normal?)
# do variables have a significant effect on each other: 


# https://towardsdatascience.com/is-normal-distribution-necessary-in-regression-how-to-track-and-fix-it-494105bc50dd
# compare log transformation
for (i in varToIncludeNR){
  # i <- varToIncludeNR[1]
  # i <- 9
  
  sampleData <- sample(as.matrix(filterNA[,i]), 1000)
  
  # test if the sample is significantly different from normality (shapiro wilk statistic)
  shapiroTest<- shapiro.test(sampleData) # P<0.05 ==> not normally distributed
  # Take note that if the sample size is greater than 5000, you should use test statistics instead of the p-value as the indicator to decide.
  # If the value of p is equal to or less than 0.05, then the hypothesis of 
  # normality will be rejected by the Shapiro test. On failing, the test can state 
  # that the data will not fit the distribution normally with 95% confidence.
  
  tableFile <- paste0("./results/temp_maps/stats/",
                      'lm_',dbToUse1,'_', names(columnClasses[i]), '.txt')
  
  sink(tableFile,  append = T)
  writeLines("\nXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX \nInitialize log-transformed comparison")

  sink()  # returns output to the console
  for(di in dependentVar){
    # di <- dependentVar[1]
    
    form <- paste(di," ~",  paste0(names(columnClasses[i])))
    
    # test linear fits for each variable with netchange per year
    fit <- lm(form, data = log10t,
              na.action = na.exclude) # fit the model
    
    sink(tableFile, 
         append = T)
    
    print(paste0('db used to create log10t: ', dbToUse1))
    print(paste0(form))
    print(summary(fit))
    print(paste0('date created: ', Sys.Date()))
    print('------------------------------------------------------------------')
    sink()  # returns output to the console
  
  
  
  OutputNames <- paste0("./results/temp_maps/stats/",
                  'normality_', names(filterNA)[i],
                    '.jpeg')
  # if log transformed, change outputnames
  # if (i %in% varlogTNR) {
  #   OutputNames <- paste0("./results/temp_maps/stats/",
  #                         'normality_', names(filterNA)[i],
  #                         '_logt.jpeg')
  # }
  if(!file.exists(OutputNames) ){
  # create a figure for it 
  jpeg(OutputNames,
       width = 2143, height = 894)
  
  par(mfrow=c(2,2))
  hist(as.matrix(filterNA[,i]), breaks="FD", freq=FALSE,
       main=paste0(names(filterNA)[i]), 
       xlab=paste0(names(filterNA)[i]))
  
  lines(density(as.matrix(filterNA[,i])), col="blue", lwd=2)
  
  # normal density
  M <- mean(as.matrix(filterNA[,i]))
  SD <- sd(as.matrix(filterNA[,i]))
  curve(dnorm(x, mean=M, sd=SD), from=min(as.matrix(log10t[,i]), na.rm = T), 
        to=max(as.matrix(filterNA[,i]), na.rm = T),
        col="red", lwd=2, add=TRUE)
  
  mtext(paste0('shapiroTest, p-val: ', 
               trimws(format(round(shapiroTest$p.value, 4), nsmall=4))), side=3)
  
  car::qqPlot(as.matrix(filterNA[,i]), main = paste0(names(filterNA)[i], ' raw data'),
              ylab = paste0(names(filterNA)[i]) )
  # qqnorm(as.matrix(filterNA[,i]), pch=20, cex=2)
  # qqline(as.matrix(filterNA[,i]), col="gray60", lwd=2)
  
  # log -transfomed histogram
  hist(as.matrix(log10t[,i]), breaks=250, freq=FALSE,
       main=paste0(names(log10t)[i], ' log transformed'), 
       xlab=paste0(names(log10t)[i]))
  
  test <- car::qqPlot(as.matrix(log10t[,i]), main = paste0(names(filterNA)[i], ' log transformed data'))
  # qqnorm(as.matrix(log10t[,i]), pch=20, cex=2)
  # qqline(as.matrix(log10t[,i]), col="gray60", lwd=2)
  dev.off()
  }
  

  # cexVal <- 2
  # form <- paste("endpointRate ~",  paste0(names(filterNA)[i]))
  # test linear fits for each variable with netchange per year
  
  if(dbToUse1 == "allFiles_posMudbank"){
    rawPlot <- ggplot(filterNA, #%>% dplyr::filter(abs(eval(as.name(names(filterNA)[i]))) < 2500),
                      aes(eval(as.name(names(filterNA)[i])), endpointRate) ) +
      geom_point() +
      facet_wrap(~Country) +
      labs(x = paste0(names(filterNA)[i])) +
      stat_smooth(method = lm) +
      stat_smooth(method = lm, formula = y ~ poly(x, 3, raw = TRUE), color = 'red')+
      scale_y_continuous(limits = c(-500, 500))
    
    logPlot <- ggplot(log10t %>% dplyr::filter(eval(as.name(names(filterNA)[i])) >3),
                      aes(eval(as.name(names(filterNA)[i])),endpointRate) ) +
      geom_point() +
      labs(x = paste0('log(', names(filterNA)[i], ')')) +
      stat_smooth()
    # scale_y_continuous(limits = c(-500, 500))
    
    combinCorrPlot <- rawPlot + logPlot +
      plot_layout(ncol = 1, nrow = 2, widths = c(4, 1), heights = c(1, 4), ) &
      theme(plot.background = element_rect(fill = '#d9d9d9', colour = '#d9d9d9'))
    
    ggsave(combinCorrPlot, filename =paste0("./results/temp_maps/stats/",
                                            'corrPlot_', names(filterNA)[i],
                                            '.jpeg'),
           width = 13.1, height = 7.25, units = c('in'), dpi = 1200)
  }

  }
  # fit <- lm(form, data = log10t,
  #           na.action = na.exclude) # fit the model
  # fitGam <- mgcv::gam(endpointRate ~ s(eval(as.name(names(filterNA)[i]))), data = filterNA)
  # summary(fit)

  # zero-inflated model for the mudbank extent: fit poission regression / Hurdle models
  # https://stats.stackexchange.com/questions/187824/how-to-model-non-negative-zero-inflated-continuous-data
  # https://data.library.virginia.edu/getting-started-with-hurdle-models/
  # 
  # mod.hurdle <- hurdle(endpointRate ~ ., data = filterNA)
  # hist(filterNA$mudbank_medianExtent, breaks = 50)
  # testZero <-data.frame(cbind(filterNA$mudbank_medianExtent, log10t$endpointRate))
  # colnames(testZero) <- c('mudbank_medianExtent', 'endpointRate')
  # 
  # testZero$mudbank_medianExtent <- as.integer(testZero$mudbank_medianExtent)
  # testZero$endpointRate <- as.integer(testZero$endpointRate)
  # fitPoissoin <- pscl::zeroinfl(endpointRate ~ mudbank_medianExtent,
  #                               data = testZero, dist = "negbin", EM = TRUE)

 }

# 5) scale + center: normalize raw data (not the log-transformed data...)
normalized <- transform(filterNA,
                Country=factor(Country, levels=c("FrenchGuiana","Suriname","Guyana")))
normalized[unique(c(varToIncludeNR,dependentCol))] <-
            apply(normalized[unique(c(varToIncludeNR,dependentCol))], 2,
                   scale, center = T, scale = T)

for (i in varToIncludeNR){
  # i <- varToIncludeNR[1]
  
  cexVal <- 2
  # save file
  tableFile <- paste0("./results/temp_maps/stats/",
                      'lm_',dbToUse1,'_', names(columnClasses[i]), '.txt')
  
  sink(tableFile, append = T)

  writeLines("\n XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX \ninitialize normalized linear model")

  sink()  # returns output to the console
  for(di in dependentVar){
    # di <- dependentVar[2]
    
    form <- paste(di," ~",  paste0(names(columnClasses[i])))
    
    # test linear fits for each variable with netchange per year
    fit <- lm(form, data = normalized,
              na.action = na.exclude) # fit the model
    
    sink(tableFile, 
         append = T)
    
    print(paste0('db used to create normalized: ', dbToUse1))
    print(paste0(form))
    print(summary(fit))
    print(paste0('date created: ', Sys.Date()))
    print('------------------------------------------------------------------')
    sink()  # returns output to the console
    
    outName <- paste0("./results/temp_maps/stats/",
                      'linearRegress_', dbToUse1, '_',di,"~" ,names(columnClasses[i]),
                      '.jpeg')
    
    if(!file.exists(outName)){
      jpeg(outName,
           width = 2143, height = 894)
      
      par(mfrow=c(3,2))
      plot(fit, which=1, col=c("blue"), cex.lab=cexVal, cex.axis=cexVal, cex.main=cexVal, cex.sub=cexVal) # Residuals vs Fitted Plot
      plot(fit, which=2, col=c("red"), cex.lab=cexVal, cex.axis=cexVal, cex.main=cexVal, cex.sub=cexVal)  # Q-Q Plot
      plot(fit, which=3, col=c("blue"), cex.lab=cexVal, cex.axis=cexVal, cex.main=cexVal, cex.sub=cexVal)  # Scale-Location Plot: The residuals have equal variance(occupy equal space) above and below the line and along the length of the line
      plot(fit, which=4, col=c("orange"), cex.lab=cexVal, cex.axis=cexVal, cex.main=cexVal, cex.sub=cexVal)  # Cook's distance
      plot(fit, which=5, col=c("purple"), cex.lab=cexVal, cex.axis=cexVal, cex.main=cexVal, cex.sub=cexVal)  # leverage
      plot(fit, which=6, col=c("purple"), cex.lab=cexVal, cex.axis=cexVal, cex.main=cexVal, cex.sub=cexVal)  # leverage vs cooks distance
      mtext(paste0(names(columnClasses[i])), side = 3, line = -2, outer = TRUE, cex = 2)
      
      
      # predicted <- predict(fit)   # Save the predicted values
      # residuals <- residuals(fit) # Save the residual values
      
      dev.off()
      par(mfrow=c(1,1))}
    
  }
  
  # signif(cooks.distance(fit),3)
  # influenceSR <- influence.measures(fit)
  # infl <- which(apply(influenceSR$is.inf, 1, any))  # which are influencial
  # summary(influenceSR)
  # plot(rstudent(influenceSR) ~ hatvalues(influenceSR)) # recommended by some
  # plot(influenceSR, which = 5) # an enhanced version of that via plot(<lm>)
}

# 6) explanatory analysis 
res2 <- Hmisc::rcorr(as.matrix(normalized[varToIncludeNR])) # apply correlation matrix
# normalizedCorr <- Hmisc::rcorr(as.matrix(normalized[varToIncludeNR]), type="pearson")
flatCorM <- flattenCorrMatrix(res2$r, res2$P) # flatten

# including checking for colinearity between variables
# plot correlation matrix
corrplot::corrplot.mixed(cor(as.matrix(log10t[c(varToIncludeNR, dependentCol)])), 
                         order = 'AOE')

# glm allows to fit other regression models: binomial (logistic) or possion
fit0 <- lm(endpointRate ~ 1, data = normalized ) # family = binomial("logit")
fit1 <- lm(endpointRate ~ freqOcc, data = normalized ) # family = binomial("logit")
fit_glm <- glm(endpointRate ~ freqOcc + meanFraction + mudbank_medianExtent + p_swh,# + mean_smoothedMeanFract + mudbank_medianExtent + meanCurve + meanOrient,
                data = normalized)

# difference between linear and quadratic model: Aikaike information criterion
Aikaike <- AIC(fit1, fit_glm) 
#==> lower AIC suggests it is preferred
print(paste0(row.names(Aikaike[which.min(Aikaike$AIC),]), ' is prefered'))

# likelihood ratio test: comparing nested generalized linear models
anova (fit0,fit1, test = "LRT") # P < 0.05 == reject 0-hypothesis: conclude that the full model offers a bitter fit
anova (fit0,fit1, test = "F")
lrtest(fit_glm, # compare full model (first one mentioned) to an nested / reduced model
       fit1)

# summary(fit_glm) # ==? check if interaction term is significant?
jtools::summ(fit_glm)

# interactions::interact_plot(model = fit_glm, pred = freqOcc, # centered = 'none', ==> already centered previously
#                             modx = meanFraction, interval = T) # plot.points = TRUE
# interpretation: 
# parallel lines: absence of heterogeinit / interaction
# differently sloped lines indicate heterogeinity



# only unique per pos: endpointRate, freqOcc
# linear plots
# scatter <- ggplot(eval(as.name(paste(dbToUse))),
#                   aes(x = p_swh, y = normalized2, color = Country) ) +
#   geom_point(alpha = 0.2) +
#   # facet_wrap(~Country) +
#   # labs(x = paste0(names(filterNA)[i])) +
#   stat_smooth(method = lm)
# # stat_smooth(method = lm, formula = y ~ poly(x, 3, raw = TRUE), color = 'red')+
# # scale_y_continuous(limits = c(-500, 500))
# 
# scatter

# test linear model per country
for (i in varToIncludeNR){
  # i <- varToIncludeNR[1]
  
  cexVal <- 2
  
  # file with the summary of linear model
  tableFile <- paste0("./results/temp_maps/stats/",
                      'lm_',dbToUse1,'_', names(columnClasses[i]), '.txt')
  # tableFile
  # save file
  sink(tableFile, append = T)
  
  writeLines('Initialize grouped model')
  sink()  # returns output to the console
  for(di in dependentVar){
    di <- dependentVar[1]
    
    form <- paste(di," ~",  paste0(names(columnClasses[i])))
    
    # per country
    fittedModels <- filterNA %>% dplyr::group_by(Country) %>%
      do(model = lm(form, data = .))

    # test linear fits for each variable with netchange per year
    # fit <- lm(form, data = filterNA %>% group_by(~Country),
    #           na.action = na.exclude) # fit the model
    
    sink(tableFile, 
         append = T)

    
    print(paste0('db used to create filterNA: ', dbToUse1))
    sink()  # returns output to the console
    for(modi in 1: nrow(fittedModels)){
      sink(tableFile,
           append = T)
      # modi <-1
      print(paste0('country: ',
        levels(droplevels(fittedModels$Country))[modi]))
      print(summary(fittedModels[2][[1]][[modi]]))
      sink()  # returns output to the console
    }
    sink(tableFile, 
         append = T)
    print(paste0('date created: ', Sys.Date()))
    print('------------------------------------------------------------------')
    sink()  # returns output to the console
    
  }
}

# 7) Linear and stepwise multiple regression 
#'     https://www.statology.org/multiple-linear-regression-r/
#'     
#'     Also referred to as forward selection of variables (??? See Michaud 2022)
#'     
#'     https://stackoverflow.com/questions/43441937/construction-of-nested-model-sequence-in-r
#'     https://www.scribbr.com/statistics/anova-in-r/
#'     

options(scipen=999) # disble scientific notation
form = 'lm' # lm / glm / gls 
# GLS ==> account for spatial auto correlation (Otero 2020) and https://stats.stackexchange.com/questions/34325/regression-modelling-with-unequal-variance
# GLM ==> nonlinear regression when variance in sample is not constant or errors not normally distributed
# https://www.datascienceblog.net/post/machine-learning/interpreting_generalized_linear_models/

# database to use:
dbToUse <- 'filterNA' #  filterNA / log10t / normalized

set.seed(7)
training.samples <-
  caret::createDataPartition(eval(as.name(paste(dbToUse)))[[as.name(dependentVar)]],p = 0.8, list = FALSE)

train.data  <- eval(as.name(paste(dbToUse)))[training.samples,]#filterNA[training.samples, ]#normalized[training.samples, ]
test.data <-  eval(as.name(paste(dbToUse)))[-training.samples, ]#normalized[-training.samples, ]
i <- 1
# start out with nothing exlcuded
vifExcl <- NULL
stats <- data.frame(
  db = NULL,
  RMSE = NULL,
  R2 = NULL
)

# define different types of global models (incl. all relevant variables)
# define family for GLM, else it is the same as a regular lm() form

# logistic regression: https://www.statology.org/logistic-regression-in-r/
# family="binomial" ==> response variable is binary
fullModel <-  paste0(dependentVar, " ~ ", paste(
  colnames(eval(as.name(paste(dbToUse)))[varToIncludeNR])[!colnames(normalized)[varToIncludeNR] %in% c(vifExcl)],  # 'p_swh','deltaCoast'
  collapse="+"))
# fullModel <- paste0( "endpointRate ~ freqOcc")

Globalmodel <- lm(formula(fullModel), data = train.data)
Globalmodelglm <- glm(formula(fullModel, family="binomial"), data = train.data)
# Globalmodelgls <- gls(formula(fullModel), data = train.data)

# relative importance for linear models only (use goodness of fit statistic R2)
relativeImportance <- calc.relimp(Globalmodel, type = c("lmg")) # rela=T: force metrics to sum to 100% 

totalVariance <- relativeImportance@var.y                  # Total response variance
coefficientDetermination <- relativeImportance@R2          #  
univariateR2_1 <- cor(train.data[[dependentVar]], train.data[,varToIncludeNR])^2 # 
# univariateR2  <- relativeImportance@first                  # squared correlations of the regressors with the response
decomposed<- round(relativeImportance@lmg,3)                         # relative importance corresponding to provided type 




# summary(Globalmodel)
# likelihood ratio test: comparing nested generalized linear models
# lrt <- anova (Globalmodel,Globalmodelgls, test = "LRT") # P < 0.05 == reject 0-hypothesis: conclude that the full model offers a better fit
# Ft <- anova (Globalmodel,Globalmodelgls, test = "F")

modelStats <- summary(Globalmodel)
predictions <- Globalmodel %>% predict(test.data)

# png(file = paste0("./results/temp_maps/stats/",
#     'predict_',dependentVar,'_',form,'_multipleLinear_', i,
#     '.jpeg'),
#     width=1200, height=700)

plot(predictions,test.data[[as.name(dependentVar)]])
mtext(paste0(fullModel), side=3)
text(-3000,1000, paste0('r2: ', round(caret::R2(predictions, test.data[[as.name(dependentVar)]]),2), '\n',
                    'RMSE: ', round(caret::RMSE(predictions, test.data[[as.name(dependentVar)]]),2) ))
text(50, 100, paste0('exluded: ', paste0(c(vifExcl), collapse = ' _ ') ))
abline(a=0, b = 1, col = 'red')
# dev.off()
i <- i + 1

VIFglobal <-car::vif(Globalmodel)

# update column names that should be excluded
vifExcl <- c(vifExcl, names(VIFglobal)[VIFglobal >= max(VIFglobal)])

stats <-  rbind(
  stats, data.frame(
    model = form,
    modelr2 = round(modelStats$r.squared,2),
    modelrmse = round(modelStats$r.squared^2,2),
    modelsigma = round(modelStats$sigma,2),
    RMSE = caret::RMSE(predictions, test.data[[as.name(dependentVar)]]),
    R2 = caret::R2(predictions, test.data[[as.name(dependentVar)]]),
    vif = max(VIFglobal)
  ))

row.names(stats) <- vifExcl

# It seems that 'meanFraction' and 'meanOrient' have high colinearity with other independent variables.
# But if mean_smoothedMeanFract is removed the meanFraction can also be used.

## relative contribution to the total amount of variation explained
# see steur et al., 2022 using it as measure of relative importance of the variables
# https://stats.stackexchange.com/questions/79399/calculate-variance-explained-by-each-predictor-in-multiple-regression-using-r
# https://www.r-bloggers.com/2012/08/the-relative-importance-of-predictors-let-the-games-begin/
# anGlobal <- anova(Globalmodel) # Anova
# afss <- anGlobal$"Sum Sq"
# anGlobal <- cbind(anGlobal,PctExp=round(afss/sum(afss),3)) # coefficient of determination # *100 (percentage)
# 
# relim <- relaimpo::calc.relimp(Globalmodel, type = c("lmg"), rela = F) # type lmg is from lindemann (1980), relative = F:  the metrics sum to R^2
# explained <- relim@R2 # variance explained
# explainedManual <- sum(anGlobal$PctExp[1:8]) # exclude residuals to compare to manual method
# relMetrics <- relim@lmg # importance (contribution to r2)
# 
# tableOverview <- rbind(data.frame(relContrR2 = round(relMetrics,3)), sum = round(sum(relMetrics), 3))
# 
# # plot in table
# ss <- tableGrob(tableOverview)
# grid.arrange(ss)

# stepwise model selection
# on why not to use this approach: https://stats.stackexchange.com/questions/20836/algorithms-for-automatic-model-selection
# varToInclude
# problems in stepAIC: meanFraction, normalized2
# Globalmodel <-  
# stepFull <- MASS::stepAIC(lm(fullModel,
#   # endpointRate ~ freqOcc +mean_smoothedMeanFract+
#   # mudbank_medianExtent+meanCurve+meanOrient+
#   # firstOrient+lastOrient,
#                                data = normalized),
#                           # scope = list(upper = ~Eth*Sex*Age*Lrn, lower = ~1),
#                           direction="both")

# Test all models
# variables to exclude: 
# colnames(normalized)[varToIncludeNR] %in% c('meanFraction', 'meanOrient')
# varToExcl <-  colnames(eval(as.name(dbToUse)))[varToIncludeNR] %in% 
#   c('meanFraction', "meanOrient","noMudbank", "firstOrient", "lastOrient", "meanOrient")

varstoTest <- c("alongshorePos","meanCurvature_250","mean_smoothedMeanFract","mudbank_medianExtent",
                "centeredOrientation2_250","wave_power_storm","P_swh_storm","swh_storm","p_swh")
varstoTest <- colnames(eval(as.name(paste(dbToUse)))[varToIncludeNR])


varstoTestNr <- which(colnames(eval(as.name(paste(dbToUse)))) %in% varstoTest)
mod <- leaps::regsubsets(as.formula(paste(dependentVar, '~ .')), data = normalized[,c(varstoTestNr, dependentCol)] , nvmax = 100, 
                  nbest = 10, really.big= T, method = "exhaustive")

am <- summary(mod)$which[,-1]

# create unique models
pred <- lapply(1:nrow(am), function(x) colnames(am)[which(am[x,])])
lm.mod.form <- lapply(pred, function(x) paste0(form, "(", dependentVar, " ~ ", paste(x, collapse="+"), ", data=",dbToUse, ')'))

all.mods <- lapply(lm.mod.form, function(x) eval(parse(text=x)))

comparison.output <- data.frame(model=1:length(all.mods), 
                                db = dbToUse,
                                modType =  form,
                                y=dependentVar, 
                                x=sapply(1:length(pred), function(x)paste0(pred[[x]], collapse="+")))
# this is for evaluating the accuracy of the model
comparison.output$AIC <- sapply(all.mods, AIC)
# comparison.output$AIC2 <- unlist(sapply(all.mods, glance)['AIC',])
# comparison.output$BIC <- sapply(all.mods, BIC)

# this is for estimating prediction capacity of the model
comparison.output$r2 <- unlist(lapply(all.mods, function(x) round(caret::R2(predict(x, test.data), test.data[[as.name(dependentVar)]]),3)))
comparison.output$rmse <- unlist(lapply(all.mods, function(x) round(caret::RMSE(predict(x, test.data), test.data[[as.name(dependentVar)]]),3)))


varExpl <- lapply(anovas, function(q){
  # q = anovas[[90]]
  M = rbind(
    round(q$`Pr(>F)`,3),
    round(q$`Sum Sq`/sum(q$`Sum Sq`),3))
  colnames(M) <- paste0(rownames(q))
  row.names(M) <- c('pval', 'r2')
  
  return(M[2,])
  
  # ifelse(!is.na(any(M[1,] < 0.05)),  
  #             M[,M[1, ] < 0.05],
  #             rbind(0,0))) # filter out insignificant variables. Check if that is necessary
})

if (class(all.mods[[1]]) == "lm"){ # if linear model: extract objects
  
  anovas <- lapply(all.mods, function(i){
                  anova(i)})
  # round(testPvals$`Pr(>F)`,3) <- anovas[[90]]
  
  # rownames(q)
  
  # resdiual standard deviation
  comparison.output$residSE <-  unlist(sapply(all.mods, summary)['sigma',]) # 
  # comparison.output$adjR2 <- unlist(sapply(all.mods, summary)['adj.r.squared',])
  comparison.output$adjR2 <- unlist(sapply(all.mods, glance)['adj.r.squared',])
  comparison.output$pval <- round(unlist(sapply(all.mods, glance)['p.value',]),3)
  # comparison.output$fstat <-  unlist(sapply(all.mods, summary)['fstatistic',])v
  comparison.output$statistic <- round(unlist(sapply(all.mods, glance)['statistic',]),2)
  
  # coefficient of determination (r2) 
  coefDet <- rbind.fill(lapply(varExpl,function(y){as.data.frame(t(y),stringsAsFactors=FALSE)}))
  
  
  comparison.output <- cbind(comparison.output, coefDet)
  
  
} #stop("Not an object of class 'lm' ")


# test example:
testMod <- all.mods[[25]]
testAnova <- anovas[[25]]
testVarExpl <- varExpl[[25]]

# compare to relimp functionality
relImportance <- calc.relimp(testMod, type = c("lmg")) # rela=T: force metrics to sum to 100% 



if (class(all.mods[[1]]) == "glm"){
  comparison.output$deviance <- unlist(sapply(all.mods, summary)['deviance',]) # might not work for multiple linear models
}



# spatial dependency: https://gis.stackexchange.com/questions/431943/applying-morans-i-across-columns
# moran I







# https://towardsdatascience.com/understanding-linear-regression-output-in-r-7a9cbda948b3 ==> linear model
# https://www.datascienceblog.net/post/machine-learning/interpreting_generalized_linear_models/ ==> glm
# one example:


# testAll <- summary(all.mods[[1]])
# ftest <- testAll$fstatistic
# residSE <- testAll$sigma
# pvalModel <- pf(ftest[1],ftest[2],ftest[3],lower.tail=F)
# 
# anovaTable <- anova(all.mods[[1]])
# 
# # of the coefficients
# pvalAnova <- round(anovaTable$'Pr(>F)',3) # p val of the f-test
# varianceExpl <- round(anovaTable$`Sum Sq`/sum(anovaTable$`Sum Sq`),5)
# 
# pvalsCoefficients <- round(summary(all.mods[[1]])$coefficients[,4]  ,3)
# # comparison.output[1,]
# 
# #The intercept is essentially the expected value of the coastline to change over time 
# # parameters after it are saying how much more coastline would change per year if parameters are considered
# 
# # standardized coefficients: if standardized they suggest variable importance
# testAll$coefficients[-1,1]
# 
# #  standard error of the regression:  an estimate of the standard deviation of the coefficient
# testAll$coefficients[-1,2]
# # Identify the independent variable that has the largest absolute value for its standardized(!) coefficient
# 
# # t-value: n general, we want our coefficients to have large t-statistics, because it indicates that our standard error is small in comparison to our coefficient.
# round(testAll$coefficients[-1,3],1)
# # p-value: he p-value, in association with the t-statistic, help us to understand how significant our coefficient is to the model.
# # It means we are confident that the coefficient is not zero, meaning the coefficient does in fact add value to the model by helping to explain the variance within our dependent variable.
# round(unlist(testAll$coefficients[-1,4]),4)

#Multiple R-Squared (Coefficient of Determination) /  coefficient of determination (for simple linear regression)
#  It tells us what percentage of the variation within our dependent variable that the independent variable is explainin
# The Adjusted R-squared value is used when running multiple linear regression and can conceptually be thought of in the 
# same way we described Multiple R-squared. The Adjusted R-squared value shows what percentage of the variation 
# within our dependent variable that all predictors are explaining.

# The residual standard error 
# Residual Standard Error is measure of the quality of a linear regression fit. 
# tells us the average amount that the actual values of Y (the dots) differ from the predictions (the line) in units of Y.

# coefficient of determination (r2) of multiple regression
# Per transect (or groups of transects) and then untangling the variable importance (expressed in r2)


smallestAIC <- comparison.output %>% arrange(Residuals) %>% head(20)

# table theme:
# https://cran.r-project.org/web/packages/gridExtra/vignettes/tableGrob.html
t1 <- ttheme_default(
  core=list(
    # fg_params= list(fontface=c(rep("bold", 1), rep("plain",10))), # text parameters rows
    fg_params= list(fontface="plain", fontsize = 10), # text parameters rows
    bg_params = list(fill=c(rep(c("grey95", "grey90"),
                                length.out=10))
                     # ,alpha = rep(c(1,0.5), each=5)
  )),
  colhead=list(fg_params=list(fontface="bold", fontsize = 12)) # column titles
)
# tt2 <- ttheme_minimal()



tableAIC <- tableGrob(smallestAIC, theme = t1, rows = NULL)
# tableAIC$widths <- unit(rep(1/ncol(smallestAIC), ncol(smallestAIC)), "npc")

# png(file = paste0("./results/temp_maps/stats/",
#                   'outputTable_',dependentVar,'_',form,'_', dbToUse,
#                   '3.jpeg'),
#     width=1600, height=600)

# grid.arrange(tableAIC) # make it fitting // # grid.table(smallestAIC)
# invisible(capture.output(dev.off()))# dev.off()
# compare a selection?
# e.g. create a matrix for F-test results and log likelohoods? 
subsetModels <- all.mods[smallestAIC$model]
subsetModForms <- unlist(lm.mod.form[smallestAIC$model])

df_LRT <- data.frame(matrix(ncol = nrow(smallestAIC), nrow = nrow(smallestAIC)), 
                    row.names = smallestAIC$model)
df_FT <- data.frame(matrix(ncol = nrow(smallestAIC), nrow = nrow(smallestAIC)), 
                     row.names = smallestAIC$model)
colnames(df_LRT) <- c(smallestAIC$model)
colnames(df_FT) <- c(smallestAIC$model)
diag(df_LRT) <- 0
diag(df_FT) <- 0

colnamsToExclude <- NA


for(i in colnames(df_LRT)){
  # i<- colnames(df_LRT)[2]
  colnamsToExclude <- cbind(colnamsToExclude, i)
  
  for(q in row.names(df_LRT)[!(row.names(df_LRT) %in% colnamsToExclude)]){
    # q<- colnames(df_LRT)[4]
    
    #Likelihood ratio test
    # test column against row: if P<0.05 the column is a better fit
    lrt <- anova (all.mods[[as.numeric(i)]],all.mods[[as.numeric(q)]], test = "LRT")
    sigP <- lrt$`Pr(>Chi)`[2]
    
    # F test
    Ft <- anova (all.mods[[as.numeric(i)]],all.mods[[as.numeric(q)]], test = "F") 
    
    df_LRT[rownames(df_LRT)==q,
           colnames(df_LRT)==i] <- sigP
    
    df_FT[rownames(df_FT)==i, # if p<0.05 the regression model fits better than the main model: independt variables improve the fit
           colnames(df_FT)==q] <- Ft$`Pr(>F)`[2]
  }
}

logTests <- tableGrob(round(df_LRT,3)) # likelihood ratio tests
fTests <- tableGrob(round(df_FT,3))    # F - tests  
grid.arrange(logTests)

# apply principal component analysis
# assuming the full model is known (not sure what the best method is to define the parameters?)
# 
# # first correlation matrix
# corM <- cor(normalized[,c(varToIncludeNR)]) # 
# # covM <- cov(filterNA[,c(varToIncludeNR)])
# sds <- sapply(filterNA[varToIncludeNR], sd) # sd's from the (not standardized) data (else all are 1)
# 
# mosthighlycorrelated(normalized[varToIncludeNR], length(varToIncludeNR)) # cor coefs
# 
# # PCA on the correlation matrix 
# # PCA <- princomp(covmat = corM, scores = T)
# PCA <- princomp(filterNA[varToIncludeNR], scores = T, cor = T)  # if applied on raw data scores are possible to compute
# PCAsum <- summary(PCA, loadings = T)
# PCA2 <- prcomp(corM)
# 
# plot(PCA$sdev^2, xlab = "Component number", # scree diagram
#        ylab = "Component variance", type = "l", main = "Scree diagram")
# plot(log(PCA$sdev^2), xlab = "Component number", # log eigenvalue diagram
#       ylab = "log(Component variance)", type="l",
#       main = "Log(eigenvalue) diagram")
# 
# # bivariate plots of the first principle components
# pairs(PCA$scores[,1:3], 
#         panel = function(x,y, ...) {
#               MVA::bvbox(cbind(x,y), add = TRUE)
#         })
# 
# # change depending on PCA
# out <- sapply(1:2, function(i) {
#     plot(filterNA$endpointRate,PCA$scores[,i],
#            xlab = paste("PC", i, sep = ""), ylab = "change per year (m)")
# })

# biplot(PCA, col = c("gray", "black"))
# same directions of arrows: parameters are correlated
# the numbers are a observation and relate to the position which they 'score high in'
# 
# normalized_reg <- lm(endpointRate ~PCA$scores, 
#                   data = normalized)
# summary(normalized_reg)

# swh_storm              ==> Significant wave height (storm)
# p_swh                  ==> wave power corrected for coastline angle
# P_swh_storm            ==> wave power storm season corrected for coastline angle
# mudbank_medianExtent, 
# meanCurvature_250, 
# centeredOrientation2_250 
# mean_smoothedMeanFract

# spatial variability in explanatory power of different indicators
# varToInclude: unique per year and pos
# "alongshorePos" is excluded when looking at alongshore distrubution of r2 
independed <- c( "centeredOrientation2_250", "meanCurvature_250", 
  "mean_smoothedMeanFract", "mudbank_medianExtent", "P_swh_storm")

# in case of multiple linear regression
independedVar <- ifelse(length(independed) > 1,
                        paste0(independed, collapse="+"),
                        independed)

dependentVar <- 'normalized2' # normalized2 / deltaCoast
groupingVar <- 'alongshore' # alongshore / Country / alongshorePos

# temporal: does through time one of the variables explains significant changes in 
# coastline position??
dfFitsCountries <- eval(as.name(paste(dbToUse))) %>%
  dplyr::group_by(eval(as.name(groupingVar)),year_col) %>%
  dplyr::summarize(
    Country = Country[1],
    alongshore = alongshore[1],
    alongshorePos = alongshorePos[1], # only correct if groupingVar is alongshorePos
    # year_col = year_col[1],
    # endpointRate = mean(endpointRate),
    mudbank_medianExtent = mudbank_medianExtent[1],
    mean_smoothedMeanFract = mean(mean_smoothedMeanFract),
    meanCurvature_250 = mean(meanCurvature_250),
    p_swh = mean(p_swh),
    swh_storm = mean(swh_storm),
    deltaCoast = mean(deltaCoast,na.rm=T),
    # p_swh_storm = mean(p_swh_storm),
    wave_power_storm= mean(wave_power_storm, na.rm=T),
    normalized2 = mean(normalized2,na.rm=T),
    P_swh_storm = mean(P_swh_storm, na.rm =T), 
    centeredOrientation2_250 = mean(centeredOrientation2_250)) %>%
  
  ungroup() %>% # normalized / log transformed or filterNA (raw values)
  nest_by(eval(as.name(groupingVar))) %>%
  dplyr::mutate(fit = list(lm(as.formula(paste0(dependentVar, '~', independedVar)), data = data))) %>%
  # dplyr::mutate(fit2 = list(lm(as.formula(paste0('normalized2~', independedVar)), data = data))) %>%
  # dplyr::mutate(fit = list(lm(as.formula(paste0('normalized2~centeredOrientation2_250+deltaCoast')), data = data))) %>%
  dplyr::summarize(augment(fit, se_fit = T)) # adds information about observations to a dataset
  # dplyr::summarize(tidy(fit))              # summarizes information about model components (intercept, estimate, P, t-testvalue)
# dplyr::summarize(glance(fit))            # reports information about the entire model:
# resdiual standard deviation/error (sigma), t-stat and corresponding P-val


# select a model (e.g. max adjusted R2)
idModel <- which(comparison.output$adjR2== max(comparison.output$adjR2))
# comparison.output[idModel,]
modelToUse <- all.mods[[idModel]]


# positions/transects that have no changing shoreline: and can thus not
# be used for determineing the relevant importance of coastline changes (covariance is 0) ==> depends on the variables included in the model!

# excludeTransects <- c(182000, 186000, 185000, 1108000, 1096000, 1041000, 968000, 
#                     1245000, 1246000,1248000,1201000,1190000)
excludeTransects<- c(968000, 182000,185000,186000, 965000, 1108000,1096000,
                     1041000,1248000,1245000,1201000,1190000)


# fit a linear model for the data of each transect
fitLMTransects <- eval(as.name(paste(dbToUse)))  %>% 
  dplyr::mutate(alongshorePoskm = alongshorePos/1000) %>% # alongshore pos is a independed variable (also used for plotting)
  dplyr::filter(!c(alongshorePos %in% excludeTransects)) %>%
  
  # nest all the data for each unique position
  nest_by(Country,pos,alongshorePoskm) %>%
  
  # fit a linear model on the data column
  # dplyr::mutate(fit = list(lm(modelToUse$call, data = data))) %>%
  dplyr::mutate(fit = list(lm(as.formula(paste0(dependentVar, '~', independedVar)), data = data))) %>%
  
  # determine relative importance of each parameter in each model.
  # RELA is false to get r2 value
  # TO DO: check if RANK should be TRUE or False?
  dplyr::mutate(relimp =  list(calc.relimp(fit , type = c("lmg"), rela = F, rank = TRUE)$lmg)) %>%
  # for each independed variable the r2 sums up to r-squared (from glanced)

  dplyr::mutate(tidied = list(tidy(fit, se_fit = T)), # summarizes information about model components (intercept, estimate, P, t-testvalue)
                glanced = glance(fit),                # reports information about the entire model:
                augmented = list(augment(fit))        # adds information about observations to a dataset
                # dev = list(deviance(fit))           # deviance reports SSE ==> also returned by glanced (deviance)
                ) %>% 
  dplyr::select(!c(data, fit)) %>%
  
  # output to seperate columns
  unnest_wider(relimp, names_sep = '_' ) %>% 
  dplyr::mutate(glancedR2 = glanced$r.squared,
                glancedP = glanced$p.value)

# get p-values for each variable in each model in a seperate column
variablesP <- t(sapply(fitLMTransects$tidied, function(x) x$p.value))
colnames(variablesP) <- paste0('pval_', fitLMTransects$tidied[[1]]$term) # rename

# reshape (to long format) for plotting purpose
variablesP <- cbind(fitLMTransects %>% 
                          dplyr::select(c(Country, pos, alongshorePoskm)), 
                    variablesP) %>%
  gather(., variable, pval, 
         paste0("pval_", c(fitLMTransects$tidied[[1]]$term)[2:length(fitLMTransects$tidied[[1]]$term)]), 
         factor_key=TRUE) %>%
  mutate_at("variable", str_replace, "pval_", "") #%>% # rename

# create long format
# pos, variable and value (r2) 
fitLMTransectsLong <- fitLMTransects %>% 
  dplyr::select(c(Country, pos, alongshorePoskm,glancedP,
                  # colnames(fitLMTransects %>%
                  #            dplyr::select(contains(paste0("pval_", colnames(eval(as.name(paste(dbToUse)))[varToIncludeNR]))))),
                  colnames(fitLMTransects %>%
                             dplyr::select(matches("relimp_"))))) %>%
  gather(., variable, r2val, c(paste0("relimp_", c(fitLMTransects$tidied[[1]]$term)[2:length(fitLMTransects$tidied[[1]]$term)])), factor_key=TRUE) %>%
  mutate_at("variable", str_replace, "relimp_", "") %>% # rename
  # add the p-values
  left_join(variablesP, 
            c("alongshorePoskm" = "alongshorePoskm",
              "variable" = "variable"), keep = T, suffix = c("", ".y"))
  
  
values5 <- c('#fdbf6f','#d95f02','#7570b3','#e7298a','#66a61e')
# plot spatial distribution of coefficient of determination (r2) for the multiple regression
stackedBar <- ggplot(fitLMTransectsLong 
                     # %>%
                      # dplyr::filter(pval<0.05) # filter pval of individual parameters
                      # dplyr::filter(glancedP<0.05)    # filter pval of model
                     ,
                     aes(x=pos/1000, y = r2val, fill = variable)) +
  geom_bar(position="stack", stat="identity") +
  facet_wrap(~Country, nrow = 3) +
  scale_y_continuous(breaks = seq(0, 1, by = .5)) +
  
  scale_fill_manual(labels = c("Orientation", "Fraction",
                               "curcature", "mudbank",
                               "wavePower"), values = values5) + # values = c("blue", "red")
  
  labs(y=expression("Coefficient of determination"~ (r^2)), x = "alongshore pos [km]") + 
  theme(axis.line.x = element_line(size = 0.5, colour = "black"),
        axis.line.y = element_line(size = 0.5, colour = "black"),
        axis.line = element_line(size= 1, colour = "black"),
        axis.title.y = element_text(size = 20, face = 'bold'),
        axis.title.x = element_text(size = 20, face = 'bold'),
        axis.text.x = element_text(size = 18,  hjust = .5, vjust = .5),
        axis.text.y = element_text(size = 18, hjust = .5, vjust = .5),
        legend.title = element_text(size = 20),
        legend.background = element_rect(fill = '#d9d9d9',  colour = '#d9d9d9'),
        legend.key=element_blank(),
        legend.text = element_text(size = 15),
        legend.position = c(0.8,0.8),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        # strip.text.x =element_blank(),
        plot.background = element_rect(fill = '#d9d9d9'))
stackedBar

# ggsave(stackedBar, filename = paste0("./results/temp_maps/",
#                                    'spatioTemporal_r2coefs_',dbToUse,'_',dependentVar,
#                                    '_filterNone',
#                                              '_',  format(Sys.Date(), "%Y%m%d"),'.jpeg'),
#        width = 13.1, height = 7.25, units = c('in'), dpi = 1200)
  

dfCortransects <- eval(as.name(paste(dbToUse))) %>%
    group_by(Country, alongshorePos,pos) %>%
    
    dplyr::summarize(
      corrmudbank_medianExtent= cor.test(eval(as.name(paste(dependentVar))),mudbank_medianExtent)[["estimate"]], # corr coef (corr coef * corr coef = r2)
      pmudbank_medianExtent= cor.test(eval(as.name(paste(dependentVar))),mudbank_medianExtent)[["p.value"]], # and the P
      
      corrp_swh= cor.test(eval(as.name(paste(dependentVar))),p_swh)[["estimate"]], # 
      pp_swh= cor.test(eval(as.name(paste(dependentVar))),p_swh)[["p.value"]],
      
      
      corrP_swh_storm= cor.test(eval(as.name(paste(dependentVar))),P_swh_storm)[["estimate"]], # 
      pP_swh_storm= cor.test(eval(as.name(paste(dependentVar))),P_swh_storm)[["p.value"]],
      
      corrwave_power_storm= cor.test(eval(as.name(paste(dependentVar))),wave_power_storm)[["estimate"]], # wave power storm season
      pwave_power_storm= cor.test(eval(as.name(paste(dependentVar))),wave_power_storm)[["p.value"]],
      corrmean_smoothedMeanFract = cor.test(eval(as.name(paste(dependentVar))),mean_smoothedMeanFract)[["estimate"]], # corr coef
      pmean_smoothedMeanFract= cor.test(eval(as.name(paste(dependentVar))),mean_smoothedMeanFract)[["p.value"]],
      corrswh_storm = cor.test(eval(as.name(paste(dependentVar))),swh_storm)[["estimate"]], # corr coef
      pswh_storm= cor.test(eval(as.name(paste(dependentVar))),swh_storm)[["p.value"]],
      corrcenteredOrientation2_250 = cor.test(eval(as.name(paste(dependentVar))),centeredOrientation2_250)[["estimate"]], # corr coef
      pcenteredOrientation2_250= cor.test(eval(as.name(paste(dependentVar))),centeredOrientation2_250)[["p.value"]],
      corrmeanCurvature_250= cor.test(eval(as.name(paste(dependentVar))),meanCurvature_250,na.action=na.omit)[["estimate"]], # == R
      pmeanCurvature_250= cor.test(eval(as.name(paste(dependentVar))),meanCurvature_250,
                                   na.action=na.omit, method="spearman")[["p.value"]]
    ) %>%
    ungroup() 

r2scatter <- ggplot(dfCortransects, 
                    aes(x= pos/1000, y = eval(as.name(paste0('corr',independedVar))), 
                        color = Country, 
                        shape= eval(as.name(paste0('p',independedVar))) <0.05) ) +
  geom_point() +
  geom_hline(yintercept = 0) +
  facet_wrap(~Country,ncol = 1,nrow=3, scales = "free_x")  +
  
  scale_y_continuous(limits = c(-0.8, 0.8),
                     labels = label_number(accuracy = 0.1)) +
  scale_shape_manual(name = paste0(dependentVar, '~ \n',independedVar),
                     labels = c("P>0.05", "P<0.05", ''),
                     values = c(0, 15, NA)) +
  guides(color=guide_legend('')) +
  
  labs(y="correlation coefficient (r)", x = "alongshore pos [km]") + 
  theme(axis.line.x = element_line(size = 0.5, colour = "black"),
        axis.line.y = element_line(size = 0.5, colour = "black"),
        axis.line = element_line(size= 1, colour = "black"),
        axis.title.y = element_text(size = 20, face = 'bold'),
        axis.title.x = element_text(size = 20, face = 'bold'),
        axis.text.x = element_text(size = 18,  hjust = .5, vjust = .5),
        axis.text.y = element_text(size = 18, hjust = .5, vjust = .5),
        legend.title = element_text(size = 20),
        legend.background = element_rect(fill = '#d9d9d9',  colour = '#d9d9d9'),
        legend.key=element_blank(),
        legend.text = element_text(size = 15),
        
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        strip.text.x =element_blank(),
        plot.background = element_rect(fill = '#d9d9d9'))

r2scatter

# ggsave(r2scatter, filename = paste0("./results/temp_maps/stats/",
#                                    'corrcoefs_',dbToUse,'_',dependentVar,
#                                     '_~',independedVar,
#                                              '_',  format(Sys.Date(), "%Y%m%d"),'.jpeg'),
#        width = 13.1, height = 7.25, units = c('in'), dpi = 1200)


# subsetTest<-dfCorRegions %>%
#   dplyr::filter(Country == "Suriname" & alongshore == 'posEast')

dfCorRegions <- eval(as.name(paste(dbToUse))) %>%
  group_by(Country, alongshore) %>%
  
  dplyr::summarize(
    corrmudbank_medianExtent= cor.test(eval(as.name(paste(dependentVar))),mudbank_medianExtent)[["estimate"]], # corr coef (corr coef * corr coef = r2)
    pmudbank_medianExtent= cor.test(eval(as.name(paste(dependentVar))),mudbank_medianExtent)[["p.value"]], # and the P
    
    corrp_swh= cor.test(eval(as.name(paste(dependentVar))),p_swh)[["estimate"]], # 
    pp_swh= cor.test(eval(as.name(paste(dependentVar))),p_swh)[["p.value"]],
    
    
    corrP_swh_storm= cor.test(eval(as.name(paste(dependentVar))),P_swh_storm)[["estimate"]], # 
    pP_swh_storm= cor.test(eval(as.name(paste(dependentVar))),P_swh_storm)[["p.value"]],
    
    corrwave_power_storm= cor.test(eval(as.name(paste(dependentVar))),wave_power_storm)[["estimate"]], # wave power storm season
    pwave_power_storm= cor.test(eval(as.name(paste(dependentVar))),wave_power_storm)[["p.value"]],
    corrmean_smoothedMeanFract = cor.test(eval(as.name(paste(dependentVar))),mean_smoothedMeanFract)[["estimate"]], # corr coef
    pmean_smoothedMeanFract= cor.test(eval(as.name(paste(dependentVar))),mean_smoothedMeanFract)[["p.value"]],
    corrswh_storm = cor.test(eval(as.name(paste(dependentVar))),swh_storm)[["estimate"]], # corr coef
    pswh_storm= cor.test(eval(as.name(paste(dependentVar))),swh_storm)[["p.value"]],
    corrcenteredOrientation2_250 = cor.test(eval(as.name(paste(dependentVar))),centeredOrientation2_250)[["estimate"]], # corr coef
    pcenteredOrientation2_250= cor.test(eval(as.name(paste(dependentVar))),centeredOrientation2_250)[["p.value"]],
    corrmeanCurvature_250= cor.test(eval(as.name(paste(dependentVar))),meanCurvature_250,na.action=na.omit)[["estimate"]], # == R
    pmeanCurvature_250= cor.test(eval(as.name(paste(dependentVar))),meanCurvature_250,
                                 na.action=na.omit, method="spearman")[["p.value"]]
  ) %>%
  ungroup() 

dfCorRegions<-  transform(dfCorRegions,
                          alongshore=factor(alongshore, levels=c(
                            'berbice','georgetown', 'supernaam','Barima',     
                            'posWest',"posCentre", "posEast", 
                            "maroni","sinermary", "cayenne")))

r2barAlongshore <- ggplot(dfCorRegions, 
                          aes(x = alongshore, y = eval(as.name(paste0('corr',independedVar))), 
                              color = Country, 
                              fill = eval(as.name(paste0('p',independedVar))) <0.05) ) +
  geom_bar(position="dodge", stat="identity") +
  geom_hline(yintercept = 0) +
  
  scale_y_continuous(limits = c(-0.2, 0.8),
                     labels = label_number(accuracy = 0.1)) +
  scale_fill_manual(name = paste0(dependentVar, '~ \n',independedVar),
                    labels = c("P>0.05", "P<0.05", '')
                    ,values = c('#cbd5e8', '#b3e2cd', NA)
  ) +
  guides(color=guide_legend('')) +
  
  labs(y="correlation coefficient (r)", x = "alongshore region") + 
  theme(axis.line.x = element_line(size = 0.5, colour = "black"),
        axis.line.y = element_line(size = 0.5, colour = "black"),
        axis.line = element_line(size= 1, colour = "black"),
        axis.title.y = element_text(size = 20, face = 'bold'),
        axis.title.x = element_text(size = 20, face = 'bold'),
        axis.text.x = element_text(size = 18,  hjust = .5, vjust = .5, angle = 45),
        axis.text.y = element_text(size = 18, hjust = .5, vjust = .5),
        legend.title = element_text(size = 20),
        legend.background = element_rect(fill = '#d9d9d9',  colour = '#d9d9d9'),
        legend.key=element_blank(),
        legend.text = element_text(size = 15),
        
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        strip.text.x =element_blank(),
        plot.background = element_rect(fill = '#d9d9d9'))

r2barAlongshore

# ggsave(r2barAlongshore, filename = paste0("./results/temp_maps/stats/",
#                                    'alongshore_corrcoefs_',dbToUse,'_',dependentVar,
#                                     '_~',independedVar,
#                                              '_',  format(Sys.Date(), "%Y%m%d"),'.jpeg'),
#        width = 13.1, height = 7.25, units = c('in'), dpi = 1200)

# substeTestRegions <-dfCorRegions %>%
#   dplyr::group_by(Country,alongshore,alongshorePos) %>%
#   dplyr::summarize(endpointRate = mean(endpointRate),
#                    freqOcc = mean(freqOcc),
#                    p_swh = mean(p_swh))
# regionsScatter <- ggplot(substeTestRegions, 
#                          aes(x = freqOcc, y = endpointRate,
#                              color = Country)) +
#   facet_wrap(~alongshore) +
#   geom_point() +
  # labs(y="Normalized end point rate", x = "normalized frequency of mudbank occurence") # 
# theme(axis.line.x = element_line(size = 0.5, colour = "black"),
#       axis.line.y = element_line(size = 0.5, colour = "black"),
#       axis.line = element_line(size= 1, colour = "black"),
#       axis.title.y = element_text(size = 20, face = 'bold'),
#       axis.title.x = element_text(size = 20, face = 'bold'),
#       axis.text.x = element_text(size = 18,  hjust = .5, vjust = .5, angle = 45),
#       axis.text.y = element_text(size = 18, hjust = .5, vjust = .5),
#       legend.title = element_text(size = 20),
#       legend.background = element_rect(fill = '#d9d9d9',  colour = '#d9d9d9'),
#       legend.key=element_blank(),
#       legend.text = element_text(size = 15),
#       
#       panel.grid.major = element_blank(),
#       panel.grid.minor = element_blank(), 
#       panel.background = element_blank(),
#       strip.text.x =element_blank(),
#       plot.background = element_rect(fill = '#d9d9d9'))

# regionsScatter


# ggsave(regionsScatter, filename = paste0("./results/temp_maps/stats/",
#                                           'alongshore_scatter_',dbToUse,'_',dependentVar,
#                                           '_~',independedVar,
#                                           '_',  format(Sys.Date(), "%Y%m%d"),'.jpeg'),
#        width = 13.1, height = 7.25, units = c('in'), dpi = 1200)

# 
# Now get for each transect the relative contribution of each variable included (R2)
# test on 1 transect:
up <- 1
tested <- c()

for(uniquePos in unique(eval(as.name(paste(dbToUse)))[["alongshorePos"]])[
    !( unique(eval(as.name(paste(dbToUse)))[["alongshorePos"]]) %in% tested)]){
  # uniquePos
  # if( uniquePos %in% c(182000, 186000, 185000, 1108000, 1096000, 1041000, 968000,
  #                      1245000, 1246000,1248000,1201000,1190000)) next
  
  if( uniquePos %in% c(968000, 182000,185000,186000, 965000, 1108000,1096000,
                       1041000,1248000,1245000,1201000,1190000)) next
  
    # 0 %in% cov(oneTransect$normalized2, oneTransect[independed])

  # uniquePos <-unique(eval(as.name(paste(dbToUse))))[["alongshorePos"]][100]
  oneTransect <- eval(as.name(paste(dbToUse))) %>%
    dplyr::filter(alongshorePos == uniquePos) #%>%

  # fit = lm(modelToUse$call, data = oneTransect)
  # fit = lm(formula(fullModel), data = oneTransect)
  fit = lm(as.formula(paste0(dependentVar, '~', independedVar)), data = oneTransect)
  glancedTest <- glance(fit)
  modelPvalue <- glancedTest$p.value  # model stats

  testRelim <- calc.relimp(fit , type = c("lmg"), rela = F, rank = TRUE)
  pvals <- tidy(fit, se_fit = T) # variable stats


  # testRelim@R2

    # nest_by(Country,pos) %>%
    # dplyr::mutate(fit = list(lm(formula(fullModel), data = data))) %>%
    # dplyr::mutate(relimp =  list(calc.relimp(fit , type = c("lmg"), rela = F, rank = TRUE)$lmg),
                  # augmented = list(augment(fit)),
                  # glanced = glance(fit),
    #               tidied = list(tidy(fit, se_fit = T))) %>%
    # unnest_wider(relimp, names_sep = '_' )

  print(paste(uniquePos, ' worked! ', up))
  up <- up + 1
  tested <- c(tested, as.numeric(uniquePos))
}
# 
# calc.relimp(oneTransect$fit[[1]],
#             type = c("lmg"), rela = F, rank = TRUE)
# 
# calc.relimp(lm(formula(fullModel), data = oneTransect[,c(varToIncludeNR,dependentCol)]),
#     type = c("lmg"), rela = F, rank = TRUE)
