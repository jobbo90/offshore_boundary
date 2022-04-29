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
years <- seq(from = 1985, to = 2020, by = 1)

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

reference_date <- as.Date("1986-01-01")

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

names(allFiles)[names(allFiles) == 'pos.x'] <- 'pos'
names(allFiles)[names(allFiles) == 'year_col.x'] <- 'year_col'

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
                                       (maxPosSur-pos)+maxPosFG,
                                       alongshorePos),
                alongshorePos = ifelse(Country == "Guyana",
                                       (maxPosGuy-pos)+maxPosFG+maxPosSur,
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

allFiles_refDate <- allFiles_dropPOS %>%
  # dplyr::mutate(nonOutlier = ifelse(coast_outlier == 1 & coastDist > 0 & 
  #                                     !(is.na(coastDist)), 1, 0)) %>%
  dplyr::group_by(Country, pos) %>%  # , nonOutlier
  
  # nearestDate
  dplyr::mutate(nearestDate =  as.Date(year_col[
    which.min(abs(as.Date(year_col)-reference_date))])) %>%
  
  # corresponding coastDist and coast median values assigned as baseline
  dplyr::mutate(baseline2 =  coast_median[which.min(abs(as.Date(year_col)-reference_date))],
                firstOrient = centeredOrientation2_250[
                  which.min(abs(as.Date(year_col)-as.Date("1986-01-01")))],
                lastOrient = centeredOrientation2_250[
                  which.min(abs(as.Date(year_col)-as.Date("2020-01-01")))]
                ) %>%
  
  # overwrite for entire group (country, pos) the baseline values
  dplyr::group_by(Country, pos) %>%

  #overWrite NA values with most occuring group value
  dplyr::mutate(
    nearestDate = na.aggregate(nearestDate, FUN=Mode),
    baseline = na.aggregate(baseline, FUN=Mode),
    baseline2 = na.aggregate(baseline2, FUN=Mode),
    firstOrient = na.aggregate(firstOrient, FUN=Mode),
    lastOrient = na.aggregate(lastOrient, FUN=Mode)) %>%
  # remaining NA values are posToExclude (rivermouths, there is no median computed
  dplyr::group_by(Country, pos,year_col) %>%
  
  # subtract median from value in nearest date to normalize
  dplyr::mutate(
    normalized2 = coast_median - baseline2
    ) %>% 
  dplyr::group_by(Country, pos) %>% 
  dplyr::mutate(
    firstCoastline = coast_median[
      which.min(abs(as.Date(year_col)-as.Date("1986-01-01")))],
    lastCoastline = coast_median[
      which.min(abs(as.Date(year_col)-as.Date("2020-01-01")))],
    endPointDiff = lastCoastline - firstCoastline
  ) %>%
  ungroup()
  
  
# hist(allFiles_refDate$endPointDiff, breaks =50)

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

    # amount of years in series ==> not the same as the amount of years with observation!
    yrs = length(year(max(as.Date(year_col))):year(min(as.Date(year_col)))), # incl. end
    noDataYRS = yrs-length(as.Date(unique(year_col))), # years without observation
    
    # Amount of years mudbank & no mudbank
    mudbankYRS = sum(noMudbank == 0, na.rm = T),
    noMudbankYRS = sum(noMudbank == 1, na.rm = T),
    
    # frequency of mudbank occurence 
    freqOcc = mudbankYRS/(yrs-noDataYRS), # amount of mudbanks in the years there is data
    
    meanCurve = mean(medianC_250,na.rm = T),
    meanOrient = mean(centeredOrientation2_250,na.rm = T),
    concavity = ifelse(meanCurve > 0, 'convex', 'concave'),
  ) %>%
  
  dplyr::group_by(Country, year_col, pos, noMudbank) %>% #normalized2 / Country
  dplyr::mutate(
    mean_deltaCoast = mean(deltaCoast, na.rm = T),
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
    withoutMudbank = round(if_else(is.na(withoutMudbank), 0, withoutMudbank)),
    withMudbank = round(if_else(is.na(withMudbank), 0, withMudbank))) %>%
  # amount of meters per year
  dplyr::mutate(
    withMudbank_myr = withMudbank/mudbankYRS,
    withoutMudbank_myr = withoutMudbank/noMudbankYRS,
    netChange_myr = withMudbank_myr + withoutMudbank_myr,
    endpointRate = endPointDiff/yrs) %>%
  dplyr::mutate(orientClass = ifelse(meanOrient<22.5 & meanOrient > -22.5,
                                     'N', 'other'),
                orientClass = ifelse(meanOrient<67.5 & meanOrient > 22.5,
                                     'NE', orientClass),
                orientClass = ifelse(meanOrient<112.5 & meanOrient > 67.5,
                                     'E', orientClass))
hist(allFiles_posMudbank$endpointRate, 50)

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


# plot frequency of occurence & netto result for each pos
presilience <- ggplot(allFiles_posMudbank, 
                      aes(x=freqOcc, y= endpointRate,colour=Country))+
  # facet_wrap(~Country, nrow = 3) +
  geom_point(alpha = 1) + 
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
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        strip.text.x = element_blank(),
        plot.background = element_rect(fill = '#d9d9d9'))
# presilience  
legend <- get_legend(presilience)
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

# build a data frame that contains coastline data + mudbank data 
# allFiles_mutate ==> 1 observation per year per pos
# allFiles_posMudbank ==> 1 observation per pos
# so join mutate (all years) with the desired results from posMudbank (stats per position)
toJoin <- allFiles_mutate %>%
  dplyr::select(c(Country, year_col, pos, coast_median, deltaCoast,normalized2, meanFraction, mean_smoothedMeanFract,
                  mudbank_medianExtent, mudbankObs, validMudbankObs, meanCurvature_250,medianC_250, bearing_250,
                  centeredOrientation2_250, sinuositySmoothed_250,alongshore,firstOrient, lastOrient, alongshorePos)) %>%
  left_join(allFiles_posMudbank %>% 
              dplyr::select(c(Country,pos, freqOcc, netChange_myr, yrs, noDataYRS,mudbankYRS,noMudbankYRS,
                            meanCurve, meanOrient, withMudbank_myr, withoutMudbank_myr, orientClass, endpointRate)),
            c("Country" = "Country",
              "pos" = "pos"), keep = F)

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
#' - scaling data: ensure empreical standard deviations
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
#' be unique per year (like mudbank width, deltaCoast) or only onique per position (like mudbank Occurence? Allthough occurence is normalized to the amount per year?)
#' 
#' 

# install.packages("Hmisc")
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

#http://www.sthda.com/english/wiki/correlation-matrix-a-quick-start-guide-to-analyze-format-and-visualize-a-correlation-matrix-using-r-software

dependentVar <- 'endpointRate'
# columns to consider for linear model: manual selection
varToInclude <- c('freqOcc', 'meanFraction','mean_smoothedMeanFract', 
                  'mudbank_medianExtent', 'meanCurve', 'meanOrient', 
                   'deltaCoast','firstOrient', 'lastOrient', 
                  'alongshorePos'
)

# 1) preproces: remove NA + to numeric values
filterNA <- na.omit(toJoin)
columnClasses <- flatten(data.frame(lapply(filterNA,class))) # class of columns
numericClasses <- which(columnClasses != 'character')#names(columnClasses)[which(columnClasses == 'character')]

filterNA[numericClasses] <- sapply(filterNA[numericClasses], as.numeric) # apply here which ones to include (and thus convert to numeric)

# col numbers
dependentCol <- which(names(filterNA) %in% dependentVar)
varToIncludeNR <- which(names(filterNA) %in% varToInclude)

# 2) + 3) test for normality and homoscadcity
# example to check for normality: mudbank_medianExtent (0 should be handled, can't set them to NA but the zero indicates no mudbank)

for (i in numericClasses){
  # i <- 28-8
  
  cexVal <- 2
  
  form <- paste("endpointRate ~",  paste0(names(columnClasses[i])))
  # test linear fits for each variable with netchange per year
  fit <- lm(form, data = filterNA,
            na.action = na.exclude) # fit the model
  outName <- paste0("./results/temp_maps/stats/",
                    'linear_residuals_rawData_', names(columnClasses[i]),
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
  par(mfrow=c(1,1))
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


# 4) log transform: e.g. mudbank median extent
# check skewness: https://rpubs.com/marvinlemos/log-transformation

mostSkewed <- function(mydataframe,colnumbers){
  # mydataframe <- filterNA
  # colnumbers <- varToIncludeNR
  
  
  skewnesM <- matrix(data = NA, nrow = length(colnumbers), ncol = 1)
  i<- 1
  for (c in colnumbers){
    # print(c)
    # skewnesM[i,1] <- colnames(filterNA[,c])
    # skewnesM[i,1] <- skewness(filterNA[,c])
    skewnesM[i,1] <- EnvStats::skewness(unlist(filterNA[,c]), na.rm = T)
    i<- i+1
  }
  row.names(skewnesM) <- colnames(filterNA[,colnumbers])
  skewnesM <- as.data.frame(skewnesM)
  # assign human-friendly names
  names(skewnesM) <- c("skewness")
  # sort and print the top n correlations
  head(skewnesM[order(abs(skewnesM[,1]),decreasing=T),],n=length(colnumbers))
  
  # return(skewnesM[order(abs(skewnesM[,1]),decreasing=T),])
  return(skewnesM)
}
logTransformNegatives <- function(x){
  # x <- filterNA$deltaCoast
  # x <- -1
  # minX <-  min(filterNA$deltaCoast, na.rm = T)
  
  # log transform: larger than 0 
  # logT<-sign(x)*log(abs(x))
  
  # ==> regular LOG produces NAN for negative values ==> scale to range starting at 0
  # ==> what to do with log(0) ==? add a constant value
  logT <- log(x - min(x, na.rm=T)+0.001)
  return(logT)
  }

# which ones to log transform: based on skewness?
skew <- mostSkewed(filterNA, varToIncludeNR)
toskewed <- row.names(skew)[which(abs(skew) >0.5)]
toSkewedNr <- which(names(filterNA) %in% toskewed)

# candidates for log transforming:
# coast_median, deltaCoast
log10t <- filterNA
log10t[varToIncludeNR] <- sapply(filterNA[varToIncludeNR], logTransformNegatives) 

# https://towardsdatascience.com/is-normal-distribution-necessary-in-regression-how-to-track-and-fix-it-494105bc50dd
# bc <- MASS::boxcox((filterNA$endpointRate)^2 ~ filterNA$deltaCoast)
# (lambda <- bc$x[which.max(bc$y)])

# compare log transformation
for (i in varToIncludeNR){
  # i <- varToIncludeNR[1]
  i <- 9
  print(names(filterNA)[i])
  sampleData <- sample(as.matrix(filterNA[,i]), 1000)
  
  # test if the sample is significantly different from normality (shapiro wilk statistic)
  shapiroTest<- shapiro.test(sampleData) # P<0.05 ==> not normally distributed
  # Take note that if the sample size is greater than 5000, you should use test statistics instead of the p-value as the indicator to decide.
  # If the value of p is equal to or less than 0.05, then the hypothesis of 
  # normality will be rejected by the Shapiro test. On failing, the test can state 
  # that the data will not fit the distribution normally with 95% confidence.
  
  OutputNames <- paste0("./results/temp_maps/stats/",
                  'normality_', names(filterNA)[i],
                    '.jpeg')
  # if log transformed, change outputnames
  # if (i %in% varlogTNR) {
  #   OutputNames <- paste0("./results/temp_maps/stats/",
  #                         'normality_', names(filterNA)[i],
  #                         '_logt.jpeg')
  # }
  if(!file.exists(OutputNames)){
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

# 5) scale + center: normalize
normalized <- filterNA # filterNA / log10t
normalized[varToIncludeNR] <- sapply(normalized[varToIncludeNR], scale, 
                                     center = T, scale = T)

# 6) explanatory analysis 
res2 <- Hmisc::rcorr(as.matrix(normalized[varToIncludeNR])) # apply correlation matrix
flatCorM <- flattenCorrMatrix(res2$r, res2$P) # flatten

# including checking for colinearity between variables
# plot correlation matrix
corrplot::corrplot.mixed(cor(as.matrix(normalized[c(varToIncludeNR, dependentCol)])), 
                         order = 'AOE')


# glm allows to fit other regression models: binomial (logistic) or possion
fit0 <- lm(deltaCoast ~ 1, data = normalized ) # family = binomial("logit")
fit1 <- lm(deltaCoast ~ freqOcc, data = normalized ) # family = binomial("logit")
fit_glm <- glm(deltaCoast ~ freqOcc * meanFraction + mudbank_medianExtent,# + mean_smoothedMeanFract + mudbank_medianExtent + meanCurve + meanOrient,
                data = normalized)

# difference between linear and quadratic model: Aikaike information criterion
Aikaike <- AIC(fit1, fit_glm) 
#==> lower AIC suggests it is preferred
print(paste0(row.names(Aikaike[which.min(Aikaike$AIC),]), ' is prefered'))

# likelihood ratio test: comparing nested generalized linear models
# anova (fit0,fit1, test = "LRT") # P < 0.05 == reject 0-hypothesis: conclude that the full model offers a bitter fit 
# anova (fit0,fit1, test = "F") 
# lrtest(fit_glm, # compare full model (first one mentioned) to an nested / reduced model
       # fit1) 

# summary(fit_glm) # ==? check if interaction term is significant?
# jtools::summ(fit_glm)

# interactions::interact_plot(model = fit_glm, pred = freqOcc, # centered = 'none', ==> already centered previously
#                             modx = meanFraction, interval = T) # plot.points = TRUE
# interpretation: 
# parallel lines: absence of heterogeinit / interaction
# differently sloped lines indicate heterogeinity


# 
# chart correlation
# # 1. Open jpeg file
# jpeg(paste0("./results/temp_maps/",
#            'full_correlationChart',
#            '_',  format(Sys.Date(), "%Y%m%d"),'.jpeg'),
#      width = 2143, height = 894)
# # 2. Create the plot
# chart.Correlation(as.matrix(normalized[varToIncludeNR]), histogram=TRUE, pch=15,
#                   cex.labels =2, font.labels = 4)
# 
# # 3. Close the file
# dev.off()
# 

# 7) stepwise multiple regression ?????? ==> which independepnd variable to select?
#'     Also referred to as forward selection of variables (??? See Michaud 2022)
#'     Include colinearity output from previous steps
#'     
#'     apply pearson correlation / principal component analysis: variable importance
#'     
#'     https://stackoverflow.com/questions/43441937/construction-of-nested-model-sequence-in-r
#'     https://www.scribbr.com/statistics/anova-in-r/
#'     
library(leaps)
library(nlme) # for gls model
library(relaimpo) # for relative contribution
library(gridExtra)# for table plotting

form = 'glm' # lm / glm / gls 
# GLS ==> account for spatial auto correlation (Otero 2020) and https://stats.stackexchange.com/questions/34325/regression-modelling-with-unequal-variance
# GLM ==> nonlinear regression when variance in sample is not constant or errors not normally distributed
#  
# 

# start out with nothing exlcuded
vifExcl <- NULL
stats <- data.frame(
  RMSE = NULL,
  R2 = NULL
)

fullModel <-  paste0(dependentVar, " ~ ", paste(
  colnames(normalized[varToIncludeNR])[!colnames(normalized)[varToIncludeNR] %in% vifExcl], 
  collapse="+"))

set.seed(7)
training.samples <-
  caret::createDataPartition(normalized$endpointRate,p = 0.8, list = FALSE)

train.data  <- normalized[training.samples, ]
test.data <- normalized[-training.samples, ]

# define different types of global models (incl. all relevant variables)
Globalmodel <- lm(formula(fullModel), data = train.data)
# Globalmodelglm <- glm(formula(fullModel), data = train.data)
Globalmodelgls <- gls(formula(fullModel), data = train.data)
summary(Globalmodel)
# likelihood ratio test: comparing nested generalized linear models
# lrt <- anova (Globalmodel,Globalmodelgls, test = "LRT") # P < 0.05 == reject 0-hypothesis: conclude that the full model offers a better fit
# Ft <- anova (Globalmodel,Globalmodelgls, test = "F")

predictions <- Globalmodel %>% predict(test.data)
plot(predictions,test.data$endpointRate)
abline(a=0, b = 1, col = 'red')

anGlobal <- anova(Globalmodel) # Anova
VIFglobal <-car::vif(Globalmodel)

summary(anGlobal)

# update column names that should be excluded
vifExcl <- c(vifExcl, names(VIFglobal)[VIFglobal >= max(VIFglobal)])

stats <-  rbind(
  stats, data.frame(
    RMSE = caret::RMSE(predictions, test.data$endpointRate),
    R2 = caret::R2(predictions, test.data$endpointRate),
    vif = max(VIFglobal)
  ))


# 
row.names(stats) <- vifExcl
# It seems that 'meanFraction' and 'meanOrient' have high colinearity with other independent variables.
# But if mean_smoothedMeanFract is removed the meanFraction can also be used.

## relative contribution to the total amount of variation explained
# see steur et al., 2022 using it as measure of relative importance of the variables
# https://stats.stackexchange.com/questions/79399/calculate-variance-explained-by-each-predictor-in-multiple-regression-using-r
# https://www.r-bloggers.com/2012/08/the-relative-importance-of-predictors-let-the-games-begin/
afss <- anGlobal$"Sum Sq"
anGlobal <- cbind(anGlobal,PctExp=afss/sum(afss)*100)

relim <- calc.relimp(Globalmodel, type = c("lmg"), rela = F) # type lmg is from lindemann (1980), relative = F:  the metrics sum to R^2
explained <- relim@R2 # variance explained
explainedManual <- sum(anGlobal$PctExp[1:8]) # exclude residuals to compare to manual method
relMetrics <- relim@lmg # importance (contribution to r2)

tableOverview <- rbind(data.frame(relContrR2 = round(relMetrics,3)), sum = round(sum(relMetrics), 3))

# plot in table

ss <- tableGrob(tableOverview)
grid.arrange(ss)

# stepwise model selection
# on why not to use this approach: https://stats.stackexchange.com/questions/20836/algorithms-for-automatic-model-selection
# varToInclude
# problems in stepAIC: meanFraction, normalized2
# Globalmodel <-  
stepFull <- MASS::stepAIC(lm(endpointRate ~ freqOcc +mean_smoothedMeanFract+
                               mudbank_medianExtent+meanCurve+meanOrient+
                               firstOrient+lastOrient,
                               data = normalized),
                          # scope = list(upper = ~Eth*Sex*Age*Lrn, lower = ~1),
                          direction="both")

# Alternatively test all models
varstoTest <- colnames(normalized[varToIncludeNR])[!colnames(normalized)[varToIncludeNR] %in% c('meanFraction', 'meanOrient')]
varstoTestNr <- which(colnames(normalized) %in% varstoTest)
mod <- leaps::regsubsets(as.formula(paste(dependentVar, '~ .')), data=normalized[,c(varstoTestNr, dependentCol)] , nvmax = 100, 
                  nbest = 10, really.big= T, method = "exhaustive")
# head(summary(mod)$which[,-1])
am <- summary(mod)$which[,-1]

# create unique models
pred <- lapply(1:nrow(am), function(x) colnames(am)[which(am[x,])])
lm.mod.form <- lapply(pred, function(x) paste0(form, "(", dependentVar, " ~ ", paste(x, collapse="+"), ", data=normalized)"))

all.mods <- lapply(lm.mod.form, function(x) eval(parse(text=x)))

comparison.output <- data.frame(model=1:length(all.mods), 
                                y=dependentVar, 
                                x=sapply(1:length(pred), function(x)paste0(pred[[x]], collapse="+")))
comparison.output$AIC <- sapply(all.mods, AIC)
comparison.output$BIC <- sapply(all.mods, BIC)
# comparison.output$r2 <- sapply(all.mods, summary)
# test$adj.r.squared <- summary(all.mods[[1]])

smallestAIC <- comparison.output %>% arrange(AIC) %>% head(10)

tableAIC <- tableGrob(smallestAIC)
grid.arrange(tableAIC)

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

# first correlation matrix
corM <- cor(normalized[,c(varToIncludeNR)]) # 
# covM <- cov(filterNA[,c(varToIncludeNR)])
sds <- sapply(filterNA[varToIncludeNR], sd) # sd's from the (not standardized) data (else all are 1)

mosthighlycorrelated(normalized[varToIncludeNR], length(varToIncludeNR)) # cor coefs

# PCA on the correlation matrix 
# PCA <- princomp(covmat = corM, scores = T)
PCA <- princomp(filterNA[varToIncludeNR], scores = T, cor = T)  # if applied on raw data scores are possible to compute
PCAsum <- summary(PCA, loadings = T)
PCA2 <- prcomp(corM)

plot(PCA$sdev^2, xlab = "Component number", # scree diagram
       ylab = "Component variance", type = "l", main = "Scree diagram")
plot(log(PCA$sdev^2), xlab = "Component number", # log eigenvalue diagram
      ylab = "log(Component variance)", type="l",
      main = "Log(eigenvalue) diagram")

# bivariate plots of the first principle components
pairs(PCA$scores[,1:3], 
        panel = function(x,y, ...) {
              MVA::bvbox(cbind(x,y), add = TRUE)
        })

# change depending on PCA
out <- sapply(1:6, function(i) {
    plot(normalized$endpointRate,PCA$scores[,i],
           xlab = paste("PC", i, sep = ""), ylab = "change per year (m)")
})

# biplot(PCA, col = c("gray", "black"))
# same directions of arrows: parameters are correlated
# the numbers are a observation and relate to the position which they 'score high in'

normalized_reg <- lm(endpointRate ~PCA$scores, 
                  data = normalized)
summary(normalized_reg)

