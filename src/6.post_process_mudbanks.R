## ---------------------------
#'
#' Script name: Pre-proces time series
#'
#' Short Description: pre proces GEE exports into valid estimates of coastline
#' position and estimates of presence/absence of mudbanks.
#' 
#'
#' Author: Job de Vries
#'
#' Date Created: 2020-11-16
#'
#' Copyright (c) Job de Vries, 2021
#' Email: j.devries4 at uu.nl
#'
## ---------------------------
#'
#' Description
#' 
#'
#'
## ---------------------------

rm(list = ls())
wd<-getwd()

## ---------------------------

options(scipen = 6, digits = 4) # I prefer to view outputs in non-scientific notation
memory.limit(30000000)     # this is needed on some PCs to increase memory allowance, but has no impact on macs.

## ---------------------------

#' load up the packages 
source("./src/packages.R")       # loads up all the packages
# ee_Initialize()                  # Initialize Earth Engine
## ---------------------------
source("./src/functions.R")

## ---------------------------

years <- c(seq(1985, 2021, 1))
aoi <-  c('FrenchGuiana') # Suriname / Braamspunt / WegNaarZee / FrenchGuiana / Guyana

# Valib Observation Threshold
VT <- list('Suriname' = 0.3,     # 0.3 / 0.2
           "FrenchGuiana" = 0.2, # 0.3 / 0.2
           "Guyana" = 0.2)
# Fraction Theshold
FT <- list('Suriname' = 0.2,        # 0.3 / 0.2
           "FrenchGuiana" = 0.3,
           "Guyana" = 0.1)
NeighborhoodSeize
NB <- list('Suriname' = 5000, 
            "FrenchGuiana" = 5000,
            "Guyana" = 5000)
# minimum sequence length
SL <- list('Suriname' = 5, # 5 - 7
            "FrenchGuiana" = 6, # 5 - 7
            "Guyana" = 6)

validObsThresholds <- VT[[aoi]]
neighborhoods <- NB[[aoi]]
fractThresholds <- FT[[aoi]]
sequenceLengths <- SL[[aoi]]

# search window used to define local minimums in mud fractions.
initialSearchWindows <- c(25) #25 
initialLowerLevelWindows <- c(3) # 3
slopeThersholds <- c(-0.1)

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
               "Guyana" = posToExcludeGUY)
posToExclude <- allPos[[aoi]]

# select previously calculated coastline positions
folderSelect <- as.matrix(list.files(paste0('data/processed/coastlines'), full.names = T))
df <- rewrite(folderSelect);

# only csv's
df <- df[grep('coastlines.csv', folderSelect, ignore.case = T),]
# years <- 2009
filtered <- vector('list', 100)
for (q in seq_along(years)) {
    year = years[q]
    filters = c(year, aoi)

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
filtered <- unique(filtered)
# bind_rows!!!
mudbanks <- do.call(plyr::rbind.fill,
                         lapply(as.matrix(filtered)[,1],
                                function(x) read.csv(x,
                                stringsAsFactors = FALSE, sep = ',',
                                na.strings=c("","NA")))
                         )

# all dates
uniqueDates <- unique(mudbanks$DATE_ACQUIRED)
all_years <- unique(mudbanks$year_col)
group_pos <- unique(mudbanks$pos)

# calculate mudbank extents (relative, abs and slope drop extents)
mudbanks <- mudbanks %>%
  dplyr::mutate(mudbank_extent = axisDist - coast_median,
                mudbank_extent_abs = axisDistAbs - coast_median,
                mudbank_extent_slope = axisDistSlope - coast_median,
                mudbank_outlier = 0, # set outlier: assume nothing is an outlier
                SmoothedSlopes = (SmoothedPeakFract - maxExtentIndex) / 
                  (SmoothedPeak - maxExtent) * 10000,
                mudbankObs = NA,              # amount of mudbank obs
                d_p_r = paste0(DATE_ACQUIRED, '_',areaName), # create a unique identifier
                x = -1, y = -1,
                procesDate = gsub('-', '',as.Date(Sys.time(), 'YYYYMMdd', tz = "UTC"))) 
# Replace Inf with NA
is.na(mudbanks$SmoothedSlopes) <- do.call(cbind,lapply(mudbanks$SmoothedSlopes, is.infinite))


# calculate slope of the super smoothed line
# mudbanks$SmoothedSlopes <- (mudbanks$SmoothedPeakFract - mudbanks$maxExtentIndex) / 
#   (mudbanks$SmoothedPeak -  mudbanks$maxExtent) * 10000

# mudbanks$mudbank_extent <- mudbanks$axisDist - mudbanks$coast_median  
# mudbanks$mudbank_extent_abs <- mudbanks$axisDistAbs - mudbanks$coast_median  
# mudbanks$mudbank_extent_slope <- mudbanks$axisDistSlope - mudbanks$coast_median  
# mudbanks$mudbankObs <- NA

# translate other distances into coordinates
mudbanks <- get_dists2(mudbanks, mudbanks$originX, mudbanks$originY, 
                                  mudbanks$bearing, 
                                  c('axisDistAbs', 'axisDistSlope', 'maxExtent'))

# count for each year, pos the total amount of relevant mudbank observations
# thus exclude -1 values
mudbanks2 <- mudbanks %>% 
  
  dplyr::group_by(pos, year_col) %>%
  plyr::mutate(validObs = ifelse(axisDist == -1 | (!is.na(coastDist) & 
                                                     axisDist < coastDist), 
                                 NA, 1)) %>%
  # some coastdists are NA, throws problems because can't do comparison to estimate offshore position
  dplyr::group_by(pos, year_col, validObs) %>% # count observations for validObs =1 & NA
  dplyr::mutate(mudbankObs = n()) %>%
  dplyr::mutate(meanFraction = mean(SmoothedPeakFract, na.rm=T)) %>%
  
  # equal values per year/pos combination
  dplyr::group_by(pos, year_col) %>% 
  dplyr::mutate(mudbankObs = ifelse(is.na(validObs), # replace NA with the count for valid obs
                                    Mode(mudbankObs[validObs==1]), mudbankObs)) %>%
  dplyr::mutate(meanFraction = ifelse(is.na(validObs), # replace NA with valid mean smoothed value
                                      Mode(meanFraction[validObs==1]), meanFraction)) %>%
  
  ungroup()
  

duplicate <- mudbanks2 %>%
  group_by_at(vars(DATE_ACQUIRED, pos,areaName)) %>%
  filter(n()>1) %>%
  ungroup()
# unique(duplicate$DATE_ACQUIRED)

mudbanks2 <- unique(mudbanks2) 

# reshape mudbanks such that each relative, absolute and slope drop gets it own data-entry for each pos
# so triplicate each row and overwrite the values in the corresponding columns
# transform such that for each pos all three coordinates become a separate entry with unique x,y coords
mudbanks3 <- mudbanks2 %>%
  slice(rep(1:n(), each = 3)) %>%     # triplicate each row
  dplyr::group_by(pos, DATE_ACQUIRED,areaName) %>%
  dplyr::mutate(dropClass = c("rel", "abs", 'slope')) %>% # assign a column indicating the type of drop
  dplyr::ungroup() %>%
  dplyr::group_by(pos,DATE_ACQUIRED) %>%
  dplyr::mutate(x = ifelse(dropClass == 'rel', peakCoordX, x),
                y = ifelse(dropClass == 'rel', peakCoordY, y)) %>%
  dplyr::mutate(x = ifelse(dropClass == 'abs', axisDistAbsX, x),
                y = ifelse(dropClass == 'abs', axisDistAbsY, y)) %>%
  dplyr::mutate(x = ifelse(dropClass == 'slope', axisDistSlopeX, x),
                y = ifelse(dropClass == 'slope', axisDistSlopeY, y)) %>%
  dplyr::mutate(axisDist = ifelse(dropClass == 'slope', axisDistSlope, axisDist),
                mudFract = ifelse(dropClass == 'slope', mudFractSlope, mudFract)) %>%
  dplyr::mutate(axisDist = ifelse(dropClass == 'abs', axisDistAbs, axisDist),
                mudFract = ifelse(dropClass == 'abs', mudFractAbs, mudFract)) %>%
  dplyr::mutate(mudbank_extent = ifelse(dropClass == "slope", mudbank_extent_slope, mudbank_extent),
                mudbank_extent = ifelse(dropClass == "abs", mudbank_extent_abs, mudbank_extent)) %>%

  dplyr::select(-c(mudFractAbs, mudFractSlope, axisDistAbs,         # drop the columns that have just been copied
                   axisDistSlope, axisDistSlopeY, axisDistSlopeX,
                   axisDistAbsY, axisDistAbsX,
                   mudbank_extent_slope, mudbank_extent_abs,
                   peakCoordX, peakCoordY)) %>%
  dplyr::ungroup()%>% 
  dplyr::mutate(rowNR = row_number()) 

# for each imagedate; test for obvious mudbank boundary outliers
# - mudbank extent cannot be smaller than coastline position
# - mudbank extent cannot be -1
# - mudbank extent subjected to outlier test

# create a new unique identifier that combines the date and path row
# to avoid different path-row observations from the same date to be joined
uniqueIdentifier <- unique(mudbanks3$d_p_r)

combination <- length(neighborhoods)*
  length(validObsThresholds)* 
  length(initialSearchWindows)* 
  length(initialLowerLevelWindows)* 
  # length(sequenceLengths)* 
  length(slopeThersholds)*
  length(fractThresholds)

rectanglesGT <- vector('list', 100)
grandTable <- vector('list', 100)

for (n in neighborhoods) {
  for(w in initialSearchWindows){
    for(lw in initialLowerLevelWindows){
      for(sl in slopeThersholds){
      
      # n = neighborhoods[1]
      # w = initialSearchWindows[1]
      # lw = initialLowerLevelWindows[1]
      # sl = slopeThersholds[1]

      neighborhoodSize = n
      initialSearchWindow = w
      initialLowerLevelWindow = lw
      initialSlope = sl
      
      # add details to matrix for export
      mudbanks3 <- mudbanks3 %>% 
        dplyr::mutate(neighborhoodSize = n)
      
      ####################
      ### START outlier detection
      
      
      for (i in uniqueIdentifier){  
        # i <- uniqueDates[500]
        # i <- c("2014-09-28_227_56")
        # i <- uniqueIdentifier[481]
      
        # select all observations of the corresponding date
        mudbanks_selection <-subset(mudbanks3, 
                                    d_p_r == i &
                                      # mudbanks3$DATE_ACQUIRED == i &
                                      !(pos %in% posToExclude)) 
        
        lowerBoundary <- min(mudbanks_selection$pos) + neighborhoodSize
        upperBoundary <- max(mudbanks_selection$pos) - neighborhoodSize
        # allObsSP <- sp_pnt_ee(mudbanks_selection$x,mudbanks_selection$y,
        #           'allObs',"orange")
        
        
        dateOI <- unique(mudbanks_selection$DATE_ACQUIRED)
        
        # order by position
        mudbanks_selection<-mudbanks_selection[order(mudbanks_selection$pos),]
        
        #'  obvious outliers:
        #' - mudbank extent cannot be smaller than coastline position
        #' - mudbank extent cannot be -1
        #' - mudbank extent subjected to outlier test
        nonsense <-  which(mudbanks_selection$coastDist > mudbanks_selection$axisDist |
                             mudbanks_selection$axisDist == -1 |
                             is.na(mudbanks_selection$axisDist))
        
        # keep track of entries that need to be tracked as outlier
        combinations <- data.frame(DATE_ACQUIRED = rep(dateOI, length(nonsense)),
                                   d_p_r = rep(i,length(nonsense)),
                                   pos = mudbanks_selection$pos[nonsense],
                                   dropClass = mudbanks_selection$dropClass[nonsense])
        
        # update mudbanks_selection
        mudbanks_selection <- mudbanks_selection[-nonsense, ]
        
        positions_all <- as.numeric(as.character(mudbanks_selection$pos))
        distances_all <- mudbanks_selection$mudbank_extent # grab the normalized distances
        
        if(length(distances_all) == 0){ print(paste0(dateOI,  ': no viable observations'))
          next}
        
        if(length(unique(positions_all)) > 2){
          # second order polynomial fit: alongshore position & polynomial fit
          lm_out_all <-lm(distances_all ~ poly(as.numeric(positions_all),2))
          
          # plot(positions_all, distances_all)
          # predicted.intervals <- predict(lm_out_all,
          #                                data.frame(x=as.numeric(positions_all)),
          #                                interval='confidence', level=0.99)
          
          # Bonferroni-adjusted mudbank_outlier test (test largest absolute standardized residual)
          outlier_test <- car::outlierTest(lm_out_all) 
          
          # combine all outliers
          outlier_ind<- c(as.numeric(names(outlier_test$rstudent)))
          
          # keep track of entries that need to be tracked as outlier
          combinations <- rbind(combinations, data.frame(DATE_ACQUIRED = rep(dateOI, length(outlier_ind)),
                                                         d_p_r = rep(i,length(outlier_ind)),
                                                         pos = mudbanks_selection$pos[outlier_ind],
                                                         dropClass = mudbanks_selection$dropClass[outlier_ind]))
          
          # drop these most obvious outliers from the selection
          # ==> from here on not every observations has 3 data entries anymore!!!!!
          mudbanks_selection <- mudbanks_selection[-outlier_ind, ]
        }
        
        ################
        #' best case scenario of defining no mudbank transects is the values related to super smoothed 
        #' peak height and distance and same for extent height and distance
        #' these values are smoothed and therefore relatively stable
        #' 
        #' The fraction values corresponding to maxExtent and SuperSmoothed peak are
        #' therefore assumed to be representative for offshore decrease of sediment
        #' So if the slopes are significant positive they are assumed to be outliers
        #' 
        #' risks:
        #' part of the transect over land, the shorter the less likely the SuperSmoothed peak is correct
        #' the longer, less likely the drop comming after the super smoothed peak is representing mudbank boundary
        
        slopes <- mudbanks_selection$SmoothedSlopes
        
        # calculate median and sd of negative slopes only
        adjusted_median <- median(slopes[slopes < 0], na.rm = T)
        adjusted_sd <- sd(slopes[slopes < 0], na.rm = T)
        
        # drop observations with 'significant'/positive slopes 
        slopeThresh <- initialSlope        # adjusted_median + adjusted_sd
        indicesSlopes <- which(slopes > slopeThresh)
        # variableDF[which(variableDF$id == i), 'slopeThresh'] <- adjusted_median + adjusted_sd
        
        #' 
        #' Very similar to the slope, also the Super Smoothed peak fraction could 
        #' indicate no-mudbank cases: very low values can be a sign of NO mudbank
        #' 
        #' This needs to be determined based on the values in vicinity of the transect
        #' to account for spatial variability. 
        
        # update search window if needed
        # when amount of observations < searchwindow.
        searchWindow <- ifelse(length(mudbanks_selection$SmoothedPeakFract) < initialSearchWindow, 
                               2, initialSearchWindow)
        lowerLevelWindow <- ifelse(searchWindow > initialLowerLevelWindow, 
                                   initialLowerLevelWindow, 1)
        
        # calculate running average
        # runnAve <- data.frame(pos=mudbanks_selection$pos,
        #                       rolling = zoo::rollmean(mudbanks_selection$SmoothedPeakFract, 
        #                                               searchWindow,fill = NA,
        #                                               align = c('center')))
        
        means <- sapply(c("right","center","left"),
                        function(x)zoo::rollmean(mudbanks_selection$SmoothedPeakFract,
                                                 searchWindow,align = x, fill = NA))
        
        # allRunAve <- cbind(mudbanks_selection$SmoothedPeakFract, means)
        runnAve <- data.frame(pos=mudbanks_selection$pos,
                              rolling = means[,2])
        
        # fill NA on left and right side 
        rowsNA <- c(min(which(is.na(runnAve$rolling))):
                      
                      ifelse(length(diff(which(is.na(runnAve$rolling))))>1, 
                             which(diff(which(is.na(runnAve$rolling)))>1),
                             length(runnAve$rolling))
                    
        )
        runnAve[rowsNA, 'rolling'] <- means[rowsNA,3]
        rowsNA2 <- c(which(is.na(runnAve$rolling)))
        runnAve[rowsNA2, 'rolling'] <- means[rowsNA2,1]
        
        # replace very small values
        runnAve$rolling[which(runnAve$rolling < 0.001)] <- NA
        
        # get transects with local minimum values
        # localMin <- rollapply(as.zoo(runnAve$rolling), 5, function(x) which.min(x)==2)
        # indices <- runnAve[which(localMin == T), 'pos']
        
        # all tops and bottoms with different moving window sizes
        bottoms <- lapply(1:searchWindow, function(x) inflect(runnAve$rolling, threshold = x)$minima)
        tops <- lapply(1:searchWindow, function(x) inflect(runnAve$rolling, threshold = x)$maxima)
        
        # is any list empty?
        emptyList <- any(sapply(tops, function(x) length(unlist(x))) ==0)
        if(emptyList){
          # not 1:search window are potential tops but the index where tops and bottom are not NA?
          lastNonNA <- max(1,min(which(sapply(tops, function(x) all(is.na(unlist(x))))))-1)
          
          searchWindow <- lastNonNA
        }
        
        # potential local minima and maxima
        
        # why are these bottoms and tops selected based on the search window? 
        # ==> because the tops and bottoms originating from smaller search windows are 
        #     also included  because 1: searchWindow. So probably better to use
        # lastNonNA approach as done above here. 
        # is it then still necessary to have the lowerWindow defined?
        # probably yes because that is also used as an alternative to look for 
        # shoulders to the local minima. 
        # ==> probably update such that local minima are searched with a certain 
        #   moving window and the corresponding sholders are always defined on a search window of 1
        # so localmaxPos needs to be changes from searchWindow to 1.  
        
        # Why not look at all of these defined local and min and max values?
        # in that case set to 1
        allMins <- runnAve[bottoms[[1]],]
        # [which(allMins$pos > lowerBoundary&allMins$pos < upperBoundary)]
        localMinsPos <- allMins$pos[which(allMins$pos > lowerBoundary&allMins$pos < upperBoundary)]
        localMinFracts <- allMins$rolling[which(allMins$pos > lowerBoundary&allMins$pos < upperBoundary)]
        
        localMaxPos <- unique(runnAve$pos[tops[[round(searchWindow)]]])  
        
        NoMudBankPos <- c()
        
        # local mins are potential transects with NO mudbank
        localMinCount <- length(unique(localMinsPos))
        
        # save variables for checking
        # variableDF[which(variableDF$id == i), 'searchWindow'] <- searchWindow
        # variableDF[which(variableDF$id == i), 'lowerLevelWindow'] <- lowerLevelWindow
        # variableDF[which(variableDF$id == i), 'localMinCount'] <- localMinCount
        
        if(localMinCount > 1){ # gte or just gt ?? 
          for(p in 1:length(localMinsPos)){
            # p <- 3
            minPos <- localMinsPos[p]
            minFract <- localMinFracts[p]
            
            valley <- subset(mudbanks_selection, pos == minPos)
            
            # at highest level ==> pronounced peaks &  valleys
            posDist <- localMaxPos[which(abs(localMaxPos - unique(valley$pos)) > 2000 &
                                           abs(localMaxPos - unique(valley$pos)) < 
                                           searchWindow*1000)]
            
            # at lower level ==> check if this can be changed so
            # that the parameter lowerLevelWindow can be excluded?
            posDist2 <- runnAve$pos[tops[[1]]][which(abs(runnAve$pos[tops[[1]]] - 
                                                           unique(valley$pos)) > 5000 &
                                                       abs(runnAve$pos[tops[[1]]] - unique(valley$pos)) )]
            
            
            # min max needs to contain an roll observation
            minMax <- c(min(runnAve$pos[which(!is.na(runnAve$rolling))]), 
                        max(runnAve$pos[which(!is.na(runnAve$rolling))]))
            
            # if with largest searchwindow there is a peak: select it
            # else go to different level and check, final option iis nearest min or max
            shoulder1 <- ifelse(length(posDist)>0,
                                posDist[which.min(abs(unique(valley$pos) - posDist))],
                                ifelse(length(posDist2)>0,
                                       posDist2[which.min(abs(unique(valley$pos) - posDist2))],
                                       minMax[which.min(abs(unique(valley$pos) - 
                                                              minMax))]))
            
            # determine on which side it is (left (= 1) or right (= -1) shoulder)
            testSign <- sign(minPos - shoulder1) 
            # # avoid using positions 2x
            posDist <- posDist[!(posDist %in% c(shoulder1, minPos))]
            posDist2 <- posDist2[!(posDist2 %in% c(shoulder1, minPos))]
            
            # Only keep candidates that are not used yet
            if(testSign == 1){
              posDist_new <- posDist[posDist - minPos > 1]
              posDist2_new <- posDist2[posDist2 - minPos > 1]
              minMax<-minMax[!(minMax %in% c(shoulder1, minPos))]
            } else{
              posDist_new <- posDist[posDist - minPos < -1]
              posDist2_new <- posDist2[posDist2 - minPos < -1]
              minMax<-minMax[!(minMax %in% c(shoulder1, minPos))]
            }
            
            shoulder2 <- ifelse(length(posDist_new)>0,
                                posDist_new[which.min(abs(unique(valley$pos) - posDist_new))],
                                ifelse(length(posDist2_new)>0,
                                       posDist2_new[which.min(abs(unique(valley$pos) - posDist2_new))],
                                       minMax[which.min(abs(unique(valley$pos) - 
                                                              minMax))   ]))
            
            # if valley depth is small & difference with meanMud is large 
            # then it is stil a mudbank
            shoulderHeight1 <- unique(runnAve[runnAve$pos %in% c(shoulder1) &
                                                !is.na(runnAve$rolling) ,2])
            shoulderHeight2 <- unique(runnAve[runnAve$pos %in% c(shoulder2) &
                                                !is.na(runnAve$rolling) ,2])
            
            
            avgDepth <- mean(c(shoulderHeight1, shoulderHeight2) - minFract, na.rm = T)
            # also needs to be positive?? Negative values indicate very high fraction
            # corresponding with the valley, e.g. no true 'valley'
            
            # 1) determine if it is truly a local minimum that corresponds to no mudbank
            diffFract <-mean((valley$SmoothedPeakFract - valley$meanMud)) 
            differenceTest <- unique(diffFract) < sd(mudbanks_selection$SmoothedPeakFract)
            
            
            depthTest <- avgDepth > sd(mudbanks_selection$SmoothedPeakFract)
            
            # there needs to be a second shoulder observation
            # 2) look wihtin a certain distance for other pos that have a similar mean mud value
            neighborhood <- subset(mudbanks_selection, 
                                   pos > min(c(shoulder1, shoulder2)) &
                                     pos < max(c(shoulder1, shoulder2)))
            
            sufficientDataTest <- !is.na(shoulder2) & nrow(neighborhood) > 1
            if(sufficientDataTest == F){
              next
            }
            
            # if depthTest false & difference Test FALSE: potential mudbank: skip loop
            # such that transects are not tested
            if(depthTest == F && differenceTest == F){ 
              
              next
            } 
            
            # If any of the shoulders is lower than the valley -> no outliers 
            # shoulderHeightTest <- (minFract<shoulderHeight1[1]) | 
            #   (minFract<shoulderHeight2[1])
            if((minFract<shoulderHeight1[1]) == F | 
               (minFract<shoulderHeight2[1]) == F){
              next
            }
            
            # if the fraction of the valley is below an absolute value: no mudbank
            # e.g. if minFract<fractThreshold (e.g. 0.2, 0.3, 0.4 or 0.5?)
            
            # if(minFract > fractThreshold){
            #   next
            # }
            
            # all positions in neighborhood that have a lower or similar fraction value
            # are excluded
            # in scenario's where the neighborhood is wedged in between 2 mudbanks
            # this results in to many points being removed (e.g. the window is big)
            # But also because ALL point observations at the POS are excluded (and 
            # not only the ones that are below the fraction value)
            valleyVal <- unique(runnAve[runnAve$pos == unique(valley$pos),2])
            subset_indx <- subset(neighborhood,
                                  SmoothedPeakFract < mean(valleyVal + 
                                                             sd(neighborhood$meanMud)))
            
            indx <- which(neighborhood$SmoothedPeakFract < 
                            mean(valleyVal + 
                                   sd(neighborhood$meanMud)))
            
            #????????????
            if(length(indx) == 0){
              indx <- c(1:length(neighborhood$pos))}
            
            # consider excluding all pos that are contained within the detected range of indx?
            # poses problems when in between two mudbanks.
            posRange <- seq(min(neighborhood$pos[indx]), 
                            max(neighborhood$pos[indx]),
                            1000)
            
            NoMudBankPos <- c(NoMudBankPos, 
                              posRange)
          }
          
          
        }
        
        #' 
        #' plot example
        #' 
        # plot(mudbanks_selection$pos, mudbanks_selection$SmoothedPeakFract)
        # points(runnAve$pos,runnAve$rolling, col = 'orange')
        # abline(v = localMinsPos, lty = 3)
        # points(mudbanks_selection$pos[which(mudbanks_selection$pos %in% NoMudBankPos)],
        #   mudbanks_selection$SmoothedPeakFract[which(mudbanks_selection$pos %in% NoMudBankPos)],
        #   col = 'red', pch =16)
        # points(mudbanks_selection$pos, mudbanks_selection$meanMud, col = 'blue')
        
        # indices of detected positions that correspond to interbank phases.
        NoMudBankInd <- which(mudbanks_selection$pos %in% NoMudBankPos)
        # points that potentially correspond to interbank phases
        NoMudBank <- mudbanks_selection[which(mudbanks_selection$pos %in% NoMudBankPos),]
        # NoMudBank_sp <- sp_pnt_ee(NoMudBank$x,
        #                           NoMudBank$y,
        #                           'NoMudBank',
        #                           "red")
        
        
        # row indices in mudbanks_selection
        indicesToDrop <- unique(c(indicesSlopes, NoMudBankInd))
        
        # row numbers in the original mudbanks dataframe that correspond to these observations
        # Create unique combinations to look for in the original mudbanks:
        combinations <- rbind(combinations, 
                              data.frame(DATE_ACQUIRED = rep(dateOI, length(indicesToDrop)),
                                         d_p_r = rep(i,length(indicesToDrop)),                    
                                         pos = mudbanks_selection$pos[indicesToDrop],
                                         dropClass = mudbanks_selection$dropClass[indicesToDrop]))
        
        rownrTest <- combinations %>% 
          left_join(mudbanks3, 
                    c("DATE_ACQUIRED" = "DATE_ACQUIRED",
                      "dropClass" = "dropClass",
                      'd_p_r' = 'd_p_r',
                      "pos" = "pos"), keep = F
          ) %>%
          dplyr::select(c(pos, dropClass, DATE_ACQUIRED,rowNR))
        rownr <- rownrTest$rowNR # rownumbers in original data frame
        
        # again update mudbanks_selection by removing outliers
        mudbanks_selection <- mudbanks_selection[-indicesToDrop, ]
        
        # apply outlier detection to remaining points 
        indicesToDrop2 <- c(0) # for the final bit
        for (pnt in unique(mudbanks_selection$pos)){
          # print(pnt)
          # pnt <-  unique(mudbanks_selection$pos)[1]
          
          selected_point <- subset(mudbanks_selection, pos == pnt)
          
          # select nearby points
          ajoining_points <- subset(mudbanks_selection, 
                                    pos %in% seq(from = (selected_point$pos[1]-neighborhoodSize)[1],
                                                 to = (selected_point$pos[1]+neighborhoodSize)[1],
                                                 by = 1000) &
                                      pos != selected_point$pos[1]
                                    )
          
          combined <- rbind(selected_point,ajoining_points)
          
          # order by position
          combined <- combined[order(as.numeric(as.character(combined$pos))),]
          
          positions <- as.numeric(as.character(combined$pos))
          distances <- combined$mudbank_extent #axisDist # grap the (normalized???) distances
          fractions <- combined$mudFract
          dropClass <- combined$dropClass
          
          datatest <- data.frame(positions=positions,distances=distances, 
                                 fractions = fractions, dropClass = dropClass,
                                 x=combined$x, y = combined$y)
          
          # sufficient positions that contain information: determine outliers
          if (length(unique(datatest$positions)) > 2 & 
              length(unique(datatest$distances)) > 2) {
            
            # # calculate linear fit
            lm.out_lin <- lm(datatest$distances~as.numeric(datatest$positions))
            
            # calculate second order polynomial 
            lm.out<-lm(datatest$distances ~ poly(as.numeric(datatest$positions),2))
            # plot(as.numeric(datatest$positions), datatest$distances)
            
            
            # statSummary <- summary(lm.out)
            # r2 <- statSummary$r.squared # or adjusted r2?
            # p <- statSummary             # should be <0.05?
            # fstat <- statSummary$fstatistic # 
            
            # Bonferroni-adjusted outlier test (test largest absolute standardized residual)
            # test if no error/warning
            test2 <- has_error(car::outlierTest(lm.out), silent = !interactive())
            test3 <- has_warning(car::outlierTest(lm.out))
            
            # Allways returns a observation (with largest Studentized residual)
            # Which implies it is not allways meeting the P < 0.05 cutoff value
            outlier_test <- if(test2|test3){car::outlierTest(lm.out_lin)} else{car::outlierTest(lm.out)}
            
            # if no outliers detected:
            outlier <- datatest[as.numeric(names(outlier_test$rstudent)),]
            
            
          } else {
            # if only 0 or 1 observation; cannot determine if outlier based on neighbours
            # to continue workflow return an index outside the selected range
            outlier <- data.frame(positions= -1, distances= -1, fractions = -1)
            
          }
          
          # test the selected position index of the outlier
          # if it is selected point of interest; consider it as outlier 
          if(length(outlier$positions) > 0 &&
             outlier$positions == unique(selected_point$pos)){
            
            
            # row numbers of the outliers in the original dataFrame
            rownr <- c(rownr, which(mudbanks3$d_p_r == i &
                                      mudbanks3$pos == outlier$positions &
                                      mudbanks3$dropClass == outlier$dropClass))
            
            # row number of the outliers in the subset used in this loop
            indicesToDrop2 <- rbind(indicesToDrop2, 
                                    which(mudbanks_selection$d_p_r == i &
                                            mudbanks_selection$pos == outlier$positions &
                                            mudbanks_selection$dropClass == outlier$dropClass))
            
          }
          
        }
        
        # final update of the mudbank selections
        mudbanks_selection <-  mudbanks_selection[-indicesToDrop2, ]
        
        # # # build image collection around that year
        # pnt <- ee$Geometry$Point(c(median(mudbanks_selection$originX), median(mudbanks_selection$originY)))
        # filtCollectAnnual <- collection$filterDate(as.character(as.Date(dateOI)),
        #                                            as.character(as.Date(dateOI) + 1))$
        #   filterBounds(pnt)
        # dates <- ee_get_date_ic(filtCollectAnnual, time_end = FALSE)
        # 
        # # test_mudbankOutlier <- testSubset[which(testSubset$mudbank_outlier== 1 ),]
        # # test_mudbank <- testSubset[which(testSubset$noMudbank== 0),]
        # 
        # MudbankIndication <- sp_pnt_ee(mudbanks_selection$x,mudbanks_selection$y,
        #            'potential mudbanks',"blue")
        # 
        # Map$centerObject(filtCollectAnnual$first(), 11)
        # addImg <- Map$addLayer(filtCollectAnnual$first(), visParamsToa, paste0('landsat: ',dateOI))
        # 
        # addImg + MudbankIndication + allObsSP
        
        
        # mudbank outliers are indicated with a 1
        mudbanks3[unique(rownr), "mudbank_outlier"] <-
          as.data.frame(mudbanks3[unique(rownr),"mudbank_outlier"])[,1] + 1
        
      }
      
      ####################
      ###'
      ###'  end of (original) outlier detection loop
      ###'  
      ###'  Start workflow to define mudbank pressence / absence
      for (v in validObsThresholds){
        for (x in sequenceLengths){
          for(t in fractThresholds){

            # x = as.numeric(sequenceLengths[1])
            # v= as.numeric(validObsThresholds[1])
            # t = as.numeric(fractThresholds[1])

            sequenceLength = x
            validObsThreshold = v
            fractThreshold = t
            
            mudbanks3 <- mudbanks3 %>%
              dplyr::mutate(fractThreshold = fractThreshold,
                            sequenceLength = sequenceLength,
                            validObsThreshold = validObsThreshold)

            # Count valid (non-outlier) observations 
            mudbanks4 <- mudbanks3 %>% 
              dplyr::group_by(pos, year_col, validObs, mudbank_outlier) %>% 
              # count observations for validObs (= 1 & NA)
              dplyr::mutate(validMudbankObs = n_distinct(DATE_ACQUIRED)) %>% # count unique observations
              
              # if nonsense observation or outlier, set to NA
              dplyr::mutate(validMudbankObs = ifelse(axisDist == -1 | mudbank_outlier > 0,  
                                                     NA, validMudbankObs)) %>%
              
              
              dplyr::group_by(year_col, pos) %>%
              # replace NA with the count for valid obs==> only positions withouth valid observations in given year will remain NA
              dplyr::mutate(validMudbankObs = ifelse(is.na(validMudbankObs),
                                                     Mode(validMudbankObs), validMudbankObs)) %>%
              
              ungroup()
            
            
            # adding a indication of noMudbank positions (so all observations on that POS receive 1)
            # e.g. when frequently identified as mudbank in a year: give 0
            mudbanks5 <- mudbanks4 %>% 
              dplyr::group_by(year_col, pos) %>%
              
              dplyr::mutate(
                noMudbank = case_when( 
                  validMudbankObs/mudbankObs > validObsThreshold & 
                    meanFraction > fractThreshold ~ 0, 
                  TRUE ~ 1)) %>% 
              ungroup()
            

            # calculate offshore distance and corresponding coordinates
            mudbanks5$distX <- NA
            mudbanks5$distY <- NA
            mudbanks5$medianOffshore <- NA
            
            mudbanks6 <- mudbanks5 %>%
              dplyr::mutate(toFilter = ifelse(axisDist > 0 &
                                                mudbank_outlier == 0 & # no outlier
                                                noMudbank == 0
                                              ,0,1)) %>%
              group_by(year_col, pos, toFilter) %>%
              
              # all obesrvations that have a valid mudbank observation
              dplyr::mutate(medianOffshore = ifelse(toFilter == 0, 
                                                    median(axisDist, na.rm=T), NA)) %>%
              # dplyr::mutate(meanFract = 
              #                 ifelse(toFilter == 0, mean(SmoothedPeakFract, na.rm = T),NA)) %>%
              
              group_by(year_col, pos) %>%
              # invalid mudbank observations are filled per year and group
              dplyr::mutate(medianOffshore = fill_NA(medianOffshore)) %>%
              # dplyr::mutate(meanFract = fill_NA(meanFract)) %>%
              
              # some positions are not assigned as mudbank observation
              # they require a filled medianOffshore of 0
              dplyr::mutate(medianOffshore = ifelse(is.na(medianOffshore),
                                                    0,medianOffshore )) %>%
              
              # ungroup() %>%
              # dplyr::mutate(countObs= n_distinct(DATE_ACQUIRED)) %>%
              
              # calculate coordinates coorsponding to median offshore distance
              dplyr::mutate(distX = func_destP(originX, originY,bearing,medianOffshore)[1,1],
                            distY = func_destP(originX, originY,bearing,medianOffshore)[1,2]) %>%
              ungroup()
            
              
            # export ==> only turn on when testing different input scenarios
            
            if (combination > 1){
              # for (year in unique(format(as.Date(uniqueDates), '%Y'))){
              #   # year <- 2001
              #   # print(year)
              #   start_year <- as.Date(ISOdate(year, 1, 1))
              #   end_year <- as.Date(ISOdate(year, 12, 31))
              # 
              #   mudbanks_per_year <-subset(mudbanks6,
              #                              as.Date(DATE_ACQUIRED) >= start_year &
              #                                as.Date(DATE_ACQUIRED) <= end_year)
              # 
              # 
              #   write_csv(mudbanks_per_year,paste0("./data/processed/offshore_points/scenarios/",
              #              aoi,'_', year, '_offshore_nsize',n, '_obsT',v,
              #              '_window',w, '_lwinder',lw, '_slope',sl,
              #                '_fractT',fractThreshold, '.csv'))}
              
              # for testing / visualization define an imageCollection
              coordsFG <- list (
                c(-54, 5.918),
                c(-51.25,4.8),
                c(-51.67, 4.04),
                c(-53.9867,5.504)
              )
              coordsSUR <- list(
                c(-56.856912, 5.836168),
                c(-56.821485, 6.120976),
                c(-54.262531, 6.009777),
                c(-54.255509, 5.772303)
              )

              pol <- ee$Geometry$Polygon(
                coords = coordsFG,
                proj = "EPSG:4326",
                geodesic = FALSE
              )

              collectionL4 <- ee$ImageCollection("LANDSAT/LT04/C01/T1_TOA")
              # filterBounds(pol)

              collectionL5 <- ee$ImageCollection("LANDSAT/LT05/C01/T1_TOA")
              # filterBounds(pol)

              collectionL7 <- ee$ImageCollection("LANDSAT/LE07/C01/T1_TOA")
              # filterBounds(pol)

              collectionL8 <- ee$ImageCollection("LANDSAT/LC08/C01/T1_TOA")
              # filterBounds(pol)

              collection <- collectionL8$merge(collectionL5)$merge(collectionL7)$
                merge(collectionL4)


              vizParams = list(
                bands = c("B5", "B4", "B3"),
                min = 0.05, max = 0.5, gamma = 1.4
              )


              # testDate <- "2018-10-09_227_56"#c("2018-10-09") #uniqueDates[358]
              # testPos <- 175000
              # testSubset <- subset(mudbanks6, d_p_r == testDate)

              # test_mudbankOutlier <- testSubset[which(testSubset$mudbank_outlier== 1 ),]
              # test_mudbank <- testSubset[which(testSubset$noMudbank== 0),]

              # two-d plot
              # plot(testSubset$pos, testSubset$SmoothedPeakFract)
              # points(testSubset$pos[which(testSubset$mudbank_outlier== 1 )],
                     # testSubset$SmoothedPeakFract[which(testSubset$mudbank_outlier== 1 )],
                      # col = 'red')
              # indication of mudbank based on full year of observations?
              # points(testSubset$pos[which(testSubset$noMudbank== 0 )],
                     # testSubset$SmoothedPeakFract[which(testSubset$noMudbank== 0 )],
                     # col = 'blue')

              # spatial plot example
              # test_spatial <- sp_pnt_ee(testSubset$x,
              #                           testSubset$y, paste0('date: ',testDate),
              #                            "#d95f0e")
              #
              # pnt <- ee$Geometry$Point(c(median(testSubset$originX), median(testSubset$originY)))
              #
              # filtCollect <- collection$filterBounds(pnt)$
              #   filterDate(as.character(as.Date(min(testSubset$DATE_ACQUIRED))-1),
              #              as.character(as.Date(max(testSubset$DATE_ACQUIRED))+1))$
              #   sort("CLOUDCOVER", TRUE)
              # dates <- ee_get_date_ic(filtCollect, time_end = FALSE)
              #
              # acquisition <- ee_get_date_img(filtCollect$first())$time_start
              #
              # Map$centerObject(filtCollect$first())
              # first <- Map$addLayer(filtCollect$first(), vizParams,
              #                       paste0('landsat: ', format(as.Date(acquisition), '%Y-%m-%d')))
              #
              # first + test_spatial +
              #   sp_pnt_ee(test_mudbankOutlier$x,
              #             test_mudbankOutlier$y, paste0('outliers: ',testDate),
              #             "red") +
              #   sp_pnt_ee(test_mudbank$x,
              #             test_mudbank$y, paste0('mudbanks: ',testDate),
              #           "blue")
              #' ####################################################################################
              #'
              #' plot hovmoller

              all_years <- unique(mudbanks6$year_col)
              all_pos <- sort(unique(mudbanks6$pos))
              rectangles <- data.frame(id = character(),fill =  character(),
                                       colour = character,
                                       lengthMudbank = double(),
                                       xmin = double(), xmax = double(),
                                       ymin = double(), ymax = double(),
                                       neighborhoodSize = double(),
                                       validObsThreshold = double(),
                                       nrMudbanks = double(),
                                       sequenceLength = double(),
                                       initialSearchWindow = double(),
                                       initialLowerLevelWindow = double(),
                                       initSlopeT = double(),
                                       fractThreshold = double())


              # get median position for each year & mudbank indicators (rectangles)
              for(y in 1:length(all_years)){
                # y <- 15
                # selected_year <- '2014-01-01'
                selected_year <- all_years[y]
                annualSubset <- subset(mudbanks6,as.Date(year_col) == selected_year &
                                         coast_outlier == 1) # why again filterin coastal outliers?

                # idx <- which(mudbanks6$year_col == selected_year &
                #                mudbanks6$coast_outlier == 1)

                # all posiotions considered a mudbank during given year
                mudbankObs <- subset(annualSubset, !(pos %in% posToExclude) &
                                       noMudbank == 0)
                getPos <- unique(mudbankObs$pos)

                # sequences
                sequences <- split(getPos, cumsum(c(0, diff(getPos) > 1000)));

                # drop sublist with only x amount of consequetive mudbank observations
                filtSequences <- Filter(function(x){length(x)>sequenceLength}, sequences)

                # INCLUDE HERE (of earlier?) TO get the mean fraction value of each mudbank position
                # also for each year/position combination the amount of times it is considered a mudbank?


                startPos <- vapply(filtSequences, head, n = 1L, FUN.VALUE = numeric(1))
                endPos <- vapply(filtSequences, tail, n = 1L, FUN.VALUE = numeric(1))
                lengthMudbank <- endPos - startPos
                mudbankCount <- length(startPos)

                if(length(startPos) == 0){
                  startPos <- c(0)
                  endPos <- c(0)
                  lengthMudbank <- c(0)
                  mudbankCount <- c(0)
                }

                # get average mud fraction for each mudbank detected?
                # for each start position subset
                rectangles <- rbind(rectangles,
                                    data.frame(id = selected_year,fill = 'black',
                                               colour = 'black',
                                               xmin = startPos,
                                               xmax = endPos,
                                               lengthMudbank = lengthMudbank,
                                               nrMudbanks = mudbankCount,
                                               ymin = as.Date(selected_year)-184, # the geom_tiles per year have first of januari each year as midpoint
                                               ymax = as.Date(selected_year)+181,
                                               neighborhoodSize = n,
                                               validObsThreshold = v,
                                               sequenceLength = x,
                                               initialSearchWindow = w,
                                               initialLowerLevelWindow = lw,
                                               # slopeThresh = adjusted_median + adjusted_sd,
                                               initSlopeT = sl,
                                               fractThreshold = fractThreshold
                                    )) # so to have years overlapping this needs to be corrected


              }

              fractiontable <- subset(mudbanks6, SmoothedPeakFract >= 0)  %>%
                dplyr::group_by(year_col, pos) %>% # quarterly_col
                dplyr::summarize(mean_val = mean(SmoothedPeakFract, na.rm = T)) %>%
                ungroup()

              range <- round(quantile(fractiontable$mean_val,c(0.01,0.5, 0.99),
                                      na.rm=T), 2)

              # alongshore variation of mud fractions
              hovmoller <-
                ggplot(subset(fractiontable, mean_val >= 0 & !(pos %in% posToExclude)),
                       aes(x = pos,y = as.Date(year_col), fill=mean_val))+

                geom_tile(color= "white",size=0.1, na.rm = TRUE) +
                scale_fill_gradient2(limits = c(range[[1]], range[[3]]),
                                     breaks = c(range[[1]], range[[2]], range[[3]]),
                                     high = "#543005", low ='#313695', mid = '#f7f7f7',
                                     midpoint = range[[2]],
                                     guide = guide_colourbar(nbin=100, draw.ulim = FALSE,
                                                             draw.llim = FALSE),
                                     oob=squish, na.value = NA,
                                     name = c('Mean Fraction')) +
                geom_rect(data = rectangles, inherit.aes = FALSE,
                          aes(xmin = xmin, xmax = xmax,
                              ymin = ymin, ymax = ymax-75,
                              colour = colour), fill = NA, size = 0.8) +
                scale_colour_manual(name = ' ', values = c("black"),
                                    labels = c('mudbank'),
                                    guide = guide_legend(ncol = 2))+

                labs(y = 'Year', x = 'Alongshore Position [km]') +
                scale_x_continuous(lim=c(0,max(mudbanks6$pos)), expand = c(0,0),
                                   labels = unit_format(unit = "", scale = 0.001)) +

                theme(axis.line.x = element_line(size = 0.5, colour = "black"),
                      axis.line.y = element_line(size = 0.5, colour = "black"),
                      axis.line = element_line(size= 1, colour = "black"),
                      axis.title.y = element_text(size = 18, face = 'bold'),
                      axis.title.x = element_text(size = 18, face = 'bold'),
                      axis.text.x = element_text(size = 18,  hjust = .5, vjust = .5),
                      axis.text.y = element_text(size = 18, hjust = .5, vjust = .5),
                      legend.background = element_rect(fill = '#d9d9d9',  colour = '#d9d9d9'),
                      legend.title = element_text(colour = 'black', size = 20, face = "bold"),
                      legend.text = element_text(size = 16),
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      panel.background = element_blank(),
                      plot.background = element_rect(fill = '#d9d9d9',  colour = '#d9d9d9'))
              
              
              ggsave(plot = hovmoller, filename = paste0("./results/temp_maps/testMudbanks/",
                                                         aoi, '_nsize',n, '_obsT',v, '_sLenght',x,
                                                         '_window',w, '_lwinder',lw, '_slope',sl,
                                                         '_fractT',fractThreshold,

                                                         '_hovmollerFigure_1985_2020','.jpeg'),
                     width = 13.1, height = 7.25, units = c('in'), dpi = 1200)

              rectanglesGT <- rbind(rectanglesGT, rectangles)

              grandTable <- rbind(grandTable, variableDF)
            }
            # write_csv(rectanglesGT, paste0("./results/temp_maps/testMudbanks/", aoi,
            #                              '_mudbanksv7.csv'))
            # inputs: neighborhoodSize, validObsThreshold, sequenceLength, initSlopeT,
            # initialSearchWindow & initialLowerLevelWindow
            # outputs: lengthMudbank, nrMudbanks, slopeThresh
            
            # write_csv(grandTable, paste0("./results/temp_maps/testMudbanks/", 
            #                                aoi, '_imageScenariosv7.csv'))
            # inputs: searchWindow, lowerLevelWindow,  
            #           neighborhoodSize, sequenceL, validObs, initSlopeT
            # outputs: slopeThresh, localMinCount (amount of potential mudbank locations in each image)
            #         

          }}
      }}}}}

# export if 1 scenario was tested:
if (combination == 1){
  for (year in unique(format(as.Date(uniqueDates), '%Y'))){
    # year <- 2001
    # print(year)
    start_year <- as.Date(ISOdate(year, 1, 1))
    end_year <- as.Date(ISOdate(year, 12, 31)) 
    fileName <- paste0(wd,"/data/processed/offshore_points/", aoi,
                       '_', year, '_offshore.csv')
    print(fileName)
    
    if(!file.exists(fileName)){
    mudbanks_per_year <-subset(mudbanks6,
                               as.Date(DATE_ACQUIRED) >= start_year &
                                 as.Date(DATE_ACQUIRED) <= end_year) %>%
      dplyr::select(!c(five_year_col, bearing, geometry, quarterly_col, five_year_col,
                       slope,locf,key, toFilter, endDrop,offsetLast, endX, endY))
    
    write_csv(mudbanks_per_year, fileName)
    }
    
    
  }
}



# files <- as.matrix(list.files(paste0('./results/temp_maps/testMudbanks/'), full.names = T))
# df <- files[grep('.csv', files, ignore.case = T),]


# scenarios <- df[grep('_imageScenarios', df)]
# scenariosGT <- do.call(plyr::rbind.fill,
#                         lapply(as.matrix(df)[c(4,5,6),1],
#                                function(x) read.csv(x,
#                                                     stringsAsFactors = FALSE, sep = ',',
#                                                     na.strings=c("","NA")))
# )







# select previously calculated coastline positions
# rects <- df[grep('_mudbank', df)]
# 
# rectanglesGT <- do.call(plyr::rbind.fill,
#                     lapply(as.matrix(rects)[c(4,5,6),1],
#                            function(x) read.csv(x,
#                                                 stringsAsFactors = FALSE, sep = ',',
#                                                 na.strings=c("","NA")))
# )
# remove duplicate scenarios: 
# neighborhoodSize , validObsThreshold,initialSearchWindow, initialLowerLevelWindow, sequenceL, slopeThresh 


# scenarios <- rectanglesGT %>% 
#   group_by_at(vars(neighborhoodSize , validObsThreshold,sequenceLength,initialSearchWindow, initialLowerLevelWindow, initSlopeT)) %>%  # , collectiontype
#   filter(n()>1) %>% 
#   ungroup()


# # first estimate of important parameters in rectanglesGT:
# # validObs (min fraction of observations that is considered a mudbank)
# # sequence length: amount of neighbouring transects that is considered a mudbank
# 
# # The lowerlevelWindow and search window only seem to make a difference at 
# # low validObs (0.6) and sequenceLength 
# 
# # xaxis <- 'validObsThreshold'
# boxes <- 'sequenceLength'
# facet <- 'neighborhoodSize'
# yaxis <- 'lengthMudbank' # lengthMudbank, nrMudbanks
# 
# 
# boxplot <- ggplot(rectanglesGT, 
#                   aes(y = eval(as.name(yaxis)), 
#                       group = eval(as.name(boxes)))) +
#   facet_wrap(paste0('~', facet)) +
#   
#   geom_boxplot(outlier.colour="black", outlier.size=2, width=0.6) +          # boxplot properties
#   
#   labs(y =yaxis, fill = boxes) +  #  
#   scale_x_discrete(breaks=c(unique(rectanglesGT[[boxes]]))) +
#   
#   theme(
#     axis.line.x = element_line(size = 0.5, colour = "black"),
#     axis.line.y = element_line(size = 0.5, colour = "black"),
#     axis.line = element_line(size=1, colour = "black"),
#     axis.text.x = element_text(color = "grey20", size = 14, hjust = .5, vjust = .5, face = "bold"),
#     axis.text.y = element_text(color = "grey20", size = 14, hjust = .5, vjust = .5, face = "bold"),
#     axis.title.x = element_text(size = 18, face = 'bold'),
#     axis.title.y = element_text(size = 18, face = 'bold'),
#     
#     strip.background = element_rect(fill = "white", colour = "white"),
#     # legend.key = element_rect(fill = NA),
#     # legend.text = element_text(size = 18),
#     # legend.title = element_text(colour = 'black', size = 20, face = 'bold'),
#     # 
#     panel.border = element_blank(),
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     panel.background = element_blank(),
#     panel.spacing.x = unit(2, 'lines'),
#     # 
#     strip.text.x = element_text(size = 16, face = 'bold')
#     
#   )
# boxplot