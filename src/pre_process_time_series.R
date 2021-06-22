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
ee_Initialize()                  # 
## ---------------------------
source("./src/functions.R")

## ---------------------------

years <- c(seq(1985, 2020, 1))
aoi <-  c('Suriname') # Suriname / Braamspunt / WegNaarZee

# pos to exlcude for mudbank boundary estimates / outlier detection
posToExclude <- c(seq(138000,147000,1000),
                  seq(241000, 255000, 1000))  

# outlier detection parameters for mudbanks
initialSearchWindow <- 25            # amount of
lowerLevelWindow <- 3

# select previously calculated coastline positions
folderSelect <- as.matrix(list.files(paste0('data/processed/coastlines'), full.names = T))
df <- rewrite(folderSelect);

# only csv's
df <- df[grep('.csv', folderSelect, ignore.case = T),]
# years <- 2009
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
mudbanks <- do.call(bind_rows,
                         lapply(as.matrix(filtered)[,1],
                                function(x) read.csv(x,
                                stringsAsFactors = FALSE, sep = ',',
                                na.strings=c("","NA")))
                         )

# calculate mudbank extents (relative, abs and slope drop extents)
mudbanks$mudbank_extent <- mudbanks$axisDist - mudbanks$coast_median  
mudbanks$mudbank_extent_abs <- mudbanks$axisDistAbs - mudbanks$coast_median  
mudbanks$mudbank_extent_slope <- mudbanks$axisDistSlope - mudbanks$coast_median  

# set outlier: assume nothing is an outlier
mudbanks$mudbank_outlier <- 0

# translate other distances into coordinates
mudbanks <- get_dists2(mudbanks, mudbanks$originX, mudbanks$originY, 
                                  mudbanks$bearing, 
                                  c('axisDistAbs', 'axisDistSlope', 'maxExtent'))

# calculate slope of the super smoothed line
mudbanks$SmoothedSlopes <- (mudbanks$SmoothedPeakFract - mudbanks$maxExtentIndex) / 
  (mudbanks$SmoothedPeak -  mudbanks$maxExtent) * 10000

# Replace Inf with NA
is.na(mudbanks$SmoothedSlopes) <- do.call(cbind,lapply(mudbanks$SmoothedSlopes, is.infinite))

# amount of mudbank obs
mudbanks$mudbankObs <- NA

# count for each year, pos the total amount of relevant mudbank observations
# thus exclude -1 values
mudbanks2 <- mudbanks %>% 
  # distinct(DATE_ACQUIRED, year_col, pos, .keep_all = T) %>% # only unique observations
  dplyr::group_by(pos, year_col) %>%
  plyr::mutate(validObs = ifelse(axisDist == -1 | (!is.na(coastDist) & 
                                                     axisDist < coastDist), 
                                 NA, 1)) %>%
  # some coastdists are NA, throws problems because can't do comparison to estimate offshore position
  dplyr::group_by(pos, year_col, validObs) %>% # count observations for validObs =1 & NA
  dplyr::mutate(mudbankObs = n()) %>%
  dplyr::group_by(pos, year_col) %>%
  dplyr::mutate(mudbankObs = ifelse(is.na(validObs), # replace NA with the count for valid obs
                                    Mode(mudbankObs), mudbankObs)) %>%
  ungroup()

# reshape mudbanks such that each relative, absolute and slope drop gets it own data-entry for each pos
# so triplicate each row and overwrite the values in the corresponding columns
# transform such that for each pos all three coordinates become a separate entry with unique x,y coords
mudbanks2$x <- -1
mudbanks2$y <- -1


mudbanks3 <- mudbanks2 %>%
  slice(rep(1:n(), each = 3)) %>%     # triplicate each row
  dplyr::group_by(pos, DATE_ACQUIRED) %>%
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
  dplyr::ungroup()

# all dates
uniqueDates <- unique(mudbanks3$DATE_ACQUIRED)


# for each imagedate; test for obvious mudbank boundary outliers
for (i in uniqueDates){

  # select all observations of the corresponding date
  mudbanks_selection <-subset(mudbanks3, mudbanks3$DATE_ACQUIRED == i & 
                                !(pos %in% posToExclude)) 
  

  # order by position
  mudbanks_selection<-mudbanks_selection[order(mudbanks_selection$pos),]

  #'
  #'  obvious outliers:
  #' - mudbank extent cannot be smaller than coastline position
  #' - mudbank extent cannot be -1
  #' - mudbank extent subjected to outlier test
  nonsense <-  which(mudbanks_selection$coastDist > mudbanks_selection$axisDist |
                       mudbanks_selection$axisDist == -1 |
                       is.na(mudbanks_selection$axisDist))
  
  # keep track of entries that need to be tracked as outlier
  combinations <- data.frame(DATE_ACQUIRED = rep(i, length(nonsense)),
                             pos = mudbanks_selection$pos[nonsense],
                             dropClass = mudbanks_selection$dropClass[nonsense])
  
  # update mudbanks_selection
  mudbanks_selection <- mudbanks_selection[-nonsense, ]
  
  positions_all <- as.numeric(as.character(mudbanks_selection$pos))
  distances_all <- mudbanks_selection$mudbank_extent # grab the normalized distances
  
  if(length(distances_all) == 0){ print(paste0(i,  ': no viable observations'))
    next}
  
  
  if(length(unique(positions_all)) > 2){
    # second order polynomial fit: alongshore position & polynomial fit
    lm_out_all <-lm(distances_all ~ poly(as.numeric(positions_all),2))
    
    predicted.intervals <- predict(lm_out_all,
                                   data.frame(x=as.numeric(positions_all)),
                                   interval='confidence', level=0.99)
    
    # Bonferroni-adjusted mudbank_outlier test (test largest absolute standardized residual)
    outlier_test <- car::outlierTest(lm_out_all) 
    
    # combine all outliers
    outlier_ind<- c(as.numeric(names(outlier_test$rstudent)))
    

    # keep track of entries that need to be tracked as outlier
    combinations <- rbind(combinations, data.frame(DATE_ACQUIRED = rep(i, length(outlier_ind)),
                                                   pos = mudbanks_selection$pos[outlier_ind],
                                                   dropClass = mudbanks_selection$dropClass[outlier_ind]))
    # first + non_outliers + coastline_selection + nonsense_sp + outliers_sp
    
    #' 
    #' plotting!
    #' 
    
    # drop these most obvious outliers from the selection
    # ==> from here on not every observations has 3 data entries anymore!!!!!
    mudbanks_selection <- mudbanks_selection[-outlier_ind, ]
  }
  
  ################
  #' best case scenario of defining no mudbank transects is the values related to super smoothed 
  #' peak height and distance and same for extent height and distance
  #' these values are smoothed and therefore relatively stable
  #' 
  #' So assuming the fraction values corresponding to maxExtent and SuperSmoothed peak are
  #' representative increase between them or relative flat decrease in fraction
  #' represents no mudbank scenario
  #' 
  #' risks:
  #' part of the transect over land, the shorter the less likely the SuperSmoothed peak is correct
  #' the longer, less likely the drop comming after the super smoothed peak is representing mudbank boundary

  slopes <- mudbanks_selection$SmoothedSlopes

  # calculate median and sd of negative slopes only
  median_slope <- median(slopes[slopes < 0], na.rm = T)
  adjusted_sd <- sd(slopes[slopes < 0], na.rm = T)
  
  # drop observations with 'significant'/positive slopes 
  indicesSlopes <- which(slopes > (median_slope + adjusted_sd))

  #' 
  #' Very similar to the slope, also the Super Smoothed peak fraction could 
  #' indicate no-mudbank cases: very low values can be a sign of NO mudbank
  #' 
  #' This needs to be determined based on the values in vicinity of the transect
  #' to account for spatial variability. 

  # update search window if needed
  searchWindow <- ifelse(length(mudbanks_selection$SmoothedPeakFract) < initialSearchWindow, 2, initialSearchWindow)
  lowerLevelWindow <- ifelse(searchWindow > lowerLevelWindow, lowerLevelWindow, 1)
  
  # calculate running average
  runnAve <- data.frame(pos=mudbanks_selection$pos,
                       rolling = zoo::rollmean(mudbanks_selection$SmoothedPeakFract, 
                                               searchWindow,fill = NA))
  # replace very small values
  runnAve$rolling[which(runnAve$rolling < 0.001)] <- NA
  
  # get transects with local minimum values
  localMin <- rollapply(as.zoo(runnAve$rolling), 3, function(x) which.min(x)==2)
  indices <- runnAve[which(localMin == T), 'pos']
  
  bottoms <- lapply(1:searchWindow, function(x) inflect(runnAve$rolling, threshold = x)$minima)
  tops <- lapply(1:searchWindow, function(x) inflect(runnAve$rolling, threshold = x)$maxima)
  
  # potential local mins
  allMins <- runnAve[bottoms[[searchWindow]],]
  localMinsPos <- unique(runnAve$pos[bottoms[[searchWindow]]])
  localMaxPos <- unique(runnAve$pos[tops[[round(searchWindow)]]])
  
  NoMudBankPos <- c()
  
  # local mins are potential transects with NO mudbank
  if(length(unique(localMinsPos)) > 1){
  
    
    for(p in 1:length(localMinsPos)){
      # p <- 1
      minPos <- localMinsPos[p]
      minFract <- allMins[p,'rolling']

      valley <- subset(mudbanks_selection, pos == minPos)
      
      # at highest level ==> pronounced peaks &  valleys
      posDist <- localMaxPos[which(abs(localMaxPos - unique(valley$pos)) > 3000 &
                                     abs(localMaxPos - unique(valley$pos)) < searchWindow*1000 )]
      
      # at lower level
      posDist2 <- runnAve$pos[tops[[lowerLevelWindow]]][which(abs(runnAve$pos[tops[[lowerLevelWindow]]] - unique(valley$pos)) > 5000 &
                                     abs(runnAve$pos[tops[[lowerLevelWindow]]] - unique(valley$pos)) )]
      
      
      # min max needs to contain an roll observation
      
      minMax <- c(min(runnAve$pos[which(!is.na(runnAve$rolling))]), 
                  max(runnAve$pos[which(!is.na(runnAve$rolling))]))
      # minMax <- c(min(mudbanks_selection$pos), max(mudbanks_selection$pos))
      
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
      shoulderHeight <- unique(runnAve[runnAve$pos %in% c(shoulder1, shoulder2) &
                                         !is.na(runnAve$rolling) ,2])
      avgDepth <- mean(shoulderHeight - minFract, na.rm = T)
  
      # 1) determine if it is truly a local minimum that corresponds to no mudbank
      diffFract <-mean((valley$SmoothedPeakFract - valley$meanMud)) 
    
      differenceTest <- unique(diffFract) < sd(mudbanks_selection$SmoothedPeakFract)
      depthTest <- avgDepth > sd(mudbanks_selection$SmoothedPeakFract) 
      
      # If any of the shoulders is lower than the valley -> no outliers 
      shoulderHeightTest <- (minFract<shoulderHeight[1]) == (minFract<shoulderHeight[2])
      
      # if depthTest false & difference Test FALSE: potential mudbank: skip loop
      # such that transects are not tested
      if(depthTest == F && differenceTest == F){ 
        
        next
      } 
      
      if(shoulderHeightTest == F){
        next
      }
  
      # 2) look wihtin a certain distance for other pos that have a similar mean mud value
      neighborhood <- subset(mudbanks_selection, pos > min(c(shoulder1, shoulder2)) &
                               pos < max(c(shoulder1, shoulder2)))
      
      # all positions in neighborhood that have a lower or simila fraction value
      # are excluded
      # in scenario's where the neighbourhood is wedged in between 2 mudbanks
      # this results in to many points being removed (e.g. the window is big)
      # But also because ALL point obsevations at the POS are exlcuded (and 
      # not only the ones that are below the fraction value)
      valleyVal <- unique(runnAve[runnAve$pos == unique(valley$pos),2])
      subset_indx <- subset(neighborhood,
                            SmoothedPeakFract < mean(valleyVal + 
                                                   sd(neighborhood$meanMud)))
      
      indx <- which(neighborhood$SmoothedPeakFract < 
                      mean(valleyVal + 
                             sd(neighborhood$meanMud)))
      
      # consider excluding all pos that are contained within the detected range of indx?
      # poses problems when in between two mudbanks.
      posRange <- seq(min(neighborhood$pos[indx]), max(neighborhood$pos[indx]),
                      1000)
      
      NoMudBankPos <- c(NoMudBankPos, 
                        posRange)
    }

  
  }
  
  #' 
  #' plot example
  #' 
  
  # plot(mudbanks_selection$pos, mudbanks_selection$SmoothedPeakFract)
  # points(mudbanks_selection$pos[which(mudbanks_selection$pos %in% NoMudBankPos)],
  #  mudbanks_selection$SmoothedPeakFract[which(mudbanks_selection$pos %in% NoMudBankPos)],
  #  col = 'red')
  # points(mudbanks_selection$pos, mudbanks_selection$meanMud, col = 'blue')
  # abline(v = localMinsPos, lty = 3)
  # points(runnAve$pos,runnAve$rolling, col = 'orange')

  # points that potentially correspond to interbank phases
  # localMin <- mudbanks_selection[which(mudbanks_selection$pos %in% localMinsPos),]
  # localMin_sp <- sp_pnt_ee(localMin$x,
  #                          localMin$y,
  #                          'localMin',
  #                          "red")
  
  # indices of detected positions that correspond to interbank phases.
  NoMudBankInd <- which(mudbanks_selection$pos %in% NoMudBankPos)
  
  # or based on localMin
  # noMudBank <- mudbanks_selection[NoMudBankInd, ] 
  # noMudBank_Sp <- sp_pnt_ee(noMudBank$x,
  #                           noMudBank$y,
  #                       'low super smoothed peak',
  #                       "#e0f3db")
  
  # first + non_outliers + coastline_selection + nonsense_sp + outliers_sp +
    # pos_slopes + noMudBank_Sp + localMin_sp

  indicesToDrop <- unique(c(indicesSlopes, NoMudBankInd))
  
  # Create unique combinations to look for in the original mudbanks:
  combinations <- rbind(combinations, data.frame(DATE_ACQUIRED = rep(i, length(indicesToDrop)),
                             pos = mudbanks_selection$pos[indicesToDrop],
                             dropClass = mudbanks_selection$dropClass[indicesToDrop]))
  
  rownr <- c() # row numbers in the mudbanks that correspond to the combinations
  for(r in seq(nrow(combinations))){
    
    # look up every combination
    rownr <- rbind(rownr, which(mudbanks3$DATE_ACQUIRED == combinations[r,1] &
                                  mudbanks3$pos == combinations[r,2] &
                                  mudbanks3$dropClass == combinations[r,3]))
  }
  
  # again update mudbank Selection by removing 
  mudbanks_selection <- mudbanks_selection[-indicesToDrop, ]
  indicesToDrop2 <- c(0) # for the final bit

  # apply outlier detection to remaining points 
  for (pnt in unique(mudbanks_selection$pos)){
    selected_point <- subset(mudbanks_selection, pos == pnt)

    # select nearby points
    ajoining_points <- subset(mudbanks_selection, 
                              as.character(pos) <=  as.numeric(as.character(selected_point$pos[1]))+4000 &
                              as.character(pos) >= as.numeric(as.character(selected_point$pos[1]))-4000 &
                              as.character(pos) != as.numeric(as.character(selected_point$pos[1])))

    combined <- rbind(selected_point,ajoining_points)
    
    # order by position
    combined <- combined[order(as.numeric(as.character(combined$pos))),]

    positions <- as.numeric(as.character(combined$pos))
    distances <-combined$mudbank_extent #axisDist # grap the (normalized???) distances
    fractions <- combined$mudFract
    dropClass <- combined$dropClass
    
    datatest <- data.frame(positions=positions,distances=distances, 
                           fractions = fractions, dropClass = dropClass,
                           x=combined$x, y = combined$y)

      # sufficient positions that contain information: determine outliers
    if (length(unique(datatest$positions)) > 2) {
      
      # # calculate linear fit
      lm.out_lin <- lm(datatest$distances~as.numeric(datatest$positions))
      
      # calculate second order polynomial 
      lm.out<-lm(datatest$distances ~ poly(as.numeric(datatest$positions),2))
      r2 <- summary(lm.out)$r.squared # or adjusted r2?
      p <- summary(lm.out)                # should be <0.05?
      fstat <- summary(lm.out)$fstatistic # 

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
    if (outlier$positions == unique(selected_point$pos)){
      # print(paste0('true for pos: ', unique(selected_point$pos)))
      
      # row numbers of the outliers in the original dataFrame
      rownr <- rbind(rownr, which(mudbanks3$DATE_ACQUIRED == i &
                                    mudbanks3$pos == outlier$positions &
                                    mudbanks3$dropClass == outlier$dropClass))
      
      # row number of the outliers in the subset used in this loop
      indicesToDrop2 <- rbind(indicesToDrop2, 
                              which(mudbanks_selection$DATE_ACQUIRED == i &
                                    mudbanks_selection$pos == outlier$positions &
                                    mudbanks_selection$dropClass == outlier$dropClass))
    }
  }
  
  # also here overwrite the amount of observations that are not an outlier
  mudbanks_selection <- mudbanks_selection[-indicesToDrop2, ]

  mudbanks3[unique(rownr[,1]), "mudbank_outlier"] <-
    as.data.frame(mudbanks3[unique(rownr[,1]),"mudbank_outlier"])[,1] + 1

}


# Count valid (non-outlier) observations 
mudbanks4 <- mudbanks3 %>% 
  dplyr::group_by(pos, year_col, validObs, mudbank_outlier) %>% 
  # count observations for validObs = 1 & NA  
  dplyr::mutate(validMudbankObs = n_distinct(DATE_ACQUIRED)) %>% # count unique observations
  # if nonsense observation or outlier, set to NA
  dplyr::mutate(validMudbankObs = ifelse(axisDist == -1 | mudbank_outlier > 0,  
                                     NA, validMudbankObs)) %>%

  dplyr::group_by(year_col, pos) %>%
  # replace NA with the count for valid obs
  dplyr::mutate(validMudbankObs = ifelse(is.na(validMudbankObs), 
                                    Mode(validMudbankObs), validMudbankObs)) %>%

  ungroup()



# adding a indication of noMudbank positions (so all observations on that POS recieve 1)
# e.g. when frequently identified as mudbank in a year: give 0
mudbanks5 <- mudbanks4 %>% # annual_obs %>%
  group_by(year_col, pos) %>%
  dplyr::mutate(
    noMudbank = case_when(
      validMudbankObs/mudbankObs > 0.66 ~ 0, 
      TRUE ~ 1)) %>% 
  ungroup()

all_years <- unique(mudbanks5$year_col)
group_pos <- unique(mudbanks5$pos)

mudbanks5$distX <- NA
mudbanks5$distY <- NA
mudbanks5$medianOffshore <- NA

# get median position for each year
# only for pos which are considered to have a mudbank!
# also exclude outliers and nonsense observations
for(y in 1:length(all_years)){
  # y <- 4
  # selected_year <- '2008-01-01'
  selected_year <- all_years[y]
  
  
  for (p in 1:length(group_pos)){
    # p = 92#113
    position = group_pos[p]
    # position = 32000
    
    subsets <- subset(mudbanks5, year_col == selected_year &
                        pos == position &
                        axisDist > 0 & # nonsens observations (or use mudbank extent?)
                        mudbank_outlier == 0 & # no outlier
                        noMudbank == 0) # positions that are indicated as mudbank
    
    if (nrow(subsets) > 1){
      
      # get the distance;
      # meanOffshore <- mean(subsets$axisDist, na.rm = T)
      medianOffshore <- median(subsets$axisDist, na.rm=T)
      
      bearing <- median(subsets$bearing)
      originX <- median(subsets$originX)
      originY <- median(subsets$originY)
      
      # set origin at correct location or add coastline dist to dist of interest?
      destPoint <- destPoint(SpatialPoints(data.frame(x = originX, y = originY),
                                           CRS("+proj=longlat +datum=WGS84")),
                             bearing, medianOffshore)
      
      # in the original file all rows recieve the distX
      rowNumbers <- which(mudbanks5$year_col == selected_year &
                            mudbanks5$pos == position)
      
      mudbanks5$distX[rowNumbers] <- destPoint[1]
      mudbanks5$distY[rowNumbers] <- destPoint[2]
      mudbanks5$medianOffshore[rowNumbers] <-  medianOffshore
    }
  }
}


# export 
for (year in unique(format(as.Date(uniqueDates), '%Y'))){
  # year <- 2000
  # print(year)
  start_year <- as.Date(ISOdate(year, 1, 1))
  end_year <- as.Date(ISOdate(year, 12, 31)) 
  
  mudbanks_per_year <-subset(mudbanks5,
                             as.Date(DATE_ACQUIRED) >= start_year &
                             as.Date(DATE_ACQUIRED) <= end_year)

  write_csv(mudbanks_per_year, paste0(wd,"/data/processed/offshore_points/", aoi,
                                      '_', year, '_offshore.csv'))
  
  print(paste0(wd,"/data/processed/offshore_points/", aoi,
               '_', year, '_offshore.csv'))
  
}

