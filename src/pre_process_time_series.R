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

# seq1 <- seq(1985, 1999, 1)
seq2 <- seq(2010,2020 , 1)
# seq3 <- seq(2002, 2002, 1)

years <- c(seq2)# seq(from = 1985, to = 2020, by = 1)

# pos to exlcude for mudbank boundary estimates / outlier detection
posToExclude <- c(seq(138000,147000,1000),
                  seq(241000, 255000, 1000))  

min_Std <- 25   # minimal amount of meters difference before considered outlier
year_limit <- 4 # search window in years for finding coastline obs when insufficient values per year.
min_obs_rosner <- 10    # Amount of obs needed to perform statistical test

exportCoasts <- FALSE

mapviewOptions(basemaps = c( "Esri.WorldImagery","Esri.WorldShadedRelief", "OpenStreetMap.DE"))
leaflet() %>%
  addProviderTiles("Esri.WorldImagery")

dataFolder <- './data/raw'


# select folders
folderSelect <- as.matrix(list.files(paste0(dataFolder, '/GEE_exports'), full.names = T))
df <- rewrite(folderSelect);
# only csv's
df <- df[grep('.csv', folderSelect, ignore.case = T),]
aoi <-  c('Suriname') # Suriname / Braamspunt / WegNaarZee
path_rows <- c('229_56') # '228_56','230_56'

filtered <- vector('list', 100)
for (q in seq_along(years)) {
  for (x in seq_along(aoi)){
    for (pr in path_rows){ 
      # q <- 8
      year = as.character(years[q])
      region = aoi[x]
      
      
      filters = c(year, region, pr)
      
      filtered = rbind(filtered, df %>% 
                         dplyr::filter(
                           filters %>%
                             # apply the filter of all the text rows for each pattern
                             # you'll get one list of logical by pattern ignored_string
                             purrr::map(~ to_keep(.x, text = text)) %>%
                             # get a logical vector of rows to keep
                             purrr::pmap_lgl(all)
                         ))}
}}
filtered <- unique(filtered)
allFiles <- do.call(bind_rows, lapply(as.matrix(filtered)[,1], function(x) read.csv(x, stringsAsFactors = FALSE,
                                                                                sep = ',', na.strings=c("","NA")
                                                                                )))

# all dates
uniqueDates <- unique(allFiles[,col_of_interest(allFiles, 'DATE_ACQUIRED$')]);
drop <- c('system.index', '.geo')
keep_columns <- colnames(allFiles)[!(colnames(allFiles) %in% drop)]

# some years haven't been run with correct GEE scripts; see which dates
# testNA <- unique(allFiles$DATE_ACQUIRED[is.na(allFiles$axisDistAbs)])
# testNA2 <- allFiles[is.na(allFiles$axisDistAbs), ]

# prep input to sf data.frame class
coastlines <- reshape_csvPoints(allFiles, 'coastX', 'coastY', keep_columns)
# coastX / coastY
# change all -1 to NA
# these are the transect that resulted in no coastline estimate
coastlines$coastDist[coastlines$coastX == -1] <- NA

# sort al rows based on position & date
coastlines<-coastlines[with(coastlines, order(pos, DATE_ACQUIRED)), ]

# make groups per year, 3 months and 3 years per transect
coastlines <- coastlines %>%
  dplyr::mutate(quarterly_col = as.Date(cut(lubridate::date(coastlines$DATE_ACQUIRED),
                                     "3 month"))) %>%
  dplyr::mutate(date_col = as.Date(cut(lubridate::date(coastlines$DATE_ACQUIRED), 
                                "3 year"))) %>%
  dplyr::mutate(five_year_col = as.Date(cut(lubridate::date(coastlines$DATE_ACQUIRED), 
                                "5 year"))) %>%
  dplyr::mutate(year_col = as.Date(cut(lubridate::date(coastlines$DATE_ACQUIRED),
                                "1 year"))) 

group_dates<-unique(coastlines$year_col)        # yearly
group_pos <- unique(coastlines$pos)             # All unique positions (transect number)
group_years <- unique(coastlines$date_col)      # per 3 year
five_years <- unique(coastlines$five_year_col)

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

# for testing / visualization define an imageCollection
collectionL4 <- ee$ImageCollection("LANDSAT/LT04/C01/T1_TOA")$
  filterBounds(pol)

collectionL5 <- ee$ImageCollection("LANDSAT/LT05/C01/T1_TOA")$
  filterBounds(pol)

collectionL7 <- ee$ImageCollection("LANDSAT/LE07/C01/T1_TOA")$
  filterBounds(pol)

collectionL8 <- ee$ImageCollection("LANDSAT/LC08/C01/T1_TOA")$
  filterBounds(pol)

collection <- collectionL8$merge(collectionL5)$merge(collectionL7)$
  merge(collectionL4)

vizParams = list(
  bands = c("B5", "B4", "B3"),
  min = 0.05, max = 0.5, gamma = 1.4
)

#'
#'  estimate coastal outliers with rosner test
#'  - for each transect per 3 years to ensure sufficient observations
#'  With insufficient observations, look within search window for additional obs.
#'  
#'  - Still needs an improvement on the outlier detection with Rosner test. 
#'  Because the distribution is not normal Rosner might not be applicable for outlier detection.
#'  Also the K value (potential nr. of outliers) is not yet optimal implemented, 
#'  ideally you'd want to apply it to consistent group sizes (15 - 25) where the K
#'  is somehow determined on the distribution (and not the amount of observations)
#'  
#'  

# assume nothing is outlier and set outputs to NA
coastlines$coast_outlier <- 1
coastlines$slope         <- NA
coastlines$coastObs      <- NA

for(i in group_years){ # group_years / 
  # i<-group_years[group_years == c("2019-01-01")]
  
  for(q in group_pos){
    start <- Sys.time()
    # q <- group_pos[group_pos == 230000]

    indexs <- which(coastlines$date_col == i &  # five_year_col
                      coastlines$pos == q &
                      coastlines$coastX != -1)
    
    subsets3 <- coastlines[indexs, ]
    # get nearest observation and add it to the list
    # is .na going to throw a problem?
    reference_date <- mean(as.Date(subsets3$DATE_ACQUIRED))
    
    # Aamount of obs
    # obs_3years <- nrow(subsets3)

    # update that if subset per year is to small, include extra observations?
    # So grow the subset to at least 10(?) obs by adding nearest observations
    maxAttemp <- 0 # make sure you don't get stuck in infinite loop..
    while(nrow(subsets3) < min_obs_rosner & 
          # nrow(subsets3)+obs_3years > min_obs_rosner &
          maxAttemp < min_obs_rosner+5){
      

      # exclude dates from the year of interest
      # sample from entire dataset so that years outside the 3 year block
      # are also possible candidates
      # pitfall: any date is possible, also the ones to far away
      # so set limit at search window of x years?

      selectedDates <- subset(coastlines, coastlines$pos == q &
                                coastlines$coastX != -1 & 
                              !(coastlines$DATE_ACQUIRED %in% 
                                  subsets3$DATE_ACQUIRED)
                            )$DATE_ACQUIRED
      
      
      # exclude the ones already selected
      # are NA a problem that seems to throw warnings()?
      nearestDate <- selectedDates[1:length(selectedDates) == 
                       which.min(replace(abs(as.Date(selectedDates) - reference_date), 
                                         abs(as.Date(selectedDates) - reference_date)>year_limit*356, NA))]
      
      # if nothing is found, break the loop
      if(length(nearestDate) == 0){break}
      
      index_nearest <- which(as.Date(as.character(coastlines$DATE_ACQUIRED)) == nearestDate & 
                               coastlines$pos == q &
                           coastlines$coastX != -1)
      
      # update subsets 
      subsets3 <- rbind(subsets3, coastlines[index_nearest, ] )
      
      maxAttemp = maxAttemp + 1
    }
      
    # because subsets 3 changed in size, recalc the indices.
    subsets3_recal <- which(coastlines$DATE_ACQUIRED %in% subsets3$DATE_ACQUIRED &
                              coastlines$pos == q &
                              coastlines$coastX != -1)
      
    # apply rosner test if there is sufficient observations ==> this implies that the timeseries to look at needs to be larger than 3 years.
    # also the year limit needs to go up.
    # All detected outliers with larger std value recieve outlier == 0
    # alternatively you could iterate over x amount of observations. e.g. every 15 observations, do a outlier test
      
    # Only give the rosner output to the original subset3 indices
    # if there is >15 observations let K be estimated
    coastlines[indexs, 'coast_outlier'] <- 
        rosner(subsets3$coastDist, min_Std, 15)[which(subsets3_recal %in% indexs)]
    # Will throw an error/warning if all values are the same => nothing is assigned as outlier
      
    # plot(as.Date(subsets3$DATE_ACQUIRED), subsets3$coastDist,
    #     main = paste0(q), xlab = 'date', ylab = 'coastline position [m]')
    # points(as.Date(subsets3$DATE_ACQUIRED)[which(rosner(subsets3$coastDist, min_Std, min_obs_rosner) == 0)],
    #        subsets3$coastDist[which(rosner(subsets3$coastDist, min_Std, min_obs_rosner) == 0)],
    #        col = 'red')
    # points(as.Date(subsets3$DATE_ACQUIRED)[which(subsets3$coast_outlier == 0)],
    #        subsets3$coastDist[which(subsets3$coast_outlier == 0)],
    #        col = 'red')

    end <- Sys.time()
    dif<- difftime(end, start, "mins")
    
    
    
    if(as.numeric(dif, units="secs") > 5){
      print(paste0(as.Date(i), ' for pos: ', q,' in ', round(dif,1), ' in ', units(dif)))
    }
    
    
 
  }
  suppressWarnings(remove(subsets3, indexs, reference_date,
                          selectedDates, nearestDate, index_nearest,
                          subsets3_recal))

}



#'
#'  calculate slope of coastline change
#'  - Per year
#'  - include nearest observations
#'  - 
#'  within predefined search window (4 years difference max)
#'  
for(i in group_dates){
  start <- Sys.time()
  for(q in group_pos){
    # i<-group_dates[group_dates == c("1991-01-01")]
    # 
    # q <- group_pos[group_pos == 4000]
    # print(q)
    
    subsets_annual <- coastlines[which(coastlines$year_col == i & 
                                         coastlines$pos == q &
                                         coastlines$coastDist > -1), ]
    
    # select outliers & non outliers
    outliers <- subset(subsets_annual, coast_outlier == 0)
    nonOutliers <- subset(subsets_annual, coast_outlier == 1)
    
    # get nearest observation and add it to the list
    # if there is nothing there; half way through the year
    reference_date <- as.Date(ifelse(nrow(nonOutliers)>0, 
                             mean(as.Date(nonOutliers$DATE_ACQUIRED)), 
                             as.Date(i)+days(180)))
    
    # improvement required! 
    # slope is only applicable if quality of coastline estimate is 
    # sufficient (see landsat 7 case with no data in wetland areas)
    # if outlier detection is sufficient the slope can be correctly estimated
    
    
    # if  there is insufficient observations;
    # search for additional observations to calculate the slope
    # downside; if the observations that are detected are of good quality,
    # the slope is wronfully adjusted based on neighbouring observations
    # resulting in wrong estimate (often underestimate!) of rate of change.

    maxAttemp <- 0
    while(nrow(nonOutliers) < 8 & 
          maxAttemp < min_obs_rosner){

      # exclude the ones from the original year
      selectDates <- subset(coastlines, coastlines$pos == q &
                              coastlines$coastDist > -1 & !(coastlines$DATE_ACQUIRED %in% 
                                                            nonOutliers$DATE_ACQUIRED))$DATE_ACQUIRED
      
      # get nearest date (excluding dates outside limit)
      nearestDate <- selectDates[1:length(selectDates) == 
                                   which.min(replace(abs(as.Date(selectDates) - reference_date), 
                                                     abs(as.Date(selectDates) - reference_date)>year_limit*356, NA))]
      
      if(length(nearestDate) == 0){break}
      index_nearest <- which(as.Date(as.character(coastlines$DATE_ACQUIRED)) == nearestDate & 
                               coastlines$pos == q &
                               coastlines$coastDist > -1 &
                               coastlines$coast_outlier == 1)
      
      # update annual subset
      nonOutliers <- rbind(nonOutliers, coastlines[index_nearest, ] )
      
      maxAttemp = maxAttemp + 1
      
      
    }
    
    # only fit a line if there is a least 5 valid observations.
    if(nrow(nonOutliers) < 5){
      
      m_per_year <- NA
    } else {
      
      # calculate linear fit
      lm.out <- lm(nonOutliers$coastDist~as.numeric(as.Date(nonOutliers$DATE_ACQUIRED)))
      
      # intercept <-lm.out$coefficients[1]
      slope <- round(lm.out$coefficients[2], 5) # change per unit of x (=days)
      
      m_per_year <- slope*365
      
    }
    
    # update all slope values from the original years (incl. coastdist -1 and outliers) 
    coastlines$slope[which(coastlines$year_col == i & 
                             coastlines$pos == q)] <- as.numeric(m_per_year)
    coastlines$coastObs[which(coastlines$year_col == i & 
                                coastlines$pos == q)] <- as.numeric(nrow(nonOutliers))
    
    
  }
  
  # remove temp variables
  suppressWarnings(remove(subsets_annual, outliers,nonOutliers, reference_date,
         maxAttemp, lm.out, slope, m_per_year))
  
  end <- Sys.time()
  dif<- difftime(end, start, "mins")
  print(paste0(as.Date(i) ,' in ', round(dif,1), ' in ', units(dif)))

  # plot(as.Date(nonOutliers$DATE_ACQUIRED), nonOutliers$coastDist,
  #   main = paste0(q), xlab = paste0(i), ylab = 'coastline position',
  #   ylim=c(min(nonOutliers$coastDist)-30,max(nonOutliers$coastDist)+30),
  #   xlim=c(min(as.Date(nonOutliers$DATE_ACQUIRED)),
  #         max(as.Date(nonOutliers$DATE_ACQUIRED))))
  # points(as.Date(outliers$DATE_ACQUIRED), outliers$coastDist, col = 'red')
  # abline(lm(nonOutliers$coastDist~as.numeric(as.Date(nonOutliers$DATE_ACQUIRED))),lty = 2)
  # text(min(as.Date(subsets_annual$DATE_ACQUIRED)) + 90,
  #      max(subsets_annual$coastDist) + 25, paste0('slope = ', m_per_year, ' meter'))
}


#'
#'  Last observation carried forward
#'  - for observations that are outlier / NA
#'  

# set initial value
coastlines$locf <- coastlines$coastDist

# if coastDist NA or coast observation is an outlier; 
indices <- unique(which(is.na(coastlines$coastDist) | coastlines$coast_outlier == 0))

for(ind in indices){
  # ind<-indices[2]
  
  data_entry <- coastlines[ind, ]
  
  # select all accepted observations from the same transect
  pos_subset <- subset(coastlines, coastlines$pos == data_entry$pos &
                         coastlines$coast_outlier == 1 &
                         !is.na(coastlines$coastDist)) 
  
  # plot(pos_subset$DATE_ACQUIRED, pos_subset$locf)
  # plot(pos_subset$DATE_ACQUIRED, pos_subset$coastDist, col = 'red')
  
  if(nrow(pos_subset) > 0){ # if no observations; locf remains original observation (=NA)
    
    # calculate date difference
    dateDiff <- abs(as.Date(pos_subset$DATE_ACQUIRED) - as.Date(data_entry$DATE_ACQUIRED))
    
    # find nearest date
    dateDiff[dateDiff==0] <- max(dateDiff) # exclude the date of interest (dateDiff = 0)
    minInd <- which.min(dateDiff)
    nearest <- pos_subset[minInd,]
    
    # fill locf in original data frame
    # 
    # coastlines %>%
    #   dplyr::mutate(locf = if_else(DATE_ACQUIRED == data_entry$DATE_ACQUIRED &
    #                        pos == data_entry$pos, nearest$coastDist, -1 ))
    coastlines[ind, 'locf'] <- nearest$coastDist
      # nearest$coastDist 
  }

}


#' 
#' Coastline
#' - median distance from transect origin per year 
#' - 
#' 

# order by pos
coastlines2<-coastlines[order(coastlines$pos),]

# calculate median coastal position
# grouped by pos, year and outlier
coastlines2 <- coastlines %>% 
  dplyr::group_by(pos, year_col, coast_outlier) %>%
  dplyr::mutate(coast_median = median(coastDist, na.rm = T)) %>%
  ungroup()

# set outlier groups to NA
# this is now prefered over filling NA with nearest values. 
coastlines2$coast_median[coastlines2$coast_outlier == 0] <- NA

# key to indicate groups of years~pos
coastlines2$key <- with(rle(as.numeric(coastlines2$year_col)), 
                     rep(seq_along(lengths), 
                         lengths))

# # fill outliers (NA) with median coastal observation of that year
coastlines2 <- coastlines2 %>%
  dplyr::group_by(key) %>% # group by position & year
  dplyr::mutate(coast_median = Mode(coast_median)) %>%
  ungroup() # remove group

# only now there remain some groups with median of NA (because there was only 0 or 1 observations in that group)
# which is a problem for the next step
# so fill NA with Mode of LOCF columns
coastlines2 <- coastlines2 %>%
  dplyr::group_by(key) %>%
  dplyr::mutate(coast_median = ifelse(is.na(coast_median), locf, coast_median)) %>% 
  ungroup() 

#'
#'  calculate for each pos, each year gain/loss compared to previous year
#'  
coastlines3 <- coastlines2 %>% 
  dplyr::group_by(pos) %>%           # group_by performs calculation per group

  # calculate for each position the difference with previous
  dplyr::mutate(deltaCoast = coast_median - lag(coast_median)) %>%
  dplyr::mutate(deltaCoast = ifelse(is.na(deltaCoast), 0, deltaCoast)) %>%        # NA corresponds to first obs at each transect, set it to 0

  # make sure within each group the difference are all assigned the same value (max)
  dplyr::group_by(key) %>%
  dplyr::mutate(deltaCoast = ifelse(sign(deltaCoast[which.max(abs(deltaCoast))]) == 1, # if positive
                           max((deltaCoast), na.rm = F), # fill with max 
                           min((deltaCoast), na.rm = F))) %>% # else fill with min (to ensure negative value remains)
  ungroup()


if(exportCoasts){
  
  for (year in unique(format(as.Date(uniqueDates), '%Y'))){
    # year <- 1997
    # print(year)
    start_year <- as.Date(ISOdate(year, 1, 1))
    end_year <- as.Date(ISOdate(year, 12, 31)) 
    
    coastlines_per_year <-subset(coastlines3,
                               as.Date(DATE_ACQUIRED) >= start_year &
                                 as.Date(DATE_ACQUIRED) <= end_year)
    
    coastlines_per_year <- coastlines_per_year %>%
      dplyr::select(!c(x,y)) # drop columns that only deal with mudbank data
    
    write_csv(coastlines_per_year, paste0(wd,"/data/processed/coastlines/", aoi,
                                        '_', year, '_coastlines.csv'))
    print( paste0(wd,"/data/processed/coastlines/", aoi,
                  '_', year, '_coastlines.csv'))
    remove(coastlines_per_year)
  }
  
  
  
}

# 
# # test simple 2d plot
plot(coastlines3$pos[which(coastlines3$coast_outlier != 0)], 
     coastlines3$deltaCoast[which(coastlines3$coast_outlier != 0)])
identify(coastlines3$pos, coastlines3$deltaCoast, n=1, labels=coastlines3$pos)

twoD_pos <- 38000 

subset2d_for_testPlot <- subset(coastlines3, pos == twoD_pos)

plot(as.Date(subset2d_for_testPlot$DATE_ACQUIRED), subset2d_for_testPlot$coastDist,
     xlab="DATE_ACQUIRED", ylab="coastDist [m]",
     main = paste0('coastline position: ',twoD_pos, ' [m]'), pch = 20)
lines(unique(as.Date(subset2d_for_testPlot$year_col))+180, 
      aggregate(subset2d_for_testPlot$coast_median, list(subset2d_for_testPlot$key), median)$x, 
      col = 'black', lty = 2)
points(as.Date(subset2d_for_testPlot[subset2d_for_testPlot$coast_outlier == 0, ]$DATE_ACQUIRED),
       subset2d_for_testPlot[subset2d_for_testPlot$coast_outlier == 0, ]$coastDist,
       col = 'red',  pch = 20)
points(as.Date(subset2d_for_testPlot[subset2d_for_testPlot$coastX == -1, ]$DATE_ACQUIRED), 
       subset2d_for_testPlot[subset2d_for_testPlot$coastX == -1, ]$locf, col = 'blue')
legend("right", legend=c("Observations", "median values", 'outliers', 'locf'),
       col=c("black", "black", 'red', 'blue'), pch = c(20,NA,20,20) ,lty = c(0,2,0,0), cex=0.8)

coast_spatial <- sp_pnt_ee(subset2d_for_testPlot$coastX,
                              subset2d_for_testPlot$coastY, paste0('pos: ',twoD_pos),
                              "#d95f0e")
 

pnt <- ee$Geometry$Point(c(median(subset2d_for_testPlot$originX), median(subset2d_for_testPlot$originY)))

filtCollect <- collection$filterBounds(pnt)$
  filterDate(as.character(as.Date(min(subset2d_for_testPlot$DATE_ACQUIRED))-1), 
             as.character(as.Date(max(subset2d_for_testPlot$DATE_ACQUIRED))+1))$
  sort("CLOUDCOVER", TRUE)
dates <- ee_get_date_ic(filtCollect, time_end = FALSE)

acquisition <- ee_get_date_img(filtCollect$first())$time_start

Map$centerObject(filtCollect$first())
first <- Map$addLayer(filtCollect$first(), vizParams, paste0('landsat: ', format(as.Date(acquisition), '%Y-%m-%d')))

first + coast_spatial

################################################################################
#' filter mudbank boundary outliers
#' 
#' - Filter on neighborhood (previous 2, current and next two points)
#'     Use distance to see if the point is deviating
#'     Consider using fraction..
#'     
#'     good example dates: 2017-09-02, "2018-02-27", 2018-08-28, 2018-09-27
################################################################################

mudbanks <- coastlines3

# mudbank Distance
# be carefull with -1 values...
# this is the extent off the mudbank, carefull not to plot it with the coastline position
mudbanks$mudbank_extent <- mudbanks$axisDist - mudbanks$coast_median  

# mudbank Distance Abs Drop
mudbanks$mudbank_extent_abs <- mudbanks$axisDistAbs - mudbanks$coast_median  

# mudbank distance slope drop
mudbanks$mudbank_extent_slope <- mudbanks$axisDistSlope - mudbanks$coast_median  

# set outlier: assume nothing is an outlier
mudbanks$mudbank_outlier <- 0

# translate other distances into coordinates
mudbanks <- get_dists2(mudbanks, mudbanks$originX, mudbanks$originY, 
                                  mudbanks$bearing, 
                                  c('axisDistAbs', 'axisDistSlope', 'maxExtent'))

# calculate slope of the super smoothed line
# indication of no_mudbank scenario per transect
mudbanks$SmoothedSlopes <- (mudbanks$SmoothedPeakFract - mudbanks$maxExtentIndex) / 
  (mudbanks$SmoothedPeak -  mudbanks$maxExtent) * 10000

# Replace Inf with NA
is.na(mudbanks$SmoothedSlopes) <- do.call(cbind,lapply(mudbanks$SmoothedSlopes, is.infinite))

# reshape mudbanks such that each relative, absolute and slope drop gets it own data-entry for each pos
# so triplicate each row and overwrite the values in the corresponding columns
# transform such that for each pos all three coordinates become a separate entry with unique x,y coords
mudbanks <- mudbanks %>%               # new feature or overwrite mudbanks?
  slice(rep(1:n(), each = 3)) %>%     # triplicate each row
  dplyr::group_by(pos, DATE_ACQUIRED) %>%
  dplyr::mutate(dropClass = c("rel", "abs", 'slope')) %>% # assign a column indicating what the x,y coords should represent
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
  ungroup()
  # dplyr::select(-ends_with('.1')) # in case the create x,y coordinates were created doubble


for (i in uniqueDates){
  # i <- uniqueDates[15]
  print(i)
  # Build image Collection around selected image  
  filtCollect <- collection$filterDate(as.character(as.Date(i)-1), 
                                       as.character(as.Date(i)+1))
  dates <- ee_get_date_ic(filtCollect, time_end = FALSE)
  
  # plot image
  Map$centerObject(filtCollect$first())
  first <- Map$addLayer(filtCollect$first(), vizParams, paste0('landsat: ',i))

  # select all observations of the corresponding date
  mudbanks_selection <-subset(mudbanks, mudbanks$DATE_ACQUIRED == i & 
                                !(pos %in% posToExclude)) 
  
    # give all excluded (-1) observation an 1 for outlier in original
  # to keep track of all outliers.
  # excluded <- which(mudbanks$DATE_ACQUIRED == i & 
  #                     mudbanks$axisDist < 0 &
  #                     mudbanks$mudbank_extent < 0)
  # 
  # mudbanks[excluded, "mudbank_outlier"] <-
  #   as.data.frame(mudbanks[
  #     excluded,"mudbank_outlier"])[,1] + 1
  
  # order by position
  mudbanks_selection<-mudbanks_selection[order(mudbanks_selection$pos),]
  
  # create spatial points
  non_outliers <- sp_pnt_ee(mudbanks_selection$x, # &  mudbanks_selection$SmoothedPeak > 0
            mudbanks_selection$y,
            'all observations',
            "#d95f0e")
  coastline_selection <- sp_pnt_ee(mudbanks_selection$coastX, # &  mudbanks_selection$SmoothedPeak > 0
                            mudbanks_selection$coastY,
                            'coastline',
                            "#31a354")
  
  #'
  #' test for obvious outliers:
  #' - mudbank extent cannot be smaller than coastline position
  #' - mudbank extent cannot be -1
  #' - mudbank extent subjected to outlier test
  #' 
  
  # points with smaller mudbank boundary then coastline (false detection)
  # OR observations with mudbankextent of -1
  nonsense <-  which(mudbanks_selection$coastDist > mudbanks_selection$axisDist |
                       mudbanks_selection$axisDist == -1)
  
  nonsense_sp <- sp_pnt_ee(mudbanks_selection$x[nonsense], 
                           mudbanks_selection$y[nonsense],
                           'nonsense outliers',
                           "#fa9fb5")
  
  # keep track of entries that need to be tracked as outlier
  combinations <- data.frame(DATE_ACQUIRED = rep(i, length(nonsense)),
                             pos = mudbanks_selection$pos[nonsense],
                             dropClass = mudbanks_selection$dropClass[nonsense])
  
  # update mudbanks_selection
  mudbanks_selection <- mudbanks_selection[-nonsense, ]
  
  positions_all <- as.numeric(as.character(mudbanks_selection$pos))
  distances_all <- mudbanks_selection$mudbank_extent # grab the normalized distances
  
  if(length(distances_all) == 0){break}
  
  # second order polynomial fit: alongshore position & polynomial fit
  lm_out_all <-lm(distances_all ~ poly(as.numeric(positions_all),2))
  
  predicted.intervals <- predict(lm_out_all,
                                 data.frame(x=as.numeric(positions_all)),
                                 interval='confidence', level=0.99)
  
  # Bonferroni-adjusted mudbank_outlier test (test largest absolute standardized residual)
  outlier_test <- car::outlierTest(lm_out_all) 
  
  # combine all outliers
  outlier_ind<- c(as.numeric(names(outlier_test$rstudent)))
  
  outliers_sp <- sp_pnt_ee(mudbanks_selection$x[outlier_ind], 
                           mudbanks_selection$y[outlier_ind],
                           'obvious outliers',
                           "#fa9fb5")
  
  # keep track of entries that need to be tracked as outlier
  combinations <- rbind(combinations, data.frame(DATE_ACQUIRED = rep(i, length(outlier_ind)),
                             pos = mudbanks_selection$pos[outlier_ind],
                             dropClass = mudbanks_selection$dropClass[outlier_ind]))

  
  #' 
  #' plotting!
  #' 

  # plot(mudbanks_selection$pos, mudbanks_selection$axisDist,
  #        col = 'orange', main = paste0(i),
  #      xlab = 'alongshore position', ylab = 'mudbank extent')
  # points(mudbanks_selection$pos, mudbanks_selection$coast_median,
  #        col = 'blue')
  # points(mudbanks_selection$pos[outlier_ind],
  #        mudbanks_selection$axisDist[outlier_ind],
  #        col = 'red')
  # lines(positions_all, predicted.intervals[,1],col='green',lwd=3)
  
  # drop these most obvious outliers from the selection
  # ==> from here on not every observations has 3 data entries anymore!!!!!
  mudbanks_selection <- mudbanks_selection[-outlier_ind, ]
  

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

  # hist(slopes,  xlim=c(floor(min(slopes)), ceiling(max(slopes))),
  #   breaks = c(seq(floor(min(slopes)),ceiling(max(slopes)), 0.01)))

  # calculate median and sd of negative slopes only
  median_slope <- median(slopes[slopes < 0], na.rm = T)
  adjusted_sd <- sd(slopes[slopes < 0], na.rm = T)
  
  # drop observations with 'significant'/positive slopes 
  # Value should be in the range of -0.15 and 0 ==> not sure yet if using median is the best way
  indicesSlopes <- which(slopes > (median_slope + adjusted_sd))
  
  pos_slopes <- sp_pnt_ee(mudbanks_selection[indicesSlopes, ]$x, # &  mudbanks_selection$SmoothedPeak > 0
                          mudbanks_selection[indicesSlopes, ]$y, 
                          'positive_slopes',
                          "#3182bd")
  

  #' 
  #' Very similar to the slope, also the Super Smoothed peak fraction could 
  #' indicate no-mudbank cases: very low values can be a sign of NO mudbank
  #' 
# 
#   hist(mudbanks_selection$SmoothedPeakFract,
#      breaks =c(seq(min( mudbanks_selection$SmoothedPeakFract),
#                    ceiling(max(mudbanks_selection$SmoothedPeakFract))+0.1,0.01)))
#   
  # fraction needs to be large enough
  threshold <- median(mudbanks_selection$SmoothedPeakFract[which(mudbanks_selection$SmoothedPeakFract > 0)], na.rm = T) - sd(mudbanks_selection$SmoothedPeakFract[which(mudbanks_selection$SmoothedPeakFract > 0)])
  
  # highPeakDist <- mudbanks_selection[which(mudbanks_selection$SmoothedPeak > 7500), ]
  lowPeakOutlier <-  which(mudbanks_selection$SmoothedPeakFract < threshold)
  noMudBankTest <- mudbanks_selection[lowPeakOutlier, ] 

  noBankSp <- sp_pnt_ee(noMudBankTest$x,
                        noMudBankTest$y, 
                        'low super smoothed peak',
                        "#e0f3db")

  ########
  #' after defining no mudbank transects, try to determine applicability of:
  #' largest relative boundary (dist / fraction) 
  #' largest absolute drop boundary
  #' largest slope drop boundary
  #' 
  #' 

  # # fraction of detected offshore boundary
  # hist(mudbanks_selection$mudFract[which(mudbanks_selection$mudFract > 0)],
  #      xlim=c(0,1), breaks =c(seq(0,1,0.05)))
  # 
  # OffshoreFract <- median(mudbanks_selection$mudFract[
  #   which(mudbanks_selection$mudFract > 0)], na.rm=T)
  # OffshoreFract2 <- sd(mudbanks_selection$mudFract[
  #   which(mudbanks_selection$mudFract > 0)], na.rm=T)
  # 
  # lowPeakOutlier <-  which(mudbanks_selection$mudFract < 0.1)
  # highPeakOutlier <- which(mudbanks_selection$mudFract > 0.6)
  # 
  # SpatialPointslow <- sp_pnt_ee(mudbanks_selection$x[lowPeakOutlier],
  #                               mudbanks_selection$y[lowPeakOutlier], 
  #                               'low',
  #                               "#af8dc3")
  # SpatialPointsHigh <- sp_pnt_ee(mudbanks_selection$x[highPeakOutlier],
  #                               mudbanks_selection$y[highPeakOutlier], 
  #                               'high',
  #                               "#7fbf7b")
  # 
  # first + SpatialPointslow + SpatialPointsHigh
  

  # contrasting fractions for abs drop / re drop
  # contrasting distances for abs drop / rel drop
  # # shows nicely which RELATIVE distances are wrong! (if tresholds can be finetuned)
  # contrastingFractAbs <- mudbanks_selection[which(mudbanks_selection$mudFract[mudbanks_selection$dropClass == 'abs'] > 0.4 &
  #                                                 mudbanks_selection$mudFract[mudbanks_selection$dropClass == 'rel'] < 0.6 &
  #                                                 mudbanks_selection$mudFract[mudbanks_selection$dropClass == 'abs'] != 
  #                                                   mudbanks_selection$mudFract[mudbanks_selection$dropClass == 'rel']), ]
  # 
  # plot(mudbanks_selection$mudFract[mudbanks_selection$dropClass == 'rel'],
  #      mudbanks_selection$mudFract[mudbanks_selection$dropClass == 'abs'])
  # points(contrastingFractAbs$mudFract[mudbanks_selection$dropClass == 'rel'],
  #        contrastingFractAbs$mudFract[mudbanks_selection$dropClass == 'abs'],
  #        col = 'red')

  indicesToDrop <- unique(c(indicesSlopes, lowPeakOutlier))
  
  # combinations to look for in the original mudbanks:
  combinations <- rbind(combinations, data.frame(DATE_ACQUIRED = rep(i, length(indicesToDrop)),
                             pos = mudbanks_selection$pos[indicesToDrop],
                             dropClass = mudbanks_selection$dropClass[indicesToDrop]))
  
  rownr <- c() # row numbers in the mudbanks that correspond to the combinations
  for(r in seq(nrow(combinations))){
    
    # look up every combination
    rownr <- rbind(rownr, which(mudbanks$DATE_ACQUIRED == combinations[r,1] &
                                  mudbanks$pos == combinations[r,2] &
                                  mudbanks$dropClass == combinations[r,3]))
  }
  
  # again update mudbank Selection
  mudbanks_selection <- mudbanks_selection[-indicesToDrop, ]

  for (pnt in unique(mudbanks_selection$pos)){
    # pnt <- unique(mudbanks_selection$pos)[2]
    
    selected_point <- subset(mudbanks_selection, pos == pnt)
    
# 
#     plot(selected_point$axisDist,
#          selected_point$mudFract,
#          main = paste0(unique(selected_point$pos)), xlab = 'distance', ylab = 'fraction',
#          ylim=c(0, 1),
#          xlim=c(0,max(selected_point$maxExtent) + 1000))
# 
#     points(unique(selected_point$SmoothedPeak), unique(selected_point$SmoothedPeakFract), col = 'green' )
#     points(unique(selected_point$maxExtent), unique(selected_point$maxExtentIndex), col = 'red')
#     abline(unique(selected_point$meanMud), 0)
#     abline(v=unique(as.numeric(selected_point$coastDist)))

    # temp <- subset(mudbanks_selection, )
    
    # row.names(mudbanks_selection[pnt,])
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
    
    # datatest_sp <- sp_pnt_ee(datatest$x,
    #                          datatest$y,
    #                          'datatest_subset',
    #                          "#e0f3db")
    # # first + datatest_sp

      # sufficient positions that contain information: determine outliers
    if (length(unique(datatest$positions)) > 2) {
      
      # # calculate linear fit
      lm.out_lin <- lm(datatest$distances~as.numeric(datatest$positions))
      
      # calculate second order polynomial 
      lm.out<-lm(datatest$distances ~ poly(as.numeric(datatest$positions),2))
      r2 <- summary(lm.out)$r.squared # or adjusted r2?
      p <- summary(lm.out)                # should be <0.05?
      fstat <- summary(lm.out)$fstatistic # 

      # pred.int <- predict(lm.out,data.frame(x=positions),
      #                     interval='confidence',level=0.99)
      # plot
      # plot(positions, distances)
      # lines(positions, pred.int[,1],col='green',lwd=3)

      # slope <- lm.out$coefficients[2]
      # potential benefit: slope says something about direction; negative slope; front of mudbank
      # low pos > further east, decreasing mudbank_extent ==> front of mud bank. 
      
      # residuals
      # resid <- lm.out$residuals
      # maxResid <- which.max(abs(resid))
      
      # Bonferroni-adjusted outlier test (test largest absolute standardized residual)
      # test if no error/warning
      test2 <- has_error(car::outlierTest(lm.out), silent = !interactive())
      test3 <- has_warning(car::outlierTest(lm.out))
      
      # Allways returns a observation (with largest Studentized residual)
      # Which implies it is not allways meeting the P < 0.05 cutoff value
      outlier_test <- if(test2|test3){car::outlierTest(lm.out_lin)} else{car::outlierTest(lm.out)}
      
      # if no outliers detected:
      # point with a Pos outside the range is selected
      # outlier<- data.frame(ifelse(length(outlier_test$rstudent) > 0,
      #                  datatest[as.numeric(names(outlier_test$rstudent)),],
      #                  data.frame(positions= -1, distances= -1, fractions = -1)))
      outlier <- datatest[as.numeric(names(outlier_test$rstudent)),]
      

    } else {
      # if only 0 or 1 observation; cannot determine if outlier based on neighbours
      # to continue workflow return an index outside the selected range
      # outlier <- min(as.numeric(as.character(mudbanks_selection$pos)))-1000
      
      outlier <- data.frame(positions= -1, distances= -1, fractions = -1)
      
    }

    # test the selected position index of the outlier
    # if it is selected point of interest; consider it as outlier 
    if (outlier$positions == unique(selected_point$pos)){
      # print(paste0('true for pos: ', unique(selected_point$pos)))
      
      rownr <- rbind(rownr, which(mudbanks$DATE_ACQUIRED == i &
                           mudbanks$pos == outlier$positions &
                           mudbanks$dropClass == outlier$dropClass))
      
      # look up matching entry
      # rownr <- which(mudbanks$DATE_ACQUIRED == i &
      #                    mudbanks$pos == outlier$positions &
      #                    mudbanks$dropClass == outlier$dropClass)
        
      # # now get the indication for outliers inside the original mudbanks
      # mudbanks[rownr, "mudbank_outlier"] <-
      #     as.data.frame(mudbanks[rownr ,"mudbank_outlier"])[,1] + 1

    }
    
  }
  
  
  mudbanks[rownr, "mudbank_outlier"] <-
    as.data.frame(mudbanks[rownr,"mudbank_outlier"])[,1] + 1
  
  # redefine mudbankSelection for plotting
  mudbanks_selection_plot <-subset(mudbanks, mudbanks$DATE_ACQUIRED == i & 
                                # mudbanks$axisDist >= 0 & 
                                !(pos %in% posToExclude)) 
  mudbankOutlier <-  which(mudbanks_selection_plot$mudbank_outlier >0)
  
  SpatialPointsAll <- sp_pnt_ee(mudbanks_selection_plot$x[-mudbankOutlier],
                                mudbanks_selection_plot$y[-mudbankOutlier], 'all_drop',
                                "#c51b8a")

  
  SpatialPointsOutl <- sp_pnt_ee(mudbanks_selection_plot$x[mudbankOutlier],
                                 mudbanks_selection_plot$y[mudbankOutlier], 
                                 'outlier',
                                 "#1c9099")
  # first + SpatialPointsAll + SpatialPointsOutl
  # first + coastline_selection + non_outliers + nonsense_sp + pos_slopes + noBankSp + SpatialPointsOutl
  
  
  # first + non_outliers + outliers_sp + coastline_selection
}


for (year in unique(format(as.Date(uniqueDates), '%Y'))){
  # year <- 2000
  # print(year)
  start_year <- as.Date(ISOdate(year, 1, 1))
  end_year <- as.Date(ISOdate(year, 12, 31)) 
  
  mudbanks_per_year <-subset(mudbanks,
                             as.Date(DATE_ACQUIRED) >= start_year &
                             as.Date(DATE_ACQUIRED) <= end_year)

  write_csv(mudbanks_per_year, paste0(wd,"/data/processed/offshore_points/", aoi,
                                      '_', year, '_offshore.csv'))
  
  print(paste0(wd,"/data/processed/offshore_points/", aoi,
               '_', year, '_offshore.csv'))
  
}

