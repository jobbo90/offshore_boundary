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


seq2 <- seq(1985, 2020, 1)
years <- c(seq2)

# pos to exlcude for mudbank boundary estimates / outlier detection
posToExclude <- c(seq(138000,147000,1000),
                  seq(241000, 255000, 1000))  

min_Std <- 25   # minimal amount of meters difference before considered outlier
year_limit <- 4 # search window in years for finding coastline obs when insufficient values per year.
min_obs_rosner <- 10    # Amount of obs needed to perform statistical test

exportCoasts <- T

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

path_rows <- c( '229_56', '228_56', '230_56') # '228_56','230_56'

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



#get duplicated: export includes 21-12-2002 and 31-12-2014 observation 2x 
duplicates <- allFiles %>%
  group_by(DATE_ACQUIRED, pos) %>%
  filter(n()>1)

allFiles <- unique(allFiles) 

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
      dplyr::select(!c(x,y)) # drop columns that only deal with mudbank data?
    
    write_csv(coastlines_per_year, paste0(wd,"/data/processed/coastlines/", aoi,
                                        '_', year, '_coastlines.csv'))
    print( paste0(wd,"/data/processed/coastlines/", aoi,
                  '_', year, '_coastlines.csv'))
    remove(coastlines_per_year)
  }
  
  
  
}

# 
# # test simple 2d plot
# plot(coastlines3$pos[which(coastlines3$coast_outlier != 0)], 
#      coastlines3$deltaCoast[which(coastlines3$coast_outlier != 0)])
# identify(coastlines3$pos, coastlines3$deltaCoast, n=1, labels=coastlines3$pos)

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

# select folders
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
processedCoastlines <- do.call(bind_rows,
                         lapply(as.matrix(filtered)[,1],
                                function(x) read.csv(x,
                                stringsAsFactors = FALSE, sep = ',',
                                na.strings=c("","NA")))
                         )

mudbanks <- processedCoastlines # coastlines3 or processedCoastlines to avoid processing all the outliers for coastline again

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
mudbanks$mudbankObs <- NA

subset2d_for_testPlot2 <- subset(mudbanks, year_col == c('2018-01-01'))

# count for each year, pos the total amount of relevant mudbank observations
# thus exclude -1 values
mudbanks2 <- mudbanks %>% #subset2d_for_testPlot2 %>% 
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
  # dplyr::select(c(DATE_ACQUIRED, year_col, pos,axisDist,mudbankObs, coastDist,
                  # validObs)) %>%
  ungroup()

# reshape mudbanks such that each relative, absolute and slope drop gets it own data-entry for each pos
# so triplicate each row and overwrite the values in the corresponding columns
# transform such that for each pos all three coordinates become a separate entry with unique x,y coords
mudbanks2$x <- -1
mudbanks2$y <- -1


mudbanks3 <- mudbanks2 %>%
  # dplyr::group_by(pos, year_col) %>% # count observations per pos each year
  # dplyr::mutate(mudbankObs = n()) %>% # like this it would als include all nonsense observations
  # dplyr::ungroup() %>%
  
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
  dplyr::ungroup()

  # dplyr::select(-ends_with('.1')) # in case the create x,y coordinates were created doubble
  # dplyr::select(mudbankObs) / pull()

# all dates
uniqueDates <- unique(mudbanks3$DATE_ACQUIRED)


for (i in uniqueDates){
  # i <- uniqueDates[486]
  # i <- uniqueDates[which(uniqueDates == c('1987-10-26'))]
  # print(i)
  
  # Build image Collection around selected image
  # filtCollect <- collection$filterDate(as.character(as.Date(i)),
  #                                      as.character(as.Date(i)+1))
  # dates <- ee_get_date_ic(filtCollect, time_end = FALSE)
  # # plot image
  # Map$centerObject(filtCollect$first())
  # first <- Map$addLayer(filtCollect$first(), vizParams, paste0('landsat: ',i))

  # select all observations of the corresponding date
  mudbanks_selection <-subset(mudbanks3, mudbanks3$DATE_ACQUIRED == i & 
                                !(pos %in% posToExclude)) 
  

  # order by position
  mudbanks_selection<-mudbanks_selection[order(mudbanks_selection$pos),]
  
  # create spatial points
  # non_outliers <- sp_pnt_ee(mudbanks_selection$x, # &  mudbanks_selection$SmoothedPeak > 0
  #           mudbanks_selection$y,
  #           'all observations',
  #           "#d95f0e")
  # coastline_selection <- sp_pnt_ee(mudbanks_selection$coastX, # &  mudbanks_selection$SmoothedPeak > 0
  #                           mudbanks_selection$coastY,
  #                           'coastline',
  #                           "#31a354")
  
  #'
  #' test for obvious outliers:
  #' - mudbank extent cannot be smaller than coastline position
  #' - mudbank extent cannot be -1
  #' - mudbank extent subjected to outlier test
  #' 
  
  # points with smaller mudbank boundary then coastline (false detection)
  # OR observations with mudbankextent of -1
  nonsense <-  which(mudbanks_selection$coastDist > mudbanks_selection$axisDist |
                       mudbanks_selection$axisDist == -1 |
                       is.na(mudbanks_selection$axisDist))
  
  # nonsense_sp <- sp_pnt_ee(mudbanks_selection$x[nonsense],
  #                          mudbanks_selection$y[nonsense],
  #                          'nonsense outliers',
  #                          "#fa9fb5")

  # first + non_outliers + coastline_selection + nonsense_sp
  
  # keep track of entries that need to be tracked as outlier
  combinations <- data.frame(DATE_ACQUIRED = rep(i, length(nonsense)),
                             pos = mudbanks_selection$pos[nonsense],
                             dropClass = mudbanks_selection$dropClass[nonsense])
  
  # update mudbanks_selection
  mudbanks_selection <- mudbanks_selection[-nonsense, ]
  
  # here it is best to count the amount of valid observations
  # remember that this up to 3 observations per transect.
  # validObs <- nrow(mudbanks_selection)
  # mudbanks_selection <- mudbanks_selection %>% 
  #   dplyr::group_by(pos, year_col) %>%
  #   dplyr::mutate(mudbankObs = 1) %>%  # assign total amount of mudbankObs value of 1
  #   dplyr::ungroup()
  # 
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
    
    # outliers_sp <- sp_pnt_ee(mudbanks_selection$x[outlier_ind],
    #                          mudbanks_selection$y[outlier_ind],
    #                          'obvious outliers',
    #                          "#fa9fb5")

    # keep track of entries that need to be tracked as outlier
    combinations <- rbind(combinations, data.frame(DATE_ACQUIRED = rep(i, length(outlier_ind)),
                                                   pos = mudbanks_selection$pos[outlier_ind],
                                                   dropClass = mudbanks_selection$dropClass[outlier_ind]))
    # first + non_outliers + coastline_selection + nonsense_sp + outliers_sp
    
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

  # hist(slopes,  xlim=c(floor(min(slopes)), ceiling(max(slopes))),
  #   breaks = c(seq(floor(min(slopes)),ceiling(max(slopes)), 0.01)))

  # calculate median and sd of negative slopes only
  median_slope <- median(slopes[slopes < 0], na.rm = T)
  adjusted_sd <- sd(slopes[slopes < 0], na.rm = T)
  
  # drop observations with 'significant'/positive slopes 
  # Value should be in the range of -0.15 and 0 ==> not sure yet if using median is the best way
  indicesSlopes <- which(slopes > (median_slope + adjusted_sd))
   
  # pos_slopes <- sp_pnt_ee(mudbanks_selection[indicesSlopes, ]$x, # &  mudbanks_selection$SmoothedPeak > 0
  #                         mudbanks_selection[indicesSlopes, ]$y,
  #                         'positive_slopes',
  #                         "#3182bd")
  # first + non_outliers + coastline_selection + nonsense_sp + outliers_sp +pos_slopes
  
  #' 
  #' Very similar to the slope, also the Super Smoothed peak fraction could 
  #' indicate no-mudbank cases: very low values can be a sign of NO mudbank
  #' 
  # 
  # hist(mudbanks_selection$SmoothedPeakFract,
       # breaks =c(seq(min( mudbanks_selection$SmoothedPeakFract),
                     # ceiling(max(mudbanks_selection$SmoothedPeakFract))+0.1,0.01)))
  # lm_out_all2 <-lm( mudbanks_selection$SmoothedPeakFract ~ 
  #                    poly(as.numeric( mudbanks_selection$pos),3))
  # 
  # predicted.intervals2 <- predict(lm_out_all2,
  #                                data.frame(x=as.numeric(mudbanks_selection$pos)),
  #                                interval='confidence', level=0.99)
  
  searchWindow <- 25
  lowerLevelWindow <- 3
  
  # update search window to make sure bottoms and tops are
  searchWindow <- ifelse(length(mudbanks_selection$SmoothedPeakFract) < searchWindow, 2, 25)
  lowerLevelWindow <- ifelse(searchWindow > lowerLevelWindow, lowerLevelWindow, 1)
  
  runnAve <- data.frame(pos=mudbanks_selection$pos,
                       rolling = zoo::rollmean(mudbanks_selection$SmoothedPeakFract, 
                                               searchWindow,fill = NA))
  
  runnAve$rolling[which(runnAve$rolling < 0.001)] <- NA
  
  # make the search window for rolling mean (above) dependend on the amount of positions with valid observations
  # posWithValidObs <- length(unique(mudbanks_selection$pos))
  
  localMin <- rollapply(as.zoo(runnAve$rolling), 3, function(x) which.min(x)==2)
  indices <- runnAve[which(localMin == T), 'pos']
  
  bottoms <- lapply(1:searchWindow, function(x) inflect(runnAve$rolling, threshold = x)$minima)
  tops <- lapply(1:searchWindow, function(x) inflect(runnAve$rolling, threshold = x)$maxima)
  
  # potential local mins
  allMins <- runnAve[bottoms[[searchWindow]],]
  localMinsPos <- unique(runnAve$pos[bottoms[[searchWindow]]])
  localMaxPos <- unique(runnAve$pos[tops[[round(searchWindow)]]])
  
  NoMudBankPos <- c()
  
  if(length(unique(localMinsPos)) > 1){
  
    
    for(p in 1:length(localMinsPos)){
      # p <- 1
      minPos <- localMinsPos[p]
      minFract <- allMins[p,'rolling']
      
      # print(minPos)
      valley <- subset(mudbanks_selection, pos == minPos)
      
      # at highest level ==> pronounced peaks &  valleys
      posDist <- localMaxPos[which(abs(localMaxPos - unique(valley$pos)) > 3000 &
                                     abs(localMaxPos - unique(valley$pos)) < searchWindow*1000 )]
      
      # at lower level (more potential canidates)
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
      
      
      # if valley depth is small (smaller than std/var/???) & difference with meanMud is large 
      # then it is stil a mudbank
      # if not => continue workflow and detect other pos with similar values
      shoulderHeight <- unique(runnAve[runnAve$pos %in% c(shoulder1, shoulder2) &
                                         !is.na(runnAve$rolling) ,2])
      # shoulderHeight <- unique(subset(mudbanks_selection, pos %in% c(shoulder1, shoulder2))$SmoothedPeakFract)
      
      avgDepth <- mean(shoulderHeight - minFract, na.rm = T)
      # valleyWidth <- max(c(shoulder1, shoulder2)) - min(c(shoulder1, shoulder2))
  
      # for each position:
      # 1) determine if it is truly a local minimum that corresponds to no mudbank
      diffFract <-mean((valley$SmoothedPeakFract - valley$meanMud)) 
      # /valley$meanMud ==> everything approaches 1, so the test comparing to sd doesnt make much sense anymore
      # probably better to compare to to 1 greater than 1 is one direction, lower than one the other?
      differenceTest <- unique(diffFract) < sd(mudbanks_selection$SmoothedPeakFract)
      depthTest <- avgDepth > sd(mudbanks_selection$SmoothedPeakFract) 
       # If any of the shoulders is lower than the valley -> no outliers 
      shoulderHeightTest <- (minFract<shoulderHeight[1]) == (minFract<shoulderHeight[2])
      
      # if depthTest false & difference Test FALSE: potential mudbank: skip loop
      # such that transects are not tested
      
    
      if(depthTest == F && differenceTest == F){ #  & shoulderHeightTest == F)
        # depthTest == F & differenceTest == F & shoulderHeight == F){
        # print(paste0(minPos,' depth: ', round(avgDepth,3), ' & difference: ', 
                     # round(diffFract,3)))
        next
      } 
      
      if(shoulderHeightTest == F){
        # print('no true valley')
        next
      }
  
      # and 2) look wihtin a certain distance for other pos that have a similar mean mud value
      # neighborhood <- subset(mudbanks_selection, pos > minPos - (5*1000) &
      #                          pos < minPos + (5*1000)  ) #searchWindow
      neighborhood <- subset(mudbanks_selection, pos > min(c(shoulder1, shoulder2)) &
                               pos < max(c(shoulder1, shoulder2))) #searchWindow
      
      # all positions in neighborhood that have a lower or simila fraction value
      # are excluded
      # in scenario's where the neighbourhood is wedged in between 2 mudbanks
      # this results in to many points being removed (e.g. the window is big)
      # But also because ALL point obsevations at the POS are exlcuded (and 
      # not only the ones that are below the fraction value)
      valleyVal <- unique(runnAve[runnAve$pos == unique(valley$pos),2])
      
      # check per pos; calculate mean and see if it is meetin the criteria
      
      # plot(neighborhood$pos, neighborhood$SmoothedPeakFract)
      # points(runnAve$pos[which(runnAve$pos %in% neighborhood$pos)],
      #        runnAve$rolling[which(runnAve$pos %in% neighborhood$pos)], col = 'orange')
      # plot(runnAve$pos[which(runnAve$pos %in% neighborhood$pos)],
      #        yvalFlip, col = 'blue')
      # abline(v = minPos)
      #
      xval <- runnAve$pos[which(runnAve$pos %in% neighborhood$pos)]
      yval <- runnAve$rolling[which(runnAve$pos %in% neighborhood$pos)]
      # yvalFlip <- sqrt(-(xval+minPos))*-1
      # 
      
      # parabol <- lm(yval~ poly(xval, 2, raw = TRUE))
      # # solve(cbind(1, xval[1], xval[1]^2), yval[1])
      # coefs <- coef(parabol)
      # 
      # parabol$coefficients[1] <- parabol$coefficients[1]*-1
      # parabol$coefficients[2] <- as.numeric(parabol$coefficients[2]*-1)
      # parabol$coefficients[3] <- parabol$coefficients[3]*-1
      # 
      # predicted.intervals <- predict(parabol,
      #                                data.frame(x=xval),
      #                                interval='confidence', level=0.95)
      # 
      # lines(xval, (predicted.intervals[,1]*-1)+minFract,col='green',lwd=3)
      # 
      subset_indx <- subset(neighborhood,
                            SmoothedPeakFract < mean(valleyVal + 
                                                   sd(neighborhood$meanMud)))
      
      indx <- which(neighborhood$SmoothedPeakFract < #neighborhood$SmoothedPeakFract < 
                      mean(valleyVal + 
                             sd(neighborhood$meanMud)))
      
      # consider excluding all pos that are contained within the detected range of indx?
      # poses problems when in between two mudbanks.
      posRange <- seq(min(neighborhood$pos[indx]), max(neighborhood$pos[indx]),
                      1000)
      
      NoMudBankPos <- c(NoMudBankPos, 
                        posRange)
                        #unique(neighborhood$pos[indx]),
                        # unique(valley$pos))
      
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
    # pnt <- unique(mudbanks_selection$pos)[2]
    
    selected_point <- subset(mudbanks_selection, pos == pnt)
    
    

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
  
  # non outliers in the original data frame
  # amountObs <- which(mudbanks3$DATE_ACQUIRED == i &
  #          !(mudbanks3$pos %in% posToExclude) &
  #         mudbanks3$mudbank_outlier == 0 &
  #         mudbanks3$coastDist < mudbanks3$axisDist &
  #         mudbanks3$axisDist != -1)
  # amount of valid observations - amont of outliers (rownr) should be equal to redefined amount of non-outliers (amounntObs)
  # validObs - length(rownr)

}


# subsetPos <- mudbanks3[which(mudbanks4$pos == 299000),]

# do something similar for non-outlier observation count

mudbanks4 <- mudbanks3 %>% #subsetPos %>%  #  subset2d_for_testPlot %>%

  dplyr::group_by(pos, year_col, validObs, mudbank_outlier) %>% # group 
  # count observations for validObs = 1 & NA  
  dplyr::mutate(validMudbankObs = n_distinct(DATE_ACQUIRED)) %>% # count unique observations
  # if nonsense observation or outlier, set to NA
  dplyr::mutate(validMudbankObs = ifelse(axisDist == -1 | mudbank_outlier > 0,  
                                     NA, validMudbankObs)) %>%

  dplyr::group_by(year_col, pos) %>%
  # replace NA with the count for valid obs
  dplyr::mutate(validMudbankObs = ifelse(is.na(validMudbankObs), 
                                    Mode(validMudbankObs), validMudbankObs)) %>%
  # dplyr::select(c(DATE_ACQUIRED, year_col, pos,axisDist,mudbankObs, coastDist,
  #                 mudbank_outlier, validObs, validMudbankObs)) %>% 
  # dplyr::select(!c(validObs)) %>% # drop the created validObs column from matrix
  ungroup()



# which( mudbanks4$validMudbankObs > mudbanks4$mudbankObs)

# forHist <- mudbanks4$validMudbankObs #mudbanks4$mudbankObs 
# 
# hist(forHist,  xlim=c(min(forHist), max(forHist)),
#   breaks = c(seq(min(forHist),max(forHist), 1)))


# subset2d_for_testPlot <- subset(mudbanks3, year_col == c('2018-01-01'))
# subset2d_for_testPlot <- subset(mudbanks3, pos == 38000)
# 
# plot(as.Date(subset2d_for_testPlot$DATE_ACQUIRED), subset2d_for_testPlot$axisDist,
#      xlab="DATE_ACQUIRED", ylab="mudbank Dist [m]",
#      main = paste0('coastline position: ',twoD_pos, ' [m]'), pch = 20)
# # lines(unique(as.Date(subset2d_for_testPlot$year_col))+180, 
# #       aggregate(subset2d_for_testPlot$coast_median, list(subset2d_for_testPlot$key), median)$x, 
# #       col = 'black', lty = 2)
# points(as.Date(subset2d_for_testPlot[subset2d_for_testPlot$mudbank_outlier > 0, ]$DATE_ACQUIRED),
#        subset2d_for_testPlot[subset2d_for_testPlot$mudbank_outlier > 0, ]$axisDist,
#        col = 'red',  pch = 20)
# legend("right", legend=c("Observations", 'outliers'),
#        col=c("black", 'red'), pch = c(20,20) ,lty = c(0,0), cex=0.8)
# 
# coast_spatial <- sp_pnt_ee(subset2d_for_testPlot$x[which(!is.na(subset2d_for_testPlot$x))],
#                            subset2d_for_testPlot$y[which(!is.na(subset2d_for_testPlot$x))], paste0('pos: ',twoD_pos),
#                            "#d95f0e")
# 
# pnt <- ee$Geometry$Point(c(median(subset2d_for_testPlot$x, na.rm = T), 
#                            median(subset2d_for_testPlot$y, na.rm = T)))
# 
# filtCollect <- collection$filterBounds(pnt)$
#   filterDate(as.character(as.Date(min(subset2d_for_testPlot$DATE_ACQUIRED))-1), 
#              as.character(as.Date(max(subset2d_for_testPlot$DATE_ACQUIRED))+1))$
#   sort("CLOUDCOVER", TRUE)
# dates <- ee_get_date_ic(filtCollect, time_end = FALSE)
# 
# acquisition <- ee_get_date_img(filtCollect$first())$time_start
# 
# Map$centerObject(filtCollect$first())
# first <- Map$addLayer(filtCollect$first(), vizParams, paste0('landsat: ', format(as.Date(acquisition), '%Y-%m-%d')))
# 
# first + coast_spatial
# 



for (year in unique(format(as.Date(uniqueDates), '%Y'))){
  # year <- 2000
  # print(year)
  start_year <- as.Date(ISOdate(year, 1, 1))
  end_year <- as.Date(ISOdate(year, 12, 31)) 
  
  mudbanks_per_year <-subset(mudbanks4,
                             as.Date(DATE_ACQUIRED) >= start_year &
                             as.Date(DATE_ACQUIRED) <= end_year)

  write_csv(mudbanks_per_year, paste0(wd,"/data/processed/offshore_points/", aoi,
                                      '_', year, '_offshore.csv'))
  
  print(paste0(wd,"/data/processed/offshore_points/", aoi,
               '_', year, '_offshore.csv'))
  
}

