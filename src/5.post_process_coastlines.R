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

# rm(list = ls())
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


aoi <-  c('Suriname') # Suriname / Braamspunt / WegNaarZee / FrenchGuiana / Guyana
years <- c(seq(1985, 2021, 1))

path_rowsSUR <- c( '229_56', '228_56', '230_56') # Suriname
path_rowsFG <- c('226_57', '227_56', '227_57', '228_56') #FrenchGuiana
path_rowsGUY <- c('230_56', '231_55', '232_54') #Guyana

path_rows <- list('Suriname' = path_rowsSUR, 
               "FrenchGuiana" = path_rowsFG,
               "Guyana" = path_rowsGUY )
path_rows <- path_rows[[aoi]]

min_Std <- 25   # minimal amount of meters difference before considered outlier
year_limit <- 4 # search window in years for finding coastline obs when insufficient values per year.
min_obs_rosner <- 10    # Amount of obs needed to perform statistical test

exportCoasts <- T
includeT2 <- F

# select folders
dataFolder <- './data/raw'
folderSelect <- as.matrix(list.files(paste0(dataFolder, '/GEE_exports'), 
                                     full.names = T))
df <- rewrite(folderSelect);
# only csv's
df <- df[grep('.csv', folderSelect, ignore.case = T),]

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

fileDate <- as.Date(file.info(as.matrix(filtered)[1,1])$mtime, format = 'YYYY-mm-dd')

allFiles <- do.call(bind_rows, lapply(as.matrix(filtered)[,1], 
                                      function(x) read.csv(x, stringsAsFactors = FALSE,
                                                           sep = ',', na.strings=c("","NA")
                                                           )
                                      %>% mutate(`DateCreated` = as.Date(file.info(x)$mtime, format = 'YYYY-mm-dd'))
                                      ))


# remove duplicate entries (31-12-yyyy occasionaly occurs double)
duplicates <- allFiles %>% 
    group_by_at(vars(DATE_ACQUIRED, pos,areaName)) %>%
    filter(n()>1) %>%
    ungroup()

allFiles <- unique(allFiles) 
allFiles$Country = aoi

# if Country is Suriname flip files 
# (necessary for GEE exports processed before 1-1-2022)
# make sure the positions are correct with processing after the new date.
MaxPos <- max(allFiles[allFiles$Country=='Suriname', "pos"])
allFiles <- allFiles %>% 
  dplyr::mutate(pos = ifelse(Country == "Suriname" & DateCreated < "2022-01-01", 
                             abs(pos-MaxPos),
                             pos))
# test if the flipped worked
testPos <- allFiles %>%
  group_by_at(vars(DATE_ACQUIRED, pos,areaName)) %>%
  filter(areaName == '228_56') %>%
  dplyr::mutate(posDif = pos2 - pos)
# filter(DateCreated > "2022-01-01")
# sort(unique(testPos$pos))

# all dates
uniqueDates <- unique(allFiles[,col_of_interest(allFiles, 'DATE_ACQUIRED$')])
drop <- c('system.index', '.geo', 'DateCreated')
keep_columns <- colnames(allFiles)[!(colnames(allFiles) %in% drop)]

# reformat collectionType if necessary (in old version collection type was not included)
# first runs where T1 only
if(is.null(allFiles$collectiontype)){
  allFiles$collectiontype <- 'T1'
}

allFiles$collectiontype[which(is.na(allFiles$collectiontype))] <- 'T1'

# include T2 observations or not?
if(includeT2 != T){
  
  allFiles <- subset(allFiles, collectiontype!="T2")
  
  print('T2 obsevations are NOT included')
}


# prep input to sf data.frame class
coastlines <- reshape_csvPoints(allFiles, 'coastX', 'coastY', keep_columns)

# change all -1 to NA
# these are the transect that resulted in no coastline estimate
coastlines$coastDist[coastlines$coastX == -1] <- NA

# sort al rows based on position & date
coastlines<-coastlines[with(coastlines, order(pos, DATE_ACQUIRED,collectiontype)), ]

# make groups per year, 3 months and 3 years per transect
coastlines <- coastlines %>%
  # classifiy into wet or dry season
  dplyr::mutate(seasons = ifelse(as.Date(month(DATE_ACQUIRED)) < 5 |
                                   as.Date(month(DATE_ACQUIRED)) >11,
                                 'wet','dry'
                                 )) %>%
  
  dplyr::mutate(quarterly_col = as.Date(cut(lubridate::date(DATE_ACQUIRED),
                                     "3 month"))) %>%
  dplyr::mutate(date_col = as.Date(cut(lubridate::date(DATE_ACQUIRED), 
                                "3 year"))) %>%
  dplyr::mutate(five_year_col = as.Date(cut(lubridate::date(DATE_ACQUIRED), 
                                "5 year"))) %>%
  dplyr::mutate(year_col = as.Date(cut(lubridate::date(DATE_ACQUIRED),
                                "1 year"))) 

group_dates<-unique(coastlines$year_col)        # yearly
group_pos <- unique(coastlines$pos)             # All unique positions (transect number)
group_years <- unique(coastlines$date_col)      # per 3 year
five_years <- unique(coastlines$five_year_col)

#'
#'  estimate coastal outliers with rosner test
#'  - for each transect per 3 years to ensure sufficient observations
#'  With insufficient observations, look within search window for additional obs.
#'  

# assume nothing is outlier and set outputs to NA
coastlines$coast_outlier <- 1
coastlines$slope         <- NA
coastlines$coastObs      <- NA

for(q in group_pos){ 
  svMisc::progress(q, max.value = nrow(group_pos))
  # q<-group_pos[group_pos==330000] # 330
  # 
  # indexs2 <- which(coastlines$pos == q &
  #                   coastlines$coastX != -1)
  # 
  # subsets2 <- coastlines[indexs2, ]
  # 
  # # calculate second order polynomial 
  # lm.out<-lm(as.numeric(subsets2$coastDist) ~ poly(as.numeric(as.Date(subsets2$DATE_ACQUIRED)),2))
  # 
  # r2 <- summary(lm.out)$r.squared # or adjusted r2?
  # # p$ <- summary(lm.out)                # should be <0.05?
  # fstat <- summary(lm.out)$fstatistic #
  # 
  # # Bonferroni-adjusted outlier test (test largest absolute standardized residual)
  # # test if no error/warning
  # test2 <- has_error(car::outlierTest(lm.out), silent = !interactive())
  # test3 <- has_warning(car::outlierTest(lm.out))
  # 
  # # Allways returns a observation (with largest Studentized residual)
  # # Which implies it is not allways meeting the P < 0.05 cutoff value
  # outlier_test <- if(test2|test3){car::outlierTest(lm.out_lin)} else{car::outlierTest(lm.out)}
  # 
  # # plot example:
  # x <- seq(min(as.numeric(as.Date(subsets2$DATE_ACQUIRED))), 
  #          max(as.numeric(as.Date(subsets2$DATE_ACQUIRED))), length.out = length(subsets2$coastDist))
  # y <- predict(lm.out)
  # predicted.intervals <- predict(lm.out,
  #                                data.frame(x=as.numeric(as.Date(subsets2$DATE_ACQUIRED))),
  #                                interval='confidence', level=0.99)
  #
  # plot(as.Date(subsets2$DATE_ACQUIRED), subsets2$coastDist)
  # lines(as.Date(x),predicted.intervals[,1],col = 'orange')
  # points(as.Date(subsets2$DATE_ACQUIRED)[which(subsets2$collectiontype == 'T2')],
  #        subsets2$coastDist[which(subsets2$collectiontype == 'T2')],
  #         col = 'red')
  # 
  # # rosner test would probably kick out all T2 observations?
  # points(as.Date(subsets2$DATE_ACQUIRED)[which(rosner(subsets2$coastDist, min_Std, 15)==0)],
  #        subsets2$coastDist[which(rosner(subsets2$coastDist, min_Std, 15)==0)],
  #         col = 'blue')
  # 
  # points(as.Date(subsets2$DATE_ACQUIRED[as.numeric(names(outlier_test$rstudent))]),
  #        subsets2$coastDist[as.numeric(names(outlier_test$rstudent))],
  #        col = 'green')
  
  
  for(i in group_years){ 
    # i<- group_years[5]
    
    # start <- Sys.time()

    indexs <- which(coastlines$date_col == i & 
                      coastlines$pos == q &
                      coastlines$coastX != -1)
    
    subsets3 <- coastlines[indexs, ]
    
    # get nearest observation and add it to the list
    reference_date <- mean(as.Date(subsets3$DATE_ACQUIRED))
    
    # So grow the subset to at least 10(?) obs by adding nearest observations
    maxAttemp <- 0 # make sure you don't get stuck in infinite loop..
    while(nrow(subsets3) < min_obs_rosner & 
          maxAttemp < min_obs_rosner+5){
      
      # exclude dates from the year of interest
      # sample from entire dataset so that years outside the 3 year block
      # are also possible candidates
      selectedDates <- subset(coastlines, coastlines$pos == q &
                                coastlines$coastX != -1 & 
                              !(coastlines$DATE_ACQUIRED %in% 
                                  subsets3$DATE_ACQUIRED)
                            )$DATE_ACQUIRED
      
      
      # exclude the ones already selected
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
    
    # preferably T2 collection is not used to determine outliers when there is 
    # 'sufficient' T1 observations.
    
    # Only give the rosner output to the original subset3 indices
    # if there is >15 observations let K be estimated
    coastlines[indexs, 'coast_outlier'] <- 
        rosner(subsets3$coastDist, min_Std, 15)[which(subsets3_recal %in% indexs)]
    # Will throw an error/warning if all values are the same => nothing is assigned as outlier
      
    # end <- Sys.time()
    # dif<- difftime(end, start, "mins")
    
    # if(as.numeric(dif, units="secs") > 5){
    #   print(paste0(as.Date(i), ' for pos: ', q,' in ', round(dif,1), ' in ', units(dif)))
    # }
 
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

  for(q in group_pos){
    # i<-group_dates[group_dates == c("2002-01-01")]
    # 
    # q <- group_pos[group_pos == 330000]
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
                              coastlines$coastDist > -1 & 
                              !(coastlines$DATE_ACQUIRED %in% 
                                nonOutliers$DATE_ACQUIRED))$DATE_ACQUIRED
      
      # get nearest date (excluding dates outside limit)
      nearestDate <- selectDates[1:length(selectDates) == 
                                   which.min(replace(abs(as.Date(selectDates) - 
                                                           reference_date), 
                                                     abs(as.Date(selectDates) - 
                                                           reference_date)>year_limit*356, 
                                                     NA))]
      
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
                          maxAttemp, lm.out, slope, m_per_year, selectDates, nearestDate,
                          index_nearest))
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
    coastlines[ind, 'locf'] <- nearest$coastDist
    
    
  }
  remove(pos_subset, data_entry, dateDiff, minInd, nearest)
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

# only now there remain some groups with median of NA 
#     (because there was only 0 or 1 observations in that group)
# This is a problem for the next step
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
    print(year)

    
    outfile <- paste0("./data/processed/coastlines/", aoi,
           '_', year, '_coastlines.csv')
    
    # if(!file.exists(outfile)){
      start_year <- as.Date(ISOdate(year, 1, 1))
      end_year <- as.Date(ISOdate(year, 12, 31)) 
      
      coastlines_per_year <-subset(coastlines3,
                                   as.Date(DATE_ACQUIRED) >= start_year &
                                     as.Date(DATE_ACQUIRED) <= end_year)
      
      coastlines_per_year <- coastlines_per_year %>%
        dplyr::select(!c(x,y))
      
      write_csv(coastlines_per_year, outfile)
      # select properties to include in the export
      testForExport <- coastlines_per_year %>%
        dplyr::select(c(coastX, coastY, pos,DATE_ACQUIRED,coast_outlier,
                        coastDist)) %>%
        dplyr::mutate(coastDist = ifelse(is.na(coastDist), -1, coastDist))
      
      sf_to_ee <- ee$FeatureCollection(sf_as_ee(testForExport))
      
      
      fileN <- paste0(aoi,'_',year,'_coastlines')
      assetid <- paste0(ee_get_assethome(), '/',aoi,'_foreshore/',fileN)
      
      task_vect <- ee_table_to_asset(
        collection = sf_to_ee,
        description = fileN,
        assetId = assetid,
        overwrite = TRUE
      )
      task_vect$start()
      # ee_monitoring(task_vect)
      
      # build function to export directly to GEE asset
      
      print( paste0(wd,"/data/processed/coastlines/", aoi,
                    '_', year, '_coastlines.csv'))
      remove(coastlines_per_year,sf_to_ee, start_year, end_year,task_vect)
    # }
    
   
    
    # https://cran.r-project.org/web/packages/rgee/rgee.pdf
    #http://5.9.10.113/65768592/converting-gee-script-to-be-used-with-rgee
    
    
  }
  
  
  
}

######
#' 
#'
#' test simple 2d plot
#' 
# 
twoD_pos <- 38000# 70000  # 532000

subset2d_for_testPlot <- subset(coastlines3, pos == twoD_pos) #  & coastDist < 6000

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



##########
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


coast_spatial <- sp_pnt_ee(subset2d_for_testPlot$coastX,
                           subset2d_for_testPlot$coastY, paste0('pos: ',twoD_pos),
                           "#d95f0e")

pnt <- ee$Geometry$Point(c(median(subset2d_for_testPlot$originX), median(subset2d_for_testPlot$originY)))

filtCollect <- collection$filterBounds(pnt)$
  filterDate(as.character(as.Date(min(subset2d_for_testPlot$DATE_ACQUIRED))-1),
             as.character(as.Date(max(subset2d_for_testPlot$DATE_ACQUIRED))+1))$
  # sort("CLOUDCOVER", TRUE)$
  sort('DATE_ACQUIRED', F)
dates <- ee_get_date_ic(filtCollect, time_end = FALSE)

acquisition <- ee_get_date_img(filtCollect$sort('DATE_ACQUIRED', F)$first())$time_start

Map$centerObject(filtCollect$first())
first <- Map$addLayer(filtCollect$first(), vizParams, paste0('landsat: ', format(as.Date(acquisition), '%Y-%m-%d')))

first + coast_spatial


