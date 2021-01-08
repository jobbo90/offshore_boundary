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

mapviewOptions(basemaps = c( "Esri.WorldImagery","Esri.WorldShadedRelief", "OpenStreetMap.DE"))
dataFolder <- './data/raw'
years <- c('2005', '2006','2007', '2008','2009') 

# select folders
folderSelect <- as.matrix(list.files(paste0(dataFolder, '/GEE_exports'), full.names = T))
df <- rewrite(folderSelect);
# only csv's
df <- df[grep('.csv', folderSelect, ignore.case = T),]


filtered <- vector('list', 100)
for (q in seq_along(years)) {
      # q <- 1
      year = years[q]
      
      filters = c(year)
      
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
allFiles <- do.call(rbind, lapply(as.matrix(filtered)[,1], function(x) read.csv(x, stringsAsFactors = FALSE,
                                                                                sep = ',', na.strings=c("","NA")
                                                                                )))

col_dates <- col_of_interest(allFiles, 'DATE_ACQUIRED$')
col_coastDist <- col_of_interest(allFiles, 'coastDist$')

# all unique transect (id's)
allPos <- unique(allFiles[, col_of_interest(allFiles, 'pos$')]);
uniqueX<- unique(allFiles[, col_of_interest(allFiles, 'originX$')]);
uniqueY<- unique(allFiles[, col_of_interest(allFiles, 'originY$')]);
geo<- unique(allFiles[, col_of_interest(allFiles, '.geo')]);

coastlines <- reshape_csvPoints(allFiles, 'coastX', 'coastY', c('coastDist'))
keep_columns <- c('axisDist', 'mudFract', 'endDrop', 'coastDist',
                  'originX', 'originY', '.geo')  # necessary for mudbank output
mudbanks <- reshape_csvPoints(allFiles, 'peakCoordX', 'peakCoordY', keep_columns)

# change to NA
mudbanks$coastDist[mudbanks$coastDist == -1] <- NA

# sort al rows based on position & date
mudbanks<-mudbanks[with(mudbanks, order(pos, DATE_ACQUIRED)), ]

# if coastDist = -1; replace with nearest observations
mudbanks$dist_locf <- na.locf(mudbanks$coastDist, option = "locf")   # Last Obs. Carried Forward
# this becomes a problem if the value before this is a nonsense value..

# normalize mudbank Distance:
mudbanks$distance <- mudbanks$axisDist - mudbanks$dist_locf   

# set outlier to 0
mudbanks$outlier <- 0

#' implement workflow
#' 1) filter outliers
#'     - Filter on neighborhood (previous 2, current and next two points)
#'     Use distance to see if the point is deviating
#'     Consider using fraction..
#' 2) Apply douglas pecker algorithm
#'      - Requires to define subsections (see https://www.tandfonline.com/doi/pdf/10.1559/152304099782424901?casa_token=9wn9uSUp3zYAAAAA:XYDB0pKcZcH69STl6eOAlKoMPEwIbvxtlwUwzZ00q4V-z8yOfAREUCePnd4fiZbS9H2A-woJqt0mIg  )
#'      to ensure separate mudbanks are recognized
#'      
# all unique dates
uniqueDates <- unique(allFiles[,col_dates]);

# # for testing: set to cloudfree
# cloudFree <- ee$Image(collection$filter(ee$Filter$lt("CLOUD_COVER", 30))$
#                         filterDate(as.character(as.Date(min(uniqueDates))-1), as.character(as.Date(max(uniqueDates))+1))$
#                         sort("CLOUD_COVER")$first()) #
# 
# 
# filtCollect <- collection$filterDate(as.character(reference_date-20), as.character(reference_date+20))$
#   sort("CLOUD_COVER")$first()
# 
# id <- as.Date(eedate_to_rdate(filtCollect$get("system:time_start")))

# or get around a reference date

# test <- which.min(abs(as.Date(uniqueDates) - reference_date))
# uniqueDates <- uniqueDates[1:length(uniqueDates) == test]

# good dates: 2017-09-02, "2018-02-27", 2018-08-28, 2018-09-27


for (i in uniqueDates){
  # i <- uniqueDates[3]
  # i <- uniqueDates[uniqueDates == as.Date(id)]
  
  # select relevant observations
  mudbanks_selection <-subset(mudbanks, mudbanks$DATE_ACQUIRED == i & 
                                mudbanks$axisDist >= 0 & 
                                mudbanks$distance >= 0) # still to test....
  
  # give all excluded observation an 1 for outlier in original
  # to keep track of all outliers.
  mudbanks[which(mudbanks$DATE_ACQUIRED == i & 
                   mudbanks$axisDist < 0 &
                   mudbanks$distance < 0), "outlier"] <-
    as.data.frame(mudbanks[
      which(mudbanks$DATE_ACQUIRED == i & 
              mudbanks$axisDist < 0 &
              mudbanks$distance < 0),"outlier"])[,1] + 1
  
  # order
  # coastlines_selection[order(as.character(coastlines_selection$pos)),]
  mudbanks_selection<-mudbanks_selection[order(as.numeric(as.character(mudbanks_selection$pos))),]

  # # plot
  # first+mapview(coastlines_selection, col.regions = c("red"), layer.name = 'coastline') +
    # mapview(mudbanks_selection, col.regions = c("green"), layer.name = 'mudbanks' )
  
  # outlier test full set of points:
  positions_all <- as.numeric(as.character(mudbanks_selection$pos))
  distances_all <-mudbanks_selection$distance # grab the normalized distances
  
  # second order polynomial fit: alongshore position & polynomial fit
  lm_out_all <-lm(distances_all ~ poly(as.numeric(positions_all),2))
  
  predicted.intervals <- predict(lm_out_all,
                                 data.frame(x=as.numeric(positions_all)),
                                 interval='confidence', level=0.99)
  
  # Bonferroni-adjusted outlier test (test largest absolute standardized residual)
  outlier_test <- car::outlierTest(lm_out_all) 
  outlier_ind<-as.numeric(names(outlier_test$rstudent))
  
  # Give outlier +1 in the source file to keep track
  mudbanks[which(mudbanks$DATE_ACQUIRED == i & 
             mudbanks$pos %in% positions_all[outlier_ind]), "outlier"] <-
    as.data.frame(mudbanks[
      which(mudbanks$DATE_ACQUIRED == i & 
              mudbanks$pos %in% positions_all[outlier_ind]),"outlier"])[,1] + 1
  
  # drop the most obvious outliers from the selection
  mudbanks_selection <- mudbanks_selection[-outlier_ind, ]
  
  # plot
  # plot(positions_all, distances_all)
  # lines(positions_all, predicted.intervals[,1],col='green',lwd=3)
  # points(positions_all[outlier_ind],distances_all[outlier_ind], col = 'red')

  # sanity check:
  # points( as.numeric(as.character(mudbanks_selection$pos)), mudbanks_selection$distance, col = 'blue')
  
  for (pnt in 1:nrow(mudbanks_selection)){
    # pos_of_interst <- 91000 #130000
    # selected_point <-mudbanks_selection[which(mudbanks_selection$pos == pos_of_interst),]
    selected_point <-mudbanks_selection[pnt,]
    
    
    # select nearby points
    # a) nearest 2?
    # b) based on pos
    ajoining_points <- subset(mudbanks_selection, as.character(pos) <=  as.numeric(as.character(selected_point$pos))+4000 &
                                as.character(pos) >= as.numeric(as.character(selected_point$pos))-4000 &
                                as.character(pos) != as.numeric(as.character(selected_point$pos)))
    
    combined <- rbind(selected_point,ajoining_points)
    
    # order by pos
    combined_ordered <-combined[order(as.numeric(as.character(combined$pos))),]
    
    # plot selected points
    # first +
      # mapview(mudbanks_selection, col.regions = c("blue"), layer.name = c('mudbanks_selection')) +
      # mapview(selected_point, col.regions = c("red"), layer.name = c('selected_point')) +
      # mapview(ajoining_points, col.regions = c("green"), layer.name = c('ajoining_points'))
  
    positions<- as.numeric(as.character(combined_ordered$pos))
    distances <-combined_ordered$distance # grap the normalized distances
    fractions <- combined_ordered$mudFract
    datatest <- data.frame(positions=positions,distances=distances, fractions = fractions)
    
    # test <- loess(distances ~ a +(positions*b) + (c*positions) + (d*positions*distances) + e*positions^2 + f*distances^2,
    #               data = datatest,
    #               start = list(a=1, b = 1, c = 1, d = 1, e = 1, f =1), span=.75)
    # # https://stats.stackexchange.com/questions/176361/trouble-in-fitting-data-to-a-curve-nls?noredirect=1&lq=1
    

    # plot(positions,fractions, col ='red')
    # abline(lm(datatest$fractions~as.numeric(datatest$positions)),lty = 2)
    
    # plot(fractions, distances, col = 'blue')
    # abline(lm(datatest$distances~as.numeric(datatest$fractions)),lty = 2)
    # 

    
    # when sufficient observations calculate second order polynomial
    if (length(unique(datatest$distances)) > 2) {
      
      
      # # calculate linear fit
      lm.out_lin <- lm(datatest$distances~as.numeric(datatest$positions))
      
      # calculate second order polynomial 
      lm.out<-lm(datatest$distances ~ poly(as.numeric(datatest$positions),2))
      r2 <- summary(lm.out)$r.squared # or adjusted r2?
      p <- summary(lm.out)                # should be <0.05?
      fstat <- summary(lm.out)$fstatistic # 
      
      # summary(lm.out)
      # plot 
      # plot(positions, distances)
      # abline(lm(datatest$distances~as.numeric(datatest$positions)),lty = 2)
      
      # plot(fitted(lm.out),residuals(lm.out))
      
      # or B-splines?
      # splines <- lm(datatest$distances ~ bs(as.numeric(datatest$positions), df = 10))

      pred.int <- predict(lm.out,data.frame(x=positions),
                          interval='confidence',level=0.99)
      # lines(positions, pred.int[,1],col='green',lwd=3)
      
      intercept <-lm.out$coefficients[1]
      slope <- lm.out$coefficients[2]
      # potential benefit: slope says something about direction; negative slope; front of mudbank
      # low pos > further east, decreasing distance ==> front of mud bank. 
      # only in case of mudbank
      
      # residuals
      resid <- lm.out$residuals
      maxResid <- which.max(abs(resid))
      
      # # cookds distance > 1 considered as outlier?
      # cooksd <- cooks.distance(lm.out)
      # plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")
      # abline(h = 4*mean(cooksd, na.rm=T), col="red")
      # 
      # influential <- as.numeric(names(cooksd)[(cooksd > (4/nrow(datatest)))])
      
      # # outliers test
      # # https://statsandr.com/blog/how-to-do-a-t-test-or-anova-for-many-variables-at-once-in-r-and-communicate-the-results-in-a-better-way/
      # outlier_test <- car::outlierTest(lm.out) # Bonferroni-adjusted outlier test (test largest absolute standardized residual)

      
      test2 <- has_error(car::outlierTest(lm.out), silent = !interactive())
      test3 <- has_warning(car::outlierTest(lm.out))

      outlier_test <- if(test2|test3){car::outlierTest(lm.out_lin)} else{car::outlierTest(lm.out)}
      
      # if no outliers detected:
      # allways returns one point to ensure continuation of worklfow
      # point with a Pos outside the range is selected
      outlierIndex <- ifelse(length(outlier_test$rstudent) > 0,
                             as.numeric(as.character(combined_ordered[as.numeric(names(outlier_test$rstudent)),]$pos)),
                             min(as.numeric(as.character(mudbanks_selection$pos)))-1000)
      
      # drop outlier ==> not sure if necessary 
      # might be necessary if re-run this script for second sweep
      # new_dataTest <- datatest[-outlierIndex,]
      

      # for (ind in 1:length(outlierIndex)){
      #   corresponding_pos <- as.numeric(as.character(combined_ordered[outlierIndex,]$pos))
      #   
      #   
      #   mudbanks[which(mudbanks$pos == corresponding_pos & 
      #                    mudbanks$DATE_ACQUIRED == i), "outlier"] <- 
      #     as.data.frame(mudbanks[which(row.names(mudbanks) == row.names(selected_point)),"outlier"])[,1] + 1
      #     
      # }  
      
      
    } else { # stop("'degree' must be less than number of unique points")
      
      # if only 1 observation; cannot determine if outlier based on neighbours
      outlierIndex <- min(as.numeric(as.character(mudbanks_selection$pos)))-1000
      
    }
      
    
    
    
    if (outlierIndex == as.numeric(as.character(selected_point$pos))){
      
      mudbanks[which(row.names(mudbanks) == row.names(selected_point)), "outlier"] <-
        as.data.frame(mudbanks[
          which(row.names(mudbanks) == 
                  row.names(selected_point)),"outlier"])[,1] + 1
      
    }
    

    


  }
  # # redefine mudbank selection for plotting
  # mudbanks_selection <-subset(mudbanks, mudbanks$DATE_ACQUIRED == i & 
  #                               mudbanks$axisDist >= 0 & 
  #                               outlier == 0) 
  # mudbank_selection_Outlier <- subset(mudbanks, mudbanks$DATE_ACQUIRED == i & 
  #                                       mudbanks$axisDist >= 0 &
  #                                       outlier >= 1)

}


# export output to folder
# per year?
# '.data/processed/offshore_points'

for (year in unique(format(as.Date(uniqueDates), '%Y'))){
  
  start_year <- as.Date(ISOdate(year, 1, 1))
  end_year <- as.Date(ISOdate(year, 12, 31)) 
  
  mudbanks_per_year <-subset(mudbanks, 
                             as.Date(DATE_ACQUIRED) >= start_year,
                             as.Date(DATE_ACQUIRED) <= end_year)

  write_csv(mudbanks_per_year, paste0(wd,"/data/processed/offshore_points/229_56", 
                                      '_', year, '_offshore.csv'))
  
  
}