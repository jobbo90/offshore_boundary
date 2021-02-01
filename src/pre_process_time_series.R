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
years <-  seq(from = 2008, to = 2009, by = 1)
  #c('2005', '2006','2007', '2008','2009') 

min_Std <- 100 # minimal amount of meters difference before concidered outlier

# select folders
folderSelect <- as.matrix(list.files(paste0(dataFolder, '/GEE_exports'), full.names = T))
df <- rewrite(folderSelect);
# only csv's
df <- df[grep('.csv', folderSelect, ignore.case = T),]
aoi <-  c('Suriname') 

filtered <- vector('list', 100)
for (q in seq_along(years)) {
  for (x in seq_along(aoi)){
      # q <- 8
      year = as.character(years[q])
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
                         ))}
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
uniqueDates <- unique(allFiles[,col_dates]);

keep_columns <- c('axisDist', 'mudFract', 'endDrop', 'coastDist',
                  'originX', 'originY', 'coastX', 'coastY')
# keep_columns <- colnames(allFiles)

mudbanks <- reshape_csvPoints(allFiles, 'peakCoordX', 'peakCoordY', keep_columns)

# change to NA
mudbanks$coastDist[mudbanks$coastDist == -1] <- NA

# sort al rows based on position & date
mudbanks<-mudbanks[with(mudbanks, order(pos, DATE_ACQUIRED)), ]

# make groups of 3 months per transect
mudbanks <- mudbanks %>%
  mutate(quarterly_col = as.Date(cut(lubridate::date(mudbanks$DATE_ACQUIRED),
                                     "3 month"))) %>%
  mutate(date_col = as.Date(cut(lubridate::date(mudbanks$DATE_ACQUIRED), 
                                "3 year"))) %>%
  mutate(year_col = as.Date(cut(lubridate::date(mudbanks$DATE_ACQUIRED),
                                "1 year"))) 

group_dates<-unique(mudbanks$year_col)
group_pos <- unique(mudbanks$pos)
group_years <- unique(mudbanks$date_col)

# assume nothing is outlier
mudbanks$coast_outlier <- 1

#'
#'  estimate coastal outliers with rosner test
#'  - for each transect per 3 years to ensure sufficient observations
#'  
#'  Still poses problems for some transects
#'  Resulting in negative mudbank distances, especially at transects 
#'  near river mouths. 

for(i in group_years){
  # i<-group_dates[group_dates == c("2008-01-01")]
  
  for(q in group_pos){
    # pos_to_test <- 	250000
    # q <- group_pos[group_pos == pos_to_test]
    # print(q)
    # subsets2 <- subset(mudbanks,  pos == q & 
    #                      date_col == i &
    #                      coastDist > -1) # test if NA cause trouble for Rosner function
    # 
    indexs <- which(mudbanks$date_col == i & 
                      mudbanks$pos == q &
                      mudbanks$coastDist > -1)
    
    subsets3 <- mudbanks[indexs, ]
    # rownr <- as.character(indexs)
    
    #somehow row numbers are altered
    mudbanks[indexs, 'coast_outlier'] <- 
      rosner(subsets3$coastDist, min_Std)
    
    # test<-as.data.frame(mudbanks[row.names(mudbanks) %in% indexs, 'mudbank_outlier'])[,1]
    
    # testCoastline <-st_as_sf(SpatialPointsDataFrame(
    #   data.frame(subsets2$coastX, subsets2$coastY),
    #   proj4string=CRS("+proj=longlat +datum=WGS84"),
    #   data = data.frame(subsets2$pos)))

    # mapview(testCoastline, col.region = c('green')) + mapview(subsets2)
    # detect outliers (give them a 0!!!)

    
    # plot(as.Date(subsets3$DATE_ACQUIRED), subsets3$coastDist,
    #     main = paste0(q), xlab = 'date', ylab = 'coastline position')
    # points(as.Date(subsets3$DATE_ACQUIRED)[which(rosner(subsets3$coastDist, min_Std) == 0)],
    #        subsets3$coastDist[which(rosner(subsets3$coastDist, min_Std) == 0)],
    #        col = 'red')

  }
}

# calculate last observation carried forward
# initialy set at the observed value
mudbanks$locf <- mudbanks$coastDist

# if coastDist = -1 or coast observation is an outlier; 
# replace with nearest observations / median value per year?
indices <- unique(which(is.na(mudbanks$coastDist) | mudbanks$coast_outlier == 0))
for(ind in indices){
  # ind<-indices[2]
  
  data_entry <- mudbanks[ind, ]
  # row.names(data_entry)
  pos_subset <- subset(mudbanks, mudbanks$pos == data_entry$pos &
                         mudbanks$coast_outlier == 1 &
                         !is.na(mudbanks$coastDist)) 
  
  # plot(pos_subset$DATE_ACQUIRED, pos_subset$locf)
  # plot(pos_subset$DATE_ACQUIRED, pos_subset$coastDist, col = 'red')
  
  if(nrow(pos_subset) > 0){ # if no observations; locf remain original obsevation
    
    test <- abs(pos_subset$DATE_ACQUIRED - 
                  data_entry$DATE_ACQUIRED)
    
    test[test==0] <- max(test)
    minInd <- which.min(test)
    
    nearest <- pos_subset[minInd,]
    
    mudbanks[row.names(mudbanks) == row.names(data_entry), 'locf'] <- 
      nearest$coastDist
  }

}


mudbanks<-mudbanks[order(mudbanks$pos),]

# median observation per year 
# re-calculate median value per year, per transect position
mudbanks <- mudbanks %>% dplyr::group_by(pos, year_col, coast_outlier) %>%
  dplyr::mutate(coast_median = median(coastDist, na.rm = T))

# set outlier median dist to NA
# this median is used to normalize all mudbank distances
mudbanks$coast_median[mudbanks$coast_outlier == 0] <- NA

# fill outliers with nearest median coastal observation
# not ideal because some observations (start of year/end of year)
# recieve median value from previous year...
mudbanks <- mudbanks %>% group_by(pos, year_col) %>% mutate(coast_median = na.locf(na.locf(coast_median, na.rm=FALSE,
                                                                                           fromLast = T)))

mudbanks$mudbank_distance <- mudbanks$axisDist - mudbanks$coast_median  

# some examples
mudbanks_select <-subset(mudbanks, mudbanks$DATE_ACQUIRED == uniqueDates[2] & 
                              mudbanks$axisDist >= 0 & 
                              mudbanks$mudbank_distance >= 0) 

# test simple 2d plot 
twoD_pos <- 100000#190000
subset2d_for_testPlot <- subset(mudbanks, pos == twoD_pos)

plot(as.Date(subset2d_for_testPlot$DATE_ACQUIRED), subset2d_for_testPlot$coastDist,
     xlab="DATE_ACQUIRED", ylab="coastDist [m]",
     main = paste0('coastline position: ',twoD_pos, ' [m]'))
points(as.Date(subset2d_for_testPlot[subset2d_for_testPlot$coast_outlier == 0, ]$DATE_ACQUIRED),
       subset2d_for_testPlot[subset2d_for_testPlot$coast_outlier == 0, ]$coastDist,
       col = 'red')
lines(as.Date(subset2d_for_testPlot$DATE_ACQUIRED),  subset2d_for_testPlot$coast_median,
       col = 'blue')
# plot(mudbanks_select$pos, mudbanks_select$locf)
# points(mudbanks_select$pos, mudbanks_select$locf, col = 'red')
# 


#' filter mudbank boundary outliers
#' 
#' - Filter on neighborhood (previous 2, current and next two points)
#'     Use distance to see if the point is deviating
#'     Consider using fraction..

# good example dates: 2017-09-02, "2018-02-27", 2018-08-28, 2018-09-27

# set outlier: assume nothing is an outlier
mudbanks$mudbank_outlier <- 0


for (i in uniqueDates){
  # i <- uniqueDates[3]
  # i <- uniqueDates[uniqueDates == as.Date('2008-11-12')]
  
  # select relevant observations
  mudbanks_selection <-subset(mudbanks, mudbanks$DATE_ACQUIRED == i & 
                                mudbanks$axisDist >= 0 & 
                                mudbanks$mudbank_distance >= 0)
  
  # give all excluded observation an 1 for outlier in original
  # to keep track of all outliers.
  # - negative values (so there are cases where negative mudbank distances
  #   are actually correct)
  
  mudbanks[which(mudbanks$DATE_ACQUIRED == i & 
                   mudbanks$axisDist < 0 &
                   mudbanks$mudbank_distance < 0), "mudbank_outlier"] <-
    as.data.frame(mudbanks[
      which(mudbanks$DATE_ACQUIRED == i & 
              mudbanks$axisDist < 0 &
              mudbanks$mudbank_distance < 0),"mudbank_outlier"])[,1] + 1
  
  # order by position
  mudbanks_selection<-mudbanks_selection[order(mudbanks_selection$pos),]



  #'
  #' test for obvious outliers on full dataset
  #' 
  
  positions_all <- as.numeric(as.character(mudbanks_selection$pos))
  distances_all <-mudbanks_selection$mudbank_distance # grab the normalized distances
  
  # second order polynomial fit: alongshore position & polynomial fit
  lm_out_all <-lm(distances_all ~ poly(as.numeric(positions_all),2))
  
  predicted.intervals <- predict(lm_out_all,
                                 data.frame(x=as.numeric(positions_all)),
                                 interval='confidence', level=0.99)
  
  # Bonferroni-adjusted mudbank_outlier test (test largest absolute standardized residual)
  outlier_test <- car::outlierTest(lm_out_all) 
  outlier_ind<-as.numeric(names(outlier_test$rstudent))
  
  # Give mudbank_outlier +1 in the source file to keep track
  mudbanks[which(mudbanks$DATE_ACQUIRED == i & 
             mudbanks$pos %in% positions_all[outlier_ind]), "mudbank_outlier"] <-
    as.data.frame(mudbanks[
      which(mudbanks$DATE_ACQUIRED == i & 
              mudbanks$pos %in% positions_all[outlier_ind]),"mudbank_outlier"])[,1] + 1
  
  outlier_selection <- mudbanks_selection[outlier_ind,]
  
  # drop the most obvious outliers from the selection
  mudbanks_selection <- mudbanks_selection[-outlier_ind, ]
  
  # plot
  # plot(positions_all, distances_all, main = paste0(i),
  #      xlab = 'alongshore position', ylab = 'cross shore dist')
  # lines(positions_all, predicted.intervals[,1],col='green',lwd=3)
  # points(positions_all[outlier_ind],distances_all[outlier_ind], col = 'red')

  # # sanity check:
  # points( as.numeric(as.character(mudbanks_selection$pos)), mudbanks_selection$mudbank_distance, col = 'blue')
  # # spatial plots
  # mapview(mudbanks_selection, col.regions = c("green"), layer.name = 'mudbanks' ) +
  #   mapview(outlier_selection, col.regions =c('orange'), layer.name = 'outliers')
  # 
  
  
  # test outlier detection for fractions
  # plot(mudbanks_selection$axisDist, mudbanks_selection$mudFract)
  # population.cov <- cov(mudbanks_selection$axisDist, mudbanks_selection$mudFract)
  
  # # change coordinates to meters: seems to be a requirement for radius
  # mudbanks_selection_transformed <- spTransform(as(mudbanks_selection, 'Spatial'), 
  #             CRS(as.character("+proj=utm +zone=21 +ellps=intl +towgs84=-265,120,-358,0,0,0,0 +units=m +no_defs")))
  # 
  # # number of near neighbours
  # # dnn <- RANN::nn2(coordinates(mudbanks_selection_transformed), searchtype="radius", 
  # #           radius = 4000)$nn.idx
  # # 
  # # var.fract<- rep(NA,nrow(mudbanks_selection_transformed))
  # # z.fract <- rep(NA,nrow(mudbanks_selection_transformed)) 
  # # covVar.fract <- rep(NA,nrow(mudbanks_selection_transformed))
  # # 
  # # for(i in 1:nrow(dnn)){
  # #   # i<-1
  # #   dnn.idx <- dnn[i,]
  # #   # variance; degree of spread comapred to local neighbourhood observations
  # #   var.fract[i] <- var( mudbanks_selection_transformed[dnn.idx[dnn.idx != 0],]$mudFract, na.rm=TRUE)
  # #   
  # #   # covariance local neighbourhood
  # #   # exotext to be negative (greater mud Fract should result in lower dist)
  # #   covVar.fract[i] <-cov(mudbanks_selection_transformed[dnn.idx[dnn.idx != 0],]$mudFract,
  # #                         mudbanks_selection_transformed[dnn.idx[dnn.idx != 0],]$coastDist)
  # #   
  # #   #The modified z score is a standardized score that measures outlier strength 
  # #   # or how much a particular score differs from the typical score
  # #   z.fract[i] <- spatialEco::outliers(mudbanks_selection_transformed[dnn.idx[dnn.idx != 0],]$mudFract )[1]
  # # }
  # # # remove NA
  # # z.fract[!is.finite(z.fract)] <- 0 
  # 
  # 
  # # mudbanks_selection_sp <- as(mudbanks_selection, 'Spatial') 
  # # mudbanks_selection_transformed$var.fract <- var.fract
  # # spplot(mudbanks_selection_transformed, "var.fract", col.regions=cm.colors(10))
  # 
  # # mudbanks_selection_transformed$z.fract <- z.fract
  # # spplot(mudbanks_selection_transformed, "z.fract", col.regions=cm.colors(10))
  # 
  # # cooks'd >4/n & MORAN's I < 0
  # 
  # # plot(mPlot$x, mPlot$cook.d)
  # # points(cookd$x, cookd$cook.d, col = 'red')
  # 
  # # coocks D > n/4
  # # points(mudbanks_selection_transformed$x[which(mPlot$cook.d > 4/nrow(mPlot))],
  # #        mudbanks_selection_transformed$y[which(mPlot$cook.d > 4/nrow(mPlot))],
  # #        col = 'blue')
  # 
  # # plot(mudbanks_selection_transformed$x, mudbanks_selection_transformed$y, 
  # #      main = paste0(i),
  # #      xlab = 'x', ylab = 'y')
  # # points(mudbanks_selection_transformed$x[which( mudbanks_selection_transformed$z.fract > 9)],
  # #        mudbanks_selection_transformed$y[which( mudbanks_selection_transformed$z.fract > 9)],
  # #        col = 'red')
  # # points(mudbanks_selection_transformed$x[which(mPlot$cook.d > 4/nrow(mPlot))],
  # #        mudbanks_selection_transformed$y[which(mPlot$cook.d > 4/nrow(mPlot))],
  # #        col = 'blue')
  # 
  # # Local Morgan's I (LISA) and Cookds distnance
  # library(spdep)
  # 
  # 
  # # nearest neighbours and the resulting eucledian distances 
  # # to ensure no empty sets of neighbourhoods are returned
  # all.linked <- max(unlist(nbdists(knn2nb(knearneigh(coordinates(mudbanks_selection_transformed))), 
  #                                  coordinates(mudbanks_selection_transformed))))
  # nb <- dnearneigh(mudbanks_selection_transformed, 0, all.linked) # the neighbourhoods
  # mI <- localmoran(mudbanks_selection_transformed@data[,"mudFract"], nb2listw(nb, style="W"))
  #   # https://geodacenter.github.io/workbook/6a_local_auto/lab6a.html
  # 
  # 
  # # if (require(ggplot2, quietly=TRUE)) {
  # #   xname <- attr(mPlot, "xname")
  # #   ggplot(mPlot, aes(x=x, y=wx)) + geom_point(shape=1) + 
  # #     geom_smooth(formula=y ~ x, method="lm") + 
  # #     geom_hline(yintercept=mean(mPlot$wx), lty=2) + 
  # #     geom_vline(xintercept=mean(mPlot$x), lty=2) + theme_minimal() + 
  # #     geom_point(data=mPlot[mPlot$is_inf,], aes(x=x, y=wx), shape=9) +
  # #     geom_text(data=mPlot[mPlot$is_inf,], aes(x=x, y=wx, label=labels, vjust=1.5)) +
  # #     xlab(xname) + ylab(paste0("Spatially lagged ", xname))
  # # }
  # 
  # # Moran I plot
  # mPlot <- moran.plot(mudbanks_selection_transformed@data[,"mudFract"], 
  #                     nb2listw(nb, style="W"))
  # # cookd <- mPlot[which(mPlot$cook.d > 4/nrow(mPlot)),]
  # # cookd05 <- mPlot[which(mPlot$cook.d > .5),]
  # mI_negative <- mPlot[which(mI[,'Ii'] < 0),]
  # # 
  # # points(cookd$x, cookd$wx, col = 'red')
  # # points(cookd05$x, cookd05$wx, col = 'blue')
  # points(mI_negative$x, mI_negative$wx, col = 'orange')
  # 
  # 
  # # build new data frame from outputs
  # LocalI <- mudbanks_selection_transformed
  # LocalI@data <- data.frame(ID=rownames(LocalI@data), as.data.frame(mI), 
  #                           as.data.frame(mudbanks_selection_transformed[ ,c('pos', 'axisDist', 'mudFract')]),
  #                           is_inf = mPlot$is_inf)
  # names(LocalI@data)[6] <- "Pr"
  # spplot(LocalI, "Z.Ii", xlab="Local Morans-I", col.regions=topo.colors(30))   
  # 
  # # cat(nrow( LocalI[LocalI@data[,"Pr"] < 0.05 ,]), "obs of", 
  #     # nrow(LocalI), "are significant at p=0.05","\n")
  # 
  # # plot(LocalI, pch=19)
  # # points(LocalI[which(LocalI$Pr <= 0.01),], pch=19,col="red") # significant values
  # # points(LocalI[which(LocalI$Ii < 0),], pch=19,col="blue")    # negative Moran I
  # 
  # # significant observations with large variance
  # LocalI@data <- data.frame(LocalI@data, HotSpots=ifelse(mI[,5] <= 0.01 & mI[,4] >= mean(mI[,4]), 1, 0) )
  # LocalI@data$HotSpots <- as.factor(LocalI@data$HotSpots)
  # # LocalI@data <- data.frame(LocalI@data, CoolSpots= ifelse(mI[,1] < 0 & mI[,5] <= 0.1, 1, 0))
  # # No of the negative Mi values appears to be significant (p < 0.1)
  # 
  # spplot(LocalI, "HotSpots", xlab="Local Moran’s-I Hot Spots", col.regions=c("blue","red") )
  # # spplot(LocalI, "CoolSpots", xlab="Local Moran’s-I Hot Spots", col.regions=c("blue","red") )
  # 
  # # back to sf object for mapView
  # localI_sf <- st_as_sf(LocalI)
  # mapView(localI_sf,  zcol = "mudFract") + 
  #   mapView(LocalI[which(LocalI$Ii < 0 & LocalI$is_inf),],
  #           col.regions = 'red', layer.name = c('selected Negative Moran I')) +
  #   mapView(LocalI[which(LocalI$Ii < 0),],
  #           col.regions = 'blue', layer.name = c('all Negative Moran I')) +
  #   # mapView(localI_sf[which( mudbanks_selection_transformed$z.fract > 9),], 
  #           # col.regions = 'orange', layer.name = c('Large Z')) +
  #   mapView(localI_sf[which( LocalI$HotSpots == 1),], 
  #           col.regions = 'green', layer.name = c('large significant variance')) +
  #   mapView(localI_sf[which( mPlot$is_inf),], 
  #           col.regions = 'purple', layer.name = c('Moran I influential Points'))
  #   
  # # x <- expand.grid(1:20, 1:5)[,1]
  # # y <- expand.grid(1:20, 1:5)[,2]
  # # z <- cbind(rmvn.spa(x=x, y=y, p=2, method="exp"),
  # #            rmvn.spa(x=x, y=y, p=2, method="exp"))
  # 
  # # ( part.corr <- ncf::lisa(x=mudbanks_selection_transformed$x, 
  # #                             y=mudbanks_selection_transformed$y, 
  # #                             z=mudbanks_selection_transformed$mudFract, 
  # #                             neigh=100) )
  # # plot(part.corr$coord$x, part.corr$coord$y)
  # # 
  # # points(part.corr$coord$x[which(part.corr$p<0.05)], 
  # #        part.corr$coord$y[which(part.corr$p<0.05)], 
  # #        pch=19,col="red")
  # # 
  # # plot(part.corr)
  
  for (pnt in 1:nrow(mudbanks_selection)){
    # pos_of_interst <- 120000 #81000
    # selected_point <-mudbanks_selection[which(mudbanks_selection$pos == pos_of_interst),]
    # pnt <- 1
    
    selected_point <-mudbanks_selection[pnt,]
    
    # temp <- subset(mudbanks_selection, )
    
    # row.names(mudbanks_selection[pnt,])
    # select nearby points
    ajoining_points <- subset(mudbanks_selection, 
                              as.character(pos) <=  as.numeric(as.character(selected_point$pos))+4000 &
                              as.character(pos) >= as.numeric(as.character(selected_point$pos))-4000 &
                              as.character(pos) != as.numeric(as.character(selected_point$pos)))
    
    combined <- rbind(selected_point,ajoining_points)
    
    # order by position
    combined_ordered <-combined[order(as.numeric(as.character(combined$pos))),]

    positions<- as.numeric(as.character(combined_ordered$pos))
    distances <-combined_ordered$mudbank_distance # grap the normalized distances
    fractions <- combined_ordered$mudFract
    datatest <- data.frame(positions=positions,distances=distances, fractions = fractions)

    # plot(positions,fractions, col ='red')
    # abline(lm(datatest$fractions~as.numeric(datatest$positions)),lty = 2)
    
    # plot(fractions, distances, col = 'blue')
    # abline(lm(datatest$distances~as.numeric(datatest$fractions)),lty = 2)

    # sufficient observations: determine outliers
    if (length(unique(datatest$distances)) > 2) {
      
      # # calculate linear fit
      lm.out_lin <- lm(datatest$distances~as.numeric(datatest$positions))
      
      # calculate second order polynomial 
      lm.out<-lm(datatest$distances ~ poly(as.numeric(datatest$positions),2))
      r2 <- summary(lm.out)$r.squared # or adjusted r2?
      p <- summary(lm.out)                # should be <0.05?
      fstat <- summary(lm.out)$fstatistic # 
      # or B-splines?
      # splines <- lm(datatest$distances ~ bs(as.numeric(datatest$positions), df = 10))
      

      pred.int <- predict(lm.out,data.frame(x=positions),
                          interval='confidence',level=0.99)
      # # plot 
      # plot(positions, distances)
      # abline(lm(datatest$distances~as.numeric(datatest$positions)),lty = 2)
      
      # plot(fitted(lm.out),residuals(lm.out))
      # lines(positions, pred.int[,1],col='green',lwd=3)
      
      intercept <-lm.out$coefficients[1]
      slope <- lm.out$coefficients[2]
      # potential benefit: slope says something about direction; negative slope; front of mudbank
      # low pos > further east, decreasing mudbank_distance ==> front of mud bank. 
      # only in case of mudbank
      
      # residuals
      resid <- lm.out$residuals
      maxResid <- which.max(abs(resid))
      
      # Bonferroni-adjusted outlier test (test largest absolute standardized residual)
      # or linear fit test
      test2 <- has_error(car::outlierTest(lm.out), silent = !interactive())
      test3 <- has_warning(car::outlierTest(lm.out))
      
      # Allways returns a observation (with largest Studentized residual)
      # Which implies it is not allways meeting the P < 0.05 cutoff value
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

    } else {
      # if only 0 or 1 observation; cannot determine if outlier based on neighbours
      # to continue workflow return an index outside the selected range
      outlierIndex <- min(as.numeric(as.character(mudbanks_selection$pos)))-1000
      
    }

    # test the selected position index of the outlier
    # if it is selected point of interest; consider it as outlier 
    if (outlierIndex == as.numeric(as.character(selected_point$pos))){
      
      mudbanks[which(mudbanks$DATE_ACQUIRED == selected_point$DATE_ACQUIRED & 
                       mudbanks$pos == selected_point$pos), "mudbank_outlier"] <-
      
      
      # mudbanks[which(row.names(mudbanks) == row.names(selected_point)), "mudbank_outlier"] <-
        as.data.frame(mudbanks[
          which(mudbanks$DATE_ACQUIRED == selected_point$DATE_ACQUIRED & 
                  mudbanks$pos == selected_point$pos) ,"mudbank_outlier"])[,1] + 1
    }
    
  }
  # redefine mudbank selection for plotting
  # mudbanks_selection <-subset(mudbanks, mudbanks$DATE_ACQUIRED == i &
  #                               mudbanks$axisDist >= 0 )
  # mudbank_selection_Outlier <- subset(mudbanks, mudbanks$DATE_ACQUIRED == i &
  #                                       mudbanks$axisDist >= 0 &
  #                                       mudbank_outlier >= 1)
  # 
  # testCoastline <-st_as_sf(SpatialPointsDataFrame(data.frame(mudbanks_selection$coastX, mudbanks_selection$coastY),
  #                               proj4string=CRS("+proj=longlat +datum=WGS84"),
  #                               data = data.frame(mudbanks_selection$pos)
  #                               ))
  # 
  # mapview(mudbanks_selection, col.regions = c("blue"), layer.name = c('non outlier')) +
  # mapview(mudbank_selection_Outlier, col.regions = c("red"), layer.name = c('outlier')) +
  #   mapview(testCoastline, col.regions = c('green'))
  
  
  # # redefine subset & ajoining points
  # # to ensure previously mudbank outliers (based on distance) can exluded
  # # newlySelected_point <-mudbanks_selection[which(mudbanks_selection$pos == 	114000),]
  # newlySelected_point <-mudbanks_selection[pnt,]
  # 
  # # select nearby points
  # new_ajoining_points <- subset(mudbanks_selection,
  #                           as.character(pos) <=  as.numeric(as.character(newlySelected_point$pos))+4000 &
  #                             as.character(pos) >= as.numeric(as.character(newlySelected_point$pos))-4000 &
  #                             as.character(pos) != as.numeric(as.character(newlySelected_point$pos)))
  # 
  # combined <- rbind(newlySelected_point,new_ajoining_points)
  # combined_ordered <-combined[order(as.numeric(as.character(combined$mudbank_distance))),]
  # 
  # col <- sort(rnorm(8))
  # 
  # qplot(combined_ordered$x, combined_ordered$y, data= combined_ordered,  colour=col) + 
  #   scale_colour_gradient(low="red", high="blue")
  # 
  # plot(combined_ordered$x, combined_ordered$y,col =  combined_ordered$Col)
  # plot(combined_ordered$mudbank_distance, combined_ordered$mudFract)
  # points(newlySelected_point$mudbank_distance, newlySelected_point$mudFract, col ='red')
  # 
  # # plot(combined$pos, combined$mudFract)
  # mapview(combined_ordered, col.regions = c("green"), layer.name = 'combined' ) +
  #   mapview(mudbanks_selection, col.regions =c('orange'), layer.name = 'mudbanks_selection')
  # 
  # 
  # # # calculate linear fit
  # lm.out_lin <- lm(combined_ordered$mudFract~as.numeric(combined_ordered$mudbank_distance))
  # 
  # # calculate second order polynomial 
  # lm.out<-lm(combined_ordered$mudFract ~ poly(as.numeric(combined_ordered$mudbank_distance),2))
  # r2 <- summary(lm.out)$r.squared # or adjusted r2?
  # p <- summary(lm.out)                # should be <0.05?
  # fstat <- summary(lm.out)$fstatistic # 
  # 
  # 
  # pred.int <- predict(lm.out,data.frame(x=combined_ordered$mudbank_distance),
  #                     interval='confidence',level=0.99)
  # # # plot 
  # plot(combined_ordered$mudbank_distance, combined_ordered$mudFract)
  # points(newlySelected_point$mudbank_distance, newlySelected_point$mudFract, col ='red')
  # abline(lm(combined_ordered$mudFract~as.numeric(combined_ordered$mudbank_distance)),lty = 2)
  # 
  # # plot(fitted(lm.out),residuals(lm.out))
  # lines(combined_ordered$mudbank_distance, pred.int[,1],col='green',lwd=3)
  # 
  # outlierFractions <- car::outlierTest(lm.out_lin, order = F, cutoff = 0.1)
  # 
  # outlierIndex <- ifelse(length(outlierFractions$rstudent) > 0,
  #                        combined_ordered[as.numeric(names(outlierFractions$rstudent)), ]$pos,
  #                        min(as.numeric(as.character(mudbanks_selection$pos)))-1000)
  # plot(combined_ordered[-2,]$mudbank_distance, combined_ordered[-2,]$mudFract)
  # 
  # # var(combined_ordered$mudFract, combined_ordered$axisDist)

  
}


for (year in unique(format(as.Date(uniqueDates), '%Y'))){
  # year <- 2005
  start_year <- as.Date(ISOdate(year, 1, 1))
  end_year <- as.Date(ISOdate(year, 12, 31)) 
  
  mudbanks_per_year <-subset(mudbanks, 
                             as.Date(DATE_ACQUIRED) >= start_year &
                             as.Date(DATE_ACQUIRED) <= end_year)

  write_csv(mudbanks_per_year, paste0(wd,"/data/processed/offshore_points/229_56", 
                                      '_', year, '_offshore.csv'))
  
  
}
