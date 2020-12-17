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

## ---------------------------
source("./src/functions.R")

## ---------------------------

mapviewOptions(basemaps = c( "Esri.WorldImagery","Esri.WorldShadedRelief", "OpenStreetMap.DE"))
dataFolder <- './data/raw'
years <- c('2018', '2019') 
# transect  <- readOGR(paste0(dataFolder, '/transects'), '2009_WnZ_transect')

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
filtered <- unique(filtered)[1:2,]

allFiles <- do.call(rbind, lapply(as.matrix(filtered)[,1], function(x) read.csv(x, stringsAsFactors = FALSE,
                                                                                sep = ',', na.strings=c("","NA")
                                                                                )))

col_dates <- col_of_interest(allFiles, 'DATE_ACQUIRED$')
col_coastDist <- col_of_interest(allFiles, 'coastDist$')

# all unique dates
uniqueDates <- unique(allFiles[,col_dates]);

# all unique transect (id's)
pos <- unique(allFiles[, col_of_interest(allFiles, 'pos$')]);
uniqueX<- unique(allFiles[, col_of_interest(allFiles, 'originX$')]);
uniqueY<- unique(allFiles[, col_of_interest(allFiles, 'originY$')]);
geo<- unique(allFiles[, col_of_interest(allFiles, '.geo')]);

# test simple 2d plot coastline dist
pos_to_test <- c('236000') # 236000: loss example / 187000: gain example with clear outliers
testPos <- subset(allFiles, allFiles[,col_of_interest(allFiles, 'pos')] == pos_to_test
                    & allFiles[,col_of_interest(allFiles, 'coastDist$')] >= 0 )
testPos <- testPos[order(testPos$DATE_ACQUIRED),] #order by date

# or on all entries 
allFiles_gt0 <- subset(arrange(allFiles, pos, DATE_ACQUIRED), coastDist >=0) 

# make groups of 3 months per transect
test_allFiles <- allFiles_gt0 %>%  
  mutate(date_col = as.POSIXct(cut(lubridate::date(allFiles_gt0$DATE_ACQUIRED), "3 months"))) %>%
  mutate(year_col = as.POSIXct(cut(lubridate::date(allFiles_gt0$DATE_ACQUIRED), "3 year"))) 

group_dates<-unique(test_allFiles$year_col)
group_pos <- unique(test_allFiles$pos)

# assume nothing is outlier
test_allFiles$outlier <- 1

# estimate outliers
for(i in group_dates){
  # i<-group_dates[group_dates == c("2017-01-01")]
  
  for(q in group_pos){
    # q <- group_pos[group_pos == pos_to_test]
    subsets <- subset(test_allFiles, year_col == i & pos == q)
    # plot(as.Date(subsets$DATE_ACQUIRED), subsets$coastDist)
    rownr <- strtoi(rownames(subset(test_allFiles, year_col == i & pos == q)))
    
    # detect outliers (give them a 0!!!)
    test_allFiles[rownr, 'outlier'] <- rosner(subsets$coastDist)
    
  }
}


# plot per group after removing outliers
# check i group_by actually removes the outliers!
test_allFiles_mn <- test_allFiles %>% group_by(pos, date_col, outlier) %>%
  mutate(mn = median(coastDist)) #%>%
  # ungroup()


testSubset <- subset(test_allFiles_mn, 
                     pos == pos_to_test)

outliers <- subset(testSubset, outlier == 0)
nonOutliers <- subset(testSubset, outlier == 1)

plot(as.Date(nonOutliers$DATE_ACQUIRED), nonOutliers$coastDist, ylim = c(min(testSubset$coastDist)-30,max(testSubset$coastDist)+ 30))
points(as.Date(outliers$DATE_ACQUIRED), outliers$coastDist, col = 'red')

# median values with steps per 3 months ()
points(as.Date(nonOutliers$date_col), nonOutliers$mn, col = 'blue')

# heatmap / space-time plot

# # get for all transects an oldest coastline observation as baseline
test_allFiles_mn$baseline <- 0 
test_allFiles_mn$slope <- -1
reference_date <- as.Date("2017-01-01")

for (sid in pos) {
  # sid = pos_to_test
  # print(sid)
  i <- test_allFiles_mn$pos == sid # create a logical index
  
  # get min date with an observation
  subsetTemp2 <- subset(test_allFiles_mn, test_allFiles_mn$pos == sid &
                          test_allFiles_mn$coastDist >= 0) 
                          # test_allFiles_mn$outlier != 0) # exclude detected outliers 
  outliers <- subset(subsetTemp2, outlier == 0)
  nonOutliers <- subset(subsetTemp2, outlier == 1)

  # plot(as.Date(subsetTemp2$DATE_ACQUIRED), subsetTemp2$coastDist,ylim = c(min(subsetTemp2$coastDist)-30,max(subsetTemp2$coastDist)+ 30))
  # points(as.Date(nonOutliers$DATE_ACQUIRED), nonOutliers$coastDist, col = 'green')
  # points(as.Date(outliers$DATE_ACQUIRED), outliers$coastDist, col = 'red')
  
  # you'd want to normalize for the coastline position around the reference date
  # get first date after reference date:
  index <- which.min(abs(as.Date(nonOutliers$DATE_ACQUIRED)-reference_date))
  
  if(nrow(subsetTemp2) == 0){
    coastObs <- 0
    slope <- 0
    } else {
    coastObs <- subsetTemp2[index, 'coastDist'] 
    
    lm.out <- lm(nonOutliers$coastDist~as.numeric(as.Date(nonOutliers$DATE_ACQUIRED)))
    intercept <-lm.out$coefficients[1]
    slope <- lm.out$coefficients[2]
    
    # residuals
    resid <- lm.out$residuals
    
    maxResid <- which.max(abs(resid))
    
    # points(as.Date(nonOutliers$DATE_ACQUIRED[maxResid]), nonOutliers$coastDist[maxResid], col = 'purple')
    
    # firstSubset <- nonOutliers[1:maxResid,]
    # secondSubset <- nonOutliers[maxResid:nrow(nonOutliers),]
    # 
    # firstSubset_lm <- lm(firstSubset$coastDist~as.numeric(as.Date(firstSubset$DATE_ACQUIRED)))
    # secondSubset_lm <- lm(secondSubset$coastDist~as.numeric(as.Date(secondSubset$DATE_ACQUIRED)))
    
    # firstSubset_est <- firstSubset_lm$coefficients[1] + as.numeric(as.Date(nonOutliers$DATE_ACQUIRED)) * firstSubset_lm$coefficients[2]
    
    
    estimated <- intercept + (as.numeric(as.Date(nonOutliers$DATE_ACQUIRED))*slope)
    # plot the fitted line
    # abline(lm(nonOutliers$coastDist~as.numeric(as.Date(nonOutliers$DATE_ACQUIRED))),
           # lty = 2)
    # abline(lm(firstSubset$coastDist~as.numeric(as.Date(firstSubset$DATE_ACQUIRED))), 
    #        lty = 2)
    # abline(lm(secondSubset$coastDist~as.numeric(as.Date(secondSubset$DATE_ACQUIRED))), 
           # lty = 2)
    
    # use identify when necesary!
    # date <- identify(as.Date(subsetTemp2$DATE_ACQUIRED), subsetTemp2$coastDist, n=1, labels=as.Date(subsetTemp2$DATE_ACQUIRED))
    
    # coastObs <- subsetTemp2[subsetTemp2$DATE_ACQUIRED == min(subsetTemp2[, 'DATE_ACQUIRED']), 'coastDist']
  }
  
  
  test_allFiles_mn$baseline[i] <- as.numeric(coastObs)
  test_allFiles_mn$slope[i] <- as.numeric(slope)
}


# subtract each that values from each obs (ensure positive values)
# to normalize
test_allFiles_mn$normalized <- test_allFiles_mn$coastDist - test_allFiles_mn$baseline
test_allFiles_mn$mn_normalized <- test_allFiles_mn$mn - test_allFiles_mn$baseline

allFiles_mutate <- test_allFiles_mn %>% mutate(year = year(DATE_ACQUIRED),
                    month = month(DATE_ACQUIRED, label=TRUE),
                    day = day(DATE_ACQUIRED),
                    full_date= date(DATE_ACQUIRED),
                    full_date2= date(date_col))


allFiles_mutate <- allFiles_mutate %>% 
                      filter(!(coastDist == -1) & outlier == 0) # filter outliers & negative coastal distances

labeled.dat <- allFiles_mutate[allFiles_mutate$pos %in% c('151000') ,]



p <-ggplot(allFiles_mutate,aes(x = pos,y = full_date2, fill=mn_normalized))+ 
  geom_tile(color= "white",size=0.1) +
  # scale_fill_gradientn(colours=topo.colors(7),#na.value = "transparent",
  #                      breaks=c(0,median(allFiles_mutate$coastDist)),
  #                      labels=c("Minimum","Maximum"),
  #                      limits=c(0,median(allFiles_mutate$coastDist)))
  scale_fill_viridis(name="Max Distance",option ="C", limits = c(-500, 500), oob = scales::squish)
  # geom_text(data = labeled.dat, aes(pos,full_date, label = pos), hjust = 2)



# library(plotly)
# library(grid)
# ggplotly(p)
#  
# # downViewport('panel.3-4-3-4') 
# pushViewport(dataViewport(x, y, c(0,10), c(0.1, 0.5))) 
# pick<-grid.locator('in')
# pick.n <- as.numeric(pick)
# view.x <- as.numeric(convertX( unit(x,'native'), 'in' ))
# view.y <- as.numeric(convertY( unit(y,'native'), 'in' ))
# w <- which.min((view.x-pick.n[1])^2 + (view.y-pick.n[2])^2)
# 
# 
# ggidentify <- function (x, y, labels, xscale=NULL, yscale=NULL) { 
#   depth <- downViewport('ROOT')
#   pushViewport(dataViewport(x,y, xscale, yscale))
#   pick <- grid.locator('in')
#   while(!is.null(pick)) {
#     pick.n <- as.numeric(pick)
#     view.x <- as.numeric(convertX( unit(x,'native'), 'in' ))
#     view.y <- as.numeric(convertY( unit(y,'native'), 'in' ))
#     d <- min( (view.x-pick.n[1])^2 + (view.y-pick.n[2])^2 )
#     w <- which.min((view.x-pick.n[1])^2 + (view.y-pick.n[2])^2)
#     if (d>0.1) {
#       print("Closest point is too far")
#     } else {  
#       popViewport(n=1)
#       upViewport(depth)
#       print(last_plot() + annotate("text", label=labels[w], x = x[w], y = y[w], 
#                                    size = 5, hjust=-0.5, vjust=-0.5))
#       depth <- downViewport('panel.3-4-3-4')
#       pushViewport(dataViewport(x,y, xscale, yscale))
#     }
#     pick <- grid.locator('in')
#   }
#   popViewport(n=1)
#   upViewport(depth)
# }
# 









# https://www.statsandr.com/blog/outliers-detection-in-r/#:~:text=An%20outlier%20is%20a%20value,significantly%20from%20other%20data%20points.
# K <- length(testPos$coastDist)-2
# if(K > 10){
#   K <- 9
# }
# Rtest <- rosnerTest(testPos$coastDist,
#                    k = length(testPos$coastDist) - 2
#                    # k = 10 or if length < 10 it should be 
#                    # this value makes a difference.... apply to blocks of 3 years ==> so outlier size depends on amount of images in those 3 years?
# )
# Rtest2 <- rosner(testPos$coastDist)
# 
# indices <- Rtest$all.stats$Obs.Num[which(Rtest$all.stats$Outlier)]
# 
# # if outlier give 1 in a new column
# testPos$outlier <- 0 # TRUE
# testPos$outlier[indices] <- 1 # FALSE
# # plot outliers
# points(as.Date(testPos$DATE_ACQUIRED[testPos$outlier == 0]),testPos$coastDist[testPos$outlier == 0], col = 'red')
