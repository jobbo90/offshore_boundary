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
# setwd("I:/BackUp_D_mangroMud_202001/Research/Software/Projects/offshore_boundary")


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

dates <- col_of_interest(allFiles, 'DATE_ACQUIRED$')
coastDist <- col_of_interest(allFiles, 'coastDist$')

# all unique dates
uniqueDates <- unique(allFiles[,dates]);

# all unique transect (id's)
pos <- unique(allFiles[, col_of_interest(allFiles, 'pos$')]);
uniqueX<- unique(allFiles[, col_of_interest(allFiles, 'originX$')]);
uniqueY<- unique(allFiles[, col_of_interest(allFiles, 'originY$')]);
geo<- unique(allFiles[, col_of_interest(allFiles, '.geo')]);

# test simple 2d plot coastline dist
pos_to_test <- c('199000')
testPos <- subset(allFiles, allFiles[,col_of_interest(allFiles, 'pos')] == pos_to_test 
                    & allFiles[,col_of_interest(allFiles, 'coastDist$')] >= 0 )
testPos <- testPos[order(testPos$DATE_ACQUIRED),] #order by date


# # outlier detection: quantiles (tricky for large temporal range?)
# lower_bound <- quantile(testPos$coastDist, 0.025)
# upper_bound <- quantile(testPos$coastDist, 0.975)
# # # indices that are between quantiles
# ind <- which(testPos$coastDist > lower_bound & testPos$coastDist < upper_bound)

# plot date & coastline Distance
plot(as.Date(testPos$DATE_ACQUIRED),testPos$coastDist)

# outlier detection: rosnerTest
# remove outliers based on a interval of 1 -3 years?
# alternatively you could iterate over x amount of observations. e.g. every 15 observations, do a outlier test

# https://www.statsandr.com/blog/outliers-detection-in-r/#:~:text=An%20outlier%20is%20a%20value,significantly%20from%20other%20data%20points.
Rtest <- rosnerTest(testPos$coastDist,
                   k = 10 
                   # k = 10 or if length < 10 it should be 
                   # this value makes a difference.... apply to blocks of 3 years ==> so outlier size depends on amount of images in those 3 years?
)
indices <- Rtest$all.stats$Obs.Num[which(Rtest$all.stats$Outlier)]

# if outlier give 1 in a new column
testPos$outlier <- 0 # TRUE
testPos$outlier[indices] <- 1 # FALSE

# new<- testPos %>%
#   filter(!row_number() %in% indices) # remove indices that correspond to outliers from rosnerTest
plot(as.Date(testPos$DATE_ACQUIRED[testPos$outlier == 0]),testPos$coastDist[testPos$outlier == 0])

# cut in block of so many months?
testZoo <- as.POSIXct(cut(date(testPos$DATE_ACQUIRED), "3 months"))

# how to exclude outliers from the aggregate??
# https://datascienceplus.com/aggregate-data-frame-r/
testpos2<-merge(testPos, aggregate(testPos$coastDist, list(testZoo), median), suffixes = c("dateGroup", ".mean"))# https://stackoverflow.com/questions/38380938/aggregate-conditional-statements
# plot per group after removing outliers
testPos <- merge(testPos, aggregate(testPos$coastDist, list(testZoo), median),suffixes = c("dateGroup", ".mean"))
plot(testPos$Group.1, testPos$x)

# heatmap / space-time plot
library(lubridate) # for year/month etc.
library(dplyr)    # for mutate, such as adding cols
library(ggplot2)
# library(viridisLite)
library(viridis) # for visualization


# # get for all transects an oldest coastline observation as baseline
allFiles$baseline <- 0 
reference_date <- as.Date("2018-09-01")

for (sid in pos) {
  # sid = 297000
  # print(sid)
  i <- allFiles$pos == sid # create a logical index
  
  # get min date with an observation
  subsetTemp2 <- subset(allFiles, allFiles$pos == sid &
                          allFiles$coastDist >= 0)
  
  # get first date after reference date
  index <- which.min(abs(as.Date(subsetTemp2$DATE_ACQUIRED)-reference_date))
  
  # you'd want to normalize for the mean position around the reference date to avoid
  # outliers have to much effect
  # option 1: filter outliers
  # option 2: moving average
  
  if(nrow(subsetTemp2) == 0){
    coastObs <- 0
  } else {
    coastObs <- subsetTemp2[index, 'coastDist'] 
    # coastObs <- subsetTemp2[subsetTemp2$DATE_ACQUIRED == min(subsetTemp2[, 'DATE_ACQUIRED']), 'coastDist']
  }
  
  
  allFiles$baseline[i] <- coastObs
}


# normalization needs to be changed:
# all transects need to have 0 at the same date in order to visualized the differences
# find a date, set given date to 0, all other dates need to have the same value subtracted



# subtract each that values from each obs (ensure positive values)
# to normalize
allFiles$normalized <- allFiles$coastDist - allFiles$baseline

allFiles_mutate <- allFiles %>% mutate(year = year(DATE_ACQUIRED),
                    month = month(DATE_ACQUIRED, label=TRUE),
                    day = day(DATE_ACQUIRED),
                    full_date= date(DATE_ACQUIRED))


allFiles_mutate <- allFiles_mutate %>% 
                      select(pos,day,month,year,full_date,coastDist, normalized) %>%
                      filter(!(coastDist == -1)) # & contbr_city == 'APO AE'

labeled.dat <- allFiles_mutate[allFiles_mutate$pos %in% c('151000') ,]



p <-ggplot(allFiles_mutate,aes(x = pos,y = full_date, fill=normalized))+ 
  geom_tile(color= "white",size=0.1) +
  # scale_fill_gradientn(colours=topo.colors(7),#na.value = "transparent",
  #                      breaks=c(0,median(allFiles_mutate$coastDist)),
  #                      labels=c("Minimum","Maximum"),
  #                      limits=c(0,median(allFiles_mutate$coastDist)))
  scale_fill_viridis(name="Max Distance",option ="C", limits = c(-500, 500), oob = scales::squish)
  # geom_text(data = labeled.dat, aes(pos,full_date, label = pos), hjust = 2)



  # + scale_x_continuous(breaks =c(1,10,20,31))
  # scale_fill_viridis(name="cross shore dist",option ="C")

# define output matrices
# lines <- vector('list', length(uniqueX));
# allPoints <- vector('list', length(csv));
# df_coastDist = data.frame(matrix(NA, length(uniqueX), length(uniqueDates)+1),
#                           stringsAsFactors=F) # Empty data frame for coastal distance
# colnames(df_coastDist) <- c('pos', uniqueDates)
# 



