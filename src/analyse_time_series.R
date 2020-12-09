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

plot(as.Date(testPos$DATE_ACQUIRED),testPos$coastDist)
clicked <- identify(as.Date(testPos$DATE_ACQUIRED),testPos$coastDist,
                    n=1, labels = c(testPos$DATE_ACQUIRED))


# heatmap / space-time plot
library(lubridate) # for year/month etc.
library(dplyr)    # for mutate, such as adding cols
library(ggplot2)
library(viridisLite)



# # get for all transects an oldest coastline observation as baseline
allFiles$baseline <- 0 
reference_date <- c('')

for (sid in pos) {
  # sid = 297000
  # print(sid)
  i <- allFiles$pos == sid # create a logical index
  
  # get min date with an observation
  subsetTemp2 <- subset(allFiles, allFiles$pos == sid &
                          allFiles$coastDist >= 0)
  
  # get first date after reference date
  2018-06-01
  
  if(nrow(subsetTemp2) == 0){
    oldest_coastObs <- 0
  } else {
    oldest_coastObs <- subsetTemp2[subsetTemp2$DATE_ACQUIRED == min(subsetTemp2[, 'DATE_ACQUIRED']), 'coastDist']
  }
  
  
  allFiles$baseline[i] <- oldest_coastObs
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

# allFiles_mutate$baseline <- with(allFiles_mutate, ave(coastDist, date, FUN = min))

p <-ggplot(allFiles_mutate,aes(x = pos,y = full_date, fill=normalized)) +
  geom_tile(color= "white",size=0.1) +
  # scale_fill_gradientn(colours=topo.colors(7),#na.value = "transparent",
  #                      breaks=c(0,median(allFiles_mutate$coastDist)),
  #                      labels=c("Minimum","Maximum"),
  #                      limits=c(0,median(allFiles_mutate$coastDist)))
  scale_fill_viridis(name="Max Distance",option ="C", limits = c(-100, 1000), oob = scales::squish)


  # + scale_x_continuous(breaks =c(1,10,20,31))
  # scale_fill_viridis(name="cross shore dist",option ="C")

# define output matrices
# lines <- vector('list', length(uniqueX));
# allPoints <- vector('list', length(csv));
# df_coastDist = data.frame(matrix(NA, length(uniqueX), length(uniqueDates)+1),
#                           stringsAsFactors=F) # Empty data frame for coastal distance
# colnames(df_coastDist) <- c('pos', uniqueDates)
# 



