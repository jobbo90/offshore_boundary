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

# mapviewOptions(basemaps = c( "Esri.WorldImagery","Esri.WorldShadedRelief", "OpenStreetMap.DE"))
dataFolder <- './data/processed'
aoi <- c('FrenchGuiana', 'Suriname', 'Guyana')  #'FrenchGuiana', 'Suriname', 'Guyana'
years <- seq(from = 1985, to = 2021, by = 1)

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
               "Guyana" = posToExcludeGUY )

# groups for Suriname
posWest <- c(seq(0 ,130000,1000))
posEast <- c(seq(234000 , 380000 ,1000))
posCentre <- c(seq(131000 , 233000 ,1000))
surList = list('posWest' = posWest,
               'posEast' = posEast,
               'posCentre' = posCentre)

# groups For Guyana
Barima <- c(seq(0 ,223000,1000))
supernaam <- c(seq(224000 , 316000 ,1000))
georgetown <- c(seq(317000 , 460000 ,1000)) # georgetow - berbice
berbice <- c(seq(461000, 532000, 1000)) # berbice - corenteyne

guyList = list('Barima' = Barima,
               'supernaam' = supernaam,
               'georgetown' = georgetown,
               'berbice' = berbice)

# groups for french-guiana
maroni <- c(seq(0 ,110000,1000)) # maroni sinamary
sinermary <- c(seq(111000 , 207000 ,1000)) # sinermary - cayenne
cayenne <- c(seq(208000 , 403000 ,1000)) # cayene - brazil

fgList = list('maroni' = maroni,
              'sinermary' = sinermary,
              'cayenne' = cayenne)

reference_date <- as.Date("2006-01-01")
normalize_date <- as.Date("1986-01-01")

# select folders
folderSelect <- as.matrix(list.files(paste0(dataFolder), full.names = T))
df <- rewrite(folderSelect);
# only csv's
df <- df[grep('.csv', folderSelect, ignore.case = T),]


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

# allFiles = bind_rows(allFilesFG, allFilesSur,allFilesGuy)
allFiles <- unique(do.call(rbind, lapply(as.matrix(filtered)[,1],
                                         function(x) read.csv(x,
                                                              stringsAsFactors = FALSE,
                                                              sep = ',',
                                                              na.strings=c("","NA")
                                         ))))

names(allFiles)[names(allFiles) == 'pos.x'] <- 'pos'
names(allFiles)[names(allFiles) == 'year_col.x'] <- 'year_col'

# remove duplicate observations to have accurate stats
# these are the triplicates?
allFiles <- type_convert(allFiles)


#---------------------------
#'
#' Filter 
#' 
#---------------------------
maxPosFG <- max(allFiles$pos[which(allFiles$Country == "FrenchGuiana")])
maxPosSur <- max(allFiles$pos[which(allFiles$Country == "Suriname")])
maxPosGuy <- max(allFiles$pos[which(allFiles$Country == "Guyana")])

# drop transects near  river mouths
allFiles_dropPOS <- allFiles %>%
  # accumulate all coastline positions to compute alongshore distance guianas
  dplyr::mutate(alongshorePos =  ifelse(Country == "FrenchGuiana",
                                        maxPosFG-pos,0),
                alongshorePos = ifelse(Country == "Suriname",
                                       (maxPosSur-pos)+maxPosFG+1000,
                                       alongshorePos),
                alongshorePos = ifelse(Country == "Guyana",
                                       (maxPosGuy-pos)+maxPosFG+maxPosSur+1000,
                                       alongshorePos)
                ) %>%
  
  dplyr::mutate(toFilter = 0) %>%
  dplyr::mutate(toFilter = ifelse((Country == "Suriname" & 
                                    pos %in% posToExcludeSUR),1,toFilter),
                toFilter = ifelse((Country == "FrenchGuiana" & 
                                    pos %in% posToExcludeFG),1,toFilter),
                toFilter = ifelse((Country == "Guyana" & 
                                    pos %in% posToExcludeGUY),1,toFilter)) %>%
  filter(toFilter == 0) %>%
  dplyr::select(-c(toFilter)) %>%
  dplyr::select(c(Country, year_col, alongshorePos, pos,distX, distY))


####################### 
#' 
#'  
#' link coastline positions to wave data and export as intermediate output
#' 

# wave power database
folderWavePower <- list.files(
  paste0('D:/WOTRO/Research/Software/Projects/offshore_boundary/data/raw/ERA5_waves_yearly'), 
  full.names = T, pattern = '.csv$', recursive = T)

# dfWavePower <- folderWavePower[grep('.csv',folderWavePower , ignore.case = T),]

testRead <-readr::read_csv(folderWavePower)

wavePower <- unique(do.call(rbind, lapply(as.matrix(folderWavePower)[,1],
                                function(x) read.csv(x,
                                                     stringsAsFactors = FALSE,
                                                     sep = ',',
                                                     na.strings=c("","NA")
                                                      ))))

wavePower <- type_convert(wavePower)


# id <- 45
# fileToOverWrite = as.matrix(folderWavePower)[id,1]
# 
# csvToread = read.csv(fileToOverWrite,
#          stringsAsFactors = FALSE,
#          sep = ',',
#          na.strings=c("","NA"))
# 
# colnames(csvToread)
# 
# write_csv(csvToread %>%
#             dplyr::select(!c(expver)), fileToOverWrite)



wavePower <- wavePower %>%
  dplyr::mutate(date = as.Date(time, format = "%Y-%m-%d")) %>% # format = "%d-%m-%Y")
  dplyr::group_by(country,longitude,latitude) %>%
  dplyr::mutate(id = cur_group_id()) %>% # add a location ID for easy referencing
  ungroup()

# plotLocations <- ggplot(data=wavePower,
#                       aes(x=as.Date(date),y = swh,
#                           colour = country), alpha = 1) +
#   geom_line(aes(y=rollmean(swh, 15, na.pad=TRUE))) +
#   facet_wrap(~id)
#   # geom_point(size = 3, alpha = 0.6)

endSeason = 4 # month (inclusive)
startSeason = 12 # month

# locations ERA5 observations 
wavePowerLocations <- wavePower %>% 
  dplyr::distinct(country,longitude, latitude,id)

# for each coordinate get nearest position

wavePowerMutate <- wavePower %>%
  dplyr::mutate(year_col = as.Date(paste0(year, '-01-01')),
                # consistent names
                Country = ifelse(country == "French Guiana", 'FrenchGuiana',
                                 ifelse(country == "Guiana", 'Guyana',
                                        country)),
                month = as.numeric(format(date, "%m"))) %>%
  # create inside wave power 1 observation per season (storm - calm)
  dplyr::group_by(Country,longitude,latitude) %>%
  dplyr::mutate(yearNR =  cumsum(month ==12)) %>%
  dplyr::group_by(yearNR) %>%
  dplyr::mutate(
    modeYear = Mode(year), # for each group assign most occuring year
    season = ifelse(month == startSeason | month <= endSeason, # define season: december untill march
                paste0('storm'),
                paste0('calm'))) %>%
  ungroup() %>%
  dplyr::select(!c(country, yearNR,month ))

# plotSeasons <- ggplot(data=wavePowerSeasons,
#                       aes(x=as.Date(year_col),y = swh_storm,
#                           colour = id), alpha = 1) +
#   facet_wrap(~Country, ncol = 1, nrow = 3) +
#   geom_point(size = 3, alpha = 0.6)

wavePower_sp <- SpatialPoints(data.frame(
  wavePowerLocations$longitude, wavePowerLocations$latitude),
  proj4string=CRS("+proj=longlat +datum=WGS84"))


# transect locations
transectsLocations <- allFiles_dropPOS %>%
  
  dplyr::group_by(alongshorePos) %>%
  dplyr::distinct(pos, .keep_all = TRUE) %>%
  dplyr::select(Country,alongshorePos, pos, distX, distY) %>%
  ungroup()

pointLoc <- SpatialPoints(data.frame( # use distX, distY for now as point representaion of the transect
  transectsLocations$distX, transectsLocations$distY),
  proj4string=CRS("+proj=longlat +datum=WGS84"))
  
# get distances between transectlocations and wavewatch3 locations
distances <-  rgeos::gDistance( spTransform(pointLoc,CRS("+init=epsg:32621")), 
                         spTransform(wavePower_sp,CRS("+init=epsg:32621")), byid=T)
nearest <- apply(distances,2, which.min)  
dist <- apply(distances, 2, min)

# 
transectsLocations$ERAx = wavePowerLocations$longitude[nearest] # X coorindate nearest ERA5  
transectsLocations$EARy = wavePowerLocations$latitude[nearest]  # Y coordinates nearest ERA5 obesrvation 
transectsLocations$dist = dist                                      # distance to nearest ERA5 observation
transectsLocations$nearestC = wavePowerLocations$country[nearest]   # sanity check: country is correct? ==> not always the case 
transectsLocations$ERAid = wavePowerLocations$id[nearest]

# # update for testing
# coastPoints2 <- SpatialPointsDataFrame(data.frame(
#   transectsLocations$distX, transectsLocations$distY),
#   proj4string=CRS("+proj=longlat +datum=WGS84"),
# data = data.frame(transectsLocations))
# 
# color = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
# col=sample(color, unique(coastPoints2$ERAid))
# 
# mapview::mapview(wavePower_sp, col.regions=list('red')) +
#   mapview::mapview(coastPoints2, zcol = "ERAid", color = col,burst = TRUE)


# for each pos in transectLocations there is now a corresponding ERA 5 observation 
# join allFiles_dropPOS and wavePower based on matching that is defined in transectsLocations
allFiles_merge <- allFiles_dropPOS %>%
  ungroup() %>%
  # merge the location information based on unqie alongshore position identifier
  dplyr::left_join(transectsLocations %>%
              dplyr::select(!c(Country,pos,nearestC, dist, distX, distY)),
            c("alongshorePos" = "alongshorePos")) #%>%

# complete computing wave energy flux for each transect location
# first assign 
allFiles_mutate <- allFiles_merge %>% 
  dplyr::mutate(year_col = as.Date(paste0(year_col))) %>% # organize formatting 
  dplyr::group_by(Country,alongshorePos, year_col) %>%
  dplyr::summarize(start=as.Date(year_col), # create start date and end date 
                   end=ymd(year_col) + years(1)-1,
                   ERAid = ERAid[1])

# intermediate step: Expand table to have a unique observation per hour
# and join
step2 <- allFiles_mutate %>%
  # dplyr::filter(ERAid == 1) %>%
  group_by(ERAid,Country,alongshorePos, year_col) %>% # for each unique observation
  
  transmute(date = list(seq(min(start), max(end), by = "day"))) %>% # duplicate for each day
  unnest(date, keep_empty = T) %>%
  dplyr::mutate(date = as.Date(date)) %>%
  dplyr::mutate(ERAid = as.numeric(ERAid)) %>%
  ungroup() %>%
  # join with hourly wave energy observations 
  left_join(wavePowerMutate %>%
              dplyr::mutate(id = as.numeric(id)) %>%
              dplyr::select(!c(Country)),
            c( "ERAid" ="id",
              'date' ='date'
               ), keep = F)

# subsetTest <- step2 %>%
#   dplyr::filter(ERAid == 6, alongshorePos==89000) 

# Go back to annual mean observations
wavePowerTable <- step2 %>%
  dplyr::rename(year_col = year_col.x) %>%
  dplyr::group_by(ERAid,alongshorePos,year_col, season) %>%
  dplyr::mutate(
      season = ifelse(is.na(season), # overwrite NA 
                      ifelse(month(as.Date(date)) == startSeason | 
                             month(as.Date(date)) <= endSeason, # define season
                             paste0('storm'),
                             paste0('calm')),
                      season)) %>%
  dplyr::summarize(
    Country = Country[1],
    wave_power = mean(wave_power, na.rm = T), # per pos, and year a mean wave energy flux
    longitude = longitude[1],
    latitude = latitude[1], 
    P_swh = mean(P_swh, na.rm = T), # inital estimate of wave energy flux
    
    mwp = mean(mwp, na.rm = T),
    swh = mean(swh, na.rm = T),      # 
    mwd = mean(mwd, na.rm = T),
    startSeason = startSeason, # which season range is considered for storm & calm
    endSeason = endSeason,) %>% 
  dplyr::group_by(ERAid,Country,alongshorePos, year_col) %>%
  pivot_wider(names_from = season, values_from = c(wave_power,P_swh, mwp,swh,mwd)) %>%
  ungroup() 


for (cntr in unique(wavePowerTable$Country)){
  for (year in unique(format(as.Date(wavePowerTable$year_col), '%Y'))){
    # year <- 2001
    # cntr <- unique(wavePowerTable$Country)[1]
    start_year <- as.Date(ISOdate(year, 1, 1))
    # end_year <- as.Date(ISOdate(year, 12, 31)) 
    fileName <- paste0(wd,"/data/processed/wavePower/", cntr,
                       '_', year, '_wavePower.csv')
    
    
    if(!file.exists(fileName)){
      wavePower_per_year <-wavePowerTable %>%
        dplyr::filter(Country == cntr &
                       as.Date(year_col) == start_year) 
        
      
      write_csv(wavePower_per_year, fileName)
    }
  }
}





# plots for wave-power: time series and spatial distribution
dens_pSWH <- ggplot(wavePowerTable, aes(x = as.numeric(P_swh_storm), fill = Country)) + 
  # geom_density(alpha = 0.4) + 
  geom_histogram(position = 'identity', bins = 75, alpha = 0.8) +
  facet_wrap(~Country, nrow = 3) +
  theme(
    # legend.position = "none",
        plot.background = element_rect(fill = '#d9d9d9',  colour = '#d9d9d9'),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        
        axis.line.x = element_line(size = 0.5, colour = "black"),
        axis.line.y = element_line(size = 0.5, colour = "black"),
        axis.line = element_line(size=1, colour = "black"),
        # axis.text.x = element_blank(),
        # axis.text.y = element_blank(),
        # axis.title.y = element_blank(),
        # axis.title.x = element_blank()
        )
# dens_pSWH

pSWH_timeSeries <- ggplot(wavePowerTable, aes(x = as.Date(year_col), 
                                              y = as.numeric(P_swh_storm),
                                color = Country)) + 
  geom_point(alpha = 0.4, position = position_jitter(width = 150)) + 
  facet_wrap(~Country)+
  theme(
    # legend.position = "none",
    plot.background = element_rect(fill = '#d9d9d9',  colour = '#d9d9d9'),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    
    axis.line.x = element_line(size = 0.5, colour = "black"),
    axis.line.y = element_line(size = 0.5, colour = "black"),
    axis.line = element_line(size=1, colour = "black")
  )
# pSWH_timeSeries

