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
options(dplyr.summarise.inform = FALSE)
# ee_Initialize()

## ---------------------------
source("./src/functions.R")

## ---------------------------

mapviewOptions(basemaps = c( "Esri.WorldImagery","Esri.WorldShadedRelief", "OpenStreetMap.DE"))
dataFolder <- './data/processed'
# years <- c('2005', '2006','2007', '2008','2009')
years <- seq(from = 1985, to = 2020, by = 1)
sequenceLength <- 5 
aoi <- c('FrenchGuiana','Guyana', 'Suriname') # 'FrenchGuiana','Guyana', 'Suriname'

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
# reformat Guyana position values such that x-axis also starts at 0 

allPos <- list('Suriname' = posToExcludeSUR,
               "FrenchGuiana" = posToExcludeFG,
               "Guyana" = posToExcludeGUY )

# posToExclude <- allPos[[aoi]]

# select folders
folderSelect <- as.matrix(list.files(paste0(dataFolder, '/offshore_points'), full.names = T))
df <- rewrite(folderSelect);

# only csv's
df <- df[grep('.csv', folderSelect, ignore.case = T),]
filtered <- vector('list', 100)
for (q in seq_along(years)) {
  for (x in seq_along(aoi)){
    # q <- 1
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
  }}
filtered <- unique(filtered)
allFiles <- unique(do.call(rbind, lapply(as.matrix(filtered)[,1], 
                                         function(x) read.csv(x, stringsAsFactors = FALSE,
                                                              sep = ',', 
                                                              na.strings=c("","NA")
                                         ))))

allFiles_dropPOS <- allFiles %>%
  dplyr::mutate(toFilter = 0) %>%
  dplyr::mutate(toFilter = ifelse((Country == "Suriname" & 
                                     pos %in% allPos[["Suriname"]]),1,toFilter),
                toFilter = ifelse((Country == "FrenchGuiana" & 
                                     pos %in% allPos[["FrenchGuiana"]]),1,toFilter),
                toFilter = ifelse((Country == "Guyana" & 
                                     pos %in% allPos[["Guyana"]]),1,toFilter)) %>%
  filter(toFilter == 0) %>%
  dplyr::select(-c(toFilter))

#' implement workflow
#' 1) filter outliers & transects with NO mudbank(see pre-processing)
#' 2) create annual estimates of mudbank position or mudbank 
#' 3) alternatively apply douglas pecker algorithm
#'      - Requires to define subsections (see https://www.tandfonline.com/doi/pdf/10.1559/152304099782424901?casa_token=9wn9uSUp3zYAAAAA:XYDB0pKcZcH69STl6eOAlKoMPEwIbvxtlwUwzZ00q4V-z8yOfAREUCePnd4fiZbS9H2A-woJqt0mIg  )
#'      to ensure separate mudbanks are recognized
#'      

# myColors <- data.frame(id = character(), col = character(), year = character(),
#                        positionGroup = character(), mean = double())

rectangles <- data.frame(id = character(),fill =  character(),
                         colour = character(),Country = character(),
                         xmin = double(), xmax = double(),
                         ymin = double(), ymax = double(),
                         xminTmin1 = double(), # pos previous year
                         xmaxTmin1 = double(),  ## pos previous year
                         lengthMudbank = double(), #length current year
                         lengthMudbankTmin1 = double(), # length previous year  
                         nrMudbanks = double(),
                         area = double(),
                         mudbankMaxWidth = double(),
                         mudbankMeanWidth = double(),
                         mudbankMedianWidth = double(),
                         mudbankFract = double(),
                         transectFract = double(),
                         migrationSpeed = double(), # speed based on front migration speed
                         speedMeanPos = double(), # speed based on mean posiotns
                         overlapMudbanks = double())     # amount of overlap [meters]

countries <- unique(allFiles_dropPOS$Country)
all_pos <- sort(unique(allFiles_dropPOS$pos))

# aggregated pos gos per x Meters:
summarisePos <- 10000
breaks <- seq(0, max(allFiles_dropPOS$pos), summarisePos)

# position label for plotting x-axis
poslabel <- seq(summarisePos/2, max(allFiles_dropPOS$pos)-summarisePos/2, summarisePos)

allFiles_dropPOS <- allFiles_dropPOS %>%
  group_by(Country) %>%
  dplyr::mutate(
    newPos = cut(pos,breaks, #right = T, include.lowest = F,
                             labels = poslabel)) %>%
  ungroup() 


allFiles_dropPOS$negPos <- 1
allFiles_dropPOS$alongshore_negPos <- 1

for (cntr in countries){
  # cntr <- countries[2]
  # print(cntr)
  
  all_years <- sort(unique(pull(allFiles_dropPOS, year_col)))
  
  # get median position for each year & mudbank indicators (rectangles)
  for(y in 1:length(all_years)){
    # y <- 16
    # selected_year <- '2000-01-01'
    selected_year <- all_years[y]
    idx <- which(as.Date(allFiles_dropPOS$year_col) == as.Date(selected_year) & 
                   allFiles_dropPOS$coast_outlier == 1 &
                   allFiles_dropPOS$Country == cntr)
    annualSubset <- subset(allFiles_dropPOS,
                           Country == cntr &
                           as.Date(year_col) == as.Date(selected_year) &
                           coast_outlier == 1) %>%
      filter(dropClass == 'rel') # probably only needed here
    
    # all posiotions considered a mudbank during given year
    mudbankObs <- subset(annualSubset, #!(pos %in% posToExclude) &
                           noMudbank == 0)
    getPos <- unique(mudbankObs$pos)
    
    # sequences
    sequences <- split(getPos, cumsum(c(0, diff(getPos) > 1000)));
    
    # drop sublist with only x amount of consequetive mudbank observations
    filtSequences <- Filter(function(x){length(x)>sequenceLength}, sequences)
    
    startPos <- vapply(filtSequences, head, n = 1L, FUN.VALUE = numeric(1))
    endPos <- vapply(filtSequences, tail, n = 1L, FUN.VALUE = numeric(1))
    lengthMudbank <- endPos - startPos
    mudbankCount <- length(startPos)
    
    # INCLUDE HERE (of earlier?) TO get the mean fraction value of each mudbank position
    mudbankID <- as.numeric(names(filtSequences))
    # names(filtSequences) <- rep('mudbank', mudbankCount)
    
    yearStat <- data.frame(maxOffshoreDist = double(),
                           mudbankFraction = double(),
                           transectFraction= double(),
                           meanOffshoreDist = double(),
                           medianOffshoreDist = double(),
                           area = double())
                           
    
    if(length(startPos) != 0){
      for(q in 1:length(mudbankID)){
        # q <- 1
        # print(filtSequences[[q]])
        # for each sequence get mean fraction and max extent (medianOffshore)
        testFilter <- annualSubset %>% filter(pos %in% filtSequences[[q]])
        d <- max(testFilter$medianOffshore) #maximum distance corresponding to mudbank extent
        dmean <- mean(testFilter$medianOffshore, na.rm = T)
        dmed <- median(testFilter$medianOffshore, na.rm = T)
        f <- mean(testFilter$meanFraction)
        transectMean <-  mean(testFilter$meanMud[testFilter$meanMud>0])
        # "meanMud" ==> transect mean          
        # "mudFract" ==> 
        # maxExtentIndex
        
        # compute area
        area <- testFilter %>%
          group_by(year_col,pos) %>%
          # compute the mudbank area (m^2) at each position
          dplyr::summarise(medianOffshore = mean(medianOffshore, na.rm=T)* 1000) %>%
          ungroup() %>%
          # sum for all positions and change to km^2 
          dplyr::summarise(area = sum(medianOffshore)/ 1000000) # 
   
        yearStat<-rbind(yearStat, data.frame(maxOffshoreDist = d,
                                             meanOffshoreDist = dmean,
                                             medianOffshoreDist = dmed,
                                             mudbankFraction =f,
                                             transectFraction = transectMean,
                                             area = area))
        
      }
    }

    if(length(startPos) == 0){
      startPos <- c(0)
      endPos <- c(0)
      lengthMudbank <- c(0)
      mudbankCount <- c(0)
      yearStat<-rbind(yearStat, data.frame(maxOffshoreDist = -1,
                                           mudbankFraction = -1,
                                           meanOffshoreDist = -1,
                                           medianOffshoreDist = -1,
                                           transectFraction = -1,
                                           area = -1
                                           ))
    }
    
    
    # get migration speed: look for overlapping sequences 
    prevYear <- rectangles %>% filter(Country == cntr, 
                                      as.Date(id)==as.Date(selected_year) - years(1))
    
    if(length(prevYear)>1){
      
      df <- data.frame(start1 = startPos, end1= endPos) # t
      df2 <-data.frame(start2 = prevYear$xmin, end2 = prevYear$xmax) # t - 1
      
      data.table::setkey(data.table::setDT(df), start1, end1)
      data.table::setkey(data.table::setDT(df2), start2, end2)
      
      overlap <- data.table::foverlaps(df,df2, which=TRUE, type = 'any')
      # type = any, also test for 'within'

      compareYears <- data.frame(df[overlap$xid], # current mudbank position
                                 df2[overlap$yid], # previous mudbank pos
                                 length = lengthMudbank[overlap$xid],
                                 lengthtMin1 = prevYear$lengthMudbank[overlap$yid]) %>%
        dplyr::mutate(tmean = ((end1-start1)/2)+start1,           # mean position
                      tmin1Mean = ((end2-start2)/2)+start2) %>%   # mean position    
      
        dplyr::mutate(speed = (start2-start1),
                      speedMeanPos =tmin1Mean-tmean) %>% # m/yr
        rowwise()%>%
        dplyr::mutate(overlap =  sequenceOverlap(start1, end1,start2,end2)) %>% # overlap with previous year
        dplyr::group_by(start1, end1) %>%
        # duplicates indicate two mudbanks in previous year: select one with most overlap
        dplyr::filter(overlap == max(overlap), .preserve = T) %>%        # 
        
        # if overlap is equal: select fastest migration speed
        dplyr::mutate(the_rank = rank(-speed, ties.method = "first"))%>% 
        dplyr::filter(the_rank == 1, .preserve = T) %>%
        dplyr::ungroup()

    
      
    }#else{}
    
    
    
    # get average mud fraction for each mudbank detected?
    # for each start position subset
    rectangles <- rbind(rectangles,
                        data.frame(id = selected_year,fill = 'black',
                                   colour = 'black',
                                   Country = cntr,
                                   xmin = startPos,
                                   xmax = endPos,
                                   xminTmin1 = compareYears$start2, # pos previous year
                                   xmaxTmin1 = compareYears$end2,  ## pos previous year
                                   lengthMudbank = lengthMudbank, #length current year
                                   lengthMudbankTmin1 = compareYears$lengthtMin1, # length previous year  
                                   nrMudbanks = mudbankCount,
                                   ymin = as.Date(selected_year)-184, # the geom_tiles per year have first of januari each year as midpoint
                                   ymax = as.Date(selected_year)+181,
                                   area = yearStat$area,
                                   mudbankMaxWidth = yearStat$maxOffshoreDist,
                                   mudbankMeanWidth = yearStat$meanOffshoreDist,
                                   mudbankMedianWidth = yearStat$medianOffshoreDist,
                                   mudbankFract = yearStat$mudbankFraction,
                                   transectFract = yearStat$transectFraction, # annual mean transect fraction
                                   migrationSpeed = compareYears$speed, # speed based on front migration speed
                                   speedMeanPos  = compareYears$speedMeanPos, # speed based on mean posiotns
                                   overlapMudbanks  = compareYears$overlap*1000     # amount of overlap 
                        )) 
    
    meanVal <- ifelse(nrow(annualSubset) > 0, 
                      mean(annualSubset$deltaCoast, na.rm =T),
                      0)
    
    # medianVal <- median(annualSubset$deltaCoast, na.rm =T)
    
    # myColors <- rbind(myColors, data.frame(id = 'annualChange', year =  selected_year,
    #                                        positionGroup = 'all', mean = meanVal,
    #                                        col =  ifelse(meanVal>0 , '#2166ac', # purple if positive
    #                                                      ifelse(meanVal < 0, '#b2182b', # green if negative
    #                                                             "grey90"))))
    print(paste0(selected_year, ': ', round(meanVal,2)))
    if(meanVal < 0){
      
      allFiles_dropPOS$negPos[idx] <- 0}
  }
}

rectangles <- rectangles %>%
  rowwise() %>%
  dplyr::mutate(LWratio = lengthMudbank/mudbankMeanWidth)

# write_csv(rectangles, paste0(wd,"/data/processed/mudbanks/mudbanks.csv"))
annualStats <- rectangles %>%
  dplyr::group_by(Country, id) %>%  # group per year & country
  dplyr::mutate(meanArea = mean(area, na.rm = T),
                meanLength = mean(lengthMudbank, na.rm = T),
                meanMudbankCount = mean(nrMudbanks, na.rm = T),
                mudbankMeanWidth = mean(mudbankMeanWidth, na.rm = T),
                meanLWratio =mean(LWratio, na.rm = T) 
                ) %>%
  filter(eval(as.name(paste(speedToPlot))) >= 0 &
           eval(as.name(paste(speedToPlot))) < 10000 ) %>%  
  dplyr::summarize(meanSpeed = mean(migrationSpeed, na.rm = T),
                   maxSpeed = max(migrationSpeed, na.rm = T),
                   sdSpeed = sd(migrationSpeed, na.rm = T),
                   meanSpeed = mean(migrationSpeed, na.rm = T),
                   meanArea = meanArea[1],
                   meanLength = meanLength[1],
                   meanMudbankCount = meanMudbankCount[1],
                   mudbankMeanWidth = mudbankMeanWidth[1],
                   meanLWratio = meanLWratio[1]
                   )

# write_csv(annualStats, paste0(wd,"/data/processed/mudbanks/mudbanks_annualStats.csv"))


rectangles <- transform(rectangles,
                            Country=factor(Country, levels=c("FrenchGuiana","Suriname","Guyana")))


# as alternative for the loop? ==> check if working
allFiles_dropPOS2 <- allFiles_dropPOS %>%
  group_by(Country, newPos) %>%
  # dplyr::filter(coast_outlier == 1, .preserve = F) %>%
  dplyr::mutate(meanCoastlineChange = 
                  mean(deltaCoast[which(coast_outlier == 1)], na.rm =T)) %>%
  ungroup() %>% 
  dplyr::mutate(alongshore_negPos = ifelse(meanCoastlineChange < 0,
                                           0,alongshore_negPos))
  


# for each group of positions: calculate mean coastline change
# for (np in unique(allFiles_dropPOS$newPos[!is.na(allFiles_dropPOS$newPos)])){
#   # np <- unique(annualSubset$newPos)[10]
#   posSubset <- subset(allFiles_dropPOS, newPos == as.numeric(as.character(np)) &
#                         coast_outlier == 1) 
#   # corresponding index in original file
#   subsetIDX <- which(allFiles_dropPOS$newPos == as.numeric(as.character(np)) &
#                        allFiles_dropPOS$coast_outlier == 1)
#   
#   meanCoastlineChange <- mean(posSubset$deltaCoast, na.rm = T)
#   
#   # myColors <- rbind(myColors, data.frame(id = 'positionChange', year =  'all',
#   #                                        positionGroup = as.character(np), mean = meanCoastlineChange,
#   #                                        col =  ifelse(meanCoastlineChange>0 , '#2166ac', # blue if positive
#   #                                                      ifelse(meanCoastlineChange < 0, '#b2182b', # red if negative
#   #                                                             "grey90"))))
#   print(paste0(np, ': ', round(meanCoastlineChange,2)))
#   if(meanCoastlineChange < 0 ){
#     allFiles_dropPOS$alongshore_negPos[subsetIDX] <- 0
#   }
# }

# boxplot idicating the annual variation in coastline changes
AllNonOutliers <- subset(allFiles_dropPOS, coast_outlier == 1)
annualVariation <- ggplot(AllNonOutliers,
                          aes(x=as.Date(year_col), y = deltaCoast , 
                              group = as.factor(year_col))) +  # alpha = negPos
  
  geom_boxplot(aes(fill = as.factor(negPos)),  outlier.shape=NA)+#outlier.colour="black") +          
  scale_fill_manual('mean change', labels = c('Erosion', 'Accretion'),
                    values = c('#7b3294',  "#008837"),
                    guide = guide_legend(reverse=T)) +  
  geom_hline(yintercept = 0, color= 'red',
             linetype="dashed", size = 1) +
  guides()+
  coord_flip() +
  labs(y ='Coastline Change [m/yr]', y = "") +
  scale_y_continuous(limits=c(-150,150)) +
  theme(
    axis.line.x = element_line(size = 0.5, colour = "black"),
    axis.line.y = element_line(size = 0.5, colour = "black"),
    axis.text.x = element_text(color = "grey20", size = 12, hjust = .5, vjust = .5),
    axis.text.y = element_text(color = "grey20", size = 12, hjust = .5, vjust = .5),
    axis.title.x = element_text(size = 12, face = 'bold'),
    axis.title.y = element_blank(), 
    legend.key = element_rect(fill = NA),
    strip.background = element_rect(fill = "white", colour = "white"),
    legend.background = element_rect(fill = '#d9d9d9',  colour = '#d9d9d9'),
    legend.text = element_text(size = 12),
    legend.position = c(.8, .8),
    legend.title = element_text(colour = 'black', size = 16, face = 'bold'),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.spacing.x = unit(2, 'lines'),
    plot.background = element_rect(fill = '#d9d9d9', colour = '#d9d9d9'))

# annualVariation
legendAnnual <- get_legend(annualVariation)
annualVariation <- annualVariation + theme(legend.position = 'none')

AllNonOutliers$newPos <-  as.numeric(as.character(AllNonOutliers$newPos))
# now similarly to annual variation get for each group of position the variation
# in coastline change

# subset of data to plot in aggregated pos
# exclude transects
subsetToPlot <- subset(AllNonOutliers)#, !(pos %in% posToExclude))

# adjust the labels to account for positions
adjustLabels <- subsetToPlot %>%
  dplyr::group_by(newPos) %>%
  dplyr::mutate(newPos2 = Mode(pos)) %>% #ifelse(any(newPos %in% posToExclude), Mode(pos), newPos)) %>%
  ungroup()

# adjustLabels <- subset(adjustLabels, deltaCoast != 0)

aggregatedPos <- ggplot(adjustLabels, aes(x=newPos, y = deltaCoast, 
                                          group = newPos)) +  
  geom_hline(yintercept = 0, color= 'red',
             linetype="dashed", size = 1) +
  geom_boxplot(aes(fill = as.factor(alongshore_negPos)), outlier.shape=NA)+      
  scale_fill_manual('mean change', labels = c('Erosion', 'Accretion'),
                    values = c('#7b3294', "#008837"),
                    guide = guide_legend(reverse=T)) +
  
  
  labs(y =  'Coastline Change \n [m/yr]', x = "Alongshore Position [km]") +
  scale_y_continuous(limits=c(-150,150)) +
  scale_x_continuous(lim=c(0,max(adjustLabels$pos)+5000), expand = c(0,0),
                     labels = unit_format(unit = "", scale = 0.001)) +
  # scale_x_reverse(lim=c(max(adjustLabels$pos)+5000, 0), expand = c(0,0),
  # labels = unit_format(unit = "", scale = 0.001)) +
  theme(
    axis.line.x = element_line(size = 0.5, colour = "black"),
    axis.line.y = element_line(size = 0.5, colour = "black"),#element_blank(),
    axis.text.x = element_text(color = "grey20", size = 12, hjust = .5, vjust = .5),
    axis.title.x = element_text(size = 12, face = 'bold'),
    axis.text.y = element_text(color = "grey20", size = 12, hjust = .5, vjust = .5),
    axis.title.y = element_text(size = 12, face = 'bold'),
    legend.background = element_rect(fill = '#d9d9d9',  colour = '#d9d9d9'),
    legend.key = element_rect(fill = NA),
    legend.position = c(.8, .8),
    legend.title = element_blank(),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.spacing.x = unit(2, 'lines'),
    plot.background = element_rect(fill = '#d9d9d9',  colour = '#d9d9d9'),
    strip.text.x = element_text(size = 16, face = 'bold') # Facet titles
  )

# aggregatedPos

legendPos <- get_legend(aggregatedPos)
aggregatedPos <- aggregatedPos + theme(legend.position = 'none')

####
#'
#' hovmoller plots
#'
#####


posOfInterest <- sort(c(50000, 
                        210000, 
                        310000))#c(50000, 210000, 310000) # c(34000, 187000, 118000)   # 
dataPOI <- subset(allFiles_dropPOS, (pos %in% posOfInterest))

poiOriginX <- c(as.matrix(tapply(dataPOI$originX, dataPOI$pos, median)))
poiOriginY <- c(as.matrix(tapply(dataPOI$originY, dataPOI$pos, median)))

# 
# Spatio temporal variation
range <- round(quantile(allFiles_dropPOS$deltaCoast,c(0.05, 0.95), na.rm=T))

p <-ggplot(subset(allFiles_dropPOS, !is.na(deltaCoast)), #& !(pos %in% posToExclude)),
           aes(x = pos,y = as.Date(year_col), fill=deltaCoast)) + 
  
  geom_tile(color= "white",size=0.1, na.rm = TRUE) +
  facet_wrap(~Country, ncol = 1, nrow = 3) +
  # resolve here that values larger than the range are indicated in the colourbar 
  # now it reads as if the largest value is -125m/yr where as actually there is locations that
  # have larger values.
  # scale_fill_gradient2(name = 'change [m/yr] \n',limits = c(range[[1]],range[[2]]),
  #                      # breaks = c(range[[1]], range[[1]]/2, 0, range[[2]]/2, range[[2]]),
  # 
  #                      low = '#7b3294', high = "#008837", mid = '#f7f7f7',
  #                      guide = guide_colourbar(nbin=100, draw.ulim = FALSE,
  #                                              draw.llim = FALSE),
  #                      na.value = NA, oob=squish) + # squish clamps all values to be within min & max of limits arguments


  scale_fill_gradientn(name = 'coastline change \n [m/yr] \n',
                       breaks = c(range[[1]], range[[1]]/2, 0, range[[2]]/2, range[[2]]),
                       limits = c(range[[1]],range[[2]]),
                       colours = c('#7b3294', '#f7f7f7', "#008837"),
                       guide = guide_colourbar(nbin=100, draw.ulim = FALSE, draw.llim = FALSE),
                       oob=squish,
                       values = scales::rescale(c(range[[1]], -50, 0, 50, range[[2]]))
  ) +
  # geom_vline(xintercept = posOfInterest, color= 'red',
  #            linetype="dashed") +
  # geom_text() +
  # annotate("text", label = as.roman(1:length(poiOriginX)),
  #          x = posOfInterest + 6000,
  #          y = rep(as.Date('1984-04-30'),length(poiOriginX)),
  #          size = 6, colour = "red") +
  
  geom_rect(data = rectangles, inherit.aes = FALSE,
            aes(xmin = xmin, xmax = xmax,
                ymin = ymin, ymax = ymax-125, colour = colour), 
            fill = NA, size = 0.75) + 
  
  scale_colour_manual(name = ' ', values = c("black"),
                      labels = c('mudbank'),
                      guide = guide_legend(ncol = 2))+
  
  labs(y = 'Year', x = 'Alongshore Position [km]') + 
  # scale_x_continuous(lim=c(0,max(adjustLabels$pos)+5000), expand = c(0,0),
  #                    labels = unit_format(unit = "", scale = 0.001)) +
  # scale_x_reverse(lim=c(max(allFiles$pos)+4000, 0), expand = c(0,0),
  # labels = unit_format(unit = "", scale = 0.001)) + #
  theme(axis.line.x = element_line(size = 0.5, colour = "black"),
        axis.line.y = element_line(size = 0.5, colour = "black"),
        axis.line = element_line(size= 1, colour = "black"),
        axis.title.y = element_text(size = 12, face = 'bold'),
        axis.title.x = element_text(size = 12, face = 'bold'),
        axis.text.x = element_text(size = 12,  hjust = .5, vjust = .5),
        axis.text.y = element_text(size = 12, hjust = .5, vjust = .5),
        legend.background = element_rect(fill = '#d9d9d9',  colour = '#d9d9d9'),
        legend.spacing.y = unit(0.1, 'cm'),
        legend.title = element_text(colour = 'black', size = 14, face = "bold"),
        # legend.key = element_rect(fill = NA),
        legend.text = element_text(size = 12),
        # legend.position = 'none',
        panel.grid.major = element_blank(), # remove grid lines
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        plot.background = element_rect(fill = '#d9d9d9',  colour = '#d9d9d9'))
p

legend <- get_legend(p)

p <- p + theme(legend.position = 'none')

kustlijn <- readOGR(dsn = paste0(wd,'/data/raw/shapes'),
                    layer = 'coastline_guyana') # coastline_FrenchGuana / coastline_suriname_disc / coastline_guyana

shapefile_df <- fortify(kustlijn)
shapefile_df$class <- NA

# class(shapefile_df$id)

# shapefile_df$class[which(shapefile_df$id %in% c(3,2))] <- 'east'
# shapefile_df$class[which(shapefile_df$id %in% c(0,1,4))] <- 'center'
# shapefile_df$class[which(shapefile_df$id %in% c(5,6))] <- 'west'

# change order
# shapefile_df$class <- factor(shapefile_df$class, levels = c("west", "center", "east"))

floor_dec <- function(x, level=1) round(x - 5*10^(-level-1), level)
ceiling_dec <- function(x, level=1) round(x + 5*10^(-level-1), level)

mapped <- ggplot() +
  geom_path(data = shapefile_df, 
            aes(x = long, y = lat, group = group, colour = class)) + # 
  geom_point(aes(x = poiOriginX,  y = poiOriginY), colour = 'red',
             size = 3) +
  
  scale_x_continuous(limits=c(min(shapefile_df$long),  
                              max(shapefile_df$long)),  #limits=c(-57.1, -53.95),
                     expand = c(0,0)) +
  scale_y_continuous(breaks=seq(round(min(shapefile_df$lat),1),
                                round(max(shapefile_df$lat)+0.1, 1), 
                                round((5.8-4.1)/3,1))) + # breaks=c(5.8, 6.0)
  # scale_color_manual(values = c("#E7B800", "#2E9FDF", "#33a02c"),
  #                    labels = c('West                                           ',
  #                               'Center                         ',
  #                               'East')) +
  # guides(colour=guide_legend(ncol=3, label.position = "right")) + #keywidth = 0.5, default.unit = 'inch'
  
  # geom_text() +
  # annotate("text", label = as.roman(1:length(poiOriginX)),
  #          x = poiOriginX,
  #          y = poiOriginY-0.15,
  #          size = 4, colour = "red") +
  theme(axis.line.x = element_line(size = 0.5, colour = "black"),
        axis.line.y = element_line(size = 0.5, colour = "black"),
        axis.line = element_line(size= 1, colour = "black"),
        axis.title.y = element_text(size = 14, face = 'bold'),
        axis.title.x = element_text(size = 14, face = 'bold', hjust = 0.4),
        axis.text.x = element_text(size = 12,  hjust = .5, vjust = .5),
        axis.text.y = element_text(size = 12, hjust = .5, vjust = .5),
        legend.title = element_blank(),
        legend.position = c(.5, -.3),
        legend.background = element_rect(fill = NA,  colour = NA),
        legend.text = element_text(size = 10, face = "bold", margin = margin(r=30, unit = "pt")),
        # legend.spacing.x = unit(7.0, 'cm'),
        panel.grid.major = element_blank(), # remove grid lines
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        plot.background = element_rect(fill = '#d9d9d9',  colour = '#d9d9d9')) 

# coord_map changes the aspect ratio, making it impossible to define relative heights in
# patchwork library: https://stackoverflow.com/questions/48924219/vertically-align-of-plots-of-different-heights-using-cowplotplot-grid-when-u
map_projected <- mapped +
  coord_map() +
  scale_x_continuous(limits=c(-57.1, -53.95),
                     expand = c(0,0))#, limits=c(0,30000),)

left2 <- plot_grid(p, aggregatedPos, ncol = 1, align = 'v', #, mapped
                   nrow = 3, rel_heights = c(2.5, 1, 0.5),         # adjust lay out 
                   labels = c('A', 'C', 'D'), vjust =c(+4, -1,-1), # adjust label position
                   hjust = -2)

legends <- plot_grid(legend, NULL, legendAnnual, nrow = 1, ncol = 3, 
                     rel_widths = c(1,-2,3),
                     axis = 't', align = 'v') + 
  theme(panel.background = element_rect(fill = '#d9d9d9',  colour = '#d9d9d9')) #plot.margin = unit(c(0,0,0,0), 'cm')
# legends

right <- plot_grid(annualVariation, legends, ncol = 1, nrow = 2, 
                   rel_heights = c(2.5, 1.5),
                   labels = c('B'), hjust=-4, vjust = +4) +  
  theme(panel.background = element_rect(fill = '#d9d9d9',  colour = '#d9d9d9'))

final <- plot_grid(left2, right, align = 'h', ncol = 2,
                   rel_widths = c(2.5, 1)) + 
  theme(panel.background = element_rect(fill = '#d9d9d9', colour = '#d9d9d9'))

# final
ggsave(plot = final, filename = paste0("./results/temp_maps/", aoi,'_hovmollerFigure_1985_2020_',
                                       format(Sys.Date(), "%Y%m%d"),'.jpeg'),
       width = 13.1, height = 7.25, units = c('in'), dpi = 1200)
# ggsave(plot = final, filename = paste0("D:/WOTRO/Research/Reporting/Publications/",
#                          "AlongshoreVariability_mudbanks/submissionFiles/",
#                          "figures/Figure_6.pdf"),
#        width = 13.1, height = 7.25, units = c('in'), dpi = 1200)


#make sure only bottom (final) plot gets a legend
plotcounter <- 1
library(grid)

for (position in posOfInterest){
  # position <- posOfInterest[1]
  
  subsetPos <- subset(allFiles_dropPOS, pos == position)
  coastDistRange <- round(quantile(subsetPos$coastDist,c(0.005, 0.99), na.rm=T))
  years <- c(min(subsetPos$DATE_ACQUIRED), max(subsetPos$DATE_ACQUIRED))
  
  # all years considered a mudbank
  getYears <- unique(subsetPos[subsetPos$noMudbank == 0, 'year_col'])
  
  if(length(getYears) < 1) {
    getYears <- as.character('1980-01-01')
  }
  
  
  # sequences <- split(as.Date(getYears$year_col), cumsum(c(0, diff(as.Date(getYears$year_col)) == 365)));
  # drop sublist with only x amount of consequetive mudbank observations
  # filtSequences <- Filter(function(x){length(x)>3}, sequences)
  # startPos <- vapply(filtSequences, head, n = 1L, FUN.VALUE = numeric(1))
  # endPos <- vapply(filtSequences, tail, n = 1L, FUN.VALUE = numeric(1))
  
  dateFrames <- data.frame(id = rep(position, length(getYears)), 
                           fill = '#7fbf7b',colour = 'black',
                           xmin = as.Date(getYears),
                           xmax = as.Date(getYears) + 365,
                           ymin = coastDistRange[1],
                           ymax = coastDistRange[2]+50)
  
  
  legendPos <- as.character(ifelse(plotcounter == length(posOfInterest),
                                   'bottom',
                                   'none'))
  
  twoDPlot <- ggplot(subsetPos, 
                     aes(x= as.Date(DATE_ACQUIRED), y = coastDist)) + 
    geom_rect(data = dateFrames, inherit.aes = FALSE,
              aes(xmin = xmin, xmax = xmax, ymin = ymin,ymax = ymax, fill = fill),
              colour = NA) +
    scale_fill_manual(name = '', values = c('#7fbf7b'), labels = c('mudbank')) +
    geom_line(inherit.aes = FALSE, aes(x = as.Date(DATE_ACQUIRED), y = coast_median),
              alpha = 0.9, size = 1.2) +
    # geom_smooth(inherit.aes = FALSE, aes(x = as.Date(DATE_ACQUIRED), y = coast_median),
    #           alpha = 0.9, size = 1.2, se = F) +
    geom_point(size = 3, aes(colour = as.factor(coast_outlier)), alpha = 0.6) +
    scale_y_continuous(limits=c(coastDistRange[1],coastDistRange[2]+50)) +
    
    scale_color_manual(name = "Observations",
                       values = c('red', 'blue'),
                       labels = c("outlier", "coastal distance")) +
    
    
    # annotate("text", label = paste0(LETTERS[plotcounter]),
    #          x = date(years[1]),
    #          y = coastDistRange[2]+50,
    #          size = 14,colour = "black", hjust = +2.2, vjust = -0.2) +
    
    scale_x_date(labels = date_format("%Y"),
                 limits=date(c(years[1],years[2]))) +
    # guides(colour=guide_legend(ncol=2)) +
    ggtitle( paste0('position: ', position/1000)) +
    labs(x = "year", y = "coastline position [m]") +
    theme(axis.line.x = element_line(size = 0.5, colour = "black"),
          axis.line.y = element_line(size = 0.5, colour = "black"),
          axis.line = element_line(size= 1, colour = "black"),
          axis.title.y = element_text(size = 20, face = 'bold'),
          axis.title.x = element_text(size = 20, face = 'bold'),
          axis.text.x = element_text(size = 16,  hjust = .5, vjust = .5),
          axis.text.y = element_text(size = 16, hjust = .5, vjust = .5),
          
          # legend
          legend.title = element_blank(),
          legend.background = element_rect(fill = alpha('#d9d9d9', 0.7),  
                                           colour = '#d9d9d9'),
          legend.text = element_text(size = 20),
          legend.position = legendPos,#c(.85, .28),
          
          plot.title = element_text(hjust = 0.5, size = 25, face = 'bold',
                                    vjust = -1), 
          panel.grid.major = element_blank(), # remove grid lines
          panel.grid.minor = element_blank(), 
          panel.background = element_blank(),
          plot.background = element_rect(fill = '#d9d9d9', colour = '#d9d9d9'),
          
          panel.border = element_blank())
  
  # gt <- ggplot_gtable(ggplot_build(twoDPlot))
  # gt$layout$clip[gt$layout$name == "panel"] <- "off"
  # grid.draw(gt)
  
  # ggsave(plot = twoDPlot, filename = paste0("./results/temp_maps/",aoi,
  #                                           '_pos_', position,
  #                                           '_1985_2020_',
  #                                           format(Sys.Date(), "%Y%m%d"),
  #                                           '.jpeg'),
  #        width = 8.5, height = 3.7, units = c('in'), dpi = 1200)
  # width = 13.1, height = 7.25
  plotcounter <- plotcounter + 1
}
# 


# plot alongshore variability of mud fractions
range <- round(quantile(subset(allFiles_dropPOS, 
                               !is.na(meanMud) & 
                                 #allFiles_dropPOS$mudbank_outlier <1 &
                                 meanMud > 0 )$meanMud,c(0.05,0.5, 0.99), 
                        na.rm=T), 2)

# for faster plotting...
fractiontable <- subset(allFiles_dropPOS,!is.na(SmoothedPeakFract) & 
                          SmoothedPeakFract >= 0)  %>%
  dplyr::group_by(year_col, pos) %>% # quarterly_col
  dplyr::summarize(mean_val = mean(SmoothedPeakFract, na.rm = T)) %>%
  ungroup()

range <- round(quantile(fractiontable$mean_val,c(0.01,0.6, 0.99),
                        na.rm=T), 2)


# alongshore variation of mud fractions
hovmoller <-
  ggplot(subset(fractiontable, mean_val >= 0 ), #!(pos %in% posToExclude)
         aes(x = pos,y = as.Date(year_col), fill=mean_val))+
  # ggplot(subset(allFiles_dropPOS, !is.na(meanMud) & meanMud > 0
  #                         & !(pos %in% posToExclude)),
  #                  aes(x = pos,y = as.Date(year_col), fill=meanMud))+
  geom_tile(color= "white",size=0.1, na.rm = TRUE) + 
  scale_fill_gradient2(limits = c(range[[1]], range[[3]]),
                       breaks = c(range[[1]], range[[2]], range[[3]]),
                       high = "#543005", low ='#313695', mid = '#f7f7f7', # '#7b3294', '#f7f7f7', "#008837"
                       midpoint = range[[2]],
                       guide = guide_colourbar(nbin=100, draw.ulim = FALSE,
                                               draw.llim = FALSE),
                       oob=squish, na.value = NA,
                       name = c('Mean Fraction')) + #"grey50"
  
  geom_rect(data = rectangles, inherit.aes = FALSE,
            aes(xmin = xmin, xmax = xmax,
                ymin = ymin, ymax = ymax-125, colour = colour), 
            fill = NA, size = 0.75) + 
  scale_colour_manual(name = ' ', values = c("black"),
                      labels = c('mudbank'),
                      guide = guide_legend(ncol = 2))+
  
  labs(y = 'Year', x = 'Alongshore Position [km]') +
  scale_x_continuous(lim=c(0,max(adjustLabels$pos)+5000), expand = c(0,0),
                     labels = unit_format(unit = "", scale = 0.001)) +
  # scale_x_reverse(lim=c(max(allFiles_dropPOS$pos)+4000, 0), expand = c(0,0),
  #                 labels = unit_format(unit = "", scale = 0.001)) +
  theme(axis.line.x = element_line(size = 0.5, colour = "black"),
        axis.line.y = element_line(size = 0.5, colour = "black"),
        axis.line = element_line(size= 1, colour = "black"),
        axis.title.y = element_text(size = 18, face = 'bold'),
        axis.title.x = element_text(size = 18, face = 'bold'),
        axis.text.x = element_text(size = 18,  hjust = .5, vjust = .5),
        axis.text.y = element_text(size = 18, hjust = .5, vjust = .5),
        legend.background = element_rect(fill = '#d9d9d9',  colour = '#d9d9d9'),
        legend.title = element_text(colour = 'black', size = 20, face = "bold"),
        legend.text = element_text(size = 16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        plot.background = element_rect(fill = '#d9d9d9',  colour = '#d9d9d9'))

hovmoller

outputName <- ifelse(length(aoi) > 1, 
                     'Guianas', aoi)

ggsave(plot = hovmoller, filename = paste0("./results/temp_maps/", outputName,'_fraction_yearly_1985_2020_',
                                           format(Sys.Date(), "%Y%m%d"),'.jpeg'),
       width = 13.1, height = 7.25, units = c('in'), dpi = 1200)
# ggsave(plot = hovmoller, filename = paste0("D:/WOTRO/Research/Reporting/Publications/",
#                                        "AlongshoreVariability_mudbanks/submissionFiles/",
#                                        "figures/Figure_4.pdf"),
#        width = 13.1, height = 7.25, units = c('in'), dpi = 1200)

# legend <- get_legend(hovmoller)
# 
# hovmoller <- hovmoller + theme(legend.position = 'none')
# 
# # file <- list.files(paste0(main_scenario_folder),pattern = ".tif$", full.names = T)
# file <- '//ad.geo.uu.nl/FG/WOTRO/Research/Site_Suriname_all/Raw_data/GEE/TDOM_cloudscore/Suriname_test_Landsat_TOA_median_hybrid_2013_2015_1_365.tif'
# brickRGB <- brick(file)
# 
# bandNames <- c('blue', 'green', 'red', 'nir', 'swir1',  'swir2', 'unknown', 'TIR', 'BQA')
# names(brickRGB) <- bandNames
# 
# # brickRGB <- fortify(brickRGB)
# 
# rgb_plt <- 
#   ggRGB(img = brickRGB,  r = 'red', g = 'green', b = 'blue', 
#         stretch = "lin", alpha = 0.8) + # select bands to plot
#   scale_x_continuous(limits=c(-57.1, -53.95),
#                      expand = c(0,0)) +
#   scale_y_continuous(limits=c(5.7, 6.3),
#                      breaks=c(5.8, 6.0, 6.2), expand = c(0,0)) +
#   labs(y = 'Lat', x = 'Long') +
#   labs(caption='Landsat (USGS): median composite 2014') +
#   theme(axis.line.x = element_line(size = 0.5, colour = "black"),
#         axis.line.y = element_line(size = 0.5, colour = "black"),
#         axis.line = element_line(size= 1, colour = "black"),
#         axis.title.y = element_text(size = 12, face = 'bold'),
#         axis.title.x = element_text(size = 12, face = 'bold'),
#         axis.text.x = element_text(size = 12,  hjust = .5, vjust = .5),
#         axis.text.y = element_text(size = 12, hjust = .5, vjust = .5),
#         legend.background = element_rect(fill = '#d9d9d9',  colour = '#d9d9d9'),
#         legend.title = element_text(colour = 'black', size = 14, face = "bold"),
#         # legend.key = element_rect(fill = NA),
#         legend.text = element_text(size = 12),
#         plot.caption = element_text(hjust = 0.5),
#         # legend.position = 'none',
#         panel.grid.major = element_blank(), # remove grid lines
#         panel.grid.minor = element_blank(), 
#         panel.background = element_blank(),
#         plot.background = element_rect(fill = '#d9d9d9',  colour = '#d9d9d9'))
# 
# rgb_plt
# 
# fractionRGB <- plot_grid(hovmoller,rgb_plt, ncol = 1, align = 'v',
#                                    nrow = 2, rel_heights = c(3, 2),
#                                    rel_widths = c(0.5, 15))
# graphicalAbstract <- plot_grid(fractionRGB, legend, ncol = 2,
#                                rel_widths = c(2.5, 0.5)) + 
#   theme(panel.background = element_rect(fill = '#d9d9d9', colour = '#d9d9d9'))

# ggsave(plot = graphicalAbstract, filename = paste0("./results/temp_maps/", aoi,'_fraction_yearly_1985_2020_RGB',
#         format(Sys.Date(), "%Y%m%d"),'.jpeg'),
#         width = 13.1, height = 7.25, units = c('in'), dpi = 1200)

