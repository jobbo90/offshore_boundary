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

dataFolder <- './data/processed/offshore_points/scenarios'
# years <- c('2005', '2006','2007', '2008','2009')
years <- seq(from = 1985, to = 2020, by = 1)
aoi <- c("FrenchGuiana",'Suriname','Guyana') # "FrenchGuiana",'Suriname','Guyana'

speedToPlot <- 'migrationSpeed' # 'speedMeanPos' / 'migrationSpeed'


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
# posToExclude <- allPos[[aoi]]

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

# select folders
fileNames <- as.matrix(list.files(paste0(dataFolder), full.names = F)) 

fileNames_df<-data.frame(stringr::str_split_fixed(
  fileNames[grep('.csv', fileNames, ignore.case = T),], 
  "_", 9))
colnames(fileNames_df) <- c('Country','year','string',
                            'nsize','obsT','window',
                            'lwinder','slope','fractT')
# output of migration speed stats
grandTable <- c()

fileNames_df$fractT <- str_replace(fileNames_df$fractT,  ".csv", '')

# unique scenario's based on nsize, 
scenarioGroups <- fileNames_df %>%
  group_by(Country) %>%
  distinct(obsT,fractT, slope) %>%
  dplyr::filter(Country %in% aoi) # only scenarios that are also loaded

folderSelect <- as.matrix(list.files(paste0(dataFolder), full.names = T)) # , '/offshore_points'
dfFiles <- rewrite(folderSelect)

# only csv's
dfFiles <- dfFiles[grep('.csv', folderSelect, ignore.case = T),]

sequenceLengths <- c(3,5,7)
# from df get all files matching the scenario

for(scenario in 1:nrow(scenarioGroups)){
  # scenario = 1
  scenarioOI <- scenarioGroups[scenario,]
  filtered <- vector('list', 100)
  
  obsThresh = as.character(scenarioOI$obsT)
  slopThres = as.character(scenarioOI$slope)
  fractThresh = as.character(scenarioOI$fractT)
  region = as.character(scenarioOI$Country)

    for (q in seq_along(years)) {
      for (x in seq_along(aoi)){
          # q <- 1
          year = years[q]
          filters = c(year, region,obsThresh,slopThres,fractThresh)
          filtered = rbind(filtered, dfFiles %>% 
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
                                          sep = ',', na.strings=c("","NA")
                                          ))))
    
    # all unique dates
    uniqueDates <- unique(allFiles[,'DATE_ACQUIRED']);
    all_years <- unique(allFiles$year_col)
    group_pos <- unique(allFiles$pos)
    countries <- unique(allFiles$Country)
    
    allFiles_dropPOS <- allFiles %>%
      dplyr::mutate(toFilter = 0) %>%
      dplyr::mutate(alongshore = 'NA') %>%
      dplyr::mutate(toFilter = ifelse((Country == "Suriname" & 
                                         pos %in% allPos[["Suriname"]]),1,toFilter),
                    toFilter = ifelse((Country == "FrenchGuiana" & 
                                         pos %in% allPos[["FrenchGuiana"]]),1,toFilter),
                    toFilter = ifelse((Country == "Guyana" & 
                                         pos %in% allPos[["Guyana"]]),1,toFilter)) %>%
      filter(toFilter == 0) %>%
      dplyr::select(-c(toFilter)) %>%
      rowwise() %>%
      dplyr::mutate(
        # a bit slow but fine for now?
        alongshore = ifelse(Country == "Suriname", 
                  gsub('([0-9])', '',  names(which(unlist(surList)==pos)), perl = T),
                   alongshore),
        alongshore = ifelse(Country == "FrenchGuiana",
                   gsub('([0-9])', '',  names(which(unlist(fgList)==pos)), perl = T),
                   alongshore),
        alongshore = ifelse(Country == "Guyana",
                            gsub('([0-9])', '',  names(which(unlist(guyList)==pos)), perl = T),
                            alongshore)
            )
    
    
    

    
    for(slength in 1:length(sequenceLengths)){
      
      # slength = 1
      sequenceLength = sequenceLengths[slength]
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
      
      for (cntr in countries){
      # cntr <- countries[1]
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
    
    rectangles <- transform(rectangles,
                            Country=factor(Country, levels=c("FrenchGuiana","Suriname","Guyana")))
    rectangles_reformatPos <- rectangles %>%
      dplyr::mutate(xmin = ifelse(Country == 'Guyana',
                                  xmin - 39000,
                                  xmin),
                    xmax = ifelse(Country == 'Guyana',
                                  xmax - 39000,
                                  xmax)) %>%
      dplyr::filter(xmin > -1)
    
    pSpeed <-ggplot(rectangles_reformatPos, #& !(pos %in% posToExclude)),
                    aes(x = xmin,y = as.Date(id))) + 
      
      
      geom_rect(data = rectangles_reformatPos, inherit.aes = FALSE,
                aes(xmin = xmin, xmax = xmax,
                    ymin = ymin, ymax = ymax-125), 
                fill = 'black',color='black', size = 0.7) + 
      
      # these are swaped around: first the empty box, then the filled rectangles: easier to visualize
      geom_rect(data = subset(rectangles_reformatPos,  eval(as.name(paste(speedToPlot)))  >= 0 &
                                eval(as.name(paste(speedToPlot)))  < 10000), 
                inherit.aes = FALSE,
                aes(xmin = xmin, xmax = xmax,
                    ymin = ymin, ymax = ymax-125, 
                    fill =  eval(as.name(paste(speedToPlot)))/1000),
                size = 0.8) +
      facet_wrap(~Country, ncol = 1, nrow = 3) +
      labs(x =  'Alongshore position [km]', y = "Year") +
      scale_fill_gradientn(name = 'migration \n [km/yr] \n',
                           colours = c( "#4575b4",'#ffffbf','#d73027' ),
                           
      ) +
      theme(axis.line.x = element_line(size = 0.5, colour = "black"),
            axis.line.y = element_line(size = 0.5, colour = "black"),
            axis.line = element_line(size= 1, colour = "black"),
            axis.title.y = element_text(size = 12, face = 'bold'),
            axis.title.x = element_text(size = 12, face = 'bold'),
            axis.text.x = element_text(size = 12,  hjust = .5, vjust = .5),
            axis.text.y = element_text(size = 12, hjust = .5, vjust = .5),
            legend.background = element_rect(fill = '#f0f0f0',  colour = '#f0f0f0'),
            legend.spacing.y = unit(0.1, 'cm'),
            legend.title = element_text(colour = 'black', size = 14, face = "bold"),
            strip.text.x = element_text(size = 16, face = 'bold'),
            strip.background = element_rect(fill = '#f0f0f0',  colour = '#f0f0f0'),
            legend.text = element_text(size = 12),
            # legend.position = c(0.9,0.8),
            panel.grid.major = element_blank(), # remove grid lines
            panel.grid.minor = element_blank(), 
            panel.background = element_blank(),
            plot.background = element_rect(fill = '#f0f0f0',  colour = '#f0f0f0'))
    
    # pSpeed
    
    # migrationStats <- rectangles %>%
    #   dplyr::group_by(Country, id) %>%  # group per year & country
    #   filter(eval(as.name(paste(speedToPlot))) >= 0 &
    #            eval(as.name(paste(speedToPlot))) < 10000 ) %>%  
    #   dplyr::summarize(mean=mean(eval(as.name(paste(speedToPlot))), na.rm = T),
    #                    max = max(eval(as.name(paste(speedToPlot))), na.rm = T),
    #                    stdv = sd(eval(as.name(paste(speedToPlot))), na.rm = T))
    # 
    overallMean <- rectangles %>%
      dplyr::group_by(Country) %>%  # group per year & country
      filter(eval(as.name(paste(speedToPlot))) >= 0 &
               eval(as.name(paste(speedToPlot))) < 10000 ) %>%  
      dplyr::summarize(mean=mean(eval(as.name(paste(speedToPlot))), na.rm = T),
                       max = max(eval(as.name(paste(speedToPlot))), na.rm = T),
                       stdv = sd(eval(as.name(paste(speedToPlot))), na.rm = T)) 
    
    
    migration_speed <- ggplot(annualStats, aes(x=as.Date(id), y=meanSpeed/1000, 
                                                  color = Country)) +
      facet_wrap(~Country, ncol = 1, nrow = 3) + #  scales = "free_y"
      
      geom_point(size = 3, alpha = 0.6) +
      labs(y =  'Migration Speed [km/yr]', x = "Year") +
      geom_hline(data = overallMean,  aes(yintercept = mean/1000)) +
      
      scale_x_date(date_breaks = "3 years", date_labels = "%Y") +
      
      theme(
        axis.line.x = element_line(size = 0.5, colour = "black"),
        axis.line.y = element_line(size = 0.5, colour = "black"),#element_blank(),
        axis.text.x = element_text(color = "grey20", size = 14, hjust = .5, vjust = .5),
        axis.title.x = element_text(size = 18, face = 'bold'),
        axis.text.y = element_text(color = "grey20", size = 14, hjust = .5, vjust = .5),
        axis.title.y = element_text(size = 18, face = 'bold'),
        legend.background = element_rect(fill = '#d9d9d9',  colour = '#d9d9d9'),
        legend.key = element_rect(fill = NA),
        legend.position = c(.15, .9),
        legend.title = element_blank(),
        legend.text =  element_text(size = 18, face = 'bold'),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.spacing.x = unit(2, 'lines'),
        plot.background = element_rect(fill = '#d9d9d9',  colour = '#d9d9d9'),
        strip.text.x =element_blank() #  element_text(size = 16, face = 'bold'
      )
    # migration_speed
    
    
    grandTable <-rbind(grandTable, data.frame(annualStats, overalMean = overallMean$mean, speedToPlot = speedToPlot,
                             region= region, obsThresh = obsThresh, fractThresh = fractThresh,
                             slopThres = slopThres, sequenceLength= sequenceLength))
    
    # ggsave(migration_speed,
    #         filename = paste0("./data/processed/offshore_points/scenarios/",'timeseries_',speedToPlot,
    #                          '_',region,'_',obsThresh,'_',fractThresh,'_',slopThres,
    #                          '_seqL',sequenceLength, '_',
    #                          format(Sys.Date(), "%Y%m%d"),'.jpeg'),
    #        width = 13.1, height = 7.25, units = c('in'), dpi = 1200)
    # ggsave(pSpeed,
    #        filename = paste0("./data/processed/offshore_points/scenarios/",'hovmoller_',speedToPlot,
    #                          '_',region,'_',obsThresh,'_',fractThresh,'_',slopThres,
    #                          '_seqL',sequenceLength, '_',
    #                          format(Sys.Date(), "%Y%m%d"),'.jpeg'),
    #        width = 13.1, height = 7.25, units = c('in'), dpi = 1200)
    
  }
}

for (cn in 1:length(aoi)){
  # cn <- 1
  country = aoi[cn]


  write_csv(  grandTable %>%
                dplyr::filter(Country == country),
            paste0(wd,"/data/temp/", country,
                   '_', speedToPlot, '_scenarioVariability','.csv'))



}


############## 
## scenario variability 

# sceneario Folder
tempFolder <- './data/temp'

folderSelect <- as.matrix(list.files(paste0(tempFolder), full.names = T)) # , '/offshore_points'
dfFiles <- rewrite(folderSelect)

# the outputs 
allScenarios <- unique(do.call(rbind, lapply(as.matrix(dfFiles)[,1], 
                                         function(x) read.csv(x, stringsAsFactors = FALSE,
                                                              sep = ',', na.strings=c("","NA")
                                         )))) %>%
  dplyr::mutate(groupName = paste0(obsThresh,'_', slopThres, '_',
                                   fractThresh, '_',sequenceLength))%>%
  dplyr::group_by(Country, id,speedToPlot) %>%
  dplyr::mutate(annualMean = mean(mean, na.rm=T)) %>%
  ungroup()

# Country, speedToPlot, obsThresh, fractThresh, slopThres,sequenceLength
groupsOfScenarios <- allScenarios %>%
  group_by(Country) %>%

  distinct(speedToPlot,annualMean, obsThresh, fractThresh, slopThres,sequenceLength,
           overalMean, groupName)
  

# mean = mean migration for given year
# overallmean = mean migration for entire period
# aes(x=eval(as.name(xaxis)), y = value, fill = eval(as.name(boxes))))
facet <- 'obsThresh' # 'fractThresh', 'sequenceLength', 'slopThres'
boxes <- ggplot(data = groupsOfScenarios, 
                aes(x= as.factor(Country), y = overalMean/1000, 
                    fill = as.factor(eval(as.name(facet))))) +
  geom_boxplot() +
  facet_wrap(~speedToPlot) +
  labs(y = "migration speed [km/yr]") + 
  theme(
    axis.line.x = element_line(size = 0.5, colour = "black"),
    axis.line.y = element_line(size = 0.5, colour = "black"),
    axis.line = element_line(size=1, colour = "black"),
    axis.text.x =element_text(color = "grey20", size = 18, hjust = .5, vjust = .5, face = "bold"),
    axis.text.y = element_text(color = "grey20", size = 18, hjust = .5, vjust = .5, face = "bold"),
    axis.title.y = element_text(size = 20, face = 'bold'),
    axis.title.x =  element_blank(),
    
    legend.background = element_rect(fill = '#d9d9d9',  colour = '#d9d9d9'),
    legend.key = element_rect(fill = NA),
    legend.text = element_text(size = 25),#element_blank(),
    legend.title =  element_blank(),
    legend.position = 'bottom',#c(.2, -.07),
    
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.spacing.x = unit(2, 'lines'),
    plot.background = element_rect(fill = '#d9d9d9',  colour = '#d9d9d9'))
  
boxes
# ggsave(boxes, filename = paste0("./results/temp_maps/",
#                                               'Guianas_migrationSpeed_boxplots_',
#                                               facet, '_',
#                                               format(Sys.Date(), "%Y%m%d"),'.jpeg'),
#        width = 13.1, height = 7.25, units = c('in'), dpi = 1200)

####
## annual times series
annualTimeseries <- allScenarios %>%
  dplyr::filter(as.Date(id) > as.Date('1999-01-01')) %>%
  group_by(Country) %>%
  dplyr::summarize(overallMean = mean(mean, na.rm = T),
                   region = Country[1])

# Country, speedToPlot, obsThresh, fractThresh, slopThres,sequenceLength
timeseriesScenarios <- ggplot(data = allScenarios ,
            aes(x=year(as.Date(id)), y = mean/1000, fill = as.factor(groupName),
                color = as.factor(speedToPlot))) +
  geom_point(size = 3, alpha = 0.1) +
  geom_line(aes(y=annualMean/1000,
                group = as.factor(speedToPlot)),
            size = 1.5) +
  geom_hline(data = annualTimeseries,aes(yintercept = overallMean/1000))+
  # geom_smooth(method="auto", se=F, fullrange=FALSE) +
  facet_wrap(~region, ncol = 1,nrow = 3) + 
  labs(x='date', y = "migration speed [km/yr]") +
  # scale_color_manual()
  guides(fill = "none", 
         color=guide_legend(override.aes=list(alpha=1))) +
  theme(
    axis.line.x = element_line(size = 0.5, colour = "black"),
    axis.line.y = element_line(size = 0.5, colour = "black"),
    axis.line = element_line(size=1, colour = "black"),
    axis.text.x =element_text(color = "grey20", size = 18, hjust = .5, vjust = .5, face = "bold"),
    axis.text.y = element_text(color = "grey20", size = 18, hjust = .5, vjust = .5, face = "bold"),
    axis.title.y = element_text(size = 20, face = 'bold'),
    axis.title.x = element_text(size = 20, face = 'bold'),
    
    legend.background = element_rect(fill = '#d9d9d9',  colour = '#d9d9d9'),
    legend.key = element_rect(fill = NA),
    legend.text = element_text(size = 25),#element_blank(),
    legend.title =  element_blank(),
    legend.position = 'bottom',#c(.2, -.07),
    
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.spacing.x = unit(2, 'lines'),
    strip.background = element_rect(fill = "#d9d9d9", colour = "#d9d9d9"),
    strip.text.x = element_text(size = 18, face = "bold"), # Facet titles
    plot.background = element_rect(fill = '#d9d9d9',  colour = '#d9d9d9'))

timeseriesScenarios


# ggsave(timeseriesScenarios, filename = paste0("./results/temp_maps/",
#                                   'Guianas_migrationSpeed_timeseries','_',
#                                   format(Sys.Date(), "%Y%m%d"),'.jpeg'),
#       width = 13.1, height = 7.25, units = c('in'), dpi = 1200)







