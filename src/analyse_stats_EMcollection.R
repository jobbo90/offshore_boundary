## ---------------------------
##
## Script name: Boxplot sensitivity
#'
#' Short Description: 
#' 
#' Author: Job de Vries
#' 
#' Date Created: 2020-03-19
#' 
#' Copyright (c) Job de Vries, 2020
#' 
#'  Email: j.devries4@uu.nl 
#'  
# ---------------------------
#' Description
#' reads .csv files for different sets of parameters 
#' creates boxplots indicating sensitivity of variables on these parameters  
#' 
#'     
# ---------------------------

rm(list = ls())

## set working directory 
# wd <- getwd()
# setwd("./..")   # one level up


## ---------------------------

options(scipen = 6, digits = 4) # non-scientific notation
memory.limit(30000000)     # this is needed on some PCs to increase memory allowance

## ---------------------------

## load up the packages we will need:  (uncomment as required)
# 
# require(tidyverse)
# require(data.table)
source("./src/packages.R")       # loads up all the packages we need

## ---------------------------

## load up functions into memory
source("./src/functions.R")

# library(reshape2)
## ---------------------------
#' user defined variables
#' user defined variables
buffer = c(5, 10, 15)             # 5,10, 15
lengthmax = c(10, 25,50,75)     # 10,25, 50, 75
sigma = c(0.3, 0.5, 0.7, 0.9, 1) # 0.3, 0.5, 0.7, 0.9, 1
cannyThreshold = c(0.7, 0.9, 1.1, 1.3)          # 0.7, 0.9, 1.1, 1.3 
daterange = c('1985-01-01', '2020-12-31')
aoi = c('Brazil') # FrenchGuiana / Suriname / Guyana / Brazil
dataFolder <- './data/raw/GEE_exports/EM_stats'
# dataFolder <- './data/raw/GEE_exports/EM_stats/20210831'

# control values to plot on y-axis
pattern = 'ndwi_threshold'       # ndvi_rightPeak / ndwi_threshold / ndwi_TIRintertide / ndwi_maxWater
# controls the panels
facet <- 'collectiontype'     # sigma  / MinimalGradient / buffer / length / collectiontype
xaxis <- 'buffer'            # sigma  / MinimalGradient / buffer / length
# controls the boxes to plot (fill)
boxes <- 'length'        # sigma  / MinimalGradient / buffer / length  

# what to draw
drawSignatures <- 0   #  (0 / 1)
drawObsDrops   <- 1
drawThresholds <- 0
colnames_ofInterest <- c('ndvi_rightPeakLower', 'ndvi_rightPeakUpper', 
                         'ndvi_leftPeakLower', 'ndvi_leftPeakUpper', 
                         'ndwi_maxWaterLower',
                         'ndwi_maxWaterUpper', 'ndwi_threshold', 
                         'ndwi_TIRintertide')
# select folders
folderSelect <- as.matrix(list.files(dataFolder, full.names = T,  pattern=".csv"))
metaMatrix <- as.matrix(folderSelect)
df <- rewrite(metaMatrix)

# filter folders on string match
# IMRPOVE: create custom functions
filtered = vector('list', 1000)
for (q in seq_along(buffer)) {
  for (x in seq_along(lengthmax)) {
    for (y in seq_along(sigma)) {
      for (z in seq_along(cannyThreshold)) {
        for (c in aoi){
      
      # country = c
      buf = paste0('buffer',buffer[q])
      lenmx = paste0('length',lengthmax[x])
      sig = paste0('sigma',sigma[y]*10)
      thresh = paste0('Threshold', cannyThreshold[z]*10)
      
      filters = c(buf, lenmx,sig,thresh,c)
      
      filtered = rbind(filtered, df %>% 
                         dplyr::filter(
                           filters %>%
                             # apply the filter of all the text rows for each pattern
                             # you'll get one list of logical by pattern ignored_string
                             purrr::map(~ to_keep(.x, text = text)) %>%
                             # get a logical vector of rows to keep
                             purrr::pmap_lgl(all)
                         ))
        }}}}}

# filter duplicates
filtered <- unique(filtered)

#--------------------------------------
#' build a data frame containing the information from each unique parameter combination (.csv)
#' Make it compatible with ggplot so each obs. needs to be in a seperate row.
#' 

filterSums <- vector('list', 1000)#matrix(vector(), nrow(filtered), 8)
output <- vector('list', 1000)
obs <- matrix(vector(), nrow(filtered), 2)
signaturesFinal <- vector('list', 10000)

for(i in 1:nrow(filtered)){
  # i = 2
  csv = as.matrix(read.csv2(as.character(filtered[i,1]), 
                            header = T, sep = ',', na.strings=c("","NA"))) # rewrite as matrix to read columns as numeric values
  
  # Read variable values (as character) from file
  bufferCol = grep(paste( '^', 'buffer', sep = ''), colnames(csv), fixed = F)
  bufferSize <- as.numeric(gsub("buffer", "", 
                                strsplit(as.character(filtered[i,1]),
                                         '_')[[1]][12]))

  lengthmax <- as.numeric(gsub("length", "", 
                               strsplit(as.character(filtered[i,1]),
                                        '_')[[1]][11]))
  sigma <-  as.numeric(gsub("sigma", "", 
                            strsplit(as.character(filtered[i,1]),
                                     '_')[[1]][14]))
  cannyThreshold <- as.numeric(gsub("Threshold", "", 
                                    strsplit(as.character(filtered[i,1]),
                                             '_')[[1]][13]))
  
  country <- gsub("stats/", "",strsplit(as.character(filtered[i,1]),
                             '_')[[1]][3])
  
  # create a groupIdentifier 
  groupIdentifier = paste0(bufferSize, '_buffer_', lengthmax, '_length_', sigma, '_sigma_', cannyThreshold, '_threhsold')
  
  idCol = grep(paste( '^', 'LANDSAT_PRODUCT_ID', sep = ''), colnames(csv), fixed = F)
  cloudCoverCol = grep(paste( '^', 'CLOUD_COVER$', sep = ''), colnames(csv), fixed = F)
  reflecCol <- grep(paste( '^', 'COLLECTION_CATEGORY$', sep = ''), colnames(csv), fixed = F)
  
  
  if(length(idCol) < 1){
    idCol = grep(paste( '^', 'LANDSAT_ID', sep = ''), colnames(csv), fixed = F)
  }
  
  dateCol = grep(paste( '^', 'DATE_ACQUIRED', sep = ''), colnames(csv), fixed = F)
  if(length(dateCol) < 1){
    dateCol = grep(paste( '^', 'SENSING_TIME', sep = ''), colnames(csv), fixed = F)
  }
  
  # filter on date range when necessary
  dates = data.frame(as.Date(substr(csv[, dateCol],0 , 10)))
  names(dates) <- c('date')
  
  # get the columns that containt mean/std per band and end-member combination
  # pattern is now supplemented with an $ sign to ensure 1 column output
  # or left out when multiple colomns are required (signature columns)
  colOfInterest <- col_of_interest(csv, pattern)
  ndvi_peak <- col_of_interest(csv, 'ndvi_rightPeak$')
  ndwi_thresh <- col_of_interest(csv, 'ndwi_threshold$')
  ndwi_tempInt <- col_of_interest(csv, 'ndwi_TIRintertide$')
  veg_mean <- col_of_interest(csv, 'veg_mean')
  veg_std <- col_of_interest(csv, 'veg_stdDev')
  inertide_mean <- col_of_interest(csv, 'intertide_mean')
  intertide_std <- col_of_interest(csv, 'intertide_stdDev')
  water_mean <- col_of_interest(csv, 'water_mean')
  water_std <- col_of_interest(csv, 'water_stdDev')
  
  # create unique ID per row
  imageID = csv[,idCol]
  cloudCover = csv[,cloudCoverCol]
  T1T2 = csv[,reflecCol]
  
  uniqueID = paste(csv[,idCol], paste0("_", groupIdentifier), sep="")
  names(uniqueID) <- c('ID')
  
  bandNames <- c('mean_blue', 'mean_green', 'mean_nir', 'mean_red', 'mean_swir1', 'mean_swir2') # ensure equal bandNames
  
  signaturesVeg = data.frame(uniqueID,groupIdentifier,dates,csv[, idCol], paste0('veg'),
                             apply(csv[, c(bufferCol, veg_mean, ndvi_peak, ndwi_thresh,ndwi_tempInt )], 2, as.numeric))
  names(signaturesVeg)[grep(paste0('^', 'paste0', sep = ''), colnames(signaturesVeg), fixed = F)] <- 'EMclass'
  names(signaturesVeg)[grep(paste0('^', 'veg_mean', sep = ''), colnames(signaturesVeg), fixed = F)] <- bandNames
  
  signaturesWater = data.frame(uniqueID,groupIdentifier,dates,csv[, idCol], paste0('water'),
                               apply(csv[, c(bufferCol, water_mean, ndvi_peak, ndwi_thresh,ndwi_tempInt )], 2,as.numeric))
  names(signaturesWater)[grep(paste0('^', 'paste0', sep = ''), colnames(signaturesWater), fixed = F)] <- 'EMclass'
  names(signaturesWater)[grep(paste0('^', 'water_mean', sep = ''), colnames(signaturesWater), fixed = F)] <- bandNames
  
  
  signaturesinter = data.frame(uniqueID,groupIdentifier,dates,csv[, idCol], paste0('intertide'),
                               apply(csv[, c(bufferCol, inertide_mean, ndvi_peak, ndwi_thresh,ndwi_tempInt )], 2,as.numeric))
  names(signaturesinter)[grep(paste0('^', 'paste0', sep = ''), colnames(signaturesinter), fixed = F)] <- 'EMclass'
  names(signaturesinter)[grep(paste0('^', 'intertide_mean', sep = ''), colnames(signaturesinter), fixed = F)] <- bandNames
  
  # signatures = cbind(groupIdentifier, signatures) # add scenario identifier
  signaturesFinal = rbind(signaturesFinal, signaturesVeg, signaturesWater,signaturesinter)
  
  
  ###################
  ## These result in a row per scenario obs
  
  snrVals = as.numeric(csv[, grep(paste('^','SNR_endmembers', sep = ''), colnames(csv), fixed = F)])
  rmseVals = as.numeric(csv[, grep(paste('^','meanRMSE', sep = ''), colnames(csv), fixed = F)])
  
  # Not al scenario's already contain the rmse values, was added later so include 0 value array 
  if(length(rmseVals) < 1){
    rmseVals = as.numeric(rep(0, nrow(csv)))
  }

  
  values = data.frame(group = groupIdentifier , value = as.numeric(csv[, colOfInterest]), GPF = sigma,
                      MG = cannyThreshold, buffer= bufferSize, length = lengthmax, 
                      imageID = imageID, snr = snrVals, rmse = rmseVals,
                      CLOUD_COVER = cloudCover, collectiontype = T1T2)
  obs[i,1] = nrow(csv)              # total amount of observations
  
  updatedvalues = values[(dates> min(daterange)),] # within date range
  updatedvalues = values[(dates < max(daterange)),]
  updatedvalues = drop_na(values)          # drop NA
  updatedvalues = filter(values, values[,2]!=99) # drop 99 values
  
  obs[i,2] = nrow(updatedvalues)
  
  
  if(pattern == c('ndwi_TIRintertide')){
    # print('change values')
    values$value = (values$value+273.15)/0.1
  }
  #   # get all filter columns
  filtersFC = grep(paste0('^', 'filt_', sep = ''), colnames(csv), fixed = F)
  filterNames = colnames(csv[,filtersFC])
  
  # append total amount of observations
  filterSums = rbind(filterSums, data.frame(group = groupIdentifier , 
                                            value = nrow(csv), sigma = sigma,
                                            cannyThreshold = cannyThreshold, 
                                            bufferSize = bufferSize, 
                                            lengthmax = lengthmax, 
                                            T1Obs = length(which(T1T2 == 'T1')),
                                            T2Obs = length(which(T1T2 == 'T2')),
                                            filterNames = 'filt_0_totObs'))
  
  subsetCSVt1 <- csv[which(T1T2 == 'T1'),]
  
  # itirate over all the filters; get the amount of observations passing the filter  
  for(q in 1:length(filtersFC)){
    # q <- 1
    totalVal <- sum(as.numeric(csv[, filtersFC[q]]), 
                    na.rm = T)
    T1Val <- sum(as.numeric(subsetCSVt1[, filtersFC[q]]), 
                 na.rm = T)
    T2Val <- totalVal-T1Val
    
    filterSums = rbind(filterSums, 
                       data.frame(group = groupIdentifier , 
                                  value = sum(as.numeric(csv[, filtersFC[q]]), 
                                              na.rm = T), 
                                  sigma = sigma,
                                  cannyThreshold = cannyThreshold, 
                                  bufferSize = bufferSize, 
                                  lengthmax = lengthmax, 
                                  T1Obs = T1Val,
                                  T2Obs = T2Val,
                                  filterNames = filterNames[q]))
  }

  
  # store in an empty vector     
  output = rbind(output, values)
  
}

# filter -1 values in spectral columns ==> doesn't matter which spectral band
columns <- grep(paste0('^', 'mean', sep = ''), colnames(signaturesFinal), fixed = F)
signaturesFinal = subset(signaturesFinal,signaturesFinal[,columns[1]] > 0)


# probably melt the signaturesFinal data
# unique identifier should contain imageID + groupIdentifier
dataMelt<- melt(signaturesFinal,id.vars=c('uniqueID', 'groupIdentifier', 'EMclass','buffer'), 
                measure.vars=c('mean_blue','mean_green', 'mean_red','mean_nir', 'mean_swir1', 'mean_swir2'))

dataMelt <- dataMelt %>%
  mutate(centralVal = case_when(dataMelt$variable == 'mean_blue' ~  0.47,
                                dataMelt$variable == 'mean_green' ~  0.56,
                                dataMelt$variable == 'mean_red' ~  0.66,
                                dataMelt$variable == 'mean_nir' ~  0.84,
                                dataMelt$variable == 'mean_swir1' ~  1.65,
                                dataMelt$variable == 'mean_swir2' ~  2.32,
                                TRUE ~ 0))


#---------
#' create plots:
#' 1. Boxplots with the range of band values per end-member
#' 2. Boxplots for the range of thresholds per parameter set 

filterLabels = c('Vegetation', 'Water', 'Mud')

# # for boxPlotSignatures filter to 1 scenario only
scenario <- unique(dataMelt$groupIdentifier)
# dataMelt <- subset(dataMelt, groupIdentifier %in% scenario)

variable_names <- data.frame(matrix(ncol = length(scenario), nrow = 1))

for(i in seq_len(length(scenario))){
  # i <- 1
  
  split <- strsplit(as.character(scenario[i]), '_')[[1]]
  
  variable_names[1,i] <- paste0('buffer: ', split[1], ', length: ', split[3],
                                ', Gaussian Filter: ', split[5], ', Gradient Magnitude: ', split[7])
  
  colnames(variable_names)[i] <- as.character(scenario[i])
  
}


if (drawSignatures > 0){

  bandcentrals <- c(0.47, 0.56, 0.66, 0.84, 1.65, 2.32)
  

  # group identifier should be the end-member class? ==> set facet wrap to end-member class
  boxplotSignatures <- ggplot(dataMelt, aes(x=variable, y = value, fill=EMclass)) +
    geom_boxplot(outlier.colour="black", outlier.size=0.5, width=0.4) +
    scale_fill_manual(labels = filterLabels, values=c('#4daf4a','#377eb8','#e41a1c')) +
    labs(y = "Reflectance", x = 'band') + 
    # scale_x_discrete(labels = bandcentrals)+
    
    scale_x_discrete(labels=c("mean_blue" = "blue","mean_green" = "green", "mean_red" = "red",
                              "mean_nir" = "nir", "mean_swir1" = "swir1", "mean_swir2" = "swir2")) +
    # ggtitle(paste0('Spread surface Reflecatance' )) +
    coord_cartesian(ylim = c(0,0.4))   +                            # limit
    # facet_wrap(paste0('~', facet), labeller = as_labeller(unlist(variable_names)))
    facet_wrap(~groupIdentifier, labeller = as_labeller(unlist(variable_names)))  + # create a image per scenario identifier
    theme(

      axis.line.x = element_line(size = 0.5, colour = "black"),
      axis.line.y = element_line(size = 0.5, colour = "black"),
      axis.line = element_line(size=1, colour = "black"),
      axis.text.x = element_text(color = "grey20", size = 14, hjust = .5, vjust = .5, face = "bold"),
      axis.text.y = element_text(color = "grey20", size = 14, hjust = .5, vjust = .5, face = "bold"),
      axis.title.x = element_text(size = 18, face = 'bold'),
      axis.title.y = element_text(size = 18, face = 'bold'),

      strip.background = element_rect(fill = "white", colour = "white"),
      legend.key = element_rect(fill = NA),
      legend.text = element_text(size = 18),
      legend.position = c(.9, .8),
      legend.title = element_text(colour = 'black', size = 20, face = 'bold'),
      
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      panel.spacing.x = unit(2, 'lines'),
      
      strip.text.x = element_blank()#element_text(size = 20, face = 'bold') # Facet titles


      )
  
  boxplotSignatures

  # improve to set correct dimensions (e.g. fitting on A4)
  
  # ggsave(filename = paste0("./results/figures/", format(Sys.Date(), "%Y%m%d"), 
  #                          '_EM_surface_reflectance_signatures_', format(as.Date(daterange[1]), "%Y"), 
  #                          '_', format(as.Date(daterange[2]), "%Y"),
  #                          "_wrap_",facet, ".jpeg"), dpi = 900,
  #        plot = boxplotSignatures) 
  
  # ggsave(filename = paste0("./results/figures/", format(Sys.Date(), "%Y%m%d"), 
  #                          '_EM_surface_reflectance_signatures_', format(as.Date(daterange[1]), "%Y"), 
  #                          '_', format(as.Date(daterange[2]), "%Y"),
  #                          "_wrap_",facet, ".pdf"), dpi = 900,
  #        plot = boxplotSignatures) 
  
}

#------------
#' Threshold variability
#' Update data frame
#' Ensure that ony entries are compared that occuring in each parameter set
#' e.g. they are not filtered out
#' 

uniqueIDs <- as.data.frame(table(output$imageID)) #as.character(unique(output$imageID))
uniquegroups <- as.data.frame(table(output$group)) #as.character(unique(output$group))

IDtoBeRemoved <- as.character(uniqueIDs[uniqueIDs[,2] < max(uniqueIDs[,2]),1])
AmountToBeRemove <- sum(output$imageID %in% IDtoBeRemoved)
output <- output[!output$imageID %in% IDtoBeRemoved,]

output.cor <- ddply(.data=output, 
                    .(group), 
                    summarize, 
                    n=paste("n =", length(value)))

upperlimit <- max(output$value)+0.1


NR_facets <- unique(output[facet])
variable_names <- data.frame(matrix(ncol = nrow(NR_facets), nrow = 1))
for(i in seq_len(nrow(NR_facets))){
  # i <- 1
  variable_names[1,i] <- paste0(facet, ': ', as.character(NR_facets[i,]))
  
  colnames(variable_names)[i] <- as.character(NR_facets[i,])
  
}

subset <- subset(output, value!=99)

boxplot <- ggplot(subset, aes(x=eval(as.name(xaxis)), y = value, fill = eval(as.name(boxes)))) +
  facet_wrap(paste0('~', facet), labeller = as_labeller(unlist(variable_names))) + # implement if there is another variable changing ==> results in a second plot
  # geom_dotplot(binaxis='y', stackdir='center', position=position_dodge(1), binwidth = 1/100) +
  geom_boxplot(outlier.colour="black", outlier.size=2, width=0.6) +          # boxplot properties
  # stat_summary(fun.data = n_fun, geom = "text",  hjust = 0.5) +
  labs(y = "index", x = xaxis, fill = boxes) + 
  scale_x_discrete(expand=c(0.2,0)) +
  # ggtitle(paste0(pattern, " per parameter set", '\n', 'between ', daterange[1], ' and ' , daterange[2])) +
  theme(
    axis.line.x = element_line(size = 0.5, colour = "black"),
    axis.line.y = element_line(size = 0.5, colour = "black"),
    axis.line = element_line(size=1, colour = "black"),
    axis.text.x = element_text(color = "grey20", size = 14, hjust = .5, vjust = .5, face = "bold"),
    axis.text.y = element_text(color = "grey20", size = 14, hjust = .5, vjust = .5, face = "bold"),
    axis.title.x = element_text(size = 18, face = 'bold'),
    axis.title.y = element_text(size = 18, face = 'bold'),
    
    strip.background = element_rect(fill = "white", colour = "white"),
    legend.key = element_rect(fill = NA),
    legend.text = element_text(size = 18),
    # legend.position = c(.9, .8),
    legend.title = element_text(colour = 'black', size = 20, face = 'bold'),
    
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.spacing.x = unit(2, 'lines'),
    
    strip.text.x = element_text(size = 16, face = 'bold') # Facet titles
        )


boxplot

if (drawThresholds > 0){
  # improve to set correct dimensions (e.g. fitting on A4)
  
  # boxplot
  
  # ggsave(filename =  paste0("./results/figures/", format(Sys.Date(), "%Y%m%d"), '_' ,pattern, 
  #                         '_', format(as.Date(daterange[1]), "%Y"), '_', format(as.Date(daterange[2]), "%Y"),
  #                          "_wrap_",facet, ".jpeg"), dpi = 900, plot = boxplot) 
  # 
  # ggsave(filename =  paste0("./results/figures/", format(Sys.Date(), "%Y%m%d"), '_' ,pattern, 
  #                           '_', format(as.Date(daterange[1]), "%Y"), '_', format(as.Date(daterange[2]), "%Y"),
  #                           "_wrap_",facet, ".pdf"), dpi = 900, plot = boxplot) 
  
}

# for (i in 1:nrow(filtered)){
#   # i = 1
# 
#   file = as.character(filtered[i,1])
#   csv = as.matrix(read.csv2(file, header = T, sep = ',', na.strings=c("","NA"))) # rewrite as matrix to read columns as numeric values
#   
#   }


#--------------------------------------------------
#' 
#' Create scatter plots
#' 1) RMSE versus threshold
#' 2) SNR versus thresholds
#' 

scatterPoint <- ggplot(subset, aes(x=value, y = snr, fill = group)) +
  # facet_wrap(paste0('~', facet), labeller = as_labeller(unlist(variable_names))) +
  geom_smooth(method = "lm", show.legend = F) +
  geom_point() + 
  scale_fill_manual(name = 'scenario', values = c('orange'),
                    labels = c('b10_L25_sig0.7_thresh0.9')) + 
  
  labs(y = "snr", x = 'threshold') + 
  scale_x_continuous(expand=c(0.2,0), breaks = c(-0.35, -0.25, -0.15, -0.05, 0,05)) +
  theme(axis.line.x = element_line(size = 0.5, colour = "black"),
        axis.line.y = element_line(size = 0.5, colour = "black"),
        axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_blank(),
        
        legend.key = element_rect(fill = NA),
        legend.title = element_text(colour = 'black', size = 14, face = 'bold'),
        legend.text = element_text(size = 12),
        
        panel.grid.minor = element_blank(), panel.border = element_blank(),
        panel.background = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 14, face = 'bold'),
        strip.text.x = element_blank(), # Remove facet labels
        
        legend.position = c(.7, .84),
        axis.text.x=element_text(colour="black", size = 12),
        axis.text.y=element_text(colour="black", size = 12),
        axis.title.x = element_text(size = 14, face = 'bold'),
        axis.title.y = element_text(size = 14, face = 'bold'),
        
        plot.title = element_text(hjust = 0.5))

if (drawThresholds > 0){
  # improve to set correct dimensions (e.g. fitting on A4)
  
  scatterPoint
  
  # ggsave(filename =  paste0("./results/figures/", format(Sys.Date(), "%Y%m%d"), '_scatterplot', 
  #                           '_threshold_snr_', format(as.Date(daterange[1]), "%Y"), '_', format(as.Date(daterange[2]), "%Y"),
  #                           ".jpeg"), dpi = 900, plot = scatterPoint, 
  #        width = 200, height = 150, units = "mm") 
  
}




scatterrmse <- ggplot(output, aes(x=snr, y = rmse, fill = group)) +
  # facet_wrap(paste0('~', facet), labeller = as_labeller(unlist(variable_names))) +
  geom_point()
scatterrmse

#--------------------------------------------------

# When necessary filter extra 
bufferFilter = 10
faceWrap=paste0('~cannyThreshold')
filterSums = subset(filterSums, bufferSize == bufferFilter) #bufferSize == bufferFilter &  cannyThreshold == 0.9
filterLabels = c('total obs', 'Clouds < 90%', 'edgeBuffer detected', '-0.5 \u2264 OTSU \u2264 0.1', '-1 \u2264 NDWI \u2264 1', 'Pure Pixels', 
                 'SNR > 3', 'EM > 0' )

# update such that for each filtering step the total amount of observations
# the amount of T2 and T1 is aggregated
filterSumsReshape <- as.vector(unique(filterSums$filterNames))

# test <- filterSums %>%
#   group_by(filterNames) %>%
#   summarise_each(funs(value)) %>%
#   ungroup()

totalObs <- data.frame(aggregate(filterSums$value, 
                      by=list(filterNames=filterSums$filterNames), FUN=sum),
                      collection = 'all')
t1Obs <- data.frame(aggregate(filterSums$T1Obs, 
                   by=list(filterNames=filterSums$filterNames), FUN=sum),
                   collection = 'T1')
t2Obs <- data.frame(aggregate(filterSums$T2Obs, 
                   by=list(filterNames=filterSums$filterNames), FUN=sum),
                   collection = 'T2')
reshaped <- rbind(totalObs, t1Obs, t2Obs)


barPlot = ggplot(reshaped, aes(fill = filterNames, 
                                           y=x, x =  filterNames)) +
  facet_wrap(~collection)+

  geom_bar(position = 'dodge', stat = 'identity')+
  scale_fill_manual(labels = filterLabels, values=c('#d53e4f','#f46d43','#fdae61','#fee08b','#e6f598','#abdda4','#66c2a5','#3288bd')) + # fill colour

  labs(y = "Amount of Images") + 
  ggtitle(paste0("Amount of images in: ", '\n', aoi)) +

  
  theme(axis.line.x = element_line(size = 0.5, colour = "black"),
        axis.line.y = element_line(size = 0.5, colour = "black"),
        axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x=element_blank(),#element_text(colour="black", size = 12),
        axis.title.x=element_blank(),
        axis.text.y=element_text(colour="black", size = 12),
        plot.title = element_text(hjust = 0.5))

if(drawObsDrops > 0){

  barPlot
  
  ggsave(filename = paste0("./results/temp_maps/",aoi,
                           '_imageCount','_',  format(Sys.Date(), "%Y%m%d"),'.jpeg'),
         width = 20.1, height = 7.25, units = c('in'), dpi = 1200)
  
}
