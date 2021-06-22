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
library(strex)                  # only required here

## ---------------------------
source("./src/functions.R")

## ---------------------------

mapviewOptions(basemaps = c( "Esri.WorldImagery","Esri.WorldShadedRelief", "OpenStreetMap.DE"))
dataFolder <- './data/'
# raw\GEE_exports\allArrayExport_test
aoi <- c('229_56')

# select folders
folderSelect <- as.matrix(list.files(paste0(dataFolder, 'raw/GEE_exports/allArrayExport_test'), 
                                     full.names = T))
df <- rewrite(folderSelect);

# select folders
selectedCSV <- as.matrix(list.files(paste0(dataFolder, './processed/offshore_points'), full.names = T))

# only csv's
df <- df[grep('.csv', folderSelect, ignore.case = T),]
filtered <- vector('list', 100)
for (x in seq_along(aoi)){
      # q <- 1
      region = aoi[x]
      
      filters = c(region)
      
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
allFiles <- unique(do.call(rbind, lapply(as.matrix(filtered)[,1], 
                                         function(x) read.csv(x, stringsAsFactors = FALSE,
                                                              sep = ',', 
                                                              na.strings=c("","NA")
                                                              ))))
# unique(allFiles$DATE_ACQUIRED)

dateOfInterest <- "2009-11-15"
posOfInterest <- 130000

# only csv's
transectOutput <- selectedCSV[grep('.csv', selectedCSV, ignore.case = T),]
date <- year(as.Date(dateOfInterest))

transectOutput <- transectOutput[grepl(as.character(date), transectOutput, ignore.case = T)]

readData <- read.csv(transectOutput, stringsAsFactors = FALSE,
                     sep = ',', na.strings=c("","NA"))
readData_sub <- subset(readData, DATE_ACQUIRED ==dateOfInterest & pos == posOfInterest)


# subset on date & pos
dateSubset <- subset(allFiles, DATE_ACQUIRED ==dateOfInterest & pos == posOfInterest)

# colnames(dateSubset)
# [1] "system.index"            "CLOUD_COVER"             "DATE_ACQUIRED"           "areaName"               
# [5] "coastX"                  "coastY"                  "coastline"               "endX"                   
# [9] "endY"                    "mud"                     "mud_bilat"               "mud_bilat_peaks"        
# [13] "mud_bilat_valleys"       "mud_superSmooth"         "mud_superSmooth_peaks"   "mud_superSmooth_valleys"
# [17] "ndwi_threshold"          "offsetLast"              "originX"                 "originY"                
# [21] "pos"                     "x_axis"                  ".geo" 


Mud <- unlist(str_extract_numbers(
  dateSubset$mud,
  decimals = T,
  leading_decimals = T,
  negs = T,
  sci = T,
  commas = FALSE,
  leave_as_string = FALSE))

bilatMud <- unlist(str_extract_numbers(
  dateSubset$mud_bilat,
  decimals = T,
  leading_decimals = T,
  negs = T,
  sci = T,
  commas = FALSE,
  leave_as_string = FALSE))

coast <- as.numeric(unlist(regmatches( dateSubset$coastline,
                                       gregexpr("[[:digit:]]+\\.*[[:digit:]]*", 
                                                dateSubset$coastline))))
bilatPeaks <- unlist(str_extract_numbers(
  dateSubset$mud_bilat_peaks, decimals = T, leading_decimals = T,
  negs = T, sci = T, commas = FALSE, leave_as_string = FALSE))


x_ax <- as.numeric(unlist(regmatches( dateSubset$x_axis,
                                      gregexpr("[[:digit:]]+\\.*[[:digit:]]*", 
                                               dateSubset$x_axis))))
superSmooth <- as.numeric(unlist(regmatches( dateSubset$mud_superSmooth,
                                            gregexpr("[[:digit:]]+\\.*[[:digit:]]*", 
                                                     dateSubset$mud_superSmooth))))
superSmoothPeak <- as.numeric(unlist(regmatches( dateSubset$mud_superSmooth_peaks,
                                             gregexpr("[[:digit:]]+\\.*[[:digit:]]*", 
                                                      dateSubset$mud_superSmooth_peaks))))
## 
# build data frame 
df_fractions <- data.frame(mud = Mud, bilatMud = bilatMud,x_ax = x_ax,
                           bilatPeaks = bilatPeaks, supersmooth = superSmooth,
                           ssPeaks = superSmoothPeak)

sequences <- split(df_fractions$x_ax, cumsum(c(0, diff(df_fractions$x_ax) > 25)))

startPos <- vapply(sequences, head, n = 1L, FUN.VALUE = numeric(1))
endPos <- vapply(sequences, tail, n = 1L, FUN.VALUE = numeric(1))



if(length(startPos) > 1){
  # missing x-axis values
  missing_xax <- c()
  for (cnt in 1:(length(startPos)-1)){
    # cnt <- 4
    missing_xax <- c( missing_xax, seq(as.numeric(endPos[cnt]), as.numeric(startPos[cnt+1]), 25))
    
  }
  
  # get the missing x in the original dataframe
  
  df_fractions <- df_fractions %>%
    complete(x_ax = missing_xax) %>%
    arrange(x_ax)
  
}

xAxisDist <- c(readData_sub$axisDist)
yAxisDist <- c(readData_sub$mudFract)

maxExtent <- unique(readData_sub$maxExtent)
coastLine <- unique(readData_sub$coastDist)
minExtent <- unique(readData_sub$SmoothedPeak)

# diff(df_fractions$x_ax) > 25


ggplot() +
  # plot extent
  geom_rect(aes(xmin = minExtent, xmax = maxExtent,
              ymin = 0, ymax = 1, fill = 'grey'), alpha = 0.6,
            ) +
  
  # plot super smoothed
  geom_point(data = df_fractions, aes(x = x_ax, y = supersmooth,
                                      colour = 'yellow'),
              alpha = 0.5, size = 2.5) +
  # plot original mud fraction values
  geom_point(data = df_fractions, aes(x = x_ax, y = mud, colour = 'black'), 
             alpha = 0.3, size = 2) +
  # plot bilateral filter values
  geom_line(data = df_fractions, aes(x = x_ax, y = bilatMud, colour = 'red'), 
            alpha = 1, size = 2.5) +
  # plot selected bilateral Peaks (larges, abs and slope drop)
  geom_point(aes(x = xAxisDist, y = yAxisDist, colour = 'blue'), size = 5) +
  geom_segment(aes(x = xAxisDist, y = yAxisDist + 0.05,
                   xend = xAxisDist + 1000, yend = yAxisDist+ 0.3),
               arrow = arrow(length = unit(5, "mm"), ends = "first", 
                             type = 'closed'),
               colour = 'blue') +
  scale_y_continuous(name = 'Fractions') +
  scale_x_continuous(name = 'Distance [m]') +
  scale_colour_manual(name = 'Legend', values =c('red' = '#d53e4f','blue' = 'blue',
                                                 'yellow'='#ff7f00',
                                                 'black' = 'black'), 
                      labels = c('fractions','peaks', 'bilateral filter',
                                 'smoothed')) +
  scale_fill_manual(name = ' ', values = c('grey'= '#8c510a'),
                    labels = c('active migration')) +
  guides(color=guide_legend(override.aes=list(fill='#d9d9d9'), ncol = 3)) +
  theme(
    
    axis.line.x = element_line(size = 0.5, colour = "black"),
    axis.line.y = element_line(size = 0.5, colour = "black"),
    axis.line = element_line(size=1, colour = "black"),
    axis.text.x = element_text(color = "grey20", size = 14, hjust = .5, vjust = .5, face = "bold"),
    axis.text.y = element_text(color = "grey20", size = 14, hjust = .5, vjust = .5, face = "bold"),
    axis.title.y = element_text(size = 30, face = 'bold'),
    axis.title.x = element_text(size = 30, face = 'bold'),

    # legend.background = element_rect(fill = '#d9d9d9',  colour = '#d9d9d9'),
    legend.key = element_rect(fill = NA),
    legend.text = element_text(size = 50),
    legend.position = c(.7, .7),
    legend.title =  element_blank(),#element_text(colour = 'black', size = 30, face = 'bold'),
    
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.spacing.x = unit(2, 'lines')
    # plot.background = element_rect(fill = '#d9d9d9',  colour = '#d9d9d9')
  )

ggsave(filename = paste0("./results/methodology_figures/fraction_dist_",
                         gsub(x=dateOfInterest,pattern="-",replacement="",fixed=T),
                         '_pos',posOfInterest,'_',  format(Sys.Date(), "%Y%m%d"),'.jpeg'),
        width = 20.1, height = 7.25, units = c('in'), dpi = 1200)
#   
