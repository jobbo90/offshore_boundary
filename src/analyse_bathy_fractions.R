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
# data\raw\GEE_exports\bathy_obs
# select folders
folderSelect <- as.matrix(list.files(paste0(dataFolder, '/GEE_exports/bathy_obs'), full.names = T))
df <- rewrite(folderSelect)
# only csv's
df <- df[grep('.csv', folderSelect, ignore.case = T),]
aoi <-  c('survey_212') 

filtered <- vector('list', 100)

for (x in seq_along(aoi)){
      # x <- 1
      # year = as.character(years[q])
      region = aoi[x]
      
      filters = c(region)
      
      # df %>% mutate(year = year(DATE_ACQUIRED))
      
      
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
allFiles <- do.call(rbind, lapply(as.matrix(filtered)[,1], 
                                  function(x) {dat <- read.csv(x, stringsAsFactors = FALSE,
                                                      sep = ',', na.strings=c("","NA"))
                                  dat$acq_date <- as.Date(strsplit(basename(x), '_')[[1]][4], 
                                                      "%Y%m%d")
                                  
                                  # as.Date(c("1/1/2001", "1/2/2001", "1/3/2001"), "%m/%d/%Y")
                                  
                                    # basename(x)
                                  dat}
                                  ))

# observation dates in files
obs_dates <- unique(allFiles$date)

# get 2008 dates only
selection <-subset(allFiles, 
                            allFiles$acq_date <= as.Date('20081231', "%Y%m%d")) 

# boxplot <- 
  ggplot(selection, aes(x= z, y = intertide)) + 
  geom_point(shape = 1) +
  # stat_ellipse() +
  # geom_smooth() +
  facet_wrap(~acq_date, ncol=2) +
  labs(y = "fraction", x = 'depth') + 
  scale_y_continuous( limits = c(0, 1),expand = c(0,0))+
    
  theme(
    axis.line.x = element_line(size = 0.5, colour = "black"),
    axis.line.y = element_line(size = 0.5, colour = "black"),
    axis.line = element_line(size=1, colour = "black"),
    axis.text.x = element_text(color = "grey20", size = 10, hjust = .5, vjust = .5),
    axis.text.y = element_text(color = "grey20", size = 10, hjust = .5, vjust = .5),
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
    
    strip.text.x = element_text(size = 14, face = 'bold') # Facet titles
    
  )


  library(ggridges)
  
  # Marginal density plot of fraction 
ggplot(selection, aes(x = intertide, y = as.character(acq_date), fill = ..x..)) + 
  geom_density_ridges_gradient(scale = 1, rel_min_height = 0.01) +
  scale_x_continuous( limits = c(0, 1),expand = c(0.1,0)) +
  scale_fill_viridis(name = "fraction", option = "C")
  
  
    # geom_density(alpha=.5) + 
    # facet_wrap(~acq_date, ncol=2) +
    # scale_y_continuous( limits = c(0, 10),expand = c(0,0))
    # scale_fill_manual(values = c('#999999','#E69F00')) + 
    # theme(legend.position = "none")

# Marginal density plot of the depth distribution
depth_density <- ggplot(selection, aes(z)) + 
  geom_density(alpha=.5) + 
  # scale_fill_manual(values = c('#999999','#E69F00')) + 
  theme(legend.position = "none")
  

