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
# library(rgee)
# Sys.setenv("RETICULATE_PYTHON" = "C:/Users/5600944/AppData/Local/r-miniconda/envs/rgee/python.exe")
# ee_Initialize()

## ---------------------------
source("./src/functions.R")

## ---------------------------

mapviewOptions(basemaps = c( "Esri.WorldImagery","Esri.WorldShadedRelief", "OpenStreetMap.DE"))
dataFolder <- './data/processed'
# years <- c('2005', '2006','2007', '2008','2009')
years <- seq(from = 1985, to = 2020, by = 1)
aoi <- c('Suriname') # FrenchGuiana / Guayana / Suriname

# pos to exlcude for mudbank boundary estimates / outlier detection
posToExcludeSUR <- c(seq(138000,147000,1000),
                     seq(241000, 255000, 1000))  

posToExcludeFG <- c(seq(261000,270000,1000), # approuage River
                    seq(315000,334000,1000),# baia oiapoque 
                    seq(223000,225000,1000), # orapu
                    seq(205000,207000,1000) # cayenne
) 

posToExclude <- posToExcludeSUR

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

#' implement workflow
uniqueDates <- unique(allFiles[,'DATE_ACQUIRED']);
all_years <- unique(allFiles$year_col)
group_pos <- unique(allFiles$pos)

# aggregated pos gos per x Meters:
summarisePos <- 10000
breaks <- seq(0, max(allFiles$pos), summarisePos)

# get only 1 observation per observation per pos here
allFiles <- allFiles %>% 
  dplyr::group_by(DATE_ACQUIRED, pos) %>% 
  dplyr::distinct(deltaCoast, .keep_all = T) %>%
  # filter(n()>1) %>% 
  ungroup() #%>%
  # dplyr::select(DATE_ACQUIRED, pos, year_col, deltaCoast, noMudbank, 
                # mudbank_outlier)




#make sure only bottom (final) plot gets a legend
# set counter
i <- 1
fps <- 30  # frames per second
delay <- 3 # seconds

fillPosTime <- 310000
range <- round(quantile(allFiles$deltaCoast,c(0.05, 0.95), na.rm=T))
subsetPos <- subset(allFiles, pos == fillPosTime)

coastDistRange <- round(quantile(subsetPos$coastDist,c(0.005, 0.99), na.rm=T))
minDate <- min(allFiles$DATE_ACQUIRED)
maxDate <- max(allFiles$DATE_ACQUIRED)


twoDPlot <- ggplot(subsetPos, 
                   aes(x= as.Date(DATE_ACQUIRED), y = coastDist)) + 
  scale_fill_manual(name = '', values = c('#7fbf7b'), labels = c('mudbank')) +
  geom_line(inherit.aes = FALSE, aes(x = as.Date(DATE_ACQUIRED), y = coast_median),
            alpha = 0.9, size = 1.2) +
  
  geom_point(size = 3, aes(colour = as.factor(coast_outlier)), alpha = 1) +
  
  # considering fill based on deltaCoast value such that colours match
  
  scale_y_continuous(limits=c(coastDistRange[1],coastDistRange[2]+50)) +
  scale_color_manual(name = "Observations",
                     values = c('red', 'blue'),
                     labels = c("outlier", "coastal distance")) +
  
  
  scale_x_date(labels = date_format("%Y")) +
  # guides(colour=guide_legend(ncol=2)) +
  # ggtitle( paste0('position: ', fillPosTime/1000)) +
  labs(x = "year", y = "coastline position [m]") +
  theme(axis.line.x = element_line(size = 0.5, colour = "black"),
        axis.line.y = element_line(size = 0.5, colour = "black"),
        axis.line = element_line(size= 1, colour = "black"),
        axis.title.y = element_text(size = 12, face = 'bold'),
        axis.title.x = element_text(size = 12, face = 'bold'),
        axis.text.x = element_text(size = 12,  hjust = .5, vjust = .5),
        axis.text.y = element_text(size = 12, hjust = .5, vjust = .5),
        
        # # legend
        # legend.title = element_blank(),
        # legend.background = element_rect(fill = alpha('#d9d9d9', 0.7),  
        #                                  colour = '#d9d9d9'),
        # legend.text = element_text(size = 20),
        # legend.position = legendPos,#c(.85, .28),
        
        plot.title = element_text(hjust = 0.5, size = 25, face = 'bold',
                                  vjust = -1), 
        panel.grid.major = element_blank(), # remove grid lines
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        plot.background = element_rect(fill = '#d9d9d9', colour = '#d9d9d9'),
        
        panel.border = element_blank())

# twoDPlot

DatesSubset <- unique(subsetPos[,'DATE_ACQUIRED']);

# add dates where observation density is low to ensure frame rate is roughly equal
d <- as.Date(DatesSubset$DATE_ACQUIRED)
df <- data.frame(date = d, diff = c(0, diff(d)), original = 1 )

# between <- c(0, diff(d))
dateSeq <- seq(ymd(min(DatesSubset$DATE_ACQUIRED)), 
               ymd(max(DatesSubset$DATE_ACQUIRED)), 16)
df2 <- data.frame(date = dateSeq, original = 0) # diff = c(0, diff(dateSeq))

df3 <- df %>%
  full_join(df2) %>%
  arrange(date)

# throw out every 0 if it is within x days of an original observation
df3$backdiff <- c(0, diff(df3$date))

df3$forward.diff <- c(sapply(2:nrow(df3) - 1, 
                             function(i){
                               df3$date[i+1] - df3$date[i]
                             }), 0)

df4 <- df3 %>%
  mutate(diff = pmin(backdiff, forward.diff))
df5 <- df4[c(which((df4$original==0 & df4$diff>15) | 
                     (df4$original==1))),]

# select images
imgSelect <- as.matrix(list.files(paste0('./data/raw/GEE_exports/GIF/pos',fillPosTime), 
                                  full.names = T))

df_img <- rewrite(imgSelect) 
df_img <- unique(df_img[grep('.tif', imgSelect, ignore.case = T),]) %>%
  filter(str_detect(text, as.character(fillPosTime)))


# get oldest date?
date<-str_match(df_img$text[1], "310000_\\s*(.*?)\\s*.tif")[,2]

getDate <- function(str){
  
  return(str_match(str, "310000_\\s*(.*?)\\s*.tif")[,2])
}



firstImage <- df_img %>% 
  filter(str_detect(text,sort(sapply(df_img, getDate))[1]))

# initialize first image to make sure there is always an rgp plot
brickRGB <- brick(paste0(firstImage))

repr <- projectRaster(brickRGB,
                      # CRS("+init=epsg:4326"))
                      crs = CRS(as.character("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")))

rgb_plt <-
  ggRGB(img = repr,  r = 'NIR', g = 'Red', b = 'Green', stretch = "lin", alpha = 0.8) +
  # geom_point(data = data.frame(poi), 
  #            mapping = aes(x = coastX, y = coastY),
  #            colour = "blue", size = 3, alpha = 0.8) +
  labs(y = 'Lat', x = 'Long', title = paste0(as.Date(sort(sapply(df_img, getDate))[1], "%Y%m%d"))) +
  labs(caption='Landsat (USGS)                                                   @JdV') +
  theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", 
                                        size = 0.5),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 18, face = 'bold',
                                  vjust = -5), 
        
        axis.line.x = element_line(size = 0.5, colour = "black"),
        axis.line.y = element_line(size = 0.5, colour = "black"),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        
        axis.title.x = element_text(size = 14, face = 'bold'),
        axis.title.y = element_text(size = 14, face = 'bold'))
# rgb_plt

pEmpty <-ggplot(allFiles, 
           aes(x = as.numeric(pos),y = as.Date(year_col), fill=deltaCoast)) + 

  labs(y = 'Year', x = 'Alongshore Position [km]')+  #Alongshore Position [km]
  # scale_x_reverse(lim=c(max(allFiles$pos)+4000, 0), ,
  #                 labels = )  # 
  
  # 'zooming out' to much by using full x-axis range makes the resulting tiles invisble.
  scale_x_reverse(lim=c(max(allFiles$pos)+4000, 0), expand = c(0,0),
                  labels = unit_format(unit = "", scale = 0.001)) + # 
  scale_y_date(limits = c(as.Date(minDate), as.Date(maxDate))) +
  theme(axis.line.x = element_line(size = 0.5, colour = "black"),
        axis.line.y = element_line(size = 0.5, colour = "black"),
        axis.line = element_line(size= 1, colour = "black"),
        axis.title.y = element_text(size = 12, face = 'bold'),
        axis.title.x = element_text(size = 12, face = 'bold'),
        axis.text.x = element_text(size = 12,  hjust = .5, vjust = .5),
        axis.text.y = element_text(size = 12, hjust = .5, vjust = .5),

        legend.spacing.y = unit(0.1, 'cm'),
        legend.title = element_text(colour = 'black', size = 14, face = "bold"),
        # legend.key = element_rect(fill = NA),
        legend.text = element_text(size = 12),
        # legend.position =element_blank(),# c(0.1,0.5),
        panel.grid.major = element_blank(), # remove grid lines
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        plot.background = element_rect(fill = '#d9d9d9',  colour = '#d9d9d9'))
# p

totalFrames <- length(1:(delay*fps))

for(frame in 1:totalFrames){
  # frame<-1

  {if(frame < 0.5*totalFrames)
      p <- pEmpty +
        annotate("text", x=max(allFiles$pos)-80000, y = as.Date('2015-01-01'),
             label="spatio-", color="red", fontface =2, size = 14) +
        geom_hline( yintercept = as.Date('2018-01-01'), color= 'red',
                linetype="dashed")
    }
    
    {if(frame > 0.5*totalFrames)
      p <- p + annotate("text", x=max(allFiles$pos)-220000, y = as.Date('2005-01-01'),
               label="temporal", color="blue", fontface =2, size = 14,
               angle = 270) +
        geom_vline(xintercept = 150000, color= 'blue',
                   linetype="dashed")
    }  

  ggsave(plot = p, file.path("./results/GIF/frames_hovmoller",
                                 sprintf('%04d.png',i)),
         dpi=300, width=13.1, height=7.25, units='in')
  
  i <- i + 1
  
}


# dates to iterate over
df_dates_it <- as.Date(unique(df5$date)) # lijkt nog steeds niet goed te gaan?
dates_to_select <- c()



NotFancy <- function(l) {
  l <- format(l/1000, scientific = FALSE)
  parse(text=l)}

for (date in df_dates_it){
  # date <- df_dates_it[46]
  
  # get nearest date in original frame
  nearestDate <- DatesSubset$DATE_ACQUIRED[which(abs(as.Date(DatesSubset$DATE_ACQUIRED) - as.Date(date)) == 
                                     min(abs(as.Date(DatesSubset$DATE_ACQUIRED) - as.Date(date))))][1]
  
  datePattern <- gsub(x=nearestDate,
                      pattern="-",replacement="",fixed=T)
  
  file <- df_img %>% 
    filter(str_detect(text,datePattern))
  
  # load point obs
  poi <- subset(subsetPos, as.Date(as.character(DATE_ACQUIRED)) == as.Date(nearestDate) & 
                  !is.na(coastX) &
                  coastX != -1)
  
  if(nrow(file)  > 0){ # nrow(poi) > 0 & 
    brickRGB <- brick(paste0(file))
    # stacked <- stack(paste0(file))
    # crs(brickRGB) <- CRS("+init=epsg:4326")#"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    
    # test <- projectRaster(brickRGB, crs=CRS("+init=epsg:4326"), res=30)
    
    repr <- projectRaster(brickRGB,
                          # CRS("+init=epsg:4326"))
                          crs = CRS(as.character("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")))
    
    rgb_plt <-
      ggRGB(img = repr,  r = 'NIR', g = 'Red', b = 'Green', stretch = "lin", alpha = 0.8) +
      geom_point(data = data.frame(poi), 
                 mapping = aes(x = coastX, y = coastY),
                 colour = "blue", size = 3, alpha = 0.8) +
      labs(y = 'Lat', x = 'Long', title = paste0(as.Date(date))) +
      labs(caption='Landsat (USGS)                                                   @JdV') +
      theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", 
                                            size = 0.5),
            panel.background = element_blank(),
            plot.title = element_text(hjust = 0.5, size = 18, face = 'bold',
                                      vjust = -5), 
            
            axis.line.x = element_line(size = 0.5, colour = "black"),
            axis.line.y = element_line(size = 0.5, colour = "black"),
            axis.text.x = element_text(size = 10),
            axis.text.y = element_text(size = 10),
            
            axis.title.x = element_text(size = 14, face = 'bold'),
            axis.title.y = element_text(size = 14, face = 'bold'))
    
    
  }
  
  
  # accumulate dates
  dates_to_select <- c(dates_to_select, nearestDate)
  
  # select 
  subset_date_pos <- subset(subsetPos, DATE_ACQUIRED %in% dates_to_select)
  
  p <-ggplot(subset_date_pos, 
             aes(x = as.numeric(pos),y = as.Date(quarterly_col), fill=deltaCoast)) + 
    
    geom_tile( size=0.1, na.rm = TRUE) + # col = 'white'
    
    scale_fill_gradientn(
      name = 'coastline \n change \n [m/yr] \n',
      breaks = c(range[[1]], range[[1]]/2, 0, range[[2]]/2, range[[2]]),
      limits = c(range[[1]],range[[2]]),
      colours = c('#7b3294', '#f7f7f7', "#008837"),
      guide = guide_colourbar(nbin=100, draw.ulim = FALSE, draw.llim = FALSE),
      oob=squish,
      values = scales::rescale(c(range[[1]], -50, 0, 50, range[[2]]))
    ) +
    geom_hline( yintercept = as.Date(nearestDate), color= 'red',
                linetype="dashed") +
    
    labs(y = 'Year', x = 'Alongshore Position [km]')+  #Alongshore Position [km]
    # scale_x_reverse(lim=c(max(allFiles$pos)+4000, 0), ,
    #                 labels = )  # 
    
    # 'zooming out' to much by using full x-axis range makes the resulting tiles invisble.
    scale_x_reverse(lim = c(fillPosTime+10, fillPosTime-10),
                    expand = c(0,0), labels = NotFancy, 
                    breaks = c(fillPosTime-1000, fillPosTime, fillPosTime+1000)
    ) +
    scale_y_date(limits = c(as.Date(minDate), as.Date(maxDate))) +
    theme(axis.line.x = element_line(size = 0.5, colour = "black"),
          axis.line.y = element_line(size = 0.5, colour = "black"),
          axis.line = element_line(size= 1, colour = "black"),
          axis.title.y = element_text(size = 12, face = 'bold'),
          axis.title.x = element_text(size = 12, face = 'bold'),
          axis.text.x = element_text(size = 12,  hjust = .5, vjust = .5),
          axis.text.y = element_text(size = 12, hjust = .5, vjust = .5),
          legend.background = element_rect(fill = alpha('#d9d9d9', 0.7)),  
          #                                  colour = '#d9d9d9'),#element_rect(fill = '#d9d9d9',  colour = '#d9d9d9'),
          legend.spacing.y = unit(0.1, 'cm'),
          legend.title = element_text(colour = 'black', size = 14, face = "bold"),
          # legend.key = element_rect(fill = NA),
          legend.text = element_text(size = 12),
          legend.position =c(0.1,0.5),
          panel.grid.major = element_blank(), # remove grid lines
          panel.grid.minor = element_blank(), 
          panel.background = element_blank(),
          plot.background = element_rect(fill = '#d9d9d9',  colour = '#d9d9d9'))
  
  
  twoDPlottemp <- twoDPlot + geom_vline(xintercept = as.Date(date),size=1, 
                                        color= 'red',
                                        linetype="dashed") 
  
  p2 <- p + inset_element(rgb_plt, left = 0.5, bottom = 0.15,
                          right = 1, top = 0.85)
  
  left <- plot_grid(p, twoDPlottemp, ncol = 1, align = 'v',
                    nrow = 2, rel_heights = c(1, 1),         # adjust lay out 
                    vjust =c(+4, -1), # adjust label position
                    hjust = -2)
  final <- left  + inset_element(rgb_plt, left = 0.4, bottom = 0.5,
                                 right = 1.36, top = 1)

  
  ggsave(plot = final, file.path("./results/GIF/frames_hovmoller",
          sprintf('%04d.png',i)),
          dpi=300, width=13.1, height=7.25, units='in')
  i <- i +1
}

kustlijn <- readOGR(dsn = paste0(wd,'/data/raw/shapes'),
                    layer = 'coastline_suriname_disc')

shapefile_df <- fortify(kustlijn)
shapefile_df$class <- NA

# class(shapefile_df$id)

shapefile_df$class[which(shapefile_df$id %in% c(3,2))] <- 'east'
shapefile_df$class[which(shapefile_df$id %in% c(0,1,4))] <- 'center'
shapefile_df$class[which(shapefile_df$id %in% c(5,6))] <- 'west'

# change order
shapefile_df$class <- factor(shapefile_df$class, levels = c("west", "center", "east"))

floor_dec <- function(x, level=1) round(x - 5*10^(-level-1), level)
ceiling_dec <- function(x, level=1) round(x + 5*10^(-level-1), level)

mapped <- ggplot() +
  geom_path(data = shapefile_df, 
            aes(x = long, y = lat, group = group)) +
  # geom_point(aes(x = poiOriginX,  y = poiOriginY), colour = 'red',
  #            size = 3) +
  scale_x_continuous(limits=c(-57.1, -53.95),
                     expand = c(0,0)) +
  scale_y_continuous(breaks=c(5.8, 6.0)) +
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
  coord_map()


# break between both
breakFrames <- length(1:(delay*fps))-30


for(frame in 1:breakFrames){
  # frame<-1
  
    p <- pEmpty +
      annotate("text", x=max(allFiles$pos)-80000, y = as.Date('2015-01-01'),
               label="Spatial", color="red", fontface =2, size = 14) +
      geom_hline( yintercept = as.Date('2018-01-01'), color= 'red',
                  linetype="dashed")
    
  ggsave(plot = p, file.path("./results/GIF/frames_hovmoller",
                              sprintf('%04d.png',i)),
         dpi=300, width=13.1, height=7.25, units='in')
  
  i <- i + 1
  
}



####
#'
#' hovmoller plots
#' 
#'
#####




# define visualization based on entire dataset
posOfInterest <- c(fillPosTime) # accumulate position indicators
# i <- c(1) # track indices

# animate alongshore
for(transectPos in sort(group_pos)){
  # transectPos<-5000
  
  posOfInterest <- c(posOfInterest, transectPos)
  
  # data requierd for hovmoller
  dataPOI <- subset(allFiles, (pos %in% posOfInterest))
  
  # data required for spatial plot
  dataPos <- subset(allFiles, (pos %in% transectPos))
  
  # coordinates ==> required for spatial location of points
  poiOriginX <- c(as.matrix(tapply(dataPos$originX, dataPos$pos, median)))
  poiOriginY <- c(as.matrix(tapply(dataPos$originY, dataPos$pos, median)))

  
  temp <- mapped +  geom_point(aes(x = poiOriginX,  y = poiOriginY), colour = 'red',
                              size = 3)
    
  p <-ggplot(subset(dataPOI, !is.na(deltaCoast) & !(pos %in% posToExclude)),
             aes(x = pos,y = as.Date(year_col), fill=deltaCoast)) + 
    
    geom_tile(color= "white",size=0.1, na.rm = TRUE) +
    
    scale_fill_gradientn(name = 'coastline \n change \n [m/yr] \n',
                         breaks = c(range[[1]], range[[1]]/2, 0, range[[2]]/2, range[[2]]),
                         limits = c(range[[1]],range[[2]]),
                         colours = c('#7b3294', '#f7f7f7', "#008837"),
                         guide = guide_colourbar(nbin=100, draw.ulim = FALSE, draw.llim = FALSE),
                         oob=squish,
                         values = scales::rescale(c(range[[1]], -50, 0, 50, range[[2]]))
    ) +
    geom_vline(xintercept = transectPos, color= 'red',
               linetype="dashed") +
    # geom_text() +
    
    scale_colour_manual(name = ' ', values = c("black"),
                        labels = c('mudbank'),
                        guide = guide_legend(ncol = 2))+
    
    labs(y = 'Year', x = 'Alongshore Position [km]') + #Alongshore Position [km]
    scale_x_reverse(lim=c(max(allFiles$pos)+4000, 0), expand = c(0,0),
                    labels = unit_format(unit = "", scale = 0.001)) + # 
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
  legend <- get_legend(p)
  
  p <- p + theme(legend.position = 'none')

  left <- plot_grid(p, temp, ncol = 1, align = 'v',
            nrow = 2, rel_heights = c(2, 0.6),         # adjust lay out 
            vjust =c(+4, -1), # adjust label position
            hjust = -2)
  
  final <- plot_grid(left, legend, align = 'h', ncol = 2,
                     rel_widths = c(2.5, 0.5)) +
    theme(panel.background = element_rect(fill = '#d9d9d9', colour = '#d9d9d9'))
  
  
  ggsave(plot = final, file.path("./results/GIF/frames_hovmoller",
                             sprintf('%04d.png',i)),
         dpi=300, width=13.1, height=7.25, units='in')
  
  i <- i + 1
}
