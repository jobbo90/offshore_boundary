multiply <- function(x,y){
  return(x*y)
}

# detect string to to_keep 
to_keep <- function(fixed_string, text) {
  #' @title string to keep
  #' @description detect string in text
  #' @param fixed_string the pattern to match
  #' @param text the text to search in
  #' @return string to keep
  return(stringr::str_detect(text, stringr::fixed(fixed_string, ignore_case = TRUE)))
}


# Rewrite table
rewrite <- function(txt){
  #' @title rewrite  character strings
  #' @description this function rewrites 
  #' character strings to lists with tibble
  #' @param txt character string
  #' @return return the list
  return(tibble::tibble(text = c(txt)))

}

col_of_interest <- function(csv, patt){
  #' @title Column number of interest
  #' @description Return column number matching name with string pattern
  #' @param file csv file to extract column namesfrom
  #' @param patt pattern
  #' @return integer
  #' 
  #' fix the pattern, with end of line issue.
  return(grep(paste( '^', patt, '$', sep = ''), colnames(csv), fixed = F))
}

reshape_csvPoints <- function(csv, patternX, patternY, cols_to_keep){
  #' @title reshape CSV to points matrix
  #' @description Return column number matching name with string pattern
  #' @param file csv file
  #' @param patternX is the pattern describing column of x-coordinate
  #' @param patternY is the pattern describing column of Y-coordinate
  
  # csv <- allFiles
  
  # patternX <- 'peakCoordX'
  # patternY <- 'peakCoordY'
  # cols_to_keep <- c('axisDist', 'mudFract', 'endDrop', 'coastDist',
  #                   'originX', 'originY', '.geo')
  # cols_to_keep <- keep_columns
  
  dates <- col_of_interest(csv, 'DATE_ACQUIRED$')
  coastDist <- col_of_interest(csv, 'coastDist$')
  
  # all unique dates
  uniqueDates <- unique(csv[,dates]);
  
  # all unique transect (id's)
  pos <- unique(csv[, col_of_interest(csv, 'pos$')]);
  
  # grep(paste( '^', patternX, sep = ''), colnames(csv), fixed = F)
  
  # amount of transects
  uniqueX<- unique(csv[, col_of_interest(csv,'originX$')]);
  uniqueY<- unique(csv[, col_of_interest(csv,'originY$')]);
  # geo<- unique(csv[, col_of_interest(csv, '.geo')]);
  
  # define output matrices
  allPoints <- vector('list', nrow(csv))
  
  for (n in 1:length(uniqueX)){
    # n<-5
    
    # Coordinates of transects
    # coords <- qdapRegex::ex_between(as.character(geo[n]), ":[", "]}")[[1]]
    # all_digits <- regmatches(coords, gregexpr("[-[:digit:].]+", coords))[[1]]
    # 
    # begin_coords <- data.frame(lon = as.numeric(all_digits[1]), #x
    #                            lat = as.numeric(all_digits[2])) #y
    # 
    # end_coords <- data.frame(lon = as.numeric(all_digits[3]),
    #                          lat = as.numeric(all_digits[4]))  
    # x <- as.matrix(rbind(begin_coords, end_coords))

    # only return observations if patternX >= 0 and if coordinates
    
    test1transect <- subset(csv,csv[,col_of_interest(csv, 'originX$')] 
                            == uniqueX[n])
    # will this X > 0 remove missed observations of coastlines of pattern x. 
    #& csv[,col_of_interest(csv, paste( '^', patternX, sep = ''))] != -1 )

    if(nrow(test1transect) > 0){ # some transect had no obs in any of the images
      
    # subset <- test1transect[,sort(c(col_of_interest(csv, 'coastDist$'), col_of_interest(csv, 'DATE_ACQUIRED$'),
    #                                 col_of_interest(csv, 'pos$')))]
    
    # coordiates of points of interest (coastline, offshore boundary etc)
    df_out <- data.frame(x = as.numeric(test1transect[,col_of_interest(csv, paste( '^', patternX, sep = ''))]),
                         y = as.numeric(test1transect[,col_of_interest(csv, paste( '^', patternY, sep = ''))])
                         )
    classes <- lapply(csv, class) #%in% cols_to_keep

    for (ckeep in cols_to_keep){
      # ckeep<- cols_to_keep[3]
      # print(ckeep)
      classToUse <- paste(classes[ckeep], collapse = ',')
      
      vals = test1transect[,col_of_interest(csv, ckeep)]
      class(vals) <- classToUse
      df_out[,paste0(ckeep)] <- vals
    }
    
    # also store the transect coordinates
    # is this necessary??
    # df_out$trans_x0 <- as.numeric(begin_coords$lon)
    # df_out$trans_y0 <- as.numeric(begin_coords$lat)
    # df_out$trans_x1 <- as.numeric(end_coords$lon)
    # df_out$trans_y1 <- as.numeric(end_coords$lat)
    # 
    # calculate bearing of transect
    bearingTrans <- bearing(SpatialPoints(data.frame(x = df_out$originX, y = df_out$originY),
                          CRS("+proj=longlat +datum=WGS84")),
            SpatialPoints(data.frame(x = df_out$endX, y = df_out$endY),
                          CRS("+proj=longlat +datum=WGS84")))
    
    # navigational bearing scale(north= 0 & 360)
    bearing_in_compas_scale <- (bearingTrans + 360) %% 360
    
    df_out$bearing <- bearing_in_compas_scale
    
    
    allPoints <- rbind(allPoints, df_out)
    }
  }
  
  
  # make it spatial
  SpatialPoints <- SpatialPointsDataFrame(data.frame(allPoints[,'x'], allPoints[,'y'] ), 
                                           data = allPoints,
                                            # data.frame(DATE_ACQUIRED = allPoints[,'DATE_ACQUIRED'],
                                            #                  pos = allPoints[,'pos'],
                                            #                  axisDist = as.numeric(allPoints[,'axisDist']),
                                            #                  endDrop = as.numeric(allPoints[,'endDrop']),
                                            #                  coastDist = as.numeric(allPoints[,'coastDist']),
                                            #                  mudFract = as.numeric(allPoints[,'mudFract'])
                                                             # ),
                                           proj4string=CRS("+proj=longlat +datum=WGS84"))
  points_sf <- st_as_sf(SpatialPoints)
  
  return(points_sf)
}


reshape_csvLines <- function(csv){
  #' @title reshape CSV to points matrix
  #' @description Return column number matching name with string pattern
  #' @param file csv file
  #' 
  # csv <- allFiles
  
  dates <- col_of_interest(csv, 'DATE_ACQUIRED$')
  coastDist <- col_of_interest(csv, 'coastDist$')
  
  # all unique dates
  uniqueDates <- unique(csv[,dates]);
  
  # all unique transect (id's)
  pos <- unique(csv[, col_of_interest(csv, 'pos$')]);
  uniqueX<- unique(csv[, col_of_interest(csv, 'originX$')]);
  uniqueY<- unique(csv[, col_of_interest(csv, 'originY$')]);
  geo<- unique(csv[, col_of_interest(csv, '.geo')]);
  
  # define output matrices
  lines <- vector('list', length(uniqueX));
  df_coastDist = data.frame(matrix(NA, length(uniqueX), length(uniqueDates)+1),
                            stringsAsFactors=F) # Empty data frame for coastal distance
  colnames(df_coastDist) <- c('pos', uniqueDates)
  
  for (n in 1:length(uniqueX)){
    #n<-103
    
    # Coordinates of transects
    coords <- qdapRegex::ex_between(as.character(geo[n]), ":[", "]}")[[1]]
    all_digits <- regmatches(coords, gregexpr("[-[:digit:].]+", coords))[[1]]
    
    begin_coords <- data.frame(lon = as.numeric(all_digits[1]),
                               lat = as.numeric(all_digits[2]))
    
    end_coords <- data.frame(lon = as.numeric(all_digits[3]),
                             lat = as.numeric(all_digits[4]))  
    x <- as.matrix(rbind(begin_coords, end_coords))
    lines[[n]] <- Lines(list(Line(x)), ID = n)  # create line feature
    
    # only return observations if coastDist >= 0 and if coordinates
    test1transect <- subset(csv,csv[,col_of_interest(csv, 'originX$')]== uniqueX[n]
                            & csv[,col_of_interest(csv, 'coastDist$')] >= 0 )
    
    subset <- test1transect[,sort(c(col_of_interest(csv, 'coastDist$'), col_of_interest(csv, 'DATE_ACQUIRED$'),
                                    col_of_interest(csv, 'pos$')))]
    
    # construct a data frame that matches the lines
    if(is.null(nrow(subset))){
      df_coastDist[n,] <- c(as.numeric(subset[3]), rep(NA, length(uniqueDates)))
      
      # THIS THROWS ERROR LATER ON WHEN THERE IS NOTHING IN SUBSET ALSO NO POS IS ASSIGNED TO THE TRANSECT DATABASE!
      
    } else {
      # at the correct location in df_coastDist append the offshore location of mudbank boundary
      mathcingDates <- match(subset[ ,col_of_interest(subset, 'DATE_ACQUIRED')], colnames(df_coastDist))
      
      # append the pos number of the transect to the first column
      df_coastDist[n,col_of_interest(df_coastDist, 'pos$')] <-  as.numeric(subset[,col_of_interest(subset, 'pos$')])[1]
      
      # fill for each date column the retrieved value
      df_coastDist[n,mathcingDates[!is.na(c(mathcingDates))]] <- subset[,col_of_interest(subset, 'coastDist$')]
      
    }
  }
  lines_sf <- SpatialLinesDataFrame(SpatialLines(lines, proj4string=CRS("+proj=longlat +datum=WGS84")), data.frame(df_coastDist))
  
  return(lines_sf)
}


build_csvLines <- function(csv){
  #' @title reshape CSV to points matrix
  #' @description Return column number matching name with string pattern
  #' @param file csv file
  #' 
  # csv <- allFiles
  
  dates <- col_of_interest(csv, 'DATE_ACQUIRED$')
  coastDist <- col_of_interest(csv, 'coastDist$')
  
  # all unique dates
  uniqueDates <- unique(csv[,dates]);
  
  # all unique transect (id's)
  pos <- unique(csv[, col_of_interest(csv, 'pos$')]);
  uniqueX<- unique(csv[, col_of_interest(csv, 'originX$')]);
  uniqueY<- unique(csv[, col_of_interest(csv, 'originY$')]);
  
  # define output matrices
  lines <- vector('list', length(uniqueX));
  df_coastDist = data.frame(matrix(NA, length(uniqueX), length(uniqueDates)+1),
                            stringsAsFactors=F) # Empty data frame for coastal distance
  colnames(df_coastDist) <- c('pos', uniqueDates)
  
  for (n in 1:length(uniqueX)){
    # n<-3
    
    # Coordinates of transects
    # coords <- qdapRegex::ex_between(as.character(geo[n]), ":[", "]}")[[1]]
    # all_digits <- regmatches(coords, gregexpr("[-[:digit:].]+", coords))[[1]]
    
    # only return observations if coastDist >= 0 and if coordinates
    test1transect <- subset(csv,csv[,col_of_interest(csv, 'originX$')]== uniqueX[n]
                            & csv[,col_of_interest(csv, 'coastDist$')] >= 0 )
    
    subsets <- unique(test1transect[,sort(c(col_of_interest(csv, 'coastDist$'), 
                                     col_of_interest(csv, 'DATE_ACQUIRED$'),
                                     col_of_interest(csv, 'pos$')))])

    # construct a data frame that matches the lines
    if(nrow(subsets) == 0){
      df_coastDist[n,] <- c(as.numeric(pos[n]), rep(NA, length(uniqueDates)))
      
      # THIS THROWS ERROR LATER ON WHEN THERE IS NOTHING IN SUBSET ALSO 
      # NO POS IS ASSIGNED TO THE TRANSECT DATABASE!
      
      begin_coords <- data.frame(lon = -1,
                                 lat = -1)
      
      end_coords <- data.frame(lon = -1,
                               lat = -1)  
      x <- as.matrix(rbind(begin_coords, end_coords))
      
    } else {
      
      
      begin_coords <- data.frame(lon = as.numeric(test1transect$originX[1]),
                                 lat = as.numeric(test1transect$originY[1]))
      
      end_coords <- data.frame(lon = as.numeric(test1transect$endX[1]),
                               lat = as.numeric(test1transect$endY[1]))  
      x <- as.matrix(rbind(begin_coords, end_coords))
      
      # at the correct location in df_coastDist append the coastline distance 
      mathcingDates <- match(subsets[ ,col_of_interest(subsets, 'DATE_ACQUIRED')], 
                             colnames(df_coastDist))
      
      # append the pos number of the transect to the first column
      df_coastDist[n,col_of_interest(df_coastDist, 'pos$')] <-  
        as.numeric(subsets[,col_of_interest(subsets, 'pos$')])[1]
      
      # fill for each date column the retrieved value
      df_coastDist[n,mathcingDates[!is.na(c(mathcingDates))]] <- 
        subsets[,col_of_interest(subsets, 'coastDist$')]
      
    }
    
    lines[[n]] <- Lines(list(Line(x)), ID = n)  # create line feature
    
    
    # lines[[n]] <- SpatialLinesDataFrame( Lines(list(Line(x)), ID = n))
    
  }
  lines_sf <- SpatialLinesDataFrame(SpatialLines(lines,
                                                 proj4string=
                                                   CRS("+proj=longlat +datum=WGS84")),
                                    data.frame(df_coastDist))

  # lines_sf <- SpatialLines(lines, proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  return(lines_sf)
}

# outlier detection: rosnerTest
# remove outliers based on a interval of 1 -3 years?

rosner <- function(x, minStd, minObsNeeded){
  # x <- subsets3$coastDist
  
  
  # minStd <- 25
  # minObsNeeded <- 10
  
  # assume no outliers (assign value 1)
  output <- rep(1,length.out=length(x))
  
  # only apply Rosner when there is sufficient observations
  if(length(x) >= minObsNeeded){ # should be 15
  
    # the amount of observations that are not from the same distribution
    # (alsternative hypothesis in Rosner Test)
    # Ideally you;d want to use a boxplot test of some sort to indicate the amount of potential outliers
    #  such that K is more specific to the sample provided. 
    # https://ouzhang.me/2020/11/03/outliers-part2/#method-7-finding-outliers-with-hypothesis-tests
    
    K <- length(x)-2 
    
    if(K > floor(length(x)/2)){ # never bigger than 1/2 size of observations
      K <- floor(length(x)/2) # or 7?
      }
    
    # a warning is issued when the assumed Type 1 error may not be correct
    test2 <- has_error(rosnerTest(x, K,alpha = 0.01 ,warn = F), silent = !interactive())
    # throws an error when no outliers are detected... (all values are equal)
    
    if(!test2){ # if no error:
        rosnerOut <- rosnerTest(x, K,alpha = 0.01, warn = F)[['all.stats']]
        # test <- grubbs.test(x)
        # outliers:
        outliers <- rosnerOut[rosnerOut$Outlier & rosnerOut$SD.i>minStd, 'Obs.Num']
        
        output[outliers[!is.na(outliers)]] <- 0

    } 
   
  }
  
  # assumption that with smaller sample sizes there is max 1 outlier.
  if(length(x) < minObsNeeded &
     length(x) >= 3){
    
    # fix K at 1
    K <- 1
    test2 <- has_error(rosnerTest(x, K, warn = F), silent = !interactive())
    # throws an error when no outliers are detected... (all values are equal)
    
    if(!test2){ # if no error:
      rosnerOut <- rosnerTest(x, K,alpha = 0.01, warn = F)[['all.stats']]
      # test <- grubbs.test(x)
      # outliers:
      outliers <- rosnerOut[rosnerOut$Outlier & rosnerOut$SD.i>minStd, 'Obs.Num']
      
      output[outliers[!is.na(outliers)]] <- 0
      
      
    } 
    
    
  }
  
  # Based on a study using N=1,000 simulations, Rosner's (1983) Table 1 shows the 
  # estimated true Type I error of declaring at least one outlier when none exists 
  # for various sample sizes n ranging from 10 to 100, and the declared maximum 
  # number of outliers k ranging from 1 to 10. Based on that table, 
  
  # Roser (1983) declared that for an assumed Type I error level of 0.05, as long as n ≥ 25, 
  # the estimated α levels are quite close to 0.05, and that similar results were 
  # obtained assuming a Type I error level of 0.01. 
  
  # However, the table below is an 
  # expanded version of Rosner's (1983) Table 1 and shows results based on N=10,000
  # simulations. You can see that for an assumed Type I error of 0.05, the test
  # maintains the Type I error fairly well for sample sizes as small as n = 3 as
  # long as k = 1, and for n ≥ 15, as long as k ≤ 2. Also, for an assumed Type I 
  # error of 0.01, the test maintains the Type I error fairly well for sample sizes 
  # as small as n = 15 as long as k ≤ 7.
  # 
  #' 
  #' As noted, using Rosner's test requires specifying the number of suspected outliers, k, in advance. 
  #' USEPA (2013a, pp.190-191) states: “A graphical display (Q-Q plot) can be used to identify 
  #' suspected outliers needed to perform the Rosner test”, and USEPA (2009, p. 12-11) notes: 
  #' “A potential drawback of Rosner's test is that the user must first identify the maximum number 
  #' of potential outliers (k) prior to running the test. Therefore, this requirement makes the test
  #' ill-advised as an automatic outlier screening tool, and somewhat reliant on the user to identify candidate outliers.”
  
  return (output)
}




# source: https://rdrr.io/cran/kmlShape/src/R/reduceTraj.R
shortestDistanceToLines <- function(Mx,My,Ax,Ay,Bx,By){ # get distance to line
  aire <- abs((By-Ay)*(Mx-Ax)-(Bx-Ax)*(My-Ay))
  return(  aire / sqrt((Bx-Ax)^2 + (By-Ay)^2))
}

findFarestPoint_manual <- function(trajx,trajy){
  
  # trajx <- geom_ordered[range,1]
  # trajy <- geom_ordered[range,2]
  
  dmax <- 0
  index <- 1
  end <- length(trajx)
  
  if(end==2){
    index <- 1
    dmax <- 0
  }else{
    for(i in 2:(end-1)){ # for each point but the first and last
      # i <- 2
      # calculate the distance
      d <- shortestDistanceToLines(Mx=trajx[i],My=trajy[i], Ax=trajx[1],Ay=trajy[1], Bx=trajx[end],By=trajy[end])
      if ( d > dmax ) {
        # update dmax & index with the distance
        # in the end only the max distance is included (due to the if(d>dmax))
        index <- i
        dmax <- d
      }else{}
    }
  }
  
  output <- c(index, dmax)
  names(output) <- c('index', 'dmax')
  
  return(output)
  # return(c(index=index,dmax=dmax))
}

DouglasPeuckerEpsilon <- function(trajx,trajy,epsilon,spar=NA){
  # trajx <- geom_ordered[range,1]
  # trajy <- geom_ordered[range,2]
  
  # trajx <- trajx[1:index]
  # trajy <- trajy[1:index]
  
  missings <- is.na(trajx)|is.na(trajy)
  if(any(missings)){
    trajx <- trajx[!missings]
    trajy <- trajy[!missings]
  }else{}
  
  if(!is.na(spar)){trajy <- smooth.spline(trajx,trajy,spar=spar)[["y"]]}else{}
  
  # farestPoint <- findFarestPoint(trajx,trajy)
  farestPoint <- findFarestPoint_manual(trajx,trajy)
  index <- farestPoint["index"]
  end <- length(trajx)
  # points(trajx[index], trajy[index], col = 'blue')
  
  # if farest point is big enough; use it as a vertext
  if ( farestPoint["dmax"] > epsilon ) {
    recResults1 = DouglasPeuckerEpsilon(trajx[1:index],trajy[1:index], epsilon)
    recResults2 = DouglasPeuckerEpsilon(trajx[index:end],trajy[index:end], epsilon)
    
    resultTrajx = c(recResults1$x,recResults2$x[-1])
    resultTrajy = c(recResults1$y,recResults2$y[-1])
    #        d = c(farestPoint["dmax"],recResults1$d,recResults2$d)
  } else {
    resultTrajx = c(trajx[1],trajx[end])
    resultTrajy = c(trajy[1],trajy[end])
    #        d=numeric()
  }
  return(data.frame(x=resultTrajx,y=resultTrajy))
}




to_spatial_df <-  function(matrix,x_to_use,y_to_use){
  
  # x_to_use <- 'coastX'
  # y_to_use <- 'coastY'
  # matrix <-  subset(subset_for_testPlot, coast_outlier == 1)
  
  if(nrow(matrix)>0){
    df <- data.frame(matrix[[paste0(x_to_use)]], matrix[[paste0(y_to_use)]])
    
    sdf<- SpatialPointsDataFrame(df, proj4string=CRS("+proj=longlat +datum=WGS84"),
                                 data = data.frame(matrix))
  } else {
    # return empty df
    sdf<-SpatialPointsDataFrame(data.frame(x = 1, y = 1), data = 
                                  data.frame(1))
    
  }
  
  return(sdf)
  
}


# based on distance, lon, lat and bearing return x,y coordinates 
get_dists2 <- function(x, lon, lat, bearing, dist){
  
  # x original sf data.frame 
  # lon, lat are coordinates of origin points
  # bearing, pre-calculated bearing/direction of the transect
  # dist of interest that needs to be translated in coordinates
  
  # dist <- c('axisDistAbs', 'axisDistSlope')
  # x <- mudbanks
  # bearing <- x$bearing
  # lon <- x$originX
  # lat <- x$originY
  # 
  dat <- tibble(lon, lat)
  names(dat) <- c('lon', 'lat')
  
  out <- data.frame(matrix(NA, nrow = nrow(x)))
  
  for (i in 1:length(dist)){
    # i<-1
    
    pattern <- dist[i]
    coords_out <- data.frame(destPoint(dat,  bearing, x[[paste0(pattern)]]))
    
    negDist <- which( x[[paste0(pattern)]] == -1)
    
    
    out[[paste0(pattern, 'X', collapse = '')]] <- coords_out[,1]
    out[[paste0(pattern, 'Y', collapse = '')]] <- coords_out[,2]
    
    # set the coordinates which have no dist measured at -1
    out[negDist,paste0(pattern, 'X', collapse = '')] <- -1
    out[negDist,paste0(pattern, 'Y', collapse = '')] <- -1
    
  }
  
  # drop NA col
  out <- out[,-1]
  x <- cbind(data.frame(x), out)
  
  # make it spatial
  # SpatialPoints <- SpatialPointsDataFrame(data.frame(x[,'x'], x[,'y'] ),
  #                                         data = x,
  #                                         proj4string=CRS("+proj=longlat +datum=WGS84"))
  # points_sf <- suppressWarnings(st_as_sf(SpatialPoints))
  
  
  # return(points_sf)
  return(x)
}

sp_pnt_ee <- function(x,y,name,col){
  spt <- sf_as_ee(st_set_crs( # make it spatial with a crs and transfer to ee_obj
    st_sfc(st_multipoint(cbind(x,y))),4326 ))
  
  
  ee_addlayer <- Map$addLayer(
    eeObject = spt,
    name = paste0(name),
    visParams = list(
      pointRadius = 10,
      color = col
    )
  )
}


# https://stackoverflow.com/questions/29255473/most-frequent-value-mode-by-group
# x <- test
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x[!is.na(x)], ux)))]
}


# https://stackoverflow.com/questions/6836409/finding-local-maxima-and-minima
inflect <- function(x, threshold = 2){
  # x = runnAve$rolling
  # threshold = 25
  # n = -24
  up   <- sapply(1:threshold, function(n) c(x[-(seq(n))], rep(NA, n)))
  down <-  sapply(-1:-threshold, function(n) c(rep(NA,abs(n)), x[-seq(length(x), length(x) - abs(n) + 1)]))
  a    <- cbind(x,up,down)
  list(minima = which(apply(a, 1, min) == a[,1]), maxima = which(apply(a, 1, max) == a[,1]))
}



# modyfying facet scales in ggplot
# https://fishandwhistle.net/post/2018/modifying-facet-scales-in-ggplot2/
scale_override <- function(which, scale) {
  if(!is.numeric(which) || (length(which) != 1) || (which %% 1 != 0)) {
    stop("which must be an integer of length 1")
  }
  
  if(is.null(scale$aesthetics) || !any(c("x", "y") %in% scale$aesthetics)) {
    stop("scale must be an x or y position scale")
  }
  
  structure(list(which = which, scale = scale), class = "scale_override")
}
CustomFacetWrap <- ggproto(
  "CustomFacetWrap", FacetWrap,
  init_scales = function(self, layout, x_scale = NULL, y_scale = NULL, params) {
    # make the initial x, y scales list
    scales <- ggproto_parent(FacetWrap, self)$init_scales(layout, x_scale, y_scale, params)
    
    if(is.null(params$scale_overrides)) return(scales)
    
    max_scale_x <- length(scales$x)
    max_scale_y <- length(scales$y)
    
    # ... do some modification of the scales$x and scales$y here based on params$scale_overrides
    for(scale_override in params$scale_overrides) {
      which <- scale_override$which
      scale <- scale_override$scale
      
      if("x" %in% scale$aesthetics) {
        if(!is.null(scales$x)) {
          if(which < 0 || which > max_scale_x) stop("Invalid index of x scale: ", which)
          scales$x[[which]] <- scale$clone()
        }
      } else if("y" %in% scale$aesthetics) {
        if(!is.null(scales$y)) {
          if(which < 0 || which > max_scale_y) stop("Invalid index of y scale: ", which)
          scales$y[[which]] <- scale$clone()
        }
      } else {
        stop("Invalid scale")
      }
    }
    
    # return scales
    scales
  }
)

facet_wrap_custom <- function(..., scale_overrides = NULL) {
  # take advantage of the sanitizing that happens in facet_wrap
  facet_super <- facet_wrap(...)
  
  # sanitize scale overrides
  if(inherits(scale_overrides, "scale_override")) {
    scale_overrides <- list(scale_overrides)
  } else if(!is.list(scale_overrides) || 
            !all(vapply(scale_overrides, inherits, "scale_override", FUN.VALUE = logical(1)))) {
    stop("scale_overrides must be a scale_override object or a list of scale_override objects")
  }
  
  facet_super$params$scale_overrides <- scale_overrides
  
  ggproto(NULL, CustomFacetWrap,
          shrink = facet_super$shrink,
          params = facet_super$params
  )
}

# p_annoying_x_scale +
#   facet_wrap_custom(~facet_name, scales = "free", ncol = 4, scale_overrides = list(
#     scale_override(1, scale_x_continuous(breaks = c(5750, 5900))),
#     scale_override(6, scale_x_continuous(breaks = c(17800, 17900)))
#   ))

n_fun <- function(x){
  
  # x1 <- runif(1, 1, 5)
  return(data.frame(y = max(x, na.rm = T) + 10,
                    label = paste0(length(x))))
}



