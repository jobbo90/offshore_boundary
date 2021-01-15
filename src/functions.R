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
  return(grep(paste( '^', patt, sep = ''), colnames(csv), fixed = F))
}

reshape_csvPoints <- function(csv, patternX, patternY, cols_to_keep){
  #' @title reshape CSV to points matrix
  #' @description Return column number matching name with string pattern
  #' @param file csv file
  #' @param patternX is the pattern describing column of x-coordinate
  #' @param patternY is the pattern describing column of Y-coordinate
  
  # csv <- allFiles
  
  # patternX <- 'x'
  # patternY <- 'y'
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
  
  uniqueX<- unique(csv[, col_of_interest(csv,'originX$')]);
  uniqueY<- unique(csv[, col_of_interest(csv,'originY$')]);
  geo<- unique(csv[, col_of_interest(csv, '.geo')]);
  
  # define output matrices
  allPoints <- vector('list', length(csv))
  
  for (n in 1:length(uniqueX)){
    # n<-1
    
    # Coordinates of transects
    coords <- qdapRegex::ex_between(as.character(geo[n]), ":[", "]}")[[1]]
    all_digits <- regmatches(coords, gregexpr("[-[:digit:].]+", coords))[[1]]
    
    begin_coords <- data.frame(lon = as.numeric(all_digits[1]), #x
                               lat = as.numeric(all_digits[2])) #y
    
    end_coords <- data.frame(lon = as.numeric(all_digits[3]),
                             lat = as.numeric(all_digits[4]))  
    # x <- as.matrix(rbind(begin_coords, end_coords))

    # only return observations if patternX >= 0 and if coordinates
    test1transect <- subset(csv,csv[,col_of_interest(csv, 'originX$')] == uniqueX[n]
                            & csv[,col_of_interest(csv, paste( '^', patternX, sep = ''))] != -1 )

    if(nrow(test1transect) > 0){ # some transect had no obs in any of the images
      
    
    # subset <- test1transect[,sort(c(col_of_interest(csv, 'coastDist$'), col_of_interest(csv, 'DATE_ACQUIRED$'),
    #                                 col_of_interest(csv, 'pos$')))]
    
    # coordiates of points of interest (coastline, offshore boundary etc)
    df_out <- data.frame(x = as.numeric(test1transect[,col_of_interest(csv, paste( '^', patternX, sep = ''))]),
                         y = as.numeric(test1transect[,col_of_interest(csv, paste( '^', patternY, sep = ''))]),
                         DATE_ACQUIRED = as.Date(as.character(test1transect[,col_of_interest(csv, 'DATE_ACQUIRED$')])),
                         pos = as.numeric(as.character(test1transect[,col_of_interest(csv, 'pos$')]))
                         )
    
    for (c in cols_to_keep){
      # c<- cols_to_keep[7]
      # print(c)
      vals = as.numeric(test1transect[,col_of_interest(csv, c)])
      df_out[,paste0(c)] <- vals
    }
    
    # also store the transect coordinates
    df_out$trans_x0 <- as.numeric(begin_coords$lon)
    df_out$trans_y0 <- as.numeric(begin_coords$lat)
    df_out$trans_x1 <- as.numeric(end_coords$lon)
    df_out$trans_y1 <- as.numeric(end_coords$lat)
    
    
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
      
      
      begin_coords <- data.frame(lon = as.numeric(test1transect$trans_x0[1]),
                                 lat = as.numeric(test1transect$trans_y0[1]))
      
      end_coords <- data.frame(lon = as.numeric(test1transect$trans_x1[1]),
                               lat = as.numeric(test1transect$trans_y1[1]))  
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
# alternatively you could iterate over x amount of observations. e.g. every 15 observations, do a outlier test
rosner <- function(x, minStd){
  # x <- testPos$coastDist
  # x <- subsets2$coastDist
  # minStd <- 100
  
  # assume no outliers (assign value 1)
  output <- rep(1,length.out=length(x))
  
  # only aply when there is 5 observations
  if(length(x) > 5){
    
    K <- length(x)-2
    if(K > 10){
      K <- 10
    } 
    
  if(K > floor(length(x)/2)){
    K <- floor(length(x)/2)
  }
    
    # skip_to_next <- FALSE
    # Rtest$all.stats$Obs.Num[which(Rtest$all.stats$Outlier)]
    
  test2 <- has_error(rosnerTest(x, K, warn = F), silent = !interactive())
  # throws an error when no outliers are detected... (all values are equal)
  
  if(!test2){ # if no error:
      rosnerOut <- rosnerTest(x, K, warn = F)[['all.stats']]
      # outliers:
      outliers <- rosnerOut[rosnerOut$Outlier & rosnerOut$SD.i>minStd, 'Obs.Num']
      
      output[outliers[!is.na(outliers)]] <- 0
      
      
    } 
    
  #   tryCatch(output[rosnerTest(x, K, warn = F)[['all.stats']][['Obs.Num']]
  #                   [which(rosnerTest(x, K, warn = F)[['all.stats']][['Outlier']])]] <- 0,
  #            
  #            # test <- ,
  #            
  #            # include that sd.i < x amount of pixels is also not an outlier?
  #            # rosnerTest(x, K, warn = F)[['all.stats']][['SD.i']]
  #            
  #            
  #            error = function(e) {skip_to_next <<- TRUE})
  #   # test <- any(rosnerTest(x, K, warn = F)), skip_to_next <- TRUE})
  #   
  #   if(skip_to_next) { output <- rep(1,length.out=length(x)) } 
  #   
  #   
  }
  
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

