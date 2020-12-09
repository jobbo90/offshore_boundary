multiply <- function(x,y){
  return(x*y)
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

reshape_csvPoints <- function(csv, patternX, patternY){
  #' @title reshape CSV to points matrix
  #' @description Return column number matching name with string pattern
  #' @param file csv file
  #' @param patternX is the pattern describing column of x-coordinate
  #' @param patternY is the pattern describing column of Y-coordinate
  
  # csv <- csv1
  # patternX <- 'peakCoordX'
  # patternY <- 'peakCoordY'
  
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
  allPoints <- vector('list', length(csv));
  
  for (n in 1:length(uniqueX)){
    #n<-1
    
    # Coordinates of transects
    coords <- qdapRegex::ex_between(as.character(geo[n]), ":[", "]}")[[1]]
    all_digits <- regmatches(coords, gregexpr("[-[:digit:].]+", coords))[[1]]
    
    begin_coords <- data.frame(lon = as.numeric(all_digits[1]),
                               lat = as.numeric(all_digits[2]))
    
    end_coords <- data.frame(lon = as.numeric(all_digits[3]),
                             lat = as.numeric(all_digits[4]))  
    x <- as.matrix(rbind(begin_coords, end_coords))

    # only return observations if coastDist >= 0 and if coordinates
    test1transect <- subset(csv,csv[,col_of_interest(csv, 'originX$')]== uniqueX[n] 
                            & csv[,col_of_interest(csv, 'coastDist$')] >= 0 )
    
    subset <- test1transect[,sort(c(col_of_interest(csv, 'coastDist$'), col_of_interest(csv, 'DATE_ACQUIRED$'),
                                    col_of_interest(csv, 'pos$')))]
    
    # coordiates of points of interest (coastline, offshore boundary etc)
    coords <- data.frame(x = as.numeric(test1transect[,col_of_interest(csv, paste( '^', patternX, sep = ''))]),
                         y = as.numeric(test1transect[,col_of_interest(csv, paste( '^', patternY, sep = ''))]),
                         DATE_ACQUIRED = as.character(test1transect[,col_of_interest(csv, 'DATE_ACQUIRED$')]),
                         pos = as.character(test1transect[,col_of_interest(csv, 'pos$')]),
                         axisDist = as.numeric(test1transect[,col_of_interest(csv, 'axisDist$')]))
    
    allPoints <- rbind(allPoints, coords)
    
  }
  
  
  # make it spatial
  SpatialPoints <- SpatialPointsDataFrame(data.frame(allPoints[,'x'], allPoints[,'y'] ), 
                                           data = data.frame(DATE_ACQUIRED = allPoints[,'DATE_ACQUIRED'],
                                                             pos = allPoints[,'pos'],
                                                             axisDist = as.numeric(allPoints[,'axisDist'])),
                                           proj4string=CRS("+proj=longlat +datum=WGS84"))
  points_sf <- st_as_sf(SpatialPoints)
  
  return(points_sf)
}


reshape_csvLines <- function(csv){
  #' @title reshape CSV to points matrix
  #' @description Return column number matching name with string pattern
  #' @param file csv file
  
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
