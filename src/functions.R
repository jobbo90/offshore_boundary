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