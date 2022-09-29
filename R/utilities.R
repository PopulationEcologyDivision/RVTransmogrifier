#' @title addTUNITS
#' @description This function .
#' @param stratum the default is \code{NULL}. This is a dataframe of strata, each with an AREA field
#' in square nautical miles.
#' @param towDist the default is \code{1.75}. 
#' @param wingspread the default is \code{41}. 
#' @returns the original \code{stratum} object, but with an additional \code{TUNITS} field.
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @note GSWARPOUT, GSCRUISELIST and GSSPEC can NOT be used to filter the other tables.
#' @export
addTUNITS<- function(stratum = NULL,towDist = 1.75, wingspread = 41){
  #calculate strat areas into tunits; US nautical mile is 6080.2ft
  stratum$TUNITS <- NA
  stratum$TUNITS <-stratum$AREA/(towDist * (wingspread/6080.2))
  return(stratum)
}