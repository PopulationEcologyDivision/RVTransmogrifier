#' @title getSurvey
#' @description A survey is a defined by a combination of a range of months, a selection of strata, 
#' and a tow 'type'. This function ensures that all those values are combined correctly to get the 
#' valid data for a survey.
#' @param survey the default is \code{NULL}. This specifies which survey should be extracted.  Valid
#' values are "SPRING", "SUMMER", "FALL", and "4VSW". 
#' @param years the default is \code{NULL}. This parameter allows you to generate datasets for one or
#' more specific years.  A value of NULL will result in products being generated for all years for
#' which data exists, and a vector of years will result in dataset that include the specified years.
#' @param ... other arguments passed to methods (i.e. 'debug' and 'quiet')
getSurvey<-function(survey = NULL, years=NULL, ...){
  args <- list(...)
  debug <- ifelse(is.null(args$debug), F, args$debug) 
  quiet <- ifelse(is.null(args$quiet), F, args$quiet) 
  
  if (is.null(survey)) stop("Please specify a value for 'survey'")
  survey <- toupper(survey)
  
  months <- switch(survey,
                   "SPRING" = c(1,2,3,4),
                   "SUMMER" = c(5,6,7,8),
                   "FALL" = c(9,10,11,12),
                   "4VSW" = -1
  )
  thisList <- loadRVData()
  
  thisList$GSINF <- thisList$GSINF[thisList$GSINF$TYPE==1,]
  if (!is.null(years)){
    thisList$GSMISSIONS <- thisList$GSMISSIONS[which(thisList$GSMISSIONS$YEAR %in% c(years)),]
  }
  if (survey != "4VSW"){
    #get rid of 4VSW cod data
    thisList$GSINF <- thisList$GSINF[-which(thisList$GSINF$STRAT %in% c(396:411)
                                            & lubridate::month(thisList$GSINF$SDATE) %in% c(1,2,3,4)),]
    #retain appropriate months
    thisList$GSINF <- thisList$GSINF[which(lubridate::month(thisList$GSINF$SDATE) %in% months),]
  }else{
    thisList$GSINF <- thisList$GSINF[which(thisList$GSINF$STRAT %in% c(396:411)
                                           & lubridate::month(thisList$GSINF$SDATE) %in% c(1,2,3,4)),]
  }
  res <- filternator(thisList)
  if (is.numeric(res))stop("Your query did not return valid results")
  return(res)
}