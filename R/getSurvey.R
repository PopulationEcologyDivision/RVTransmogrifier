#' @title getSurvey
#' @description A survey is a defined by a combination of a range of months, a selection of strata, 
#' and a tow 'type'. This function ensures that all those values are combined correctly to get the 
#' valid data for a survey.
#' @param survey the default is \code{NULL}. This specifies which survey should be extracted.  Valid
#' values are "SPRING", "SUMMER", "FALL", and "4VSW". 
#' @param years the default is \code{NULL}. This parameter allows you to generate datasets for one or
#' more specific years.  A value of NULL will result in products being generated for all years for
#' which data exists, and a vector of years will result in dataset that include the specified years.
#' @param type1TowsOnly the default is \code{TRUE}.  "Valid" survey tows (i.e. "type 1") are those 
#' tows that fished correctly, and can be used confidently while looking data from a stratified  
#' random survey design.  Setting \code{type1TowsOnly} to \code{FALSE} will allow other types of sets 
#' to be extracted, but these shouldn't be included while assessing fish stocks.
#' @param ... other arguments passed to methods (i.e. 'keep_nullsets', debug' and 'quiet')
#' @returns a list of dataframes which have been filtered to only include data related to the 
#' specified survey and years
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
getSurvey<-function(survey = NULL, years=NULL, type1TowsOnly = TRUE,...){
  args <- list(...)
  keep_nullsets <- ifelse(is.null(args$keep_nullsets), T, args$keep_nullsets) 
  debug <- ifelse(is.null(args$debug), F, args$debug) 
  quiet <- ifelse(is.null(args$quiet), T, args$quiet) 
  
  if (is.null(survey)) stop("Please specify a value for 'survey'")
  survey <- toupper(survey)
  
  months <- switch(survey,
                   "SPRING" = c(1,2,3,4),
                   "SUMMER" = c(5,6,7,8),
                   "FALL" = c(9,10,11,12),
                   "4VSW" = -1
  )
  tblList <- loadRVData()
  


  if (type1TowsOnly) tblList$GSINF <- tblList$GSINF[tblList$GSINF$TYPE==1,]
  if (!is.null(years)){
    tblList$GSMISSIONS <- tblList$GSMISSIONS[which(tblList$GSMISSIONS$YEAR %in% c(years)),]
  }
  if (survey != "4VSW"){
    #get rid of 4VSW cod data
    tblList$GSINF <- tblList$GSINF[-which(tblList$GSINF$STRAT %in% c(396:411)
                                            & lubridate::month(tblList$GSINF$SDATE) %in% c(1,2,3,4)),]
    #retain appropriate months
    tblList$GSINF <- tblList$GSINF[which(lubridate::month(tblList$GSINF$SDATE) %in% months),]
  }else{
    tblList$GSINF <- tblList$GSINF[which(tblList$GSINF$STRAT %in% c(396:411)
                                           & lubridate::month(tblList$GSINF$SDATE) %in% c(1,2,3,4)),]
  }
  if(!is.null(args$taxa)|!is.null(args$code)|!is.null(args$aphiaid)){
    tblList      <- filterSpecies(tblList, keep_nullsets = keep_nullsets,
                                  taxa = args$taxa,
                                  code = args$code,
                                  aphiaid = args$aphiaid)
    if (class(tblList)=="numeric")stop("Requested filter removed all species")
    tblList      <- aggregateByTaxa(tblList = tblList,
                                    taxa = args$taxa,
                                    code = args$code,
                                    aphiaid = args$aphiaid)
  }
  tblList <- propagateChanges(tblList, keep_nullsets=keep_nullsets, quiet=quiet)
  if (is.numeric(tblList) & !quiet)message("Your query did not return valid results")
 
  return(tblList)
}