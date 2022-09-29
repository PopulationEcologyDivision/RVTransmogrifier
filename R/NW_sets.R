#' @title NW_sets
#' @description This function ...
#' @param tblList the default is \code{NULL}. This is a list populated with all RV dataframes. Prior 
#' to running this function they should all have been filtered via \code{propagateChanges()}.
#' @param towDist the default is \code{1.75}. This is ...
#' @param ... other arguments passed to methods (i.e. 'debug' and 'quiet')
#' @returns ...
#' @author Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
NW_sets<-function(tblList = NULL,towDist = 1.75, ...){
  args <- list(...)
  debug <- ifelse(is.null(args$debug), F, args$debug) 
  quiet <- ifelse(is.null(args$quiet), F, args$quiet) 
  
  dfRawInf <- tblList$GSINF
  dfRawCatch <- tblList$GSCAT
  str_dfNWSets<-merge(dfRawInf, dfRawCatch, all.x=T)
  str_dfNWSets[which(is.na(str_dfNWSets$TOTWGT)),c('TOTWGT','TOTNO')]<- 0
  str_dfNWSets$DIST[which(is.na(str_dfNWSets$DIST)|(str_dfNWSets$DIST==0))] <-towDist
  str_dfNWSets$RAW_TOTWGT <-str_dfNWSets$TOTWGT
  str_dfNWSets$TOTWGT <- (str_dfNWSets$TOTWGT*towDist)/str_dfNWSets$DIST
  str_dfNWSets$TOTWGT[which(!is.finite(str_dfNWSets$TOTWGT))] <-1
  str_dfNWSets$RAW_TOTNO <-str_dfNWSets$TOTNO 
  str_dfNWSets$TOTNO <- (str_dfNWSets$TOTNO*towDist)/str_dfNWSets$DIST
  str_dfNWSets$TOTNO[which(!is.finite(str_dfNWSets$TOTNO))] <- 1
  str_dfNWSets<-str_dfNWSets[order(str_dfNWSets$STRAT,str_dfNWSets$SETNO),]
  #drop things that are already in inf (except for join conveniences)
  str_dfNWSets <- str_dfNWSets[c('MISSION','SETNO','STRAT',setdiff(colnames(str_dfNWSets), colnames(dfRawInf)))]
  
  names(str_dfNWSets)[names(str_dfNWSets) == "AREA"] <- "UNIT_AREA"
  return(str_dfNWSets)
}
