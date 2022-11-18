#' @title nwSets
#' @description This function calculates the stratified values of numbers and weights by set.
#' @param tblList the default is \code{NULL}. This is a list populated with all RV dataframes. Prior 
#' to running this function they should all have been filtered via \code{propagateChanges()}.
#' @param towDist the default is \code{1.75}. This is the default tow length for the survey.
#' @param ... other arguments passed to methods (i.e. 'debug' and 'quiet')
#' @returns ...
#' @author Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
nwSets<-function(tblList = NULL,towDist = 1.75, ...){
  args <- list(...)
  debug <- ifelse(is.null(args$debug), F, args$debug) 
  quiet <- ifelse(is.null(args$quiet), F, args$quiet) 
  towDist <- ifelse(is.null(args$towDist), towDist, args$towDist) 

  if("GSCAT_agg" %in% names(tblList)){
    dfRawCatch <- tblList$GSCAT_agg
    t_field <- "TAXA_"
  } else {
    dfRawCatch <- tblList$GSCAT
    t_field <- "SPEC"
  }
  taxa<- unique(dfRawCatch[!is.na(dfRawCatch[,t_field]), t_field])
  res<-list()
  for(t in 1:length(taxa)){
    thisTaxSets<-merge(tblList$GSINF, dfRawCatch[dfRawCatch[,t_field] == taxa[t], ], all.x=T)
    thisTaxSets[which(is.na(thisTaxSets[,t_field])),t_field]<- taxa[t]
    thisTaxSets[which(is.na(thisTaxSets$TOTWGT)),c('TOTWGT','TOTNO')]<- 0
    thisTaxSets$DIST[which(is.na(thisTaxSets$DIST)|(thisTaxSets$DIST==0))] <-towDist
    thisTaxSets$RAW_TOTWGT <-thisTaxSets$TOTWGT
    thisTaxSets$TOTWGT <- round((thisTaxSets$TOTWGT*towDist)/thisTaxSets$DIST,5)
    thisTaxSets$TOTWGT[which(!is.finite(thisTaxSets$TOTWGT))] <-1
    thisTaxSets$RAW_TOTNO <-thisTaxSets$TOTNO 
    thisTaxSets$TOTNO <- round((thisTaxSets$TOTNO*towDist)/thisTaxSets$DIST,5)
    thisTaxSets$TOTNO[which(!is.finite(thisTaxSets$TOTNO))] <- 1
    thisTaxSets<-thisTaxSets[order(thisTaxSets$STRAT,thisTaxSets$SETNO),]
    #drop things that are already in inf (except for join conveniences)
    thisTaxSets <- thisTaxSets[c('MISSION','SETNO','STRAT',setdiff(colnames(thisTaxSets), colnames(tblList$GSINF)))]
    names(thisTaxSets)[names(thisTaxSets) == "AREA"] <- "UNIT_AREA"
    thisTaxSets <- thisTaxSets[with(thisTaxSets,order(MISSION, SETNO)),]
    res[[t]] <- thisTaxSets
    }
  return(res)
}
