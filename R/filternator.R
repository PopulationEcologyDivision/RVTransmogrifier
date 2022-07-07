#' @title filternator
#' @description This function is used to ensure that filtering of a single RV data object cascades
#' to all of the other known RV data objects.  For example, if GSSPECIES is filtered to only
#' "haddock", and \code{filternator} is run, then all of the various RV tables will be filtered to
#' the point where they relate directly to haddock. All remaining GSINF records would be sets that
#' caught haddock, all GSCAT records would be limited to haddock, all GSDET records would be limited
#' to haddock, etc.  Filtering is not limited to species, but any value that exists in any field in
#' any of the tables present in RVSurveyData package.
#' @param tblList the default is \code{NULL}. This is a list populated with all RV dataframes that
#' should have filtering applied to them.
#' @param keep_nullsets the default is \code{T}. This is used to control whether or not the 
#' presence/absence of species changes the returned set locations. 
#' @returns a list of filtered versions of the dataframes passed as \code{tblList}.  If the
#' filtering fails, a value of -1 will be returned.  For example, if data is filtered for a year
#' where data was not collected, a strata that doesn't exist, or a species that was not observed
#' would all result in a value of -1 being returned.
#' @author Mike McMahon
#' @note GSWARPOUT, GSCRUISELIST and GSSPEC can NOT be used to filter the other tables.
#' @export
filternator<-function(tblList = NULL, keep_nullsets=T, ...){
  args <- list(...)
  debug <- ifelse(is.null(args$debug), F, args$debug) 
  quiet <- ifelse(is.null(args$quiet), F, args$quiet) 

  LOOPAGAIN <- T
  initcnt <- sum(sapply(tblList, NROW))
  if (!quiet) message("filtering")
  while (LOOPAGAIN){
    precnt = sum(sapply(tblList, NROW))

    if ("GSINF" %in% names(tblList)){
        #these limit GSINF
        if (!keep_nullsets & "GSCAT" %in% names(tblList)) tblList$GSINF        <- tblList$GSINF[paste0(tblList$GSINF$MISSION,"_",tblList$GSINF$SETNO) %in% paste0(tblList$GSCAT$MISSION,"_",tblList$GSCAT$SETNO),]
        if ("GSMISSIONS" %in% names(tblList))             tblList$GSINF        <- tblList$GSINF[tblList$GSINF$MISSION %in% tblList$GSMISSIONS$MISSION,]
        
        if (nrow(tblList$GSINF)==0){
          if (!quiet)message("No sets remain - quitting")
          return(-1)
        }
        
        #these are limited by GSINF
        if ("GSXTYPE" %in% names(tblList))               tblList$GSXTYPE      <- tblList$GSXTYPE[tblList$GSXTYPE$XTYPE %in% tblList$GSINF$TYPE,]
        if ("GSCURNT" %in% names(tblList))               tblList$GSCURNT      <- tblList$GSCURNT[tblList$GSCURNT$CURNT %in% tblList$GSINF$CURNT,]
        if ("GSFORCE" %in% names(tblList))               tblList$GSFORCE      <- tblList$GSFORCE[tblList$GSFORCE$FORCE %in% tblList$GSINF$FORCE,]
        if ("GSHOWOBT" %in% names(tblList))              tblList$GSHOWOBT     <- tblList$GSHOWOBT[tblList$GSHOWOBT$HOWOBT %in% c(tblList$GSINF$HOWD,tblList$GSINF$HOWS) ,]
        if ("GSXTYPE" %in% names(tblList))               tblList$GSAUX        <- tblList$GSAUX[tblList$GSAUX$AUX %in% tblList$GSINF$AUX,]
        if ("GSSTRATUM" %in% names(tblList))             tblList$GSSTRATUM    <- tblList$GSSTRATUM[tblList$GSSTRATUM$STRAT %in% tblList$GSINF$STRAT,]

        if ("GSCRUISELIST" %in% names(tblList))          tblList$GSCRUISELIST <- tblList$GSCRUISELIST[tblList$GSCRUISELIST$MISSION  %in% tblList$GSINF$MISSION,]
        if ("GSGEAR" %in% names(tblList))                tblList$GSGEAR       <- tblList$GSGEAR[tblList$GSGEAR$GEAR %in% tblList$GSINF$GEAR,]
        if ("GSMISSIONS" %in% names(tblList))            tblList$GSMISSIONS   <- tblList$GSMISSIONS[tblList$GSMISSIONS$MISSION %in% tblList$GSINF$MISSION,]
        if ("GSWARPOUT" %in% names(tblList))             tblList$GSWARPOUT    <- tblList$GSWARPOUT[paste0(tblList$GSWARPOUT$MISSION,"_",tblList$GSWARPOUT$SETNO) %in% paste0(tblList$GSINF$MISSION,"_",tblList$GSINF$SETNO),]
    }
    
    if ("GSCAT" %in% names(tblList)){
      #these limit GSCAT
      if (!keep_nullsets & "GSINF" %in% names(tblList))   tblList$GSCAT        <- tblList$GSCAT[paste0(tblList$GSCAT$MISSION,"_",tblList$GSCAT$SETNO) %in% paste0(tblList$GSINF$MISSION,"_",tblList$GSINF$SETNO),]
      if ("GSINF" %in% names(tblList))                    tblList$GSCAT        <- tblList$GSCAT[paste0(tblList$GSCAT$MISSION,"_",tblList$GSCAT$SETNO) %in% paste0(tblList$GSINF$MISSION,"_",tblList$GSINF$SETNO) ,]
      if ("GSSPECIES" %in% names(tblList))                tblList$GSCAT        <- tblList$GSCAT[tblList$GSCAT$SPEC %in% tblList$GSSPECIES$CODE ,]
      if ("GSSPECIES_20220624" %in% names(tblList))       tblList$GSCAT        <- tblList$GSCAT[tblList$GSCAT$SPEC %in% tblList$GSSPECIES_20220624$CODE ,]
      
      if (nrow(tblList$GSCAT)==0){
        if (!quiet)message("No catch records remain - quitting")
        return(-1)
      }
      #these are limited by GSCAT
      if ("GSDET" %in% names(tblList))                    tblList$GSDET        <- tblList$GSDET[paste0(tblList$GSDET$MISSION,"_",tblList$GSDET$SETNO,"_",tblList$GSDET$SPEC) %in% paste0(tblList$GSCAT$MISSION,"_",tblList$GSCAT$SETNO,"_",tblList$GSCAT$SPEC) ,]
    }
    
    if ("GSDET" %in% names(tblList)){
      #these limit GSDET
      if ("GSCAT" %in% names(tblList))                    tblList$GSDET        <- tblList$GSDET[paste0(tblList$GSDET$MISSION,"_",tblList$GSDET$SETNO,"_",tblList$GSDET$SPEC) %in% paste0(tblList$GSCAT$MISSION,"_",tblList$GSCAT$SETNO,"_",tblList$GSCAT$SPEC) ,]
      if (!keep_nullsets & "GSINF" %in% names(tblList))   tblList$GSDET        <- tblList$GSDET[paste0(tblList$GSDET$MISSION,"_",tblList$GSDET$SETNO) %in% paste0(tblList$GSINF$MISSION,"_",tblList$GSINF$SETNO) ,]
 
      #these are limited by GSDET
      if ("GSMATURITY" %in% names(tblList))             tblList$GSMATURITY     <- tblList$GSMATURITY[tblList$GSMATURITY$CODE %in% tblList$GSDET$FMAT,]
      if ("GSSEX" %in% names(tblList))                  tblList$GSSEX          <- tblList$GSSEX[tblList$GSSEX$CODE %in% tblList$GSDET$FSEX,]
    }                       
    if (debug)browser()
    if ("GSSPECIES" %in% names(tblList)){
      #these limit GSSPECIES
      if ("GSSPECIES_20220624" %in% names(tblList))       tblList$GSSPECIES    <- tblList$GSSPECIES[tblList$GSSPECIES$CODE %in% tblList$GSSPECIES_20220624$CODE,]
      if ("GSCAT" %in% names(tblList))                    tblList$GSSPECIES    <- tblList$GSSPECIES[tblList$GSSPECIES$CODE %in% tblList$GSCAT$SPEC,]

      ##these are limited by GSSPECIES
      if ("GSSPEC" %in% names(tblList))                      tblList$GSSPEC    <- tblList$GSSPEC[tblList$GSSPEC$SPEC %in% tblList$GSSPECIES$CODE,]
    }
    
    if ("GSSPECIES_20220624" %in% names(tblList)){
      #these limit GSSPECIES_20220624
      if ("GSSPECIES" %in% names(tblList))          tblList$GSSPECIES_20220624 <- tblList$GSSPECIES_20220624[tblList$GSSPECIES_20220624$CODE %in% tblList$GSSPECIES$CODE,]
      if ("GSCAT" %in% names(tblList))              tblList$GSSPECIES_20220624 <- tblList$GSSPECIES_20220624[tblList$GSSPECIES_20220624$CODE %in% tblList$GSCAT$SPEC,]
      if ("GSSPECIES_TAX" %in% names(tblList))      tblList$GSSPECIES_20220624 <- tblList$GSSPECIES_20220624[tblList$GSSPECIES_20220624$APHIAID %in% tblList$GSSPECIES_TAX$APHIAID,]

      ##these are limited by GSSPECIES_20220624
      if ("GSSPEC" %in% names(tblList))                      tblList$GSSPEC    <- tblList$GSSPEC[tblList$GSSPEC$SPEC %in% tblList$GSSPECIES_20220624$CODE,]
      if ("GSSPECIES_TAX" %in% names(tblList))           tblList$GSSPECIES_TAX <- tblList$GSSPECIES_TAX[tblList$GSSPECIES_TAX$APHIAID %in% tblList$GSSPECIES_20220624$APHIAID,]
      
    }
    
    
    if ("GSSPECIES_TAX" %in% names(tblList)){
      if ("GSSPECIES_20220624" %in% names(tblList))       tblList$GSSPECIES_20220624    <- tblList$GSSPECIES_20220624[tblList$GSSPECIES_20220624$APHIAID %in% tblList$GSSPECIES_TAX$APHIAID,]
      
    }
  
    postcnt =  sum(sapply(tblList, NROW))
    if(postcnt==precnt) {
      LOOPAGAIN=FALSE
    } else{
      if (!quiet) message(".")
      if (!quiet) print(sapply(tblList, NROW))
    }
  }
  
  return(tblList)
}
