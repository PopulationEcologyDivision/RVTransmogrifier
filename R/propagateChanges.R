#' @title propagateChanges
#' @description This function is used to ensure that filtering of a single RV data object cascades
#' to all of the other known RV data objects. For example, if GSSPECIES is filtered to only
#' "haddock", and \code{propagateChanges} is run, then all of the various RV tables will be filtered to
#' the point where they relate directly to haddock. All remaining GSINF records would be sets that
#' caught haddock, all GSCAT records would be limited to haddock, all GSDET records would be limited
#' to haddock, etc. Filtering is not limited to species, but any value that exists in any field in
#' any of the tables present in RVSurveyData package.
#' @param tblList the default is \code{NULL}. This is a list populated with all RV dataframes that
#' should have filtering applied to them.
#' @param keep_nullsets the default is \code{FALSE}. This is used to control whether or not the 
#' presence/absence of species changes the returned set locations. If \code{FALSE}, the only 
#' returned records will be directly linkable to the user-filtered records. If \code{TRUE}, set 
#' locations will still be returned, even if a particular species was not caught there.
#' @param ... other arguments passed to methods (i.e. 'debug' and 'quiet')
#' @returns a list of filtered versions of the dataframes passed as \code{tblList}. If the
#' filtering fails, a value of -1 will be returned. For example, if data is filtered for a year
#' where data was not collected, a strata that doesn't exist, or a species that was not observed
#' would all result in a value of -1 being returned.
#' @author Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @note GSWARPOUT, GSCRUISELIST and GSSPEC can NOT be used to filter the other tables.
#' @export
propagateChanges<-function(tblList = NULL, keep_nullsets=FALSE, ...){
  args <- list(...)
  debug <- ifelse(is.null(args$debug), F, args$debug) 
  quiet <- ifelse(is.null(args$quiet), F, args$quiet)
  recdTables <- names(tblList)
  essentialTables <- c("GSINF","GSCAT","GSMISSIONS","GSXTYPE","GSCURNT","GSFORCE","GSHOWOBT","GSSTRATUM","GSGEAR","GSSEX","GSSPECIES","GSWARPOUT","GSAUX","GSMATURITY","dataDETS","dataLF")
  if (length(setdiff(essentialTables,recdTables))>0) stop("Missing the following tables from your tblList object: ",paste(setdiff(essentialTables,recdTables1), collapse=", "))
  LOOPAGAIN <- T

  if (!quiet) message("filtering")
  while (LOOPAGAIN){
    print(sapply(tblList, nrow))
    precnt = sum(sapply(tblList, NROW))
    #these limit GSINF
    tblList$GSINF <- merge(tblList$GSINF, unique(tblList$GSCAT[,c("MISSION","SETNO")]), all.x=keep_nullsets )
    tblList$GSINF <- merge(tblList$GSINF,  unique(tblList$GSMISSIONS[,"MISSION",drop=F]))
    if (nrow(tblList$GSINF)==0){
      if (!quiet)message("No sets remain - quitting")
      return(-1)
    }
    #these are limited by GSINF

    tblList$GSXTYPE  <- merge(tblList$GSXTYPE, unique(tblList$GSINF[,"TYPE",drop=F]),by.x="XTYPE", by.y="TYPE")
    tblList$GSCURNT  <- merge(tblList$GSCURNT, unique(tblList$GSINF[,"CURNT",drop=F]))
    tblList$GSFORCE  <- merge(tblList$GSFORCE, unique(tblList$GSINF[,"FORCE",drop=F]))
    tblList$GSHOWOBT <- tblList$GSHOWOBT[which(tblList$GSHOWOBT$HOWOBT %in% c(unique(tblList$GSINF$HOWD),unique(tblList$GSINF$HOWS))) ,]
    tblList$GSAUX    <- merge(tblList$GSAUX, unique(tblList$GSINF[,"AUX",drop=F]))
    tblList$GSSTRATUM <- merge(tblList$GSSTRATUM, unique(tblList$GSINF[,"STRAT",drop=F]))
    tblList$GSGEAR     <- merge(tblList$GSGEAR, unique(tblList$GSINF[,"GEAR",drop=F]))
    tblList$GSMISSIONS <- merge(tblList$GSMISSIONS, unique(tblList$GSINF[,"MISSION",drop=F]))
    tblList$GSWARPOUT <- merge(tblList$GSWARPOUT, unique(tblList$GSINF[,c("MISSION","SETNO")]))
    #these limit GSCAT
    tblList$GSCAT <- merge(tblList$GSCAT, unique(tblList$GSINF[,c("MISSION","SETNO")]), all.y=keep_nullsets )
    tblList$GSCAT <- merge(tblList$GSCAT,  unique(tblList$GSSPECIES[,"CODE",drop=F]), by.x="SPEC", by.y  ="CODE")
    if (nrow(tblList$GSCAT)==0){
      if (!quiet)message("No catch records remain - quitting")
      return(-1)
    }
    #these are limited by GSCAT
    tblList$dataDETS <- merge(tblList$dataDETS, unique(tblList$GSCAT[,c("MISSION","SETNO", "SPEC")])) 
    tblList$dataLF   <- merge(tblList$dataLF,   unique(tblList$GSCAT[,c("MISSION","SETNO", "SPEC")]))
    
    if(any(c("GSCAT_agg","dataLF_agg")%in% names(tblList)) & ("GSCAT" %in% names(tblList) & "GSSPECIES" %in% names(tblList))){
      tmp <- unique(merge(tblList$GSCAT[,c("MISSION", "SETNO","SPEC")], unique(tblList$GSSPECIES[,c("CODE", "TAXA_","TAXARANK_")]), by.x="SPEC", by.y="CODE"))
      tmp$SPEC <- NULL
      tblList$GSCAT_agg  <- merge(tblList$GSCAT_agg,  unique(tmp[,c("MISSION", "SETNO","TAXA_","TAXARANK_")]))
      tblList$dataLF_agg <- merge(tblList$dataLF_agg, unique(tmp[,c("MISSION", "SETNO","TAXA_","TAXARANK_")]))
    }
    
    #these limit dataDETS
    tblList$dataDETS <- merge(tblList$dataDETS, unique(tblList$GSCAT[,c("MISSION","SETNO", "SPEC")]))
    tblList$dataDETS <- merge(tblList$dataDETS, unique(tblList$GSINF[,c("MISSION","SETNO")]), all.y=keep_nullsets)
    #these are limited by dataDETS
    tblList$GSMATURITY <- merge(tblList$GSMATURITY, unique(tblList$dataDETS[,"FMAT",drop=F]), by.x="CODE", by.y="FMAT")
    tblList$GSSEX <- merge(tblList$GSSEX, unique(tblList$dataDETS[,"FSEX",drop=F]), by.x="CODE", by.y="FSEX")
    tblList$dataLF <- merge(tblList$dataLF, unique(tblList$dataDETS[,c("MISSION","SETNO", "SPEC","FSEX")]))
    
    #these limit dataLF
    tblList$dataLF <- merge(tblList$dataLF, unique(tblList$GSCAT[,c("MISSION","SETNO", "SPEC")]))

    if (!keep_nullsets) tblList$dataLF <- merge(tblList$dataLF,unique(tblList$GSINF[,c("MISSION","SETNO")]), all.y=keep_nullsets)
    
    #these are limited by dataLF
    tblList$GSMATURITY <- merge(tblList$GSMATURITY, unique(tblList$dataDETS[,"FMAT",drop=F]), by.x="CODE", by.y="FMAT")
    tblList$GSSEX <- merge(tblList$GSSEX, unique(tblList$dataDETS[,"FSEX",drop=F]), by.x="CODE", by.y="FSEX")
    tblList$dataDETS <- merge(tblList$dataDETS, unique(tblList$dataLF[,c("MISSION","SETNO", "SPEC","FSEX")]))
    
    #these limit GSSPECIES
    tblList$GSSPECIES <- merge(tblList$GSSPECIES, unique(tblList$GSCAT[,"SPEC",drop=F]), by.x="CODE", by.y="SPEC")
    
    postcnt = sum(sapply(tblList, NROW))
    if(postcnt==precnt) {
      LOOPAGAIN=FALSE
    } else{
      if (!quiet) message(".")
      if (!quiet) print(sapply(tblList, NROW))
    }
  }
  return(tblList)
}
