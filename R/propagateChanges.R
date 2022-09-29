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
  
  # knownObj <- c(""GSSEX", "GSFORCE", "strataMar_sf", "GSINF", "GSSPECIES", 
  # "GSMISSIONS", "dataDETS", "GSSTRATUM", "GSMATURITY", "GSCAT", "GSWARPOUT", 
  # "GSSPEC", "GSHOWOBT", "dataLF", "nafo_sf", "GSXTYPE", "GSGEAR", "GSCURNT", 
  # "strataMar4VSW_sf", "GSAUX")
  # reqdObjs <- c("GSINF","GSCAT","GSDET")
  
  LOOPAGAIN <- T
  initcnt <- sum(sapply(tblList, NROW))
  if (!quiet) message("filtering")
  while (LOOPAGAIN){
    precnt = sum(sapply(tblList, NROW))
    
    if ("GSINF" %in% names(tblList)){
      #these limit GSINF
      if (!keep_nullsets & "GSCAT" %in% names(tblList)) tblList$GSINF <- tblList$GSINF[which(paste0(tblList$GSINF$MISSION,"_",tblList$GSINF$SETNO) %in% paste0(tblList$GSCAT$MISSION,"_",tblList$GSCAT$SETNO)),]
      if ("GSMISSIONS" %in% names(tblList)) tblList$GSINF <- tblList$GSINF[which(tblList$GSINF$MISSION %in% tblList$GSMISSIONS$MISSION),]
      
      if (nrow(tblList$GSINF)==0){
        if (!quiet)message("No sets remain - quitting")
        return(-1)
      }
      
      #these are limited by GSINF
      if ("GSXTYPE" %in% names(tblList)) tblList$GSXTYPE <- tblList$GSXTYPE[which(tblList$GSXTYPE$XTYPE %in% tblList$GSINF$TYPE),]
      if ("GSCURNT" %in% names(tblList)) tblList$GSCURNT <- tblList$GSCURNT[which(tblList$GSCURNT$CURNT %in% tblList$GSINF$CURNT),]
      if ("GSFORCE" %in% names(tblList)) tblList$GSFORCE <- tblList$GSFORCE[which(tblList$GSFORCE$FORCE %in% tblList$GSINF$FORCE),]
      if ("GSHOWOBT" %in% names(tblList)) tblList$GSHOWOBT <- tblList$GSHOWOBT[which(tblList$GSHOWOBT$HOWOBT %in% c(tblList$GSINF$HOWD,tblList$GSINF$HOWS)) ,]
      if ("GSXTYPE" %in% names(tblList)) tblList$GSAUX <- tblList$GSAUX[which(tblList$GSAUX$AUX %in% tblList$GSINF$AUX),]
      if ("GSSTRATUM" %in% names(tblList)) tblList$GSSTRATUM <- tblList$GSSTRATUM[which(tblList$GSSTRATUM$STRAT %in% tblList$GSINF$STRAT),]
      
      if ("GSCRUISELIST" %in% names(tblList)) tblList$GSCRUISELIST <- tblList$GSCRUISELIST[which(tblList$GSCRUISELIST$MISSION %in% tblList$GSINF$MISSION),]
      if ("GSGEAR" %in% names(tblList)) tblList$GSGEAR <- tblList$GSGEAR[which(tblList$GSGEAR$GEAR %in% tblList$GSINF$GEAR),]
      if ("GSMISSIONS" %in% names(tblList)) tblList$GSMISSIONS <- tblList$GSMISSIONS[which(tblList$GSMISSIONS$MISSION %in% tblList$GSINF$MISSION),]
      if ("GSWARPOUT" %in% names(tblList)) tblList$GSWARPOUT <- tblList$GSWARPOUT[which(paste0(tblList$GSWARPOUT$MISSION,"_",tblList$GSWARPOUT$SETNO) %in% paste0(tblList$GSINF$MISSION,"_",tblList$GSINF$SETNO)),]
    }
    
    if ("GSCAT" %in% names(tblList)){
      #these limit GSCAT
      if (!keep_nullsets & "GSINF" %in% names(tblList)) tblList$GSCAT <- tblList$GSCAT[which(paste0(tblList$GSCAT$MISSION,"_",tblList$GSCAT$SETNO) %in% 
                                                                                               paste0(tblList$GSINF$MISSION,"_",tblList$GSINF$SETNO)),]
      if ("GSINF" %in% names(tblList)) tblList$GSCAT <- tblList$GSCAT[which(paste0(tblList$GSCAT$MISSION,"_",tblList$GSCAT$SETNO) %in% 
                                                                              paste0(tblList$GSINF$MISSION,"_",tblList$GSINF$SETNO)) ,]
      if ("GSSPECIES" %in% names(tblList)) tblList$GSCAT <- tblList$GSCAT[which(tblList$GSCAT$SPEC %in% tblList$GSSPECIES$CODE) ,]
      
      if (nrow(tblList$GSCAT)==0){
        if (!quiet)message("No catch records remain - quitting")
        return(-1)
      }
      #these are limited by GSCAT
      if ("dataDETS" %in% names(tblList)) tblList$dataDETS <- tblList$dataDETS[which(paste0(tblList$dataDETS$MISSION,"_",tblList$dataDETS$SETNO,"_",tblList$dataDETS$SPEC) %in% 
                                                                                       paste0(tblList$GSCAT$MISSION,"_",tblList$GSCAT$SETNO,"_",tblList$GSCAT$SPEC)) ,]
      if ("dataLF" %in% names(tblList)) tblList$dataLF <- tblList$dataLF[which(paste0(tblList$dataLF$MISSION,"_",tblList$dataLF$SETNO,"_",tblList$dataLF$SPEC) %in% 
                                                                                 paste0(tblList$GSCAT$MISSION,"_",tblList$GSCAT$SETNO,"_",tblList$GSCAT$SPEC)) ,]
    }
    
    if ("dataDETS" %in% names(tblList)){
      #these limit dataDETS
      if ("GSCAT" %in% names(tblList)) tblList$dataDETS <- tblList$dataDETS[which(paste0(tblList$dataDETS$MISSION,"_",tblList$dataDETS$SETNO,"_",tblList$dataDETS$SPEC) %in% 
                                                                                    paste0(tblList$GSCAT$MISSION,"_",tblList$GSCAT$SETNO,"_",tblList$GSCAT$SPEC)) ,]
      if (!keep_nullsets & "GSINF" %in% names(tblList)) tblList$dataDETS <- tblList$dataDETS[which(paste0(tblList$dataDETS$MISSION,"_",tblList$dataDETS$SETNO) %in% 
                                                                                                     paste0(tblList$GSINF$MISSION,"_",tblList$GSINF$SETNO)) ,]
      
      #these are limited by dataDETS
      if ("GSMATURITY" %in% names(tblList)) tblList$GSMATURITY <- tblList$GSMATURITY[which(tblList$GSMATURITY$CODE %in% tblList$dataDETS$FMAT),]
      if ("GSSEX" %in% names(tblList)) tblList$GSSEX <- tblList$GSSEX[which(tblList$GSSEX$CODE %in% tblList$dataDETS$FSEX),]
      if ("dataLF" %in% names(tblList)) tblList$dataLF <- tblList$dataLF[which(paste0(tblList$dataLF$MISSION,"_",tblList$dataLF$SETNO,"_",tblList$dataLF$SPEC,"_",tblList$dataLF$FSEX) %in% 
                                                                                 paste0(tblList$dataDETS$MISSION,"_",tblList$dataDETS$SETNO,"_",tblList$dataDETS$SPEC,"_",tblList$dataDETS$FSEX)) ,]
    } 
    
    if ("dataLF" %in% names(tblList)){
      #these limit dataLF
      if ("GSCAT" %in% names(tblList)) tblList$dataLF <- tblList$dataLF[which(paste0(tblList$dataLF$MISSION,"_",tblList$dataLF$SETNO,"_",tblList$dataLF$SPEC) %in% 
                                                                                paste0(tblList$GSCAT$MISSION,"_",tblList$GSCAT$SETNO,"_",tblList$GSCAT$SPEC)) ,]
      if (!keep_nullsets & "GSINF" %in% names(tblList)) tblList$dataLF <- tblList$dataLF[which(paste0(tblList$dataLF$MISSION,"_",tblList$dataLF$SETNO) %in% 
                                                                                                 paste0(tblList$GSINF$MISSION,"_",tblList$GSINF$SETNO)) ,]
      
      #these are limited by dataLF
      if ("GSMATURITY" %in% names(tblList)) tblList$GSMATURITY <- tblList$GSMATURITY[which(tblList$GSMATURITY$CODE %in% tblList$dataDETS$FMAT),]
      if ("GSSEX" %in% names(tblList)) tblList$GSSEX <- tblList$GSSEX[which(tblList$GSSEX$CODE %in% tblList$dataDETS$FSEX),]
      if ("dataDETS" %in% names(tblList)) tblList$dataDETS <- tblList$dataDETS[which(paste0(tblList$dataDETS$MISSION,"_",tblList$dataDETS$SETNO,"_",tblList$dataDETS$SPEC,"_",tblList$dataDETS$FSEX) %in% 
                                                                                       paste0(tblList$dataLF$MISSION,"_",tblList$dataLF$SETNO,"_",tblList$dataLF$SPEC,"_",tblList$dataLF$FSEX)) ,]
    } 
    
    if ("GSSPECIES" %in% names(tblList)){
      #these limit GSSPECIES
      if ("GSCAT" %in% names(tblList)) tblList$GSSPECIES <- tblList$GSSPECIES[which(tblList$GSSPECIES$CODE %in% tblList$GSCAT$SPEC),]
    }
    
    
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
