#' @title stratify_prepare
#' @description This function prepares a df for every encountered taxa that will be used for stratification.
#' @param tblList the default is \code{NULL}. This is a list populated with all RV dataframes. Prior 
#' to running this function they should all have been filtered via \code{propagateChanges()}.
#' @param towDist the default is \code{1.75}. This is the default tow length for the survey.
#' @param ... other arguments passed to methods (i.e. 'debug' and 'quiet')
#' @returns ...
#' @author Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
stratify_prepare<-function(tblList = NULL,bySex=F, useBins = F, towDist = 1.75,...){
  browser()
  args <- list(...)
  debug <- ifelse(is.null(args$debug), F, args$debug) 
  quiet <- ifelse(is.null(args$quiet), F, args$quiet) 
  if("GSCAT_agg" %in% names(tblList)){
    dfRawCatch <- tblList$GSCAT_agg
    t_field <- "TAXA_"
  } else {
    dfRawCatch <- tblList$GSCAT
    t_field <- "SPEC"
  }
  
  ##### correct FSEX and FLEN as requested #####
  dataLFFixed <- tblList$dataLF[!is.na(tblList$dataLF$SPEC),]
  dataDETSFixed <- tblList$dataDETS[!is.na(tblList$dataDETS$SPEC),]
  if (!bySex){
    #if not by sex, overwrite FSEX to 9 everywhere 
    dataLFFixed$FSEX   <- 9
    dataDETSFixed$FSEX <- 9
  }else{
    #if present, berried females (3) are re-categorized as 2
    dataLFFixed[which(dataLFFixed$FSEX==3),"FSEX"] <- 2
    dataDETSFixed[which(dataDETSFixed$FSEX==3),"FSEX"] <- 2
  }
  
  if (!useBins) {
    #default FLEN binsize is 1, and overwritten if requested
    dataLFFixed$LGRP <- 1
    dataDETSFixed$LGRP <- 1
  }else{ 
    specLrp <- tblList$GSSPECIES[tblList$GSSPECIES$CODE %in% c(unique(dfRawCatch$SPEC)),c("CODE","LGRP")]
    dataLFFixed <- merge(dataLFFixed, specLrp, all.x =T, by.x= t_field, by.y = "CODE")
    dataDETSFixed <- merge(dataDETSFixed, specLrp, all.x =T, by.x= t_field, by.y = "CODE")
    dataLFFixed[is.na(dataLFFixed$LGRP),"LGRP"] <- 1
    dataDETSFixed[is.na(dataDETSFixed$LGRP),"LGRP"] <- 1
    dataLFFixed$FLEN <- binSizes(dataLFFixed$LGRP, dataLFFixed$FLEN)
    dataDETSFixed$FLEN <- binSizes(dataDETSFixed$LGRP, dataDETSFixed$FLEN)
  }
  
  tblList$GSINF[which(is.na(tblList$GSINF$DIST)|(tblList$GSINF$DIST==0)),"DIST"] <-towDist
  
  tblList$GSCAT_agg <- NULL
  tblList$GSCAT <- NULL
  tblList$GSCAT <- dfRawCatch
  tblList$dataLF <- NULL
  tblList$dataLF <- dataLFFixed
  tblList$dataDETS <- NULL
  tblList$dataDETS <- dataDETSFixed
 return(tblList)
}
