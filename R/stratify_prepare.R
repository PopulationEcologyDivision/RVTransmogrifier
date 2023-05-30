## @title stratify_prepare
## @description This function prepares a df for every encountered taxa that will be used for stratification.
## @param tblList the default is \code{NULL}. This is a list populated with all RV dataframes. Prior 
## to running this function they should all have been filtered via \code{propagateChanges()}.
## @param towDist the default is \code{1.75}. This is the default tow length for the survey.
## @param ... other arguments passed to methods (i.e. 'debug' and 'quiet')
## @returns ...
## @author Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
## @export
stratify_prepare<-function(tblList = NULL, ...){
  argsFn <- as.list(environment())
  argsFn[["tblList"]] <- NULL
  argsUser <- list(...)
  args <- do.call(set_defaults, list(argsFn=argsFn, argsUser=argsUser))
  if(args$debug){
    startTime <- Sys.time()
    thisFun <- where_now()
    message(thisFun, ": started")
  }
  
  dataLFFixed <- tblList$dataLF[!is.na(tblList$dataLF$TAXA_),]
  dataDETSFixed <- tblList$dataDETS[!is.na(tblList$dataDETS$TAXA_),]
  
  #if present, berried females (3) are re-categorized as 2
  dataLFFixed[which(dataLFFixed$FSEX==3),"FSEX"] <- 2
  dataDETSFixed[which(dataDETSFixed$FSEX==3),"FSEX"] <- 2

  ##### correct FLEN #####
  if (!args$useBins) {
    dataLFFixed$LGRP <- 1
    dataDETSFixed$LGRP <- 1
  }else{ 
    specLrp <- tblList$GSSPECIES[tblList$GSSPECIES$TAXA_ %in% c(unique(tblList$GSCAT$TAXA_)),c("TAXA_","LGRP")]
    dataLFFixed <- merge(dataLFFixed, specLrp, all.x =T, by.x= "TAXA_", by.y = "TAXA_")
    dataDETSFixed <- merge(dataDETSFixed, specLrp, all.x =T, by.x= "TAXA_", by.y = "TAXA_")
    dataLFFixed[is.na(dataLFFixed$LGRP),"LGRP"] <- 1
    dataDETSFixed[is.na(dataDETSFixed$LGRP),"LGRP"] <- 1
    dataLFFixed$FLEN <- binSizes(dataLFFixed$LGRP, dataLFFixed$FLEN)
    dataDETSFixed$FLEN <- binSizes(dataDETSFixed$LGRP, dataDETSFixed$FLEN)
  }
  
  tblList$GSINF[which(is.na(tblList$GSINF$DIST)|(tblList$GSINF$DIST==0)),"DIST"] <-args$towDist
  dataDETSFixed <- merge(dataDETSFixed, tblList$GSINF[,c("STRAT", "SETNO")], all.x = T)
  dataLFFixed   <- merge(dataLFFixed, tblList$GSINF[,c("STRAT", "SETNO")], all.x = T)
  
  tblList$GSSTRATUM <- addTUNITS(tblList$GSSTRATUM)
  
  # since we may have rewritten FLEN values, we should agg dataLF, or we'll have 
  # duplicate rows; no need to do dataDETS, since each record has an identifier
  tblList$dataLF <- dataLFFixed %>%
    dplyr::group_by(dplyr::across(c(-CLEN))) %>%
    dplyr::summarise(CLEN=sum(CLEN),
                     .groups = "keep")%>% 
    as.data.frame()
  
  tblList$dataDETS <- dataDETSFixed
  if(args$debug) message(thisFun, ": completed (",round( difftime(Sys.time(),startTime,units = "secs"),0),"s)")
  return(tblList)
}
