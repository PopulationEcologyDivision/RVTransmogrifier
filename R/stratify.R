#' @title stratify
#' @description This function ...
#' @param tblList the default is \code{NULL}. This is a list populated with all RV dataframes. Prior 
#' to running this function they should all have been filtered via \code{propagateChanges()}.
#' @param ... other arguments passed to methods (i.e. 'towDist', 'debug' and 'quiet')
#' @returns ...
#' @author Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @importFrom dplyr %>%
#' @export
stratify <- function(tblList = NULL, ...){
  argsFn <- as.list(environment())
  argsFn[["tblList"]] <- NULL
  argsUser <- list(...)
  args <- do.call(set_defaults, list(argsFn=argsFn, argsUser=argsUser))
  if(args$debug){
    startTime <- Sys.time()
    thisFun <- where_now()
    message(thisFun, ": started")
  }
  tblList  <- stratify_prepare(tblList = tblList, ...)
  taxa<- unique(tblList$GSCAT[!is.na(tblList$GSCAT$TAXA_), "TAXA_"])
  
  prepareSpData <- function(taxa= NULL){
    thisSpecData <- tblList$GSCAT[tblList$GSCAT$TAXA_ == taxa, ]
    thisTaxSets <- merge(tblList$GSINF, thisSpecData, all.x=T)
    thisTaxSets[which(is.na(thisTaxSets$TAXA_)),"TAXA_"]<- taxa
    thisTaxSets[which(is.na(thisTaxSets$TAXARANK_)),"TAXARANK_"] <- tblList$GSSPECIES[which(tblList$GSSPECIES$TAXA_==taxa),"TAXARANK_"]
    thisTaxSets <- merge(thisTaxSets, tblList$GSSPECIES[,c("TAXA_", "LGRP")], all.x=T)
    thisTaxSets[which(is.na(thisTaxSets$TOTWGT)),c('TOTWGT','TOTNO')]<- 0
    thisTaxSets[which(is.na(thisTaxSets$TOTWGT_RAW)),c('TOTWGT_RAW')]<- 0
    thisTaxSets[which(is.na(thisTaxSets$TOTNO_RAW)),c('TOTNO_RAW')]<- 0
    thisTaxSets$TOTWGT[which(!is.finite(thisTaxSets$TOTWGT))] <-1
    thisTaxSets$TOTNO[which(!is.finite(thisTaxSets$TOTNO))] <- 1
    thisTaxSets <- thisTaxSets[c('MISSION','SETNO','STRAT',setdiff(colnames(thisTaxSets), colnames(tblList$GSINF)))]
    thisTaxSets <- thisTaxSets[with(thisTaxSets,order(MISSION, SETNO)),]
    names(thisTaxSets)[names(thisTaxSets) == "AREA"] <- "UNIT_AREA"
    return(thisTaxSets)
  }
  
  taxaSets <-list()
  for(t in 1:length(taxa)){
    sexed <- ifelse(tblList$GSSPECIES[tblList$GSSPECIES$TAXA_ == taxa[t], "LFSEXED"]=="Y", TRUE, FALSE)
    if (sexed != args$bySex) message("You have selected `bySex=", args$bySex, "`, which is unusual for this species (",taxa[t],")") 
    thisTaxSets <- prepareSpData(taxa= taxa[t])
    theseNW           <- stratify_NW(tblList = tblList, dfNWSets = thisTaxSets, ...)
    theseLengths      <- stratify_calcLengths(tblList = tblList, dfNWSets=thisTaxSets, ...)
    theseAges         <- stratify_calcAges(tblList = tblList, dfNWSets=thisTaxSets, 
                                           stratLengths = theseLengths$length_by_strat_total, ...)
    taxaSets[[paste0(taxa[t])]]$NUMS_WEIGHTS <- theseNW
    taxaSets[[paste0(taxa[t])]]$LENGTHS <- theseLengths
    taxaSets[[paste0(taxa[t])]]$AGES <- theseAges
  }
  metadata <- stratify_Metadata(...)
  taxaSets$metaData <- metadata
  if(args$debug) message(thisFun, ": completed (",round( difftime(Sys.time(),startTime,units = "secs"),0),"s)")
  return(taxaSets)
}


