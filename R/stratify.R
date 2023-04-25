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
  if(!is.null(args$taxa)|!is.null(args$code)|!is.null(args$aphiaid)){
    tblList      <- filterSpecies(tblList = tblList, ...)
    if (inherits(tblList,"numeric"))stop("Requested filter removed all species")
    # tblList      <- aggregateByTaxa(tblList = tblList, ...)
  }
  #depending on the user's choices (e.g. by sex, binning, taxa), certain stuff needs to be done -
  #following function does it all, once, overwriting the tblList data with modified versions
  tblList  <- stratify_prepare(tblList = tblList, ...)
  # dfNWSets <- stratify_makeNWSets(tblList = tblList)

  taxa<- unique(tblList$GSCAT[!is.na(tblList$GSCAT$TAXA_), "TAXA_"])
  stratInfo <- tblList$GSSTRATUM
  stratInfo <- addTUNITS(stratInfo)
  colnames(stratInfo)[colnames(stratInfo)=="AREA"] <- "SQNM"

  taxaSets <-list()
  for(t in 1:length(taxa)){
    thisSpecData <- tblList$GSCAT[tblList$GSCAT$TAXA_ == taxa[t], ]
    thisTaxSets <- merge(tblList$GSINF, thisSpecData, all.x=T)
    thisTaxSets[which(is.na(thisTaxSets$TAXA_)),"TAXA_"]<- taxa[t]
    thisTaxSets[which(is.na(thisTaxSets$TAXARANK_)),"TAXARANK_"] <- tblList$GSSPECIES[which(tblList$GSSPECIES$TAXA_==taxa[t]),"TAXARANK_"]
    thisTaxSets <- merge(thisTaxSets, tblList$GSSPECIES[,c("TAXA_", "LGRP")], all.x=T)
    thisTaxSets[which(is.na(thisTaxSets$TOTWGT)),c('TOTWGT','TOTNO')]<- 0
    thisTaxSets[which(is.na(thisTaxSets$TOTWGT_RAW)),c('TOTWGT_RAW')]<- 0
    thisTaxSets[which(is.na(thisTaxSets$TOTNO_RAW)),c('TOTNO_RAW')]<- 0
    thisTaxSets$TOTWGT[which(!is.finite(thisTaxSets$TOTWGT))] <-1
    thisTaxSets$TOTNO[which(!is.finite(thisTaxSets$TOTNO))] <- 1
    thisTaxSets <- thisTaxSets[c('MISSION','SETNO','STRAT',setdiff(colnames(thisTaxSets), colnames(tblList$GSINF)))]
    names(thisTaxSets)[names(thisTaxSets) == "AREA"] <- "UNIT_AREA"
    thisTaxSets <- thisTaxSets[with(thisTaxSets,order(MISSION, SETNO)),]
    
    theseNW           <- stratify_NW(tblList = tblList, dfNWSets = thisTaxSets, stratInfo = stratInfo)
    theseLengths <-stratify_calcLengths(tblList = tblList, dfNWSets=thisTaxSets, stratInfo = stratInfo, ...)
    theseALK <-stratify_alk(tblList = tblList, dfNWSets=thisTaxSets, stratInfo = stratInfo,
                            stratLengths = theseLengths$length_by_strat_total,...)
    # theseAges <-stratify_calcAges(tblList = tblList, dfNWSets=thisTaxSets, stratInfo = stratInfo, ...)

    taxaSets[[paste0(taxa[t])]]$NUMS_WEIGHTS <- theseNW
    taxaSets[[paste0(taxa[t])]]$LENGTHS <- theseLengths
    taxaSets[[paste0(taxa[t])]]$AGE <- theseALK
  }
  
  if(args$debug) message(thisFun, ": completed (",round( difftime(Sys.time(),startTime,units = "secs"),0),"s)")
  return(taxaSets)
}
  
  if(F){
  res <-list()
  for(s in 1:length(dfNWSets)){
    NW           <- stratify_NW(tblList = tblList, dfNWSets=dfNWSets[[s]])
    theseLengths <-stratify_calcLengths(tblList = tblList, dfNWSets=dfNWSets[[s]], ...)
    # ageLengthKey <-stratify_calcAgeKey(tblList = tblList,
    #                                    dfNWSets=dfNWSets[[s]],
    #                           lengthsTotals = theseLengths$length_total,
    #                           lset = theseLengths$lset,...)
    # # lengthsData$agelen<-NULL
    # # lengthsData$lset<-NULL
    # 
    # 
    
    nm<- NW[!is.na(NW[,t_field]),t_field][1]
    nm <- gsub(x = nm, pattern = "[ ()/,\\.]", replacement = "_")
    res[[paste0("sp_",nm)]]$NUM_WGT <- NW
    # res[[paste0("sp_",nm)]]$LENGTHS <- theseLengths
    # res[[paste0("sp_",nm)]]$AGES <- ageLengthKey
  }
  
  
  if(args$debug) message(thisFun, ": completed (",round( difftime(Sys.time(),startTime,units = "secs"),0),"s)")
  return(res)
  }
