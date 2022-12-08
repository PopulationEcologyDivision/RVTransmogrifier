#' @title filterSpecies
#' @description This function facilitates filtering by species, and can filter by "code", "aphiaid", 
#' or "taxa". Only one of these filter types may be used at a time.  "code" is first priority, and 
#' if multiple filter types are sent, only "code" will be applied.  If "code" is not sent, "aphiaid"
#' will take priority over "taxa".
#' @param tblList the default is \code{NULL}. This is a list populated with all RV dataframes that
#' should have filtering applied to them.
#' @param code the default is \code{NULL}. If data should be limited to a particular species, enter 
#' the species code here
#' @param aphiaid the default is \code{NULL}. If data should be limited to a particular aphiaid, 
#' enter the aphiaid here.
#' @param taxa the default is \code{NULL}. Any value found in any of "SCIENTIFICNAME", "KINGDOM", 
#' "PHYLUM", "CLASS", "ORDER", "FAMILY", or "GENUS" can be specified (e.g. \code{taxa=c("GADIDAE")})
#' @param keep_nullsets the default is \code{TRUE}.
#' @returns a list of filtered versions of the dataframes passed as \code{tblList}. If the
#' filtering fails, a value of -1 will be returned. For example, if data is filtered for a year
#' where data was not collected, a strata that doesn't exist, or a species that was not observed
#' would all result in a value of -1 being returned.
#' @author Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
#' @param ... other arguments passed to methods (i.e. 'debug' and 'quiet')
filterSpecies <- function(tblList = NULL, code=NULL, aphiaid=NULL, taxa = NULL, keep_nullsets=T,...){
  args <- list(...)
  debug <- ifelse(is.null(args$debug), F, args$debug) 
  quiet <- ifelse(is.null(args$quiet), F, args$quiet) 
  #create thing to hold the final requested species - i.e. all taxa that match filter
  req_Spp <- tblList$GSSPECIES[FALSE,]
  req_Spp$TAXA_ <- character()
  req_Spp$TAXARANK_ <- character()
  req_Spp_Code <- req_Spp[FALSE,]
  req_Spp_AphiaID <- req_Spp[FALSE,]
  req_Spp_Taxa <- req_Spp[FALSE,]
  
  if (!is.null(code))    {
    if (!is.null(aphiaid) | !is.null(taxa)){
      message("Only one filter type can be done at once - only 'code' will be used")
    }
    for (c in 1:length(code)){
      these <- tblList$GSSPECIES[which(tblList$GSSPECIES$CODE %in% code[c]),]
      if (nrow(these)==0){
        message("No species with a species code of ",code[c]," were found")
        next
      }
      these$TAXA_    <- NA
      these$TAXARANK_<- NA
      req_Spp_Code <- rbind.data.frame(req_Spp_Code, these)
    }
    
    req_Spp<-req_Spp_Code
  } else if (!is.null(aphiaid)) {
    if (!is.null(taxa)){
      message("Only one filter type can be done at once - only 'aphiaid' will be used")
    }
    for (a in 1:length(aphiaid)){
      these <- tblList$GSSPECIES[which(tblList$GSSPECIES$APHIAID %in% aphiaid[a]),]
      if (nrow(these)==0){
        message("No species with an aphiaid of ",aphiaid[a]," were found")
        next
      }
      these$TAXA_    <- NA
      these$TAXARANK_<- NA
      req_Spp_AphiaID <- rbind.data.frame(req_Spp_AphiaID, these)
    }
    req_Spp<-req_Spp_AphiaID
  } else if (!is.null(taxa))    {
    #Taxa filter must capture all spec within the taxa, as well as the rank of the taxa (e.g. "FAMILY")
    for (t in 1:length(taxa)){
      these <- tblList$GSSPECIES[which(apply(tblList$GSSPECIES[,c("SCIENTIFICNAME","KINGDOM","PHYLUM","CLASS","ORDER","FAMILY","GENUS")], 1, function(r) any(r %in% taxa[t]))), ]
      if (nrow(these)==0){
        message("No species with a taxa of ",taxa[t]," were found")
        next
      }
      these$TAXA_ <- taxa[t]
      rankCheck <- which(apply(these[,!names(these) %in% c("COMMENTS","TAXA_")], 2, function(b) any(grepl(taxa[t], b))))
      rankCheck <- utils::stack(rankCheck)
      rankCheck$ind <- as.character(rankCheck$ind)
      if (nrow(rankCheck)==1){
        these$TAXARANK_ <- rankCheck$ind
      }else {
        rankCheck <- rankCheck[which.max(rankCheck$values),]
        if (nrow(rankCheck)>1){
          stop("Can't differentiate between multiple taxomomic levels with the same number of records (",paste(rankCheck$ind, collapse=","),")")
        }
        #message('Multiple potential matches for "',taxa[t],'" - defaulting to the usage with the most records ("',rankCheck$ind,'")')
        these$TAXARANK_  <- rankCheck$ind
      }
      req_Spp_Taxa <- rbind.data.frame(req_Spp_Taxa, these)
    }
    req_Spp<-req_Spp_Taxa
  }
  if (nrow(req_Spp) > 0 & nrow(req_Spp) < nrow(tblList$GSSPECIES)){
    tblList$GSSPECIES <- req_Spp
  } else if (nrow(req_Spp)==0){
    message("Species filter resulted in zero species remaining - cancelling")
    return(-1)
  }else if (nrow(req_Spp) == nrow(tblList$GSSPECIES)){
    message("Species filter would not remove any species - cancelling")
    return(tblList)
  }
  

  if (!is.null(taxa) && nrow(req_Spp_Taxa)>0){
    tblList <- aggregateByTaxa(tblList=tblList)
    tblList$dataDETS <- merge(tblList$dataDETS, tblList$GSSPECIES[,c("CODE","TAXA_", "TAXARANK_")], by.x="SPEC", by.y="CODE")
  }
  tblList <- propagateChanges(tblList, quiet=T, keep_nullsets = keep_nullsets)
  if (inherits(tblList,"numeric")){
    stop("Filter resulted in 0 records.")
  }
  return(tblList)
}