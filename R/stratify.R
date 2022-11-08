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
  args <- list(...)
  
  if(!is.null(args$taxa)|!is.null(args$code)|!is.null(args$aphiaid)){
    tblList      <- filterSpecies(tblList, keep_nullsets = T,
                                  taxa = args$taxa,
                                  code = args$code,
                                  aphiaid = args$aphiaid)
    if (class(tblList)=="numeric")stop("Requested filter removed all species")
    tblList      <- aggregateByTaxa(tblList = tblList,
                                    taxa = args$taxa,
                                    code = args$code,
                                    aphiaid = args$aphiaid)
  }
  
  dfNWSets  <- nwSets(tblList = tblList, ...)
  res <-list()
  for(s in 1:length(dfNWSets)){
    thisDat   <- nwStrat(tblList = tblList, dfNWSets=dfNWSets[[s]])
    nm<- thisDat[!is.na(thisDat$TAXA_),"TAXA_"][1]
    nm <- gsub(x = nm, pattern = "[ ()/,\\.]", replacement = "_")
    # res[[nm]]$NWSet   <- dfNWSets[[s]]
    res[[nm]] <- thisDat
  }
  return(res)
}