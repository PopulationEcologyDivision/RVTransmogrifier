#' @title stratify
#' @description This function ...
#' @param tblList the default is \code{NULL}. This is a list populated with all RV dataframes. Prior 
#' to running this function they should all have been filtered via \code{propagateChanges()}.
#' @param ... other arguments passed to methods (i.e. 'towDist', 'debug' and 'quiet')
#' @returns ...
#' @author Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @importFrom dplyr %>%
#' @export
stratify <- function(tblList = NULL, bySex=F, ageBySex = F, towDist = 1.75, useBins = F,...){
  args <- list(...)
  if(!is.null(args$taxa)|!is.null(args$code)|!is.null(args$aphiaid)){
    tblList      <- filterSpecies(tblList = tblList, 
                                  code=args$code, 
                                  aphiaid=args$aphiaid, 
                                  taxa = args$taxa, 
                                  taxaAgg = args$taxaAgg, 
                                  keep_nullsets=args$keep_nullsets)
    if (inherits(tblList,"numeric"))stop("Requested filter removed all species")
    tblList      <- aggregateByTaxa(tblList = tblList,
                                    taxa = args$taxa,
                                    code = args$code,
                                    aphiaid = args$aphiaid)
  }
  
  #depending on the user's choices (e.g. by sex, binning, taxa), certain stuff needs to be done -
  #following function does it all, once, overwriting the tblList data with modified versions
  tblList  <- stratify_prepare(tblList = tblList, bySex = bySex, useBins=useBins, towDist=towDist, ...)
  dfNWSets <- stratify_makeNWSets(tblList = tblList)
  # t_field <- ifelse("GSCAT_agg" %in% names(tblList), "TAXA_", "SPEC")
  res <-list()
  for(s in 1:length(dfNWSets)){
    NW           <- stratify_NW(tblList = tblList, dfNWSets=dfNWSets[[s]])
    browser()
    theseLengths <-stratify_calcLengths(tblList = tblList, dfNWSets=dfNWSets[[s]], 
                                        bySex = bySex, useBins=useBins)
    ageLengthKey <-stratify_calcAgeKey(tblList = tblList,
                                       dfNWSets=dfNWSets[[s]],
                              lengthsTotals = theseLengths$length_total,
                              lset = theseLengths$lset,
                              ageBySex = ageBySex,
                              useBins=useBins)
    # lengthsData$agelen<-NULL
    # lengthsData$lset<-NULL
    
    
    nm<- NW[!is.na(NW[,t_field]),t_field][1]
    nm <- gsub(x = nm, pattern = "[ ()/,\\.]", replacement = "_")
    res[[paste0("sp_",nm)]]$NUM_WGT <- NW
    res[[paste0("sp_",nm)]]$LENGTHS <- theseLengths
    res[[paste0("sp_",nm)]]$AGES <- ageLengthKey
  }
  return(res)
}