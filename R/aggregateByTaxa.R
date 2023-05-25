### @title aggregateByTaxa
## @description This function .
## @param tblList the default is \code{NULL}. This is a list populated with all RV dataframes that
## should have filtering applied to them.
## @param code the default is \code{NULL}. If data should be limited to a particular species, enter 
## the species code here.
## @param aphiaid the default is \code{NULL}. If data should be limited to a particular aphiaid, 
## enter the aphiaid here.
## @param taxa the default is \code{NULL}. Any value found in any of "SCI_NAME", "KINGDOM", 
## "PHYLUM", "CLASS", "ORDER", "FAMILY", or "GENUS" can be specified (e.g. \code{taxa=c("GADIDAE")})

## @param ... other arguments passed to methods (e.g. 'debug' and 'quiet')
## @returns #' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
## @export
aggregateByTaxa <- function(tblList = NULL, ...){
  argsFn <- as.list(environment())
  argsFn[["tblList"]] <- NULL
  argsUser <- list(...)
  args <- do.call(set_defaults, list(argsFn=argsFn, argsUser=argsUser))
  if(args$debug){
    startTime <- Sys.time()
    thisFun <- where_now()
    message(thisFun, ": started")
  }
  

  
  if(args$debug) message(thisFun, ": completed (",round( difftime(Sys.time(),startTime,units = "secs"),0),"s)")
  return(tblList)
}