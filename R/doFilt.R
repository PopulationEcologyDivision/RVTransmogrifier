# doFilt <- function(tblList = NULL, args=NULL){
#   if(is.null(args))args<- list()
#   browser()
#   args <- setDefaultArgs(args=args)
#   if(!is.null(args$taxa)|!is.null(args$code)|!is.null(args$aphiaid)){
#     tblList      <- filterSpecies(tblList, args=args)
#     if (inherits(tblList,"numeric"))stop("Requested filter removed all species")
#     if(args$taxaAgg) tblList      <- aggregateByTaxa(tblList = tblList, args=args)
#   }
#   tblList <- propagateChanges(tblList, args=args)
#   if (is.numeric(tblList) & !args$quiet)message("Your query did not return valid results")
#   
#   return(tblList)
# }