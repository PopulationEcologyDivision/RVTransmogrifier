#' @title loadRVData
#' @description This function loads all of the tables from the RVSurveyData package into a single 
#' list.
#' @param ... other arguments passed to methods (i.e. 'debug' and 'quiet')
#' @returns a list of the ~18 data frames from the RVSurveyData package
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
loadRVData<- function(args=NULL){
  if(is.null(args))args<- list()
  # args <- setDefaultArgs(args=args)
  args <- set_defaults(args=args)
  tbls <- utils::data(package = "RVSurveyData")$results[,"Item"]
  dataEnv<-new.env()
  utils::data(package="RVSurveyData", list = tbls, envir = dataEnv)
  thedata <- as.list(dataEnv)
  return(invisible(thedata))
}
