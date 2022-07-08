#' @title loadRVData
#' @description This function loads all of the tables from the RVSurveyData package into a single 
#' list.
#' @param ... other arguments passed to methods (i.e. 'debug' and 'quiet')
#' @returns a list of the ~18 data frames from the RVSurveyData package
#' @author Mike McMahon
#' @export
loadRVData<- function(...){
  args <- list(...)
  debug <- ifelse(is.null(args$debug), F, args$debug) 
  quiet <- ifelse(is.null(args$quiet), F, args$quiet) 
  
  tbls <- data(package = "RVSurveyData")$results[,"Item"]
  dataEnv<-new.env()
  utils::data(package="RVSurveyData", list = tbls, envir = dataEnv)
  thedata <- as.list(dataEnv)
  return(thedata)
}
