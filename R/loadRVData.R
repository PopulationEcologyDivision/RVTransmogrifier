#' @title loadRVData
#' @description This function loads all of the tables from the RVSurveyData package into a single 
#' list.
#' @returns a list of the ~18 data frames from the RVSurveyData package
#' @author Mike McMahon
#' @export
loadRVData<- function(){
  tbls <- RVSurveyData::listTbls()
  dataEnv<-new.env()
  utils::data(package="RVSurveyData", list = tbls, envir = dataEnv)
  thedata <- as.list(dataEnv)
  return(thedata)
}
