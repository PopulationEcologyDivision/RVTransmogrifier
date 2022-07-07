loadRVData<- function(){
  tbls <- RVSurveyData::listTbls()
  dataEnv<-new.env()
  data(package="RVSurveyData", list = tbls, envir = dataEnv)
  thedata <- as.list(dataEnv)
  return(thedata)
}
