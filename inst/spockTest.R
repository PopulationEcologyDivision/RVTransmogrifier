library(RVSurveyData)
library(dplyr)
sourcery()
spring <- getSurvey(survey = "SPRING", years = c(2009:2021))
spring$GSSPECIES <- spring$GSSPECIES[spring$GSSPECIES$CODE==10,]
spring <- propagateChanges(spring)
dfNWSets <- NW_sets(tblList = spring)
dfNWAgg <- NW_strat(tblList = spring, dfNWSets=dfNWSets)
lengthsData <-calcLengths(tblList = spring, dfNWSets=dfNWSets, bySex = F)


library(RVSurveyData)
library(dplyr)
sourcery()
summer <- getSurvey(survey = "SUMMER", years = c(2019:2022))
summer$GSSPECIES <- summer$GSSPECIES[summer$GSSPECIES$CODE==4511,]
summer <- propagateChanges(summer)
