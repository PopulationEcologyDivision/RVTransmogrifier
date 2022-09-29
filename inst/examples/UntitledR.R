if (F){
  library(RVSurveyData)
  library(dplyr)
  library(ggplot2)
  sourcery()
  testFGP <- extractFGP("SUMMER", years = 2011)
  test<-getSurvey("SUMMER", years = 2011)
  plotRV(tblList = test, code=10, strataVar = "BIOMASS",online = F, showNullsets = T, plotPts = F)
  test<-getSurvey("SUMMER", years = 2010)
  plotRV(tblList = test, code=10, strataVar = "BIOMASS",online = F, showNullsets = T, plotPts = F)
  test<-getSurvey("SUMMER", years = 2015)
  plotRV(tblList = test, code=10, strataVar = "BIOMASS",online = F, showNullsets = T, plotPts = F)
  test<-getSurvey("SUMMER", years = 2020)
  plotRV(tblList = test, code=10, strataVar = "BIOMASS",online = F, showNullsets = T, plotPts = T)
  
  plotRV(tblList = test, FAMILY="GADIDAE", strataVar = "BIOMASS",
         multiSp = T, online = F, showNullsets = T, plotPts = T)
  # DecapodPlot <- plotRV(tblList = test, ORDER = "DECAPODA", online = F, showNullsets = F)
  # 
  # HaddockPlot <- plotRV(tblList = test, SPECIES = "MELANOGRAMMUS AEGLEFINUS", online = F, showNullsets = F)
  # 
  # SquidPlot <- plotRV(tblList = test, ORDER = "OEGOPSIDA", online = F, plotVar = "TOTWGT", showNullsets = F)
  # SquidPlot <- plotRV(tblList = test, ORDER = "OEGOPSIDA", online = F, plotVar = "TOTWGT", showNullsets = F)
  # 
  # test$GSSPECIES<- test$GSSPECIES[test$GSSPECIES$CODE==11,]
  
  
  # test <- propagateChanges(tblList = test, keep_nullsets = F, quiet=T)
  
  # plotRV(tblList = test, online = F, showNullsets = F)
  
  
  # dfNWSets <- NW_sets(tblList = test)
  # dfNWAgg <- NW_strat(tblList = test, dfNWSets=dfNWSets)
  # lengthsData <-calcLengths(tblList = test, dfNWSets=dfNWSets, bySex = F)
}