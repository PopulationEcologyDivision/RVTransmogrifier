if (F){
  library(RVSurveyData)
  library(Mar.utils)
  
  library(dplyr)
  library(ggplot2)
  library(lubridate)
  library(mapdata)
  library(maps)
  library(tidyr)
  library(readr)
  library(reshape2)
  library(stats)
  library(stringr)
  library(sf)
  
  sourcery()
  
  #extract a survey's worth of data
  SUMMER2021 <- getSurvey("SUMMER", years = 2021)
  #limit the data to only a few different taxa
  test1      <- filterSpecies(SUMMER2021, keep_nullsets = T, taxa = "ECHINODERMATA")
  
  #plot some data
  plotRV_pts(tblList = SUMMER2022, taxa=c("ECHINODERMATA"), plotNullsets = T, plotSets ="TOTNO")
  
  #stratify the data (i.e. bump up values for short tows; bump down longer tows, etc)
  strat2021     <- stratify(SUMMER2021, taxa = "ECHINODERMATA")
  plotRV(tblList = SUMMER2022, taxa=c("ECHINODERMATA"), showNullsets = T, plotSets ="ALL", 
             plotBathy = "LINES", plotCatchStrata = "MEAN_WGT", catchStrataData = strat2021$ECHINODERMATA)
  
  
  testFGP <- extractFGP("SUMMER", years = 2011)
  plotRV(tblList = test, code=10, strataVar = "BIOMASS",showNullsets = T, plotPts = F)
  test<-getSurvey("SUMMER", years = 2010)
  plotRV(tblList = test, code=10, strataVar = "BIOMASS",showNullsets = T, plotPts = F)
  test<-getSurvey("SUMMER", years = 2015)
  plotRV(tblList = test, code=10, strataVar = "BIOMASS",showNullsets = T, plotPts = F)
  test<-getSurvey("SUMMER", years = 2020)
  plotRV(tblList = test, code=10, strataVar = "BIOMASS",showNullsets = T, plotPts = T)
  
  plotRV(tblList = test, FAMILY="GADIDAE", strataVar = "BIOMASS",
         multiSp = T, showNullsets = T, plotPts = T)
  # DecapodPlot <- plotRV(tblList = test, ORDER = "DECAPODA", showNullsets = F)
  # 
  # HaddockPlot <- plotRV(tblList = test, SPECIES = "MELANOGRAMMUS AEGLEFINUS", showNullsets = F)
  # 
  # SquidPlot <- plotRV(tblList = test, ORDER = "OEGOPSIDA", plotVar = "TOTWGT", showNullsets = F)
  # SquidPlot <- plotRV(tblList = test, ORDER = "OEGOPSIDA", plotVar = "TOTWGT", showNullsets = F)
  # 
  # test$GSSPECIES<- test$GSSPECIES[test$GSSPECIES$CODE==11,]
  
  
  # test <- propagateChanges(tblList = test, keep_nullsets = F, quiet=T)
  
  # plotRV(tblList = test, showNullsets = F)
  
  
  # dfNWSets <- NW_sets(tblList = test)
  # dfNWAgg <- NW_strat(tblList = test, dfNWSets=dfNWSets)
  # lengthsData <-calcLengths(tblList = test, dfNWSets=dfNWSets, bySex = F)
}