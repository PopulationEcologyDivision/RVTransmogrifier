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
  SUMMER2020_allSp <- getSurvey("SUMMER", years = 2020)
  #simple plot call, filtering data to only cod (ie code = 10)
  plotRV(tblList = SUMMER2020_allSp, code=10, plotNullsets = T, plotSets ="TOTWGT", labelStrata = T)
  
  #Cod fun
  #Limit the data to just cod
  SUMMER2020_cod <- getSurvey("SUMMER", code = 10, years = 2020)
  #plot the cod - should be identical to the previous plot
  plotRV(tblList = SUMMER2020_cod, plotNullsets = T, plotSets ="TOTWGT", labelStrata = T)
  
  
  #generate stratified dataset of cod
  strat2020_cod  <- stratify(SUMMER2020, code = 10)

  #instead of set locations, plot the stratified BIOMASS of cod for this survey
  plotRV(tblList = SUMMER2020, code=10, plotNullsets = T, plotSets =NULL, 
         catchStrataData = strat2020_cod$`10`, plotCatchStrata = "BIOMASS_T")
  
  strat2020_GADIFORMES  <- stratify(SUMMER2020, taxa = "GADIFORMES")
  plotRV(tblList = SUMMER2020, plotNullsets = T, plotSets =NULL, 
         catchStrataData = strat2020_GADIFORMES$GADIFORMES, plotCatchStrata = "BIOMASS_T")
  
  #simple plot of all Gadiformes - includes:
  # - ARCTIC COD, COD(ATLANTIC), CUSK, FOURBEARD ROCKLING, HADDOCK, LONGFIN HAKE, 
  # - MARLIN-SPIKE GRENADIER, OFF-SHORE HAKE, POLLOCK, SILVER HAKE, SQUIRREL OR RED HAKE, 
  # - THREEBEARD ROCKLING, WHITE HAKE and NEZUMIA
  plotRV(tblList = SUMMER2020, taxa = "GADIFORMES", plotNullsets = T, plotSets ="TOTWGT")
  #simple plot of all Gadiformes
  plotRV(tblList = SUMMER2020, taxa = "ARTHROPODA", plotNullsets = T, plotSets ="TOTWGT")
  plotRV(tblList = SUMMER2020, taxa = "ARTHROPODA", plotNullsets = T, plotSets ="TOTWGT")
  
  
                            #limit the data to only a few different taxa
  test1      <- filterSpecies(SUMMER2021, keep_nullsets = T, taxa = "ECHINODERMATA")
  
  #plot some data
  plotRV(tblList = SUMMER2022, taxa=c("ECHINODERMATA"), plotNullsets = T, plotSets ="TOTNO")
  
  #stratify the data (i.e. bump up values for short tows; bump down longer tows, etc)
  strat2021     <- stratify(SUMMER2021, code = "ECHINODERMATA")
  
  plotRV(tblList = SUMMER2022, taxa=c("ECHINODERMATA"), showNullsets = T, plotSets ="ALL", 
             plotBathy = "LINES", plotCatchStrata = "MEAN_WGT", catchStrataData = strat2021$ECHINODERMATA)
  
  plotRV(plotBathy = "LINES", plotCatchStrata = "MEAN_WGT", catchStrataData = strat2021$ECHINODERMATA)
  
  
  
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