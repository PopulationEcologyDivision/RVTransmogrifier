#' @title extractOBIS_Occurrences
#' @description This function generates an OBIS-compliant "occurrences" object for a given survey 
#' for the specified years.
#' @param survey the default is \code{NULL}. Valid values are "SPRING", "SUMMER", "FALL", and "4VSW". 
#' @param years the default is \code{NULL}. Valid values range from 1970-present.
#' @param ... other arguments passed to methods (i.e. 'debug' and 'quiet')
#' @returns a dataframe
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca} \cr Heidi van Vliet
#' \email{Heidi.vanVliet@@dfo-mpo.gc.ca}
#' @importFrom dplyr %>%
#' @export
extractOBIS_Occurrences <- function(survey = NULL, years = NULL, ...){
  ts <-  format(Sys.time(), "%Y")
  thisEnv = new.env()
  results <- list()
  i=1
  # #set working directory
  # setwd("C:/Users/vanvlieth/Documents/R/RVTrawlData/4VSW")
  this <- getSurvey(survey=survey[i], years = years)
  if (is.numeric(this))stop("Your query did not return valid results")
  #load packages
  # library(readr)
  # library(dplyr)
  # library(tidyr)
  # library(stringr)
  # library(lubridate)
  # library(worrms)
  # library(reshape2)
  i=1
  #read csv's
  # DET <- read.csv("4VSW_2020_GSDET.csv", strip.white=TRUE)
  # CAT <- read.csv("4VSW_2020_GSCAT.csv", strip.white = TRUE)
  
  DET <- this$dataDETS
  CAT <- this$GSCAT
  
  #create new working df's for occurrences
  CAT <- CAT %>%
    dplyr::mutate(eventID = paste(MISSION,SETNO,sep =":")) %>%
    dplyr::mutate(occurrenceID = paste(MISSION, SETNO, SPEC, "total",sep = ":")) %>% 
    dplyr::mutate(occurrenceStatus = ifelse(TOTNO > 0, "present", "absent")) 
  
  names(CAT) <- c("parentEventID",
                  "SETNO",
                  "SPEC",
                  "total biomass",
                  "individualCount",
                  "eventID",
                  "occurrenceID",
                  "occurrenceStatus")
  CAT['recordNumber'] = ''
  
  DET <- DET %>%
    dplyr::mutate(eventID = paste(MISSION, SETNO,sep=":")) %>%
    dplyr::group_by(eventID, SPEC) %>% 
    dplyr::mutate(ID = dplyr::row_number()) %>% 
    dplyr::arrange(eventID) %>%
    dplyr::mutate(occurrenceID = paste(eventID,SPEC,ID, "subsample",sep=":"))
  
  #remove unknowns and inorganics 
  unknown <- c(1091:1095, 3999,9200,9400, 9620:9999)
  DET1 <- DET %>% dplyr::filter(!(SPEC %in% unknown))
  CAT1 <- CAT %>% dplyr::filter(!(SPEC %in% unknown))
  
  #view column names and numbers 
  # data.frame(colnames(DET1))
  DET1$FSHNO <- NULL
  
  DET1 <- DET1[,c("MISSION", "SETNO", "SPEC", "FLEN", "FWT", "FMAT", "FSEX", "AGE", "SPECIMEN_ID", "eventID", "ID", "occurrenceID")]
  names(DET1) <- c("parentEventID",
                   "SETNO",
                   "SPEC",
                   "length",
                   "weight",
                   "maturity",
                   "sex",
                   "age",
                   "recordNumber",
                   "eventID",
                   "ID",
                   "occurrenceID")
  DET1['occurrenceStatus'] = 'present'
  
  #make dataframes with only details need for occurrence core and bring together
  occurrence_CAT <- CAT1[c("eventID",
                           "occurrenceID",
                           "recordNumber",
                           "SPEC",
                           "occurrenceStatus")]
  occurrence_DET <- DET1[c("eventID",
                           "occurrenceID",
                           "recordNumber",
                           "SPEC",
                           "occurrenceStatus")]
  
  occurrences <- rbind(occurrence_CAT, occurrence_DET)
  occurrences['basisOfRecord'] = 'HumanObservation'
  

  species_taxa <- this$GSSPECIES[,c("CODE", "COMM", "APHIAID")]
  species_taxa_2<- this$GSSPECIES_TAX
  species_taxa <- merge(species_taxa, species_taxa_2)
  # species_taxa_DETS <- worrms::wm_classification_(species_taxa[,"APHIAID"])
  
  
  #clean up species in excel 
  #and do worms match online because file size too big 
  #and too many ambiguous match to do through R
  
  #merge worms match with occurrences file using species code
  # species_taxa <- read.csv("summerspecies_matched.csv", strip.white = TRUE) 

    #view column names and numbers 
  data.frame(colnames(species_taxa))
  
  #change column names 
  names(species_taxa) <- c("scientificName",
                           "vernacularName",
                           "SPEC",
                           "identificationQualifier",
                           "identificationRemarks",
                           "lifeStage",
                           "AphiaID",
                           "scientificNameID",
                           "scientificNameAuthorship",
                           "kingdom",
                           "phylum",
                           "class",
                           "order",
                           "family",
                           "genus",
                           "subgenus",
                           "specificEpithet",
                           "infraspecificEpithet")
  
  # robis::taxon(species_taxa[1:3,"APHIAID"])


  return(results)
}