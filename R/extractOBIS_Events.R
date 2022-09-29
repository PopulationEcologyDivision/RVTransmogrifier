#' @title extractOBIS_Events
#' @description This function generates an OBIS-compliant "events" object for a given survey for the 
#' specified years.
#' @param survey the default is \code{NULL}. Valid values are "SPRING", "SUMMER", "FALL", and "4VSW". 
#' @param years the default is \code{NULL}. Valid values range from 1970-present.
#' @param ... other arguments passed to methods (i.e. 'debug' and 'quiet')
#' @returns a list containing dataframes for both the OBIS "eventcore" and the "eventemof".
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca} \cr Heidi van Vliet
#' \email{Heidi.vanVliet@@dfo-mpo.gc.ca}
#' @importFrom dplyr %>%
#' @export
extractOBIS_Events <- function(survey = NULL, years = NULL, ...){
  args <- list(...)
  debug <- ifelse(is.null(args$debug), F, args$debug) 
  quiet <- ifelse(is.null(args$quiet), F, args$quiet) 
  
  ts <-  format(Sys.time(), "%Y")
  thisEnv = new.env()
  results <- list()
  i=1
  # if (is.null(survey)){
  #   survey <- c("SPRING", "SUMMER", "FALL", "4VSW")
  # }else{
  #   survey <- toupper(survey)
  # }
  # 
  # for (i in 1:length(survey)){
  #   if (is.null(years)){
  #     fn <- paste0(survey[i],"_",ts)
  #   }else{
  #     fn <- paste0(years[1],"_",survey[i],"_",ts)
  #   }
  
  this <- getSurvey(survey=survey[i], years = years)
  if (is.numeric(this))stop("Your query did not return valid results")
  
  trawl <- this$GSINF
  trawl <- merge(trawl, this$GSGEAR, all.x = T)
  missions <- this$GSMISSIONS
  #trawl (event) information
  trawl <- trawl %>%
    dplyr::mutate(eventID = paste(MISSION, SETNO, sep=":"),
           time = stringr::str_pad(TIME, 4, pad = "0"),
           starttime = sub("(\\d{2})$", ":\\1", time),
           eventDate1 = paste(SDATE,"T",starttime,sep=""),
           dst = lubridate::dst(lubridate::ymd_hm(eventDate1, tz = "America/Halifax")),
           tz = ifelse(dst == "TRUE", "-0300", "-0400"),
           eventDate = paste(eventDate1,":00",tz,sep=""),
           #need to adjust this for years without end lat/long and #of decimal places
           footprintWKT = ifelse(ELAT_DD > 0, paste("POLYLINE((",
                                                    SLONG_DD," ",SLAT_DD,", ",
                                                    ELONG_DD," ",ELAT_DD,"))",
                                                    sep=""), ""),
           locationRemarks = paste("Stratum: ", STRAT, sep=""))
  
  #extract information for event core
  events <- trawl[c("MISSION",
                    "eventID",
                    "eventDate",
                    "SLAT_DD",
                    "SLONG_DD",
                    "footprintWKT",
                    "locationRemarks")]
  
  #change names to DwC format
  names(events) <-c("parentEventID",
                    "eventID",
                    "eventDate",
                    "decimalLatitude",
                    "decimalLongitude",
                    "footprintWKT",
                    "locationRemarks")
  
  #add column to match parent events
  events['eventRemarks'] = ''
  
  #missions
  missions <- missions %>%
    dplyr::mutate(eventRemarks = paste("Vessel: ",VESEL,
                                " | Cruise Number: ", CRUNO,
                                " | Season: ", SEASON, sep=""))
  #parentevents
  names(missions)[1] <- "eventID"
  names(missions)[4] <- "eventDate"
  
  parent <- missions[c("eventID", "eventDate", "eventRemarks")]
  
  #add same columns to parent event df as event df
  parent["parentEventID"] = ""
  parent["decimalLatitude"] = ""
  parent["decimalLongitude"] = ""
  parent["footprintWKT"] = ""
  parent["locationRemarks"] = ""
  
  #merge event and parent together 
  eventcore <- rbind(events, parent)
  
  
  #add additional information
  eventcore['rightsHolder'] = 'His Majesty the King in right of Canada, as represented by the Minister of Fisheries and Oceans'
  eventcore['geodeticDatum'] = 'WGS 84'
  eventcore['license'] = 'https://creativecommons.org/licenses/by/4.0/legalcode'
  eventcore['institutionID'] = 'https://edmo.seadatanet.org/report/1811'
  eventcore['institutionCode'] = 'Bedford Institute of Oceanography (BIO)'
  eventcore['datasetName'] = paste0('MARITIMES ', survey[i],' RESEARCH VESSEL SURVEY')
  
  results$eventcore <- eventcore
  
  emof <- trawl[c("eventID",
                  "DEPTH_M",
                  "DUR",
                  "DIST",
                  "SPEED",
                  "GEARDESC",
                  "SURFACE_TEMPERATURE",
                  "BOTTOM_TEMPERATURE",
                  "BOTTOM_SALINITY")]
  
  names(emof) <- c("eventID",
                   "average depth of tow",
                   "duration of tow",
                   "trawling_distance",
                   "Average tow speed",
                   "sampling protocol",
                   "sea_surface_temperature",
                   "Sea_Bottom_Temperature",
                   "salinity_at_bottom")
  
  emof1 <- reshape2::melt(emof, id.var=c("eventID"))
  names(emof1) <- c("eventID", "measurementType", "measurementValue")
  
  #remove rows that have missing values
  emof1 <- emof1 %>% tidyr::drop_na(measurementValue)
  
  #make a reference table for measurement units
  measurementType <- c("average depth of tow", 
                       "duration of tow",
                       "trawling_distance",
                       "Average tow speed",
                       "sampling protocol",
                       "sea_surface_temperature",
                       "Sea_Bottom_Temperature",
                       "salinity_at_bottom")
  measurementTypeID <- c("http://vocab.nerc.ac.uk/collection/S06/current/S0600167/",
                         "http://vocab.nerc.ac.uk/collection/P01/current/AZDRZZ01/",
                         "http://vocab.nerc.ac.uk/collection/P01/current/LENTRACK/",
                         "http://vocab.nerc.ac.uk/collection/P01/current/TOWSPEED/",
                         "http://vocab.nerc.ac.uk/collection/P01/current/SAMPPROT/",
                         "http://vocab.nerc.ac.uk/collection/P07/current/CFSN0381/",
                         "http://vocab.nerc.ac.uk/collection/P07/current/CFSN0335/2/",
                         "http://vocab.nerc.ac.uk/collection/P01/current/ODSDM021/")
  measurementUnit <- c("meters",
                       "minutes",
                       "nautical miles",
                       "knots",
                       "",
                       "celsius",
                       "celsius",
                       "pounds per square unit")
  measurementUnitID <- c("http://vocab.nerc.ac.uk/collection/P06/current/ULAA/",
                         "http://vocab.nerc.ac.uk/collection/P06/current/UMIN/",
                         "",
                         "http://vocab.nerc.ac.uk/collection/P06/current/UKNT/",
                         "",
                         "http://vocab.nerc.ac.uk/collection/P06/current/UPAA/",
                         "http://vocab.nerc.ac.uk/collection/P06/current/UPAA/",
                         "")
  measurementRemarks <- c("",
                          "",
                          "actual tow distance",
                          "The average speed of the vessel over bottom based on GPS, to the nearest tenth of a nautical mile.",
                          "Fishing gear used",
                          "",
                          "",
                          "")
  emof_reference <- data.frame(measurementType, measurementTypeID,
                               measurementUnit, measurementUnitID, 
                               measurementRemarks)
  
  #merge emof with references
  emof2 <- merge(emof1,
                 emof_reference,
                 "measurementType",
                 all.x = TRUE)
  
  #rearrange columns
  eventemof <- emof2[c(2,1,4,3,5:7)]

  results$eventemof <-  eventemof 
  
  return(results)
}