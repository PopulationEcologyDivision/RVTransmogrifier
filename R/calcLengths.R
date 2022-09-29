#' @title calcLengths
#' @description This function ...
#' @param tblList the default is \code{NULL}. This is a list populated with all RV dataframes. Prior 
#' to running this function they should all have been filtered via \code{propagateChanges()}
#' @param dfNWSets this is the output from \code{NW_sets()}.
#' @param towDist the default is \code{1.75}. This is ...
#' @param bySex the default is \code{F}. This is ...
#' @param ... other arguments passed to methods (i.e. 'debug' and 'quiet')
#' @returns ...
#' @author Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
calcLengths<-function(tblList = NULL, dfNWSets = NULL, towDist = 1.75, bySex=F, ...){
  args <- list(...)
  debug <- ifelse(is.null(args$debug), F, args$debug) 
  quiet <- ifelse(is.null(args$quiet), F, args$quiet) 
  
  agelen <-dfNWSets
  agelen <- merge(agelen, tblList$GSSPECIES[,c("CODE", "LGRP", "LFSEXED")], all.x=T,by.x= "SPEC", by.y="CODE")
  agelen <- merge(agelen, tblList$dataLF, by= c("MISSION","SETNO", "SPEC"))
  agelen <- merge(agelen, tblList$GSINF[,c("MISSION","SETNO", "DIST")], by= c("MISSION","SETNO"), all.x=T)
  agelen$FLEN<-floor(agelen$FLEN/agelen$LGRP )*agelen$LGRP  
  agelen$CAGE<-0
  agelen$CAGE<-agelen$CLEN
  
  # agelen$CAGE <- agelen$RAW_TOTWGT/agelen$TOTWGT*(towDist/agelen$DIST)*agelen$CLEN
  agelen$FLEN<-agelen$FLEN+(agelen$LGRP*.5)-.5
  
  if (bySex) {
    allfields <-c("STRAT", "MISSION", "SETNO", "SPEC", "LGRP", "FSEX", "FLEN","CAGE") # "SPEC",
  }else{
    allfields <-c("STRAT", "MISSION", "SETNO", "SPEC", "LGRP", "FLEN", "CAGE")         # "SPEC",
  }
  
  lset=agelen[,allfields]
  # if (F){
  #   lset <- lset %>%
  #     dplyr::group_by(SPEC, CAGE) %>%
  #     dplyr::summarise(ST_ERR_WGT = Mar.utils::st_err(TOTWGT),
  #               ST_ERR_NO = Mar.utils::st_err(TOTNO),
  #               ST_ERR_BIOMASS = Mar.utils::st_err(BIOMASS),
  #               ST_ERR_ABUND = Mar.utils::st_err(ABUND))%>%
  #     as.data.frame()
  # }
  
  # message("multispecies")
  
  lset <- stats::aggregate(lset[,allfields]$CAGE,
                           lset[allfields !="CAGE"],
                           FUN=sum)
  lset <- lset[order(lset$STRAT,lset$MISSION,lset$SETNO),]
  #get lset data with all possible length groups for all sexes
  #if want to enable multispecies - maybe need a loop here...
  rng = range(lset[lset$x>0,"FLEN"], na.rm = T)
  allLength = seq(min(rng),max(rng), by=lset$LGRP[1])
  if (bySex){
    allSex = unique(lset$FSEX)
    fakeRows = expand.grid(FSEX = allSex, FLEN = allLength)
    fakeRows=fakeRows[,c("FSEX","FLEN")]
  }else{
    fakeRows = data.frame(FLEN = allLength)
  }
  fakeRows$STRAT <- "FAKE"
  fakeRows$MISSION <- "FAKE"
  fakeRows$SETNO <- -1
  fakeRows$x <- -1
  fakeRows$SPEC <- -1
  lset$LGRP <- NULL
  
  lset<-rbind(lset,fakeRows)
  if (bySex) {
    length_by_set <- reshape2::dcast(lset, STRAT + MISSION + SETNO + SPEC ~ 
                                       FSEX + FLEN, value.var = "x"   )
  }else{
    length_by_set <- reshape2::dcast(lset, STRAT + MISSION + SETNO + SPEC ~ 
                                       FLEN, value.var = "x"  )
  }
  #remove the evidence of the fakeRows
  length_by_set <- length_by_set[length_by_set$STRAT!="FAKE",]
  length_by_set$YEAR <- as.numeric(substr(length_by_set$MISSION,4,7))
  #ensure all strata and sets still present so their zeroes get included
  length_by_set=merge(dfNWSets[,c("MISSION", "STRAT", "SETNO")],length_by_set, 
                      all.x=T)
  length_by_set=length_by_set[order(length_by_set$YEAR,
                                    length_by_set$STRAT,
                                    length_by_set$MISSION,
                                    length_by_set$SETNO),]
  tblList$GSSTRATUM <- addTUNITS(tblList$GSSTRATUM)
  length_by_set <-  merge(length_by_set, tblList$GSSTRATUM[,c("STRAT","TUNITS")], all.x = T)
  idFlds<- c("YEAR","STRAT","MISSION","SETNO","SPEC","TUNITS")
  datFlds <- as.character(sort(as.numeric(names(length_by_set[!names(length_by_set) %in% idFlds]))))
  #Reorder the columns so that they go by sex, then every possible length
  length_by_set_id <- length_by_set[idFlds]
  length_by_set_dat <- length_by_set[datFlds]
  
  if (bySex) {
    browser()
    length_by_set_dat = length_by_set_dat[order(
      as.numeric(substr(colnames(length_by_set_dat),1,1)),
      as.numeric(substr(colnames(length_by_set_dat),3,5)))]
  }
  
  length_by_set_dat[is.na(length_by_set_dat)]<-0
  # length_by_set_dat = cbind(length_by_set_dat, TOTAL=rowSums(length_by_set_dat))
  length_by_set <- cbind(length_by_set_id,length_by_set_dat)
  length_by_set_totals <- length_by_set
  
  length_by_set_totals[datFlds]<-length_by_set_totals[datFlds]*length_by_set_totals$TUNITS
  length_by_set_totals$TUNITS <- length_by_set$TUNITS <- NULL
  
  #Future Mike - read this!
  #length_by_set are uncorrected values
  #length_by_set_totals have had tunits applied
  # need to add a "by year" aggregation.
  
  # length_by_yr_mean<-stats::setNames(stats::aggregate(list(
  #   MEAN = length_by_yr_total[!colnames(length_by_yr_total) %in% c("STRAT","MISSION","SETNO","YEAR", "SPEC","TUNITS")]), 
  #   by=list(YEAR=length_by_yr_total$YEAR, SPEC=length_by_yr_total$SPEC), 
  #   FUN=mean), c("YEAR","SPEC",colnames(length_by_set_dat)))
  # length_by_yr_mean[is.na(length_by_yr_mean)]<-0
  # length_by_yr_total <-  merge(length_by_yr_mean, tblList$GSSTRATUM[,c("STRAT","TUNITS")])
  # length_by_yr_total <- cbind(length_by_yr_total[1], length_by_yr_total[2:(ncol(length_by_yr_total)-1)]*length_by_yr_total$TUNITS) 
  # length_total
  # length_by_yr_mean<-stats::setNames(stats::aggregate(list(
  #   MEAN = length_by_set[!colnames(length_by_set) %in% idFlds]), 
  #   by=list(STRAT=length_by_set$YEAR), 
  #   FUN=mean), c("YEAR",colnames(length_by_set_dat)))
  # length_by_yr_mean[is.na(length_by_yr_mean)]<-0
  
  length_by_strat_mean<-stats::setNames(stats::aggregate(list(
    MEAN = length_by_set[!colnames(length_by_set) %in% idFlds]), 
    by=list(STRAT=length_by_set$STRAT), 
    FUN=mean), c("STRAT",colnames(length_by_set_dat)))
  length_by_strat_mean[is.na(length_by_strat_mean)]<-0
  
  length_by_strat_se<-stats::setNames(stats::aggregate(list(
    MEAN_SE=length_by_set[!colnames(length_by_set) %in% c("STRAT","MISSION","SETNO")]), 
    by=list(STRAT=length_by_set$STRAT), 
    FUN=Mar.utils::st_err), c("STRAT",colnames(length_by_set_dat)))
  length_by_strat_se[is.na(length_by_strat_se)]<-0
  
  
  
  length_total <-  merge(length_by_strat_mean, tblList$GSSTRATUM[,c("STRAT","TUNITS")])
  length_total <- cbind(length_total[1],
                        length_total[2:(ncol(length_total)-1)]*length_total$TUNITS)    
  
  length_total_se =  merge(length_by_strat_se, tblList$GSSTRATUM[,c("STRAT","TUNITS")])
  length_total_se = cbind(length_total_se[1],
                          length_total_se[2:(ncol(length_total_se)-1)]*length_total_se$TUNITS)
  results=list(agelen = agelen,
               length_by_set= length_by_set,
               length_by_strat_mean = length_by_strat_mean,
               length_by_strat_se = length_by_strat_se,
               length_total = length_total,
               length_total_se = length_total_se,
               lset = lset)
  
  return(results)
}