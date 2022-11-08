#' @title nwStrat
#' @description This function calculates the stratified values of numbers and weights by strata
#' @param tblList the default is \code{NULL}. This is a list populated with all RV dataframes. Prior 
#' to running this function they should all have been filtered via \code{propagateChanges()}.
#' @param dfNWSets  this is the output from \code{NW_sets()}.
#' @param ... other arguments passed to methods (i.e. 'debug' and 'quiet')
#' @returns ...
#' @author Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @importFrom dplyr %>%
#' @export
nwStrat<-function(tblList = NULL, dfNWSets= NULL, ...){
  args <- list(...)
  debug <- ifelse(is.null(args$debug), F, args$debug) 
  quiet <- ifelse(is.null(args$quiet), F, args$quiet)
  #get rid of records where n known taxa were caught

  if("TAXA_" %in% names(dfNWSets)){
    t_field <- "TAXA_"
  }else{
    t_field <- "SPEC"
  }
  tblList$GSSTRATUM <- addTUNITS(tblList$GSSTRATUM)
  tmp <- merge(dfNWSets,tblList$GSSTRATUM, by = 'STRAT')
  tmp$BIOMASS<-tmp$TOTWGT*tmp$TUNITS
  tmp$ABUND<-tmp$TOTNO*tmp$TUNITS
  #We want the submitted data by strat - so mission is irrelevant now
  tmp$MISSION <- NULL
  
  thisTaxa<- unique(tmp[!is.na(tmp[,t_field]),t_field])
  if (length(thisTaxa)>1)stop("This function can only handle on taxa at a time")
  #want to retain recs for strata where taxa was never even caught
  naTaxaStrats <- tmp[is.na(tmp[,t_field]),"STRAT"]
  taxaStrats <- tmp[!is.na(tmp[,t_field]),"STRAT"]
  emptyStrat <- setdiff(naTaxaStrats,taxaStrats)
  if (length(emptyStrat)>0){
    tmp <- tmp[!is.na(tmp[,t_field]) | (tmp$STRAT %in% emptyStrat),]
    tmp[is.na(tmp[,t_field]),t_field] <- thisTaxa
  }else{
    tmp <- tmp[!is.na(tmp[,t_field]),]
  }
  tmp.cnt <- tmp %>%
    dplyr::group_by(across(all_of(c("STRAT", t_field)))) %>%
    dplyr::summarise(COUNT = length(STRAT), 
                     .groups = "keep")%>%
    as.data.frame()
  tmp.sum <- tmp %>%
    dplyr::group_by(across(all_of(c("STRAT", t_field)))) %>%
    dplyr::summarise(TOT_WGT = round(sum(TOTWGT),5),
                     TOT_NO = round(sum(TOTNO),5), 
                     .groups = "keep")%>%
    as.data.frame()
  tmp.mean <- tmp %>%
    dplyr::group_by(across(all_of(c("STRAT", t_field)))) %>%
    dplyr::summarise(MEAN_WGT = round(mean(TOTWGT),5),
                     MEAN_NO = round(mean(TOTNO),5),
                     BIOMASS_T = round(mean(BIOMASS)/1000,5),
                     ABUND = round(mean(ABUND),5), 
                     .groups = "keep")%>%
    as.data.frame()
  tmp.sterr <- tmp %>%
    dplyr::group_by(across(all_of(c("STRAT", t_field)))) %>%
    dplyr::summarise(ST_ERR_WGT = round(Mar.utils::st_err(TOTWGT),5),
                     ST_ERR_NO = round(Mar.utils::st_err(TOTNO),5),
                     ST_ERR_BIOMASS = round(Mar.utils::st_err(BIOMASS),5),
                     ST_ERR_ABUND = round(Mar.utils::st_err(ABUND),5), 
                     .groups = "keep")%>%
    as.data.frame()
  nw<-merge(tmp.cnt,tmp.sum,by=c("STRAT",t_field))
  nw<-merge(nw, tmp.mean,by=c("STRAT",t_field))
  nw<-merge(nw, tmp.sterr,by=c("STRAT",t_field))
  nw[is.na(nw)]<-0
  return(nw)
}