#' @title stratify_NW
#' @description This function calculates the stratified values of numbers and weights by strata
#' @param tblList the default is \code{NULL}. This is a list populated with all RV dataframes. Prior 
#' to running this function they should all have been filtered via \code{propagateChanges()}.
#' @param dfNWSets  this is the output from \code{NW_sets()}.
#' @param ... other arguments passed to methods (i.e. 'debug' and 'quiet')
#' @returns ...
#' @author Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @importFrom dplyr %>%
#' @export
stratify_NW<-function(tblList = NULL, dfNWSets= NULL, ...){
  args <- list(...)
  debug <- ifelse(is.null(args$debug), F, args$debug) 
  quiet <- ifelse(is.null(args$quiet), F, args$quiet)
  #get rid of records where n known taxa were caught
  res_nw <- list()

  stratInfo <- tblList$GSSTRATUM
  stratInfo <- addTUNITS(stratInfo)
  tmp <- merge(dfNWSets,stratInfo, by = 'STRAT')
  colnames(tmp)[colnames(tmp)=="AREA"] <- "SQNM"

  strat_inf <- tmp %>% 
    select(TOTWGT,STRAT, DMIN, DMAX, SQNM, DEPTH, NAME, TUNITS) %>% 
    mutate(SOMECATCH = ifelse(TOTWGT>0, 1, 0),
           AREA_CALC= SQNM* SOMECATCH,
           TUNITS = round(TUNITS, 5)) %>% 
    dplyr::group_by(STRAT,  DMIN, DMAX, SQNM, DEPTH, NAME, TUNITS) %>%
    dplyr::summarise(AREAPROP = round(mean(SOMECATCH),3), 
                     AREACNT  = length(SOMECATCH),
                     AREAPROPSTERR = round(st_err(SOMECATCH),5),
                     AREATOT  = round(mean(AREA_CALC),5),
                     AREATOTSTERR = round(st_err(AREA_CALC),5),
                     .groups = "keep") %>%
    arrange(STRAT) %>% 
    as.data.frame()
  
  #####
  res_nw$strat_inf <- strat_inf
  #####
  res_nw$nw_set <- tmp[,c("MISSION", "STRAT", "SQNM", "SETNO", "TOTNO", "TOTWGT")] %>% arrange(MISSION, STRAT, SETNO) %>% data.frame()
  #####
  #NOTE - Noticed that the set counts/strata were incorrect for Mar.stratisfy
  # cases of size class were inflating the number of sets (ie each size class was counted a separate 
  # set) This further impacted means, std errors, abiund and biomass 
  tmp$BIOMASS_<-tmp$TOTWGT*tmp$TUNITS
  tmp$ABUND_<-tmp$TOTNO*tmp$TUNITS

  nw_strat <- tmp %>%
    dplyr::group_by(MISSION, STRAT) %>%
    dplyr::summarise(N_SETS = length(STRAT),
                     TOT_WGT = round(sum(TOTWGT),5),
                     MEAN_WGT = round(mean(TOTWGT),5),
                     ST_ERR_WGT = round(st_err(TOTWGT),5),
                     BIOMASS_T = round(mean(BIOMASS_)/1000,5),
                     ST_ERR_BIOMASS = round(st_err(BIOMASS_)/1000,5),
                     TOT_NO = round(sum(TOTNO),5),
                     MEAN_NO = round(mean(TOTNO),5),
                     ABUND = round(mean(ABUND_),5), 
                     ST_ERR_NO = round(st_err(TOTNO),5),
                     ST_ERR_ABUND = round(st_err(ABUND_),5),
                     .groups = "keep") %>% 
    arrange(MISSION, STRAT) %>%
    as.data.frame()
  #####
  res_nw$nw_strat <- nw_strat 
  ####
  return(res_nw)
}