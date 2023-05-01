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
stratify_NW<-function(tblList = NULL, dfNWSets= NULL, stratInfo = NULL, ...){
  argsFn <- as.list(environment())
  argsFn[["tblList"]] <- NULL
  argsFn[["dfNWSets"]] <- NULL
  argsUser <- list(...)
  args <- do.call(set_defaults, list(argsFn=argsFn, argsUser=argsUser))
  if(args$debug){
    startTime <- Sys.time()
    thisFun <- where_now()
    message(thisFun, ": started")
  }
  res_nw <- list()

  nw_set <- merge(dfNWSets,stratInfo, by = 'STRAT')
  nw_set$BIOMASS_<-nw_set$TOTWGT*nw_set$TUNITS
  nw_set$ABUND_<-nw_set$TOTNO*nw_set$TUNITS
  
  nw_strat_inf <- nw_set %>% 
    select(TOTWGT,STRAT, SQNM) %>%  #DMIN, DMAX, , DEPTH, TUNITS 
    mutate(SOMECATCH = ifelse(TOTWGT>0, 1, 0),
           AREA_CALC= SQNM* SOMECATCH) %>% 
    dplyr::group_by(STRAT) %>%
    dplyr::summarise(AREAPROP = round(mean(SOMECATCH),3), 
                     AREACNT  = length(SOMECATCH),
                     AREAPROPSTERR = round(st_err(SOMECATCH),5),
                     AREATOT  = round(mean(AREA_CALC),5),
                     AREATOTSTERR = round(st_err(AREA_CALC),5),
                     .groups = "keep") %>%
    arrange(STRAT) %>% 
    as.data.frame()
  
  #NOTE - Noticed that the set counts/strata were incorrect for Mar.stratisfy
  # cases of size class were inflating the number of sets (ie each size class was counted a separate 
  # set) This further impacted means, std errors, abund and biomass 

  nw_strat <- nw_set %>%
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

  res_nw$nw_strat_inf <- nw_strat_inf 
  res_nw$nw_set <- nw_set[,c("MISSION", "STRAT", "SETNO", "TOTNO", "TOTWGT")] %>% 
    arrange(MISSION, SETNO) %>% 
    data.frame()
  res_nw$nw_strat <- nw_strat 
  
  if(args$debug) message(thisFun, ": completed (",round( difftime(Sys.time(),startTime,units = "secs"),0),"s)")
  return(res_nw)
}