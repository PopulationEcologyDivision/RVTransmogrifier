## @title stratify_NW
## @description This function calculates the stratified values of numbers and weights by strata
## @param tblList the default is \code{NULL}. This is a list populated with all RV dataframes. 
## Prior to running this function they should all have been filtered via \code{propagateChanges()}.
## @param dfNWSets  this is the output from \code{NW_sets()}.
## @param ... other arguments passed to methods (i.e. 'debug' and 'quiet')
## @returns ...
## @author Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
## @importFrom dplyr %>%
## @export
stratify_NW<-function(tblList = NULL, dfNWSets= NULL, ...){
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
  nw_set <- merge(dfNWSets,tblList$GSSTRATUM, by = 'STRAT')
  nw_set$BIOMASS_<-nw_set$TOTWGT*nw_set$TUNITS
  nw_set$ABUND_<-nw_set$TOTNO*nw_set$TUNITS
  nw_set$SETS_W_CATCH <- ifelse(nw_set$TOTWGT>0, 1, 0)

  nw_strat_inf <- nw_set %>% 
    select(TOTWGT,STRAT, AREA, SETS_W_CATCH) %>%  #DMIN, DMAX, , DEPTH, TUNITS 
    mutate(AREA_CALC= AREA* SETS_W_CATCH) %>% 
    group_by(STRAT) %>%
    summarise(AREAPROP = round(mean(SETS_W_CATCH),3), 
                     AREAPROPSTERR = round(st_err(SETS_W_CATCH),5),
                     SETS_W_CATCH  = sum(SETS_W_CATCH),
                     AREATOT  = round(mean(AREA_CALC),5),
                     AREATOTSTERR = round(st_err(AREA_CALC),5),
                     NSETS = n(),
                     .groups = "keep") %>%
    left_join(tblList$GSSTRATUM[,c("STRAT","AREA","TUNITS")],by = "STRAT") %>%
    arrange(STRAT) %>% 
    as.data.frame()
  nw_strat_inf$TUNITS<- round(nw_strat_inf$TUNITS,5)
  nw_strat_inf_tots <- nw_strat_inf %>% 
    transmute(AREAPROP = AREAPROP * AREA,
              SETS_W_CATCH = SETS_W_CATCH,
              NSETS = NSETS,
              AREAPROPSTERR = AREAPROPSTERR,
              AREATOT =  AREATOT,
              AREATOTSTERR = AREATOTSTERR,
              TUNITS = TUNITS,
              AREA = AREA) %>% 
    summarise(AREAPROP = sum(AREAPROP), 
              SETS_W_CATCH = sum(SETS_W_CATCH),
              NSETS = sum(NSETS),
              AREAPROPSTERR = NA,
              AREATOT =  sum(AREATOT),
              AREATOTSTERR = NA,
              TUNITS = sum(TUNITS),
              AREA = sum(AREA)) %>% 
    transmute(STRAT = "TOTAL",
              AREAPROP = round(AREAPROP/AREA,5),
              SETS_W_CATCH = SETS_W_CATCH,
              NSETS = NSETS,
              AREAPROPSTERR = AREAPROPSTERR,
              AREATOT = AREATOT,
              AREATOTSTERR = AREATOTSTERR,
              TUNITS = round(TUNITS,5),
              AREA = AREA)

  nw_strat_inf <- rbind.data.frame(nw_strat_inf, nw_strat_inf_tots)
  #NOTE - Noticed that the set counts/strata were incorrect for Mar.stratisfy
  # cases of size class were inflating the number of sets (ie each size class was counted a separate 
  # set) This further impacted means, std errors, abund and biomass 
  
  nw_strat <- nw_set %>%
    dplyr::group_by(MISSION, STRAT) %>%
    dplyr::summarise(
      # N_SETS = length(STRAT),
                     TOT_WGT = round(sum(TOTWGT),5),
                     MEAN_WGT = round(mean(TOTWGT),5),
                     ST_ERR_WGT = round(st_err(TOTWGT),5),
                     BIOMASS_T = round(mean(BIOMASS_)/1000,5),
                     ST_ERR_BIOMASS = round(st_err(BIOMASS_)/1000,5),
                     TOT_NO = round(sum(TOTNO),5),
                     MEAN_NO = round(mean(TOTNO),5),
                     ST_ERR_NO = round(st_err(TOTNO),5),
                     ABUND = round(mean(ABUND_),5), 
                     ST_ERR_ABUND = round(st_err(ABUND_),5),
                     .groups = "keep") %>% 
    arrange(MISSION, STRAT) %>%
    as.data.frame()

  nw_strat_tots<- nw_strat %>%
    left_join(tblList$GSSTRATUM[,c("STRAT", "AREA")],by = "STRAT") %>%
    transmute(
      # N_SETS = N_SETS,
              TOT_WGT = TOT_WGT,
              MEAN_WGT_str = round(MEAN_WGT*AREA,5),
              TOT_NO = TOT_NO,
              MEAN_NO_str = round(MEAN_NO*AREA,5),
              BIOMASS_T = BIOMASS_T,
              ABUND = ABUND,
              AREA = AREA) %>% 
    summarise(
      # N_SETS = sum(N_SETS), 
              TOT_WGT = round(sum(TOT_WGT),5),
              MEAN_WGT_str = round(sum(MEAN_WGT_str),5),
              TOT_NO = round(sum(TOT_NO),5),
              MEAN_NO_str = round(sum(MEAN_NO_str),5),
              BIOMASS_T = round(sum(BIOMASS_T),5),
              ABUND = round(sum(ABUND),5),
              all_strat = sum(AREA)) %>% 
    transmute(MISSION = "TOTAL",
              STRAT = "TOTAL",
              # N_SETS = N_SETS,
              TOT_WGT = round(TOT_WGT,5), 
              MEAN_WGT = round(MEAN_WGT_str/all_strat,5),
              ST_ERR_WGT = NA,
              BIOMASS_T = round(BIOMASS_T,5),
              ST_ERR_BIOMASS = NA,
              TOT_NO = round(TOT_NO,5), 
              MEAN_NO = round(MEAN_NO_str/all_strat,5),
              ST_ERR_NO = NA,
              ABUND = round(ABUND,5),
              ST_ERR_ABUND = NA) %>% 
    as.data.frame()
  
  nw_strat <- rbind.data.frame(nw_strat,nw_strat_tots)
  nw_strat <- merge(nw_strat_inf, nw_strat, by="STRAT")
  # browser()
  # nw_set %>% select(STRAT, BIOMASS_) %>% mutate_all(~na_if(., 0)) %>% summarise(mean(BIOMASS_, na.rm = T))
  
  res_nw$nw_set <- nw_set[,c("MISSION", "STRAT", "SETNO", "TOTNO", "TOTWGT")] %>% 
    arrange(MISSION, SETNO) %>% 
    data.frame()
  res_nw$nw_strat <- nw_strat 
  
  if(args$debug) message(thisFun, ": completed (",round( difftime(Sys.time(),startTime,units = "secs"),0),"s)")
  return(res_nw)
}