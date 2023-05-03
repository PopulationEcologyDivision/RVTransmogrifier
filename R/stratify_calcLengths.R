#' @title stratify_calcLengths
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
stratify_calcLengths<-function(tblList = NULL, dfNWSets = NULL, stratInfo = NULL, ...){
  argsFn <- as.list(environment())
  argsFn[["tblList"]] <- NULL
  argsUser <- list(...)
  args <- do.call(set_defaults, list(argsFn=argsFn, argsUser=argsUser))
  if(args$debug){
    startTime <- Sys.time()
    thisFun <- where_now()
    message(thisFun, ": started")
  }
  
  theseMissions <- unique(tblList$GSMISSIONS$MISSION)
  thisTAXA <- unique(dfNWSets$TAXA_)
  # if (args$bySex & length(unique(tblList$dataDETS$FSEX))==1) args$bySex <- FALSE

  # args$bySex
  dfLen <- tblList$dataLF %>%
    filter(TAXA_ == thisTAXA & !is.na(FLEN)) %>%
    group_by(MISSION, STRAT, SETNO, TAXA_, FSEX, FLEN, LGRP) %>%
    summarise(CLEN = sum(CLEN), 
              .groups = 'keep') %>%
    as.data.frame()
  
  thisLGRP <- unique(na.omit(dfLen$LGRP))
  dfLen$LGRP <-NULL
  allLengths <- seq(min(dfLen$FLEN, na.rm = T),max(dfLen$FLEN, na.rm = T), by= thisLGRP)
  allSex <- unique(na.omit(dfLen$FSEX))
  
  #create fake dfs for all values of length and age
  emptydf_len <- tblList$GSINF[,c("MISSION", "SETNO", "STRAT")]
  emptydf_len <- expandDF(templateDF = emptydf_len, keyFields = c("STRAT","MISSION","SETNO"), expandField = "FLEN", expandVals = allLengths)
  emptydf_len <- expandDF(templateDF = emptydf_len, keyFields = c("STRAT","MISSION","SETNO", "FLEN"), expandField = "FSEX", expandVals = allSex)
  emptydf_len$TAXA_ <- thisTAXA
  emptydf_len$CLEN <- 0
  emptydf_len<- anti_join(emptydf_len, dfLen, by = c("MISSION", "STRAT", "SETNO", "FLEN")) 

  dfLen<- rbind.data.frame(dfLen, emptydf_len)

  if (!args$bySex) {
    dfLen$FSEX <- 9
  }

  length_by_set <- dfLen %>%
    group_by(MISSION, STRAT, SETNO, FLEN, FSEX) %>%
    summarise(CLEN = sum(CLEN), .groups = 'keep') %>%
    tidyr::pivot_wider(names_from = c(FSEX,FLEN), values_from = CLEN, values_fill = 0)%>%
    ungroup() %>%
    mutate(TOTAL = rowSums(.[,!names(.) %in% c("MISSION", "STRAT", "SETNO")], na.rm = T)) %>%
    arrange(MISSION, SETNO) %>%
    sexifyNames(desc="LEN") %>%
    select(MISSION, STRAT, SETNO, sort(names(.))) %>%
    select(-one_of('TOTAL'), one_of('TOTAL')) %>%
    as.data.frame()
  
  if(args$debug)message("Need to add average lengths/age/sex to age_table")
  
length_by_strat_mean <- length_by_set %>%
  select(-SETNO,-TOTAL) %>% 
  group_by(MISSION, STRAT) %>%
  summarise(across(everything(), mean), .groups = 'keep')%>%
  ungroup() %>%
  mutate(TOTAL = rowSums(.[,!names(.) %in% c("MISSION", "STRAT")], na.rm = T)) %>% 
  arrange(MISSION, STRAT) %>%
  select(MISSION, STRAT, sort(names(.))) %>%
  select(-one_of('TOTAL'), one_of('TOTAL')) %>%
  as.data.frame()

length_by_strat_se <- length_by_set %>%
  select(-SETNO,-TOTAL) %>% 
  group_by(MISSION, STRAT) %>%
  summarise(across(everything(), st_err), .groups = 'keep')%>%
  ungroup() %>%
  mutate(TOTAL = rowSums(.[,!names(.) %in% c("MISSION", "STRAT")], na.rm = T)) %>% 
  arrange(MISSION, STRAT) %>%
  select(MISSION, STRAT, sort(names(.))) %>%
  select(-one_of('TOTAL'), one_of('TOTAL')) %>%
  as.data.frame()
  
  length_by_strat_total <- length_by_strat_mean %>% 
    select(-TOTAL) %>% 
    left_join(., stratInfo[, c("STRAT", "TUNITS")], by="STRAT") %>% 
    ungroup() %>% 
    mutate(across(-c("MISSION","STRAT", "TUNITS"), ~ . * TUNITS)) %>% 
    ungroup() %>%  
    select(-TUNITS)  %>% 
    mutate(TOTAL = rowSums(.[,!names(.) %in% c("MISSION", "STRAT")], na.rm = T)) %>% 
    sexifyNames(desc="LEN") %>%
    select(MISSION, STRAT, sort(names(.))) %>% 
    select(-one_of('TOTAL'), one_of('TOTAL')) %>%
    as.data.frame()
  
  length_by_strat_total_tots <- colSums(length_by_strat_total[,!names(length_by_strat_total) %in% c("MISSION", "STRAT")])
  length_by_strat_total_tots<- c("TOTAL", NA, length_by_strat_total_tots)
  length_by_strat_total<- rbind.data.frame(length_by_strat_total,length_by_strat_total_tots)
  
  length_by_strat_total_se <- length_by_strat_se %>% 
    select(-TOTAL) %>% 
    left_join(., stratInfo[, c("STRAT", "TUNITS")], by="STRAT") %>% 
    ungroup() %>% 
    mutate(across(-c("MISSION","STRAT", "TUNITS"), ~ . * TUNITS)) %>% 
    select(-TUNITS)  %>% 
    # mutate(TOTAL = rowSums(.[,!names(.) %in% c("MISSION", "STRAT")], na.rm = T)) %>% 
    sexifyNames(desc="LEN") %>%
    select(MISSION, STRAT, sort(names(.))) %>% 
    # select(-one_of('TOTAL'), one_of('TOTAL')) %>%
    as.data.frame()

  if (args$debug) message("The 'Total' column for length_by_strat_total_sefor is not just the sum of the row values\n
                          need to figure it out.")
  length_by_strat_total_se_tots <- colSums(length_by_strat_total_se[,!names(length_by_strat_total_se) %in% c("MISSION", "STRAT")])
  length_by_strat_total_se_tots<- c("TOTAL", NA, length_by_strat_total_se_tots)
  length_by_strat_total_se<- rbind.data.frame(length_by_strat_total_se,length_by_strat_total_se_tots)
  
  results=list(length_by_set= length_by_set,
               length_by_strat_mean = length_by_strat_mean,
               length_by_strat_se = length_by_strat_se,
               length_by_strat_total = length_by_strat_total,
               length_by_strat_total_se = length_by_strat_total_se)
  if(args$debug){
    results[["debugdfLen"]]<-dfLen
  }
  if(args$debug) message(thisFun, ": completed (",round( difftime(Sys.time(),startTime,units = "secs"),0),"s)")
  return(results)
}