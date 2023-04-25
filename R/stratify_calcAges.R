#' @title stratify_calcAges
#' @description This function ...
#' @param tblList the default is \code{NULL}. This is a list populated with all RV dataframes. Prior 
#' to running this function they should all have been filtered via \code{propagateChanges()}
#' @param dfNWSets this is the output from \code{NW_sets()}.
#' @param ... other arguments passed to methods (i.e. 'debug' and 'quiet')
#' @returns ...
#' @author Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
stratify_calcAges<-function(tblList = NULL, dfNWSets = NULL, stratInfo = NULL, ...){
  argsFn <- as.list(environment())
  argsFn[["tblList"]] <- NULL
  argsUser <- list(...)
  args <- do.call(set_defaults, list(argsFn=argsFn, argsUser=argsUser))
  if(args$debug){
    startTime <- Sys.time()
    thisFun <- where_now()
    message(thisFun, ": started")
  }
  
  if (args$bySex & length(unique(tblList$dataDETS$FSEX))==1) args$bySex <- FALSE
  
  theseMissions <- unique(tblList$GSMISSIONS$MISSION)
  thisTAXA <- unique(dfNWSets$TAXA_)
  
  if (F){
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
  emptydf_len<- anti_join(emptydf_len, dfLen, by = c("MISSION", "STRAT", "SETNO", "TAXA_", "FLEN")) 
  dfLen<- rbind.data.frame(dfLen, emptydf_len)
  
  length_by_set <- dfLen %>%
    group_by(MISSION, STRAT, SETNO, FLEN, FSEX) %>%
    summarise(CLEN = sum(CLEN), .groups = 'keep') %>%
    tidyr::pivot_wider(names_from = c(FSEX,FLEN), values_from = CLEN, values_fill = 0)%>%
    arrange(MISSION, SETNO) %>% 
    sexifyNames(desc="LEN") %>%  
    as.data.frame()
  
  length_by_strat_mean <- dfLen %>%
    select(-SETNO) %>% 
    group_by(MISSION, TAXA_,STRAT, FLEN, FSEX) %>%
    summarise(across(everything(), mean), .groups = 'keep') %>% 
    tidyr::pivot_wider(names_from = c(FSEX,FLEN), values_from = CLEN, values_fill = 0)%>%
    arrange(MISSION, STRAT) %>% 
    sexifyNames(desc="LEN") %>%  
    as.data.frame()
  
  length_by_strat_se <- dfLen %>%
    select(-SETNO) %>% 
    group_by(MISSION, TAXA_,STRAT, FLEN, FSEX) %>%
    summarise(across(everything(), st_err), .groups = 'keep') %>% 
    tidyr::pivot_wider(names_from = c(FSEX,FLEN), values_from = CLEN, values_fill = 0)%>%
    arrange(MISSION, STRAT) %>% 
    sexifyNames(desc="LEN") %>%  
    as.data.frame()
  
  length_by_strat_total <- length_by_strat_mean %>% 
    left_join(., stratInfo[, c("STRAT", "TUNITS")], by="STRAT") %>% 
    ungroup() %>% 
    mutate(across(-c("MISSION", "TAXA_","STRAT", "TUNITS"), ~ . * TUNITS)) %>% 
    select(-TUNITS)  %>% 
    sexifyNames(desc="LEN") %>%
    as.data.frame()
  
  length_by_strat_total_se <- length_by_strat_se %>% 
    left_join(., stratInfo[, c("STRAT", "TUNITS")], by="STRAT") %>% 
    ungroup() %>% 
    mutate(across(-c("MISSION", "TAXA_","STRAT", "TUNITS"), ~ . * TUNITS)) %>% 
    select(-TUNITS)  %>% 
    sexifyNames(desc="LEN") %>%
    as.data.frame()
  
  results=list(length_by_set= length_by_set,
               length_by_strat_mean = length_by_strat_mean,
               length_by_strat_se = length_by_strat_se,
               length_by_strat_total = length_by_strat_total,
               length_by_strat_total_se = length_by_strat_total_se)
   message("still need to \n1) add totals (cols and rows); ")

  }
  if(args$debug) message(thisFun, ": completed (",round( difftime(Sys.time(),startTime,units = "secs"),0),"s)")
   return(results)
}