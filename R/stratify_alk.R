#' @title stratify_alk
#' @description This function ...
#' @param tblList the default is \code{NULL}. This is a list populated with all RV dataframes. Prior 
#' to running this function they should all have been filtered via \code{propagateChanges()}
#' @param dfNWSets this is the output from \code{NW_sets()}.
#' @param ... other arguments passed to methods (i.e. 'debug' and 'quiet')
#' @returns ...
#' @author Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
stratify_alk<-function(tblList = NULL, dfNWSets = NULL, stratInfo = NULL, stratLengths=length_by_strat_total, ...){
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
  
  theseMissions <- unique(dfNWSets$MISSION)
  thisTAXA <- unique(dfNWSets$TAXA_)
  thisLGRP <- unique(na.omit(dfNWSets$LGRP))
  stratTots <- stratLengths %>% filter(.,MISSION == "TOTAL") %>%
    select(-c(MISSION, STRAT, TOTAL)) %>%
    unSexifyNames(., desc = "LEN") %>%
    t(.) %>%
    as.data.frame() %>%
    rename(.,"TOTAL"="V1") %>%
    mutate(TOTAL = as.numeric(TOTAL),
           FLEN = rownames(.))
  
  alk_chk <- filter(tblList$dataDETS, !is.na(AGE)) %>% nrow()
  if (alk_chk==0){return(NA)}
  
  alk_pre <- filter(tblList$dataDETS, !is.na(AGE) & TAXA_ == thisTAXA) %>%
    group_by(MISSION,  TAXA_, FSEX, FLEN, AGE) %>%
    summarise(CAGE = length(MISSION), .groups = 'keep') %>%
    as.data.frame()
  
  # #make an initial df of all combos of mission, spec, ages, lengths and sexes
  alk_lengths <- seq(min(alk_pre$FLEN, na.rm = T),max(alk_pre$FLEN, na.rm = T), by=thisLGRP)
  alk_ages <- seq(min(alk_pre$AGE, na.rm = T),max(alk_pre$AGE, na.rm = T), by=1)
  alk_sexes <- sort(unique(alk_pre$FSEX))
  al = as.data.frame(expand.grid(MISSION = theseMissions, TAXA_ = thisTAXA, AGE = alk_ages, FLEN = alk_lengths, FSEX = alk_sexes, CAGE = 0))
  # #drop the rows from the df for those combos for which we have real data
  al<- anti_join(al, alk_pre, by = c("MISSION", "TAXA_", "AGE", "FSEX", "FLEN")) 
  alk_pre<- rbind.data.frame(alk_pre, al)
  
  
  alk_pre <- alk_pre %>%
    tidyr::pivot_wider(names_from = c(AGE), values_from = CAGE, values_fill = 0)%>%
    arrange(MISSION, TAXA_, FSEX, FLEN)
  
  # alk_pre <- alk_pre %>%
  #   tidyr::pivot_wider(names_from = c(FSEX,AGE), values_from = CAGE, values_fill = 0)%>%
  #   arrange(MISSION, TAXA_, FLEN)
  
  alk <- alk_pre %>%
    sexifyNames(desc = "AGE") %>% 
    as.data.frame()
  
  #determine what proportion of each length is each age

  for (s in 1:length(alk_sexes)){
    thisSexAlk <- alk_pre[alk_pre$FSEX %in% alk_sexes[s] ,]
    thisSexAlk[!names(thisSexAlk) %in% c("MISSION","TAXA_", "FSEX", "FLEN")] <- thisSexAlk[!names(thisSexAlk) %in% c("MISSION","TAXA_", "FSEX","FLEN")]/rowSums(thisSexAlk[!names(thisSexAlk) %in% c("MISSION","TAXA_", "FSEX","FLEN")])
    thisSexAlk[is.na(thisSexAlk)] = 0
    
    thisSexStratTots <- stratTots[grepl(x= stratTots$FLEN, pattern = paste0(alk_sexes[s],"_")),]
    thisSexStratTots$FLEN <- as.numeric(sub(paste0("^",alk_sexes[s],"_"), "", thisSexStratTots$FLEN))
    thisSexAlk<- thisSexAlk %>% left_join(., thisSexStratTots, by="FLEN")
    
    thisSexAlk <- thisSexAlk %>% 
      mutate(across(-c("MISSION","TAXA_", "FSEX", "FLEN","TOTAL"), ~ . * TOTAL)) %>% 
      select(-TOTAL)  %>% 
      sexifyNames(desc="AGE") %>%
      select(MISSION, TAXA_, FSEX, FLEN, sort(names(.))) %>% 
      arrange(MISSION, FSEX, FLEN) %>% 
      as.data.frame()
    if(s==1) {
      ageTable <- thisSexAlk
    }else{
      ageTable <- rbind.data.frame(ageTable, thisSexAlk)
    }
    
    # thisSexAlk <- alk_pre[,names(alk_pre) %in% c("MISSION", "TAXA_", "FLEN") | grepl( paste0("^",alk_sexes[s],"_") , names( alk_pre ) )]
    # thisSexAlk[!names(thisSexAlk) %in% c("MISSION","TAXA_", "FLEN")] <- thisSexAlk[!names(thisSexAlk) %in% c("MISSION","TAXA_", "FLEN")]/rowSums(thisSexAlk[!names(thisSexAlk) %in% c("MISSION","TAXA_", "FLEN")])
    # thisSexAlk[is.na(thisSexAlk)] = 0
    # 
    # thisSexStratTots <- stratTots[grepl(x= stratTots$FLEN, pattern = paste0(alk_sexes[s],"_")),]
    # thisSexStratTots$FLEN <- as.numeric(sub(paste0("^",alk_sexes[s],"_"), "", thisSexStratTots$FLEN))
    # thisSexAlk<- thisSexAlk %>% left_join(., thisSexStratTots, by="FLEN")
    # 
    # thisSexAlk <- thisSexAlk %>% 
    #   mutate(across(-c("MISSION","TAXA_", "FLEN","TOTAL"), ~ . * TOTAL)) %>% 
    #   select(-TOTAL)  %>% 
    #   sexifyNames(desc="AGE") %>%
    #   select(MISSION, TAXA_, FLEN, sort(names(.))) %>% 
    #   arrange(MISSION, FLEN) %>% 
    #   as.data.frame()
    # if(s==1) {
    #   ageTable <- thisSexAlk
    # }else{
    #   ageTable <- merge(ageTable, thisSexAlk, by =c("MISSION", "TAXA_", "FLEN"))
    # }
  }
  
  results<- list(alk= alk,
                 age_table = ageTable)

  message("still need to \n1) add totals (cols and rows);")
  
  if(args$debug) message(thisFun, ": completed (",round( difftime(Sys.time(),startTime,units = "secs"),0),"s)")
  return(results)
}