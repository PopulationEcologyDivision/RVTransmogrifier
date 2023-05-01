stratify_calcAges<-function(tblList = NULL, dfNWSets = NULL, stratInfo = NULL, stratLengths=NULL,...){
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
  thisLGRP <- unique(na.omit(dfNWSets$LGRP))
  
  stratTots <- stratLengths %>% filter(.,MISSION == "TOTAL") %>%
    select(-c(MISSION, STRAT, TOTAL)) %>%
    unSexifyNames(., desc = "LEN") %>%
    t(.) %>%
    as.data.frame() %>%
    rename(.,"TOTAL"="V1") %>%
    mutate(TOTAL = as.numeric(TOTAL),
           FLEN = rownames(.))
  
  ages_pre <- filter(tblList$dataDETS, !is.na(AGE) & TAXA_ == thisTAXA) 
  if (ages_pre %>% nrow()==0){return(NA)}
  
  alk_pre <- ages_pre %>%
    group_by(MISSION,  FSEX, FLEN, AGE) %>%
    summarise(CAGE = length(MISSION), .groups = 'keep') %>%
    as.data.frame()
  #make an initial df of all combos of mission, ages, lengths and sexes
  
  alk_lengths <- seq(min(alk_pre$FLEN, na.rm = T),max(alk_pre$FLEN, na.rm = T), by=thisLGRP)
  alk_ages <- seq(min(alk_pre$AGE, na.rm = T),max(alk_pre$AGE, na.rm = T), by=1)
  alk_sexes <- sort(unique(alk_pre$FSEX))
  alk_fake = as.data.frame(expand.grid(MISSION = theseMissions, AGE = alk_ages, FLEN = alk_lengths, FSEX = alk_sexes, CAGE = 0))
  alk_fake<- anti_join(alk_fake, alk_pre, by = c("MISSION", "AGE", "FSEX", "FLEN")) 
  alk_pre<- rbind.data.frame(alk_pre, alk_fake)
  
  alk_pre <- alk_pre %>%
    tidyr::pivot_wider(names_from = c(AGE), values_from = CAGE, values_fill = 0)%>%
    arrange(MISSION, FSEX, FLEN) 
  ####
  alw_pre <- filter(tblList$dataDETS, !is.na(AGE) & !is.na(FWT) &TAXA_ == thisTAXA) %>%
    group_by(MISSION, FSEX, FLEN, AGE) %>%
    summarise(FWT = mean(FWT)/1000, .groups = 'keep') %>%
    as.data.frame()
  
  #make an initial df of all combos of mission, ages, lengths and sexes
  alw_lengths <- seq(min(alw_pre$FLEN, na.rm = T),max(alw_pre$FLEN, na.rm = T), by=thisLGRP)
  alw_ages <- seq(min(alw_pre$AGE, na.rm = T),max(alw_pre$AGE, na.rm = T), by=1)
  alw_sexes <- sort(unique(alw_pre$FSEX))
  alw_fake = as.data.frame(expand.grid(MISSION = theseMissions, AGE = alw_ages, FLEN = alw_lengths, FSEX = alw_sexes, FWT = 0))
  
  alw_fake<- anti_join(alw_fake, alw_pre, by = c("MISSION", "AGE", "FLEN", "FSEX"))
  alw_pre<- rbind.data.frame(alw_pre, alw_fake)
  
  alw <- alw_pre %>%
    tidyr::pivot_wider(names_from = c(AGE), values_from = FWT, values_fill = 0)%>%
    sexifyNames(desc = "AGE") %>% 
    as.data.frame() %>% 
    arrange(MISSION, FSEX, FLEN) 
  
  alk <- alk_pre %>%
    sexifyNames(desc = "AGE") %>% 
    as.data.frame()
  
  for (s in 1:length(alk_sexes)){
    thisSexProp <- alk_pre[alk_pre$FSEX %in% alk_sexes[s] ,]
    
    thisSexProp[!names(thisSexProp) %in% c("MISSION", "FSEX", "FLEN")] <- thisSexProp[!names(thisSexProp) %in% c("MISSION","FSEX","FLEN")]/rowSums(thisSexProp[!names(thisSexProp) %in% c("MISSION","TAXA_", "FSEX","FLEN")])
    thisSexProp[is.na(thisSexProp)] = 0
    
    thisSexStratTots <- stratTots[grepl(x= stratTots$FLEN, pattern = paste0(alk_sexes[s],"_")),]
    thisSexStratTots$FLEN <- as.numeric(sub(paste0("^",alk_sexes[s],"_"), "", thisSexStratTots$FLEN))
    thisSexAlk<- thisSexProp %>% left_join(., thisSexStratTots, by="FLEN")
    
    thisSexAlk <- thisSexAlk %>% 
      mutate(across(-c("MISSION","FSEX", "FLEN","TOTAL"), ~ . * TOTAL)) %>% 
      select(-TOTAL)  %>% 
      sexifyNames(desc="AGE") %>%
      select(MISSION, FSEX, FLEN, sort(names(.))) %>% 
      arrange(MISSION, FSEX, FLEN) %>% 
      as.data.frame()
    if(s==1) {
      ageTable <- thisSexAlk
    }else{
      ageTable <- rbind.data.frame(ageTable, thisSexAlk)
    }
  }
  
  thisNoSexProp <- alk_pre %>% 
    select(-FSEX) %>% 
    group_by(MISSION, FLEN) %>%
    summarise(across(everything(), mean), .groups = 'keep') %>% 
    as.data.frame()
  thisNoSexProp[!names(thisNoSexProp) %in% c("MISSION", "FLEN")] <- thisNoSexProp[!names(thisNoSexProp) %in% c("MISSION","FLEN")]/rowSums(thisNoSexProp[!names(thisNoSexProp) %in% c("MISSION","FLEN")])
  thisNoSexProp[is.na(thisNoSexProp)] = 0
  
  dfAge <- tblList$dataLF %>%
    filter(TAXA_ == thisTAXA & !is.na(FLEN)) %>%
    group_by(MISSION, STRAT, SETNO, TAXA_, FLEN, LGRP) %>%
    summarise(CAGE = sum(CLEN), 
              .groups = 'keep') %>%
    as.data.frame()
  
  allLengths <- seq(min(dfAge$FLEN, na.rm = T),max(dfAge$FLEN, na.rm = T), by= thisLGRP)
  
  #create fake dfs for all values of length and age
  emptydf_age <- tblList$GSINF[,c("MISSION", "SETNO", "STRAT")]
  emptydf_age <- expandDF(templateDF = emptydf_age, keyFields = c("STRAT","MISSION","SETNO"), expandField = "FLEN", expandVals = allLengths)
  emptydf_age$CAGE <- 0
  emptydf_age<- anti_join(emptydf_age, dfAge, by = c("MISSION", "STRAT", "SETNO", "FLEN")) 
  dfAge<- rbind.data.frame(dfAge[,c("MISSION", "STRAT", "SETNO", "FLEN", "CAGE")], emptydf_age)
  
  # age_by_set <- dfAge %>%
  #   group_by(MISSION, STRAT, SETNO, FLEN) %>%
  #   summarise(CAGE = sum(CAGE), .groups = 'keep') %>%
  #   tidyr::pivot_wider(names_from = c(FLEN), values_from = CAGE, values_fill = 0)%>%
  #   ungroup() %>%
  #   mutate(TOTAL = rowSums(.[,!names(.) %in% c("MISSION", "STRAT", "SETNO")], na.rm = T)) %>%
  #   arrange(MISSION, SETNO) %>%
  #   sexifyNames(desc="AGE") %>%
  #   select(MISSION, STRAT, SETNO, sort(names(.))) %>%
  #   as.data.frame()
  
  ages_pre <- filter(tblList$dataDETS, !is.na(AGE) & TAXA_ == thisTAXA) 
  if (ages_pre %>% nrow()==0){return(NA)}
    
  age_by_set<- dfAge %>% 
    left_join(thisNoSexProp, c("MISSION","FLEN")) %>% 
    mutate(across(-c("MISSION","FLEN","STRAT", "SETNO", "CAGE"), ~ . * CAGE)) %>% 
    select(-FLEN, -CAGE) %>% 
    group_by(MISSION, STRAT, SETNO) %>% 
    summarise(across(everything(), sum, na.rm = TRUE),.groups ="keep") %>% 
    as.data.frame()

  age_by_strat_mean <- age_by_set %>% 
    select(-SETNO) %>% 
    group_by(MISSION, STRAT)  %>% 
    summarise(across(everything(), mean, na.rm = TRUE),.groups ="keep")  %>% 
    as.data.frame()
  
  age_by_strat_se <- age_by_set %>% 
    select(-SETNO) %>% 
    group_by(MISSION, STRAT)  %>% 
    summarise(across(everything(), st_err),.groups ="keep")  %>% 
    as.data.frame()
  
  age_by_strat_total <- age_by_strat_mean %>% 
    left_join(., stratInfo[, c("STRAT", "TUNITS")], by="STRAT") %>% 
    ungroup() %>% 
    mutate(across(-c("MISSION","STRAT", "TUNITS"), ~ . * TUNITS)) %>% 
    ungroup() %>%  
    select(-TUNITS)  %>% 
    select(MISSION, STRAT, sort(names(.))) %>% 
    as.data.frame()

  age_by_strat_total_se <- age_by_strat_se %>%
    left_join(., stratInfo[, c("STRAT", "TUNITS")], by="STRAT") %>%
    ungroup() %>%
    mutate(across(-c("MISSION","STRAT", "TUNITS"), ~ . * TUNITS)) %>%
    select(-TUNITS)  %>%
    select(MISSION, STRAT, sort(names(.))) %>%
    as.data.frame()

  results<- list(age_length_key= alk,
                 age_length_weight = alw,
                 age_table = ageTable,
                 age_by_set = age_by_set,
                 age_by_strat_mean = age_by_strat_mean,
                 age_by_strat_se = age_by_strat_se,
                 age_by_strat_total = age_by_strat_total,
                 age_by_strat_total_se = age_by_strat_total_se
  )
  return(results)
}