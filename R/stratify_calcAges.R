stratify_calcAges<-function(tblList = NULL, dfNWSets = NULL, stratLengths=NULL,...){  
  #stratWeights= NULL, 
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
    select(-c(MISSION, STRAT)) %>% # TOTAL
    unSexifyNames(., desc = "LEN") %>%
    t(.) %>%
    as.data.frame() %>%
    rename(.,"TOTAL"="V1") %>%
    mutate(TOTAL = as.numeric(TOTAL),
           FLEN = rownames(.))

  
  #######################
  alk_pre <- filter(tblList$dataDETS, !is.na(AGE) & TAXA_ == thisTAXA) %>%
    mutate(FSEX= case_when(args$bySex ==T ~ FSEX, 
                           args$bySex != T ~9)) %>% 
    group_by(MISSION,  FSEX, FLEN, AGE) %>%
    summarise(CAGE = length(MISSION), .groups = 'keep') %>%
    as.data.frame()
  if (alk_pre %>% nrow()==0){return(NA)}
  
  alk_lengths <- seq(min(alk_pre$FLEN, na.rm = T),max(alk_pre$FLEN, na.rm = T), by=thisLGRP)
  alk_ages    <- seq(min(alk_pre$AGE, na.rm = T),max(alk_pre$AGE, na.rm = T), by=1)
  alk_sexes   <- sort(unique(alk_pre$FSEX))
  alk_fake    <- as.data.frame(expand.grid(MISSION = theseMissions, AGE = alk_ages, FLEN = alk_lengths, FSEX = alk_sexes, CAGE = 0))
  alk_fake    <- anti_join(alk_fake, alk_pre, by = c("MISSION", "AGE", "FSEX", "FLEN")) 
  alk_pre     <- rbind.data.frame(alk_pre, alk_fake)
  alk_pre     <- alk_pre %>%
    tidyr::pivot_wider(names_from = c(AGE), values_from = CAGE, values_fill = 0)%>%
    arrange(MISSION, FSEX, FLEN) 
  
  #####################
  alw_pre <- filter(tblList$dataDETS, !is.na(AGE) & !is.na(FWT) &TAXA_ == thisTAXA) %>%
    mutate(FSEX= case_when(args$bySex ==T ~ FSEX, 
                           args$bySex != T ~9)) %>% 
    group_by(MISSION, FSEX, FLEN, AGE) %>%
    summarise(FWT = mean(FWT)/1000, .groups = 'keep') %>%
    as.data.frame()
  
  alw_lengths <- seq(min(alw_pre$FLEN, na.rm = T),max(alw_pre$FLEN, na.rm = T), by=thisLGRP)
  alw_ages    <- seq(min(alw_pre$AGE, na.rm = T),max(alw_pre$AGE, na.rm = T), by=1)
  alw_sexes   <- sort(unique(alw_pre$FSEX))
  alw_fake    <- as.data.frame(expand.grid(MISSION = theseMissions, AGE = alw_ages, FLEN = alw_lengths, FSEX = alw_sexes, FWT = 0))
  alw_fake    <- anti_join(alw_fake, alw_pre, by = c("MISSION", "AGE", "FLEN", "FSEX"))
  alw_pre     <- rbind.data.frame(alw_pre, alw_fake)
  alw <- alw_pre %>%
    tidyr::pivot_wider(names_from = c(AGE), values_from = FWT, values_fill = 0)%>%
    as.data.frame() %>% 
    arrange(MISSION, FSEX, FLEN) 
   
  #add totals to age table
  alk <- alk_pre %>%
    mutate(TOTAL = rowSums(.[,!names(.) %in% c("MISSION", "FSEX", "FLEN")], na.rm = T)) %>% 
    as.data.frame()
  alk_tots <- c("TOTAL",NA,NA,colSums(alk[,!names(alk) %in% c("MISSION", "FSEX", "FLEN")]))
  alk<-rbind(alk,alk_tots)

  # for (w in 1:length(alw_sexes)){
  #   thisALWSexProp <- alw[alw$FSEX ==alw_sexes[w] ,]
  #   thisALWSexProp[!names(thisALWSexProp) %in% c("MISSION", "FSEX", "FLEN")] <- thisALWSexProp[!names(thisALWSexProp) %in% c("MISSION","FSEX","FLEN")]/rowSums(thisALWSexProp[!names(thisALWSexProp) %in% c("MISSION","TAXA_", "FSEX","FLEN")])
  #   thisALWSexProp[is.na(thisALWSexProp)] = 0
  #   if (w==3) browser()
  #   thisSexStratTots <- stratTots[grepl(x= stratTots$FLEN, pattern = paste0(alw_sexes[w],"_")),]
  #   thisSexStratTots$FLEN <- as.numeric(sub(paste0("^",alw_sexes[w],"_"), "", thisSexStratTots$FLEN))
  #   thisSexAlw<- thisALWSexProp %>% left_join(., thisSexStratTots, by="FLEN")
  # 
  #   thisSexAlw <- thisSexAlw %>% 
  #     mutate(across(-c("MISSION","FSEX", "FLEN","TOTAL"), ~ . * TOTAL)) %>% 
  #     select(-TOTAL)  %>% 
  #     sexifyNames(desc="AGE") %>%
  #     select(MISSION, FSEX, FLEN, sort(names(.))) %>% 
  #     arrange(MISSION, FSEX, FLEN) %>% 
  #     as.data.frame()
  #   if(w==1) {
  #     alwTable <- thisSexAlw
  #     sexProp <- thisALWSexProp
  #     thisAvgWgts<- cbind(thisSexAlw[,c("MISSION", "FSEX","FLEN")], prop.table(as.matrix(thisSexAlw[,4:ncol(thisSexAlw)]), margin=2)) %>% 
  #       mutate(across(-c("MISSION","FSEX", "FLEN"), ~ . * FLEN)) %>% 
  #       select(-MISSION,-FSEX,-FLEN) %>% colSums() %>% as.vector() %>% c(c("AVG_WGT", alw_sexes[w], NA),.) %>% 
  #       t() %>% 
  #       as.data.frame()
  #   }else{
  #     alwTable <- rbind.data.frame(alwTable, thisSexAlw)
  #     sexProp <- rbind.data.frame(sexProp,thisALWSexProp)
  #     thisAvgWgts <- cbind(thisSexAlw[,c("MISSION", "FSEX","FLEN")], prop.table(as.matrix(thisSexAlw[,4:ncol(thisSexAlw)]), margin=2)) %>% 
  #       mutate(across(-c("MISSION","FSEX", "FLEN"), ~ . * FLEN)) %>% 
  #       select(-MISSION,-FSEX,-FLEN) %>% colSums() %>% as.vector() %>% c(c("AVG_WGT", alw_sexes[w], NA),.) %>% 
  #       t() %>% 
  #       as.data.frame() %>% 
  #       bind_rows(.,thisAvgWgts)
  #   }
  # }
  # 
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
      sexProp <- thisSexProp
      thisAvgLengths<- cbind(thisSexAlk[,c("MISSION", "FSEX","FLEN")], prop.table(as.matrix(thisSexAlk[,4:ncol(thisSexAlk)]), margin=2)) %>% 
        mutate(across(-c("MISSION","FSEX", "FLEN"), ~ . * FLEN)) %>% 
        select(-MISSION,-FSEX,-FLEN) %>% colSums() %>% as.vector() %>% c(c("AVG_LENGTH", alk_sexes[s], NA),.) %>% 
        t() %>% 
        as.data.frame()
    }else{
      ageTable <- rbind.data.frame(ageTable, thisSexAlk)
      sexProp <- rbind.data.frame(sexProp,thisSexProp)
      thisAvgLengths <- cbind(thisSexAlk[,c("MISSION", "FSEX","FLEN")], prop.table(as.matrix(thisSexAlk[,4:ncol(thisSexAlk)]), margin=2)) %>% 
        mutate(across(-c("MISSION","FSEX", "FLEN"), ~ . * FLEN)) %>% 
        select(-MISSION,-FSEX,-FLEN) %>% colSums() %>% as.vector() %>% c(c("AVG_LENGTH", alk_sexes[s], NA),.) %>% 
        t() %>% 
        as.data.frame() %>% 
        bind_rows(.,thisAvgLengths)
    }
  }
  if(args$debug)message("Need to add recode FSEX to words on age_table")
  sexProp[!names(sexProp) %in% c("MISSION", "FSEX","FLEN")] <- sexProp[!names(sexProp) %in% c("MISSION","FSEX","FLEN")]/rowSums(sexProp[!names(sexProp) %in% c("MISSION","FSEX","FLEN")])
  sexProp[is.na(sexProp)] = 0
  
  #get the average length, by sex
  if (args$bySex){
    colnames(thisAvgLengths) <- colnames(ageTable)  
    ageTable <- rbind.data.frame(ageTable,thisAvgLengths)
    ageTable[] <- lapply(ageTable, function(x) type.convert(as.character(x), as.is = TRUE))
  }
  
  allAgeTable <- ageTable %>% filter(MISSION != "AVG_LENGTH") %>% mutate(FSEX = replace(FSEX, TRUE, 9))%>%
    group_by(MISSION, FSEX, FLEN) %>% 
    summarize_all(sum) %>% 
    as.data.frame() 
  
  allAvgLengths<- cbind(allAgeTable[,c("MISSION", "FSEX","FLEN")], prop.table(as.matrix(allAgeTable[,4:ncol(allAgeTable)]), margin=2)) %>% 
    mutate(across(-c("MISSION","FSEX", "FLEN"), ~ . * FLEN)) %>% 
    select(-MISSION,-FSEX,-FLEN) %>% colSums() %>% as.vector() %>% c(c("AVG_LENGTH", "COMBINED", NA),.) %>% 
    t() %>% 
    as.data.frame()
  colnames(allAvgLengths) <- colnames(ageTable)  
  ageTable <- rbind.data.frame(ageTable,allAvgLengths)
  ageTable[] <- lapply(ageTable, function(x) type.convert(as.character(x), as.is = TRUE))
  ageTable[is.nan(ageTable)] <- NA
  
  dfAge <- tblList$dataLF %>%  
    filter(TAXA_ == thisTAXA & !is.na(FLEN) & !is.na(CLEN)) %>%
    mutate(FSEX= case_when(args$bySex ==T ~ FSEX, 
                           args$bySex != T ~9)) %>% 
    select(-TAXA_) %>% 
    group_by(MISSION, STRAT, SETNO, FSEX, FLEN, LGRP) %>%
    summarise(CAGE = sum(CLEN), 
              .groups = 'keep') %>%
    as.data.frame()
  
  allLengths <- seq(min(dfAge$FLEN, na.rm = T),max(dfAge$FLEN, na.rm = T), by= thisLGRP)
  allSexes <- sort(unique(dfAge$FSEX))
  
  #create fake dfs for all values of length and age
  emptydf_age <- tblList$GSINF[,c("MISSION", "SETNO", "STRAT")]
  emptydf_age <- expandDF(templateDF = emptydf_age, keyFields = c("STRAT","MISSION","SETNO"), expandField = "FLEN", expandVals = allLengths)
  emptydf_age <- expandDF(templateDF = emptydf_age, keyFields = c("STRAT","MISSION","SETNO","FLEN"), expandField = "FSEX", expandVals = allSexes)
  emptydf_age$CAGE <- 0
  emptydf_age<- anti_join(emptydf_age, dfAge, by = c("MISSION", "STRAT", "SETNO", "FLEN","FSEX")) 
  dfAge<- rbind.data.frame(dfAge[,c("MISSION", "STRAT", "SETNO", "FSEX","FLEN", "CAGE")], emptydf_age)
  
  age_by_set<- dfAge %>% 
    left_join(sexProp, c("MISSION","FSEX","FLEN")) %>% 
    mutate(across(-c("MISSION","FSEX","FLEN","STRAT", "SETNO", "CAGE"), ~ . * CAGE)) %>% 
    select(-FLEN, -CAGE,-FSEX) %>% 
    group_by(MISSION, STRAT, SETNO) %>% 
    summarise(across(everything(), sum, na.rm = TRUE),.groups ="keep") %>% 
    ungroup() %>% 
    mutate(TOTAL = rowSums(.[,!names(.) %in% c("MISSION", "STRAT","SETNO")], na.rm = T)) %>% 
    select(MISSION, STRAT, SETNO, sort(names(.))) %>% 
    select(-one_of('TOTAL'), one_of('TOTAL')) %>%
    as.data.frame() 
  
  age_by_strat_mean <- age_by_set %>% 
    select(-SETNO,-TOTAL) %>% 
    group_by(MISSION, STRAT)  %>% 
    summarise(across(everything(), mean, na.rm = TRUE),.groups ="keep")   %>% 
    ungroup() %>% 
    mutate(TOTAL = rowSums(.[,!names(.) %in% c("MISSION", "STRAT")], na.rm = T)) %>% 
    select(MISSION, STRAT, sort(names(.))) %>% 
    select(-one_of('TOTAL'), one_of('TOTAL')) %>%
    as.data.frame() 
  if (args$debug)message("The bottom row total of age_by_strat_mean is not the sum of the columns - need to figure out what it is")
  
  age_by_strat_se <- age_by_set %>% 
    select(-SETNO) %>% 
    group_by(MISSION, STRAT)  %>% 
    summarise(across(everything(), st_err),.groups ="keep")  %>% 
    ungroup() %>% 
    # mutate(TOTAL = rowSums(.[,!names(.) %in% c("MISSION", "STRAT")], na.rm = T)) %>% 
    select(MISSION, STRAT, sort(names(.))) %>%
    select(-one_of('TOTAL'), one_of('TOTAL')) %>%
    as.data.frame()
  
  age_by_strat_total <- age_by_strat_mean %>% 
    select(-TOTAL) %>% 
    left_join(., tblList$GSSTRATUM[, c("STRAT", "TUNITS")], by="STRAT") %>% 
    ungroup() %>% 
    mutate(across(-c("MISSION","STRAT", "TUNITS"), ~ . * TUNITS)) %>% 
    ungroup() %>%  
    select(-TUNITS)  %>% 
    mutate(TOTAL = rowSums(.[,!names(.) %in% c("MISSION", "STRAT")], na.rm = T)) %>% 
    select(MISSION, STRAT, sort(names(.))) %>%
    select(-one_of('TOTAL'), one_of('TOTAL')) %>%
    as.data.frame()
  
  age_by_strat_total_tots <- colSums(age_by_strat_total[,!names(age_by_strat_total) %in% c("MISSION", "STRAT")])
  age_by_strat_total_tots<- c("TOTAL", NA, age_by_strat_total_tots)
  age_by_strat_total<- rbind.data.frame(age_by_strat_total,age_by_strat_total_tots)
  
  age_by_strat_total_se <- age_by_strat_se %>%
    left_join(., tblList$GSSTRATUM[, c("STRAT", "TUNITS")], by="STRAT") %>%
    ungroup() %>%
    mutate(across(-c("MISSION","STRAT", "TUNITS"), ~ . * TUNITS)) %>%
    select(-TUNITS)  %>%
    select(MISSION, STRAT, sort(names(.))) %>%
    as.data.frame()
  if (args$debug)message("The bottom row total of age_by_strat_total_se is not the sum of the columns - need to figure out what it is")
  
  
  results<- list(age_length_key= alk,
                 age_length_weight = alw,
                 age_table = ageTable,
                 age_by_set = age_by_set,
                 age_by_strat_mean = age_by_strat_mean,
                 age_by_strat_se = age_by_strat_se,
                 age_by_strat_total = age_by_strat_total,
                 age_by_strat_total_se = age_by_strat_total_se
  )
  if(args$debug){
    results[["debugdfAge"]]<-dfAge
    results[["sexProp"]]<-sexProp
  }
  return(results)
}