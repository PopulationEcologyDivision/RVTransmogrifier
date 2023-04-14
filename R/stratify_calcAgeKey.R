stratify_calcAgeKey<-function(tblList = NULL, dfNWSets=NULL, lengthsTotals = NULL, lset = NULL, ageBySex = F, useBins=T){
  if (ageBySex & length(unique(tblList$dataDETS$FSEX))==1) ageBySex <- FALSE
  thisSpec<-  unique(dfNWSets$SPEC)
  
  if ("GSCAT_agg" %in% names(tblList)) {
    message("stratify_calcAgeKey can only be run on specific species - not taxonomic groups")
    return(NA)
  }
  if (length(thisSpec)>1)stop("stratify_calcAgeKey can only handle one species at a time")
  
  alw_ <- tblList$dataDETS[tblList$dataDETS$SPEC %in% thisSpec & !is.na(tblList$dataDETS$FWT) & !is.na(tblList$dataDETS$AGE),
                           c("SPEC", "MISSION", "SETNO", "AGE", "FLEN", "FSEX","FWT")]
  alk_   <- tblList$dataDETS[tblList$dataDETS$SPEC %in% thisSpec,
                             c("SPEC", "MISSION", "SETNO", "FSEX","FLEN", "AGE")]
  
  alk_[is.na(alk_$FLEN),"FLEN"] <- 0
  
  if(useBins){
    specBin <- unique(na.omit(tblList$GSSPECIES[tblList$GSSPECIES$CODE %in% thisSpec,"LGRP"]))
    specBin <- ifelse(length(specBin>0), specBin, 1)
    
    alw_$FLEN <- binSizes(specBin, alw_$FLEN)
    alk_$FLEN <- binSizes(specBin, alk_$FLEN)
  }else{
    specBin <- 1
  }
  
  if (ageBySex){
    #if present, berried females (3) are re-categorized as 2
    if (nrow(alk_[which(alk_$FSEX==3),])>0) alk_[which(alk_$FSEX==3),"FSEX"] <- 2
    if (nrow(alw_[which(alw_$FSEX==3),])>0) alw_[which(alw_$FSEX==3),"FSEX"] <- 2
  }else{
    #instead of handling sexed/unsexed data differently, lets just call all FSEX = 9 for unsexed 
    alk_$FSEX <- 9
    alw_$FSEX <- 9
  }
  
  alk_<- alk_[!is.na(alk_$AGE),]
  
  allLengths <- seq(min(alk_$FLEN, na.rm = T),max(alk_$FLEN, na.rm = T), by=specBin)
  allAges    <- seq(min(alk_$AGE, na.rm = T),max(alk_$AGE, na.rm = T), by=1)
  
  #create fake dfs for all values of length and age
  emptydf_alk <- expandDF(templateDF = alk_, keyFields = c("SPEC", "MISSION", "SETNO"), expandField = "AGE", expandVals = allAges)
  emptydf_alk <- expandDF(templateDF = emptydf_alk, keyFields = c("SPEC", "MISSION", "SETNO", "AGE"), expandField = "FLEN", expandVals = allLengths)
  emptydf_alk <- expandDF(templateDF = emptydf_alk, keyFields = c("SPEC", "MISSION", "SETNO", "AGE","FLEN"), expandField = "FSEX", expandVals = unique(alk_$FSEX))
  
  emptydf_alw <- expandDF(templateDF = alw_, keyFields = c("SPEC", "MISSION", "SETNO"), expandField = "AGE", expandVals = allAges)
  emptydf_alw <- expandDF(templateDF = emptydf_alw, keyFields = c("SPEC", "MISSION", "SETNO", "AGE"), expandField = "FLEN", expandVals = allLengths)
  emptydf_alw <- expandDF(templateDF = emptydf_alw, keyFields = c("SPEC", "MISSION", "SETNO", "AGE","FLEN"), expandField = "FSEX", expandVals = unique(alk_$FSEX))
  emptydf_alw$FWT <- NA
  
  #N field will allow differentiation between real and fake data
  alk_$N <- 1
  alw_$N <- 1
  emptydf_alk$N <- 0
  emptydf_alw$N <- 0
  
  emptydf_alk<- anti_join(emptydf_alk, alk_, by = c("SPEC", "MISSION", "SETNO", "AGE", "FLEN", "FSEX","N"))
  emptydf_alw<- anti_join(emptydf_alw, alw_, by = c("SPEC", "MISSION", "SETNO", "AGE", "FLEN", "FSEX","FWT","N"))
  
  alk_<- rbind.data.frame(alk_, emptydf_alk)
  alw_<- rbind.data.frame(alw_, emptydf_alw)
  
  
  alk <- alk_ %>%
    group_by(FLEN, FSEX, AGE) %>%
    summarise(CNT = sum(N), .groups = 'keep')%>%
    tidyr::pivot_wider(names_from = c(FSEX,AGE), values_from = CNT, values_fill = 0)%>%
    arrange(FLEN) %>% 
    as.data.frame()
  rownames(alk) <- alk$FLEN
  alk$FLEN <- NULL
  alk <- round(alk,4)
  
  colnames(alk) <- sub("0_", "UNKN_AGE_", colnames(alk))
  colnames(alk) <- sub("1_", "MALE_AGE_", colnames(alk))
  colnames(alk) <- sub("2_", "FEMALE_AGE_", colnames(alk))
  colnames(alk) <- sub("9_", "AGE_", colnames(alk))
  colnames(alk) <-paste0(stringr::str_extract(colnames(alk), "^(.*_)(.*?)$", group = 1),
                         sprintf("%03d",as.numeric(stringr::str_extract(colnames(alk), "^(.*_)(.*?)$", group = 2))))
  alk <- alk[, sort(colnames(alk))]
  browser()
  alw <- alw_ %>%
    group_by(FLEN, FSEX, AGE) %>%
    summarise(FWT = mean(FWT, na.rm = TRUE), .groups = 'keep')%>%
    tidyr::pivot_wider(names_from = c(FSEX,AGE), values_from = FWT, values_fill = 0)%>%
    arrange(FLEN) %>% 
    as.data.frame()
  rownames(alw) <- alw$FLEN
  alw$FLEN <- NULL
  alw[is.na(alw)]<-0
  alw <- alw/1000
  alw <- round(alw,4)
  colnames(alw) <- sub("0_", "UNKN_AGE_", colnames(alw))
  colnames(alw) <- sub("1_", "MALE_AGE_", colnames(alw))
  colnames(alw) <- sub("2_", "FEMALE_AGE_", colnames(alw))
  colnames(alw) <- sub("9_", "AGE_", colnames(alw))
  colnames(alw) <-paste0(stringr::str_extract(colnames(alw), "^(.*_)(.*?)$", group = 1),
                         sprintf("%03d",as.numeric(stringr::str_extract(colnames(alw), "^(.*_)(.*?)$", group = 2))))
  alw <- alw[, sort(colnames(alw))]
  
  #up to line 289
 
  ages_prop<-prop.table(as.matrix(alk),1) 
  ages_prop[is.na(ages_prop)]<-0
  ages_prop<-as.data.frame(ages_prop)
  lengths = colSums(lengthsTotals[,2:ncol(lengthsTotals)])   
  lengths = lengths[lengths>0]
  lengths<-as.data.frame(lengths)
  
  ages_prop_l<-merge(ages_prop,lengths, by="row.names")
  rownames(ages_prop_l)<-ages_prop_l$Row.names
  ages_prop_l$Row.names<-NULL
  ages_prop_l[, -which(names(ages_prop_l) == "lengths")]<-ages_prop_l[, -which(names(ages_prop_l) == "lengths")] * ages_prop_l[["lengths"]]
  age_table<-ages_prop_l[, -which(names(ages_prop_l) == "lengths")]
  age_table[is.na(age_table)]<-0
  age_table <- age_table[, sort(colnames(age_table))]
  
  res = list()
  res$AGE_LEN_KEY=alk
  res$AGE_LEN_WT=alw
  return(res)
}



# alw<-stats::aggregate(FWT~AGE+FLEN,data=agelen,FUN=mean)
# 
# fakeAgeRows = expand.grid(AGE = allAges, FLEN=-1, FWT = -1)
# alw=rbind(alw,fakeAgeRows)
# 
# alw<-alw[order(alw$AGE,alw$FLEN),]
# alw$FWT = alw$FWT / 1000
# alw = stats::reshape(alw,idvar='FLEN',timevar='AGE',direction='wide')
# alw = alw[alw$FLEN !=-1,]