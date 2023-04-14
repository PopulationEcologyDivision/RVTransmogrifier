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
stratify_calcLengths<-function(tblList = NULL, dfNWSets = NULL, towDist = 1.75, bySex=F, useBins = F){
  if (bySex & length(unique(tblList$dataDETS$FSEX))==1) bySex <- FALSE
  
  if ("GSCAT_agg" %in% names(tblList)) {
    message("stratify_calcLengths can only be run on specific species - not taxonomic groups")
    return(NA)
  }
  
  theseMissions <- unique(tblList$GSMISSIONS$MISSION)
  thisSpec <- unique(dfNWSets$SPEC)
  if (length(thisSpec)>1)stop("stratify_calcAgeKey can only handle one species at a time")
  
 
  
  dataDETSFixed <- merge(dataDETSFixed, tblList$GSINF[,c("STRAT", "SETNO")], all.x = T)
  dataLFFixed <- merge(dataLFFixed, tblList$GSINF[,c("STRAT", "SETNO")], all.x = T)
browser()
    #<!agelen derived from dataLF>
  #binSizes and sexes may have been changed, so aggregate
  dataLFFixedAgg <- dataLFFixed %>%
    group_by(MISSION, STRAT, SETNO, SPEC, FSEX, FLEN) %>%
    summarise(CLEN = sum(CLEN), .groups = 'keep') %>%
    as.data.frame()
  # 
  # dataDETSFixed_agg <- dataDETSFixed %>% 
  #   group_by(MISSION, STRAT, SETNO, SPEC, FSEX, FLEN) %>%
  #   summarise(CLEN = sum(CLEN), .groups = 'keep') %>%
  #   as.data.frame()
  
  alk_pre <- filter(dataDETSFixed, !is.na(AGE)) %>%
    group_by(MISSION, SPEC, FSEX, FLEN, AGE) %>%
    summarise(CAGE = length(MISSION), .groups = 'keep') %>%
    as.data.frame()

  
  #make an initial df of all combos of mission, spec, ages, lengths and sexes
  alk_lengths <- seq(min(alk_pre$FLEN, na.rm = T),max(alk_pre$FLEN, na.rm = T), by=specBin)
  alk_ages <- seq(min(alk_pre$AGE, na.rm = T),max(alk_pre$AGE, na.rm = T), by=1)
  alk_sexes <- sort(unique(alk_pre$FSEX))
  al = as.data.frame(expand.grid(MISSION = theseMissions, SPEC = thisSpec, AGE = alk_ages, FLEN = alk_lengths, FSEX = alk_sexes, CAGE = NA))
  #drop the rows from the df for those combos for which we have real data
  al<- anti_join(al, alk_pre, by = c("MISSION", "SPEC", "AGE", "FSEX", "FLEN")) 
  alk_pre<- rbind.data.frame(alk_pre, al) #,"N"

  ##### 
  alk <- alk_pre %>%
    tidyr::pivot_wider(names_from = c(FSEX,AGE), values_from = CAGE, values_fill = NA)%>%
    arrange(MISSION, SPEC, FLEN) %>% 
    as.data.frame()
  
  colnames(alk) <- sub("0_", "UNKN_AGE_", colnames(alk))
  colnames(alk) <- sub("1_", "MALE_AGE_", colnames(alk))
  colnames(alk) <- sub("2_", "FEMALE_AGE_", colnames(alk))
  colnames(alk) <- sub("9_", "AGE_", colnames(alk))
  
  # Age Table --------------------------------------------------------------    
  alk_ap<- alk
browser()
    alk_ap<-alk_ap[,!colnames(alk_ap) %in% c("MISSION","SPEC","FLEN")]
    alk_ap[is.na(alk_ap)]<-0
  ages_prop<-prop.table(as.matrix(alk_ap),1) 
  ages_prop<-ifelse(is.nan(ages_prop),0,ages_prop)
  ages_prop<-as.data.frame(ages_prop)
  
  ##### 
  alw_pre<-  stats::aggregate(FWT~MISSION+SPEC+AGE+FLEN+FSEX,data=dataDETSFixed,FUN=mean)
  a2 = expand.grid(MISSION = theseMissions, SPEC = thisSpec, AGE = alk_ages, FLEN = -1, FWT = -1, FSEX = alk_sexes)
  
  alw_pre=rbind(alw_pre,a2)
  alw_pre<-alw_pre[order(alw_pre$FSEX,alw_pre$AGE,alw_pre$FLEN),]

  alw <- alw_pre %>%
    filter(FLEN>0) %>% 
    tidyr::pivot_wider(id_cols = c(MISSION, SPEC, FLEN), names_from = c(FSEX,AGE), values_from = FWT) %>% 
    mutate_at(vars(-c(MISSION, SPEC, FLEN)), function(x) round(x/1000,6)) %>% 
    as.data.frame()
  
  colnames(alw) <- sub("0_", "UNKN_AGE_", colnames(alw))
  colnames(alw) <- sub("1_", "MALE_AGE_", colnames(alw))
  colnames(alw) <- sub("2_", "FEMALE_AGE_", colnames(alw))
  colnames(alw) <- sub("9_", "AGE_", colnames(alw))

  message("think we can add average weight by adding all of the values for a length , and then 
          dividing by count of animals of each group found in dataDETSFixed (then the same again for each age)")
  # dataDETSFixed[which(dataDETSFixed$FLEN==4.5),]
  
  # 
  # dfLen <-dfNWSets
  # dfLen <- merge(dfLen, dataLFFixed[!is.na(dataLFFixed$FLEN),])
  # dfLen <- merge(dfLen, dataDETSFixed[!is.na(dataDETSFixed$AGE),c("MISSION", "SETNO", "SPEC", "FSEX", "FLEN", "AGE")])
  # 
  # 
  # colnames(dfLen)[colnames(dfLen)=="CLEN"] <- "CAGE"
  # 
  # dfLen[which(is.na(dfLen$FLEN)),"FLEN"] <- 0
  # dfLen[which(is.na(dfLen$CAGE)),"CAGE"] <- 0
  # dfLen[which(is.na(dfLen$FSEX)),"FSEX"] <- 0
  # 
  
  
  
  # "STRAT","MISSION","SETNO", "FSEX", "FLEN","CAGE"
  #create fake dfs for all values of length and age
  emptydf_len <- expandDF(templateDF = dfLen, keyFields = c("STRAT","MISSION","SETNO"), expandField = "FLEN", expandVals = allLengths)
  emptydf_len <- expandDF(templateDF = emptydf_len, keyFields = c("STRAT","MISSION","SETNO","FLEN"), expandField = "FSEX", expandVals = unique(dfLen$FSEX))
  
  emptydf_len$CAGE <- 0
  dfLen$N <- 1
  emptydf_len$N <- 0
  emptydf_len<- anti_join(emptydf_len, dfLen, by = c("MISSION", "STRAT", "SETNO", "FLEN", "FSEX","CAGE")) #,"N"))
  dfLen<- rbind.data.frame(dfLen[,c("MISSION", "STRAT","SETNO", "FLEN", "FSEX","CAGE", "N")], emptydf_len) #,"N"
  # message("Mike - you got to here.  length_by_set doesn't perfectly match stratisfy - check 18.5 lengths of haddock")
  browser()
  # hk2<- dfLen %>%
  #   dplyr::group_by(MISSION, STRAT, SETNO, FLEN, FSEX) %>%
  #   dplyr::summarize(.groups = "keep", theSum = sum(CAGE, na.rm=TRUE)) %>%
  #   as.data.frame()
  # print(hk2[hk2$FLEN==36.5 & hk2$SETNO==63,])
  # 
  #View(dfLen[dfLen$FLEN==18.5 & dfLen$SETNO==64,])
  # View(tblList$dataLF[tblList$dataLF$SETNO=="197",])
  # View(tblList$dataDETS[tblList$dataDETS$SETNO==197 ,])
  #dataLF 2@36 3@37 = 5
  length_by_set <- dfLen %>%
    group_by(MISSION, STRAT, SETNO, FLEN, FSEX) %>%
    summarise(CNT = sum(CAGE), .groups = 'keep') %>%
    tidyr::pivot_wider(names_from = c(FSEX,FLEN), values_from = CNT, values_fill = 0)%>%
    arrange(STRAT, MISSION, SETNO) %>% 
    as.data.frame()
  colnames(length_by_set) <- sub("0_", "UNKN_LEN_", colnames(length_by_set))
  colnames(length_by_set) <- sub("1_", "MALE_LEN_", colnames(length_by_set))
  colnames(length_by_set) <- sub("2_", "FEMALE_LEN_", colnames(length_by_set))
  colnames(length_by_set) <- sub("9_", "LEN_", colnames(length_by_set))
  
  
  # colnames(length_by_set[4:ncol(length_by_set)]) <-paste0(stringr::str_extract(colnames(length_by_set[4:ncol(length_by_set)]), "^(.*_)(.*?)$", group = 1),
  #                        sprintf("%05.1f",as.numeric(stringr::str_extract(colnames(length_by_set[4:ncol(length_by_set)]), "^(.*_)(.*?)$", group = 2))))
  # length_by_set <- length_by_set[, sort(colnames(length_by_set))]
  
  length_by_strat_mean1<-stats::setNames(stats::aggregate(list(
    MEAN = length_by_set[!colnames(length_by_set) %in% c("STRAT","MISSION","SETNO")]), 
    by=list(STRAT=length_by_set$STRAT), 
    FUN=mean), c("STRAT",colnames(length_by_set)[!colnames(length_by_set) %in% c("STRAT","MISSION","SETNO")]))
  length_by_strat_mean[is.na(length_by_strat_mean)]<-0
  
  length_by_strat_mean <- length_by_set %>%
    select(-SETNO) %>% 
    group_by(MISSION, STRAT) %>%
    summarise_all(mean, .groups = 'keep') 
  length_by_strat_mean <- round(length_by_strat_mean[,!names(length_by_strat_mean) %in% c("MISSION", "STRAT")],4)
  
  length_by_strat_se <- length_by_set %>%
    select(-SETNO) %>% 
    group_by(MISSION, STRAT) %>%
    # mutate_if(is.numeric, ~na_if(., 0)) %>% 
    summarise(across(where(is.numeric), ~ sd(.x)/ sqrt(length(.x)), .names = "SE_{.col}"))
  length_by_strat_se <- round(length_by_strat_se[,!names(length_by_strat_se) %in% c("MISSION", "STRAT")],4)
  
  
  
  length_by_strat_mean[is.na(length_by_strat_mean)]<-0
  
  length_by_strat_se<-stats::setNames(stats::aggregate(list(
    MEAN_SE=length_by_set[!colnames(length_by_set) %in% c("STRAT","MISSION","SETNO")]), 
    by=list(STRAT=length_by_set$STRAT), 
    FUN=Mar.utils::st_err), c("STRAT",colnames(length_by_set_dat)))
  length_by_strat_se[is.na(length_by_strat_se)]<-0
  
  
  
  length_total <-  merge(length_by_strat_mean, tblList$GSSTRATUM[,c("STRAT","TUNITS")])
  length_total <- cbind(length_total[1],
                        length_total[2:(ncol(length_total)-1)]*length_total$TUNITS)    
  
  length_total_se =  merge(length_by_strat_se, tblList$GSSTRATUM[,c("STRAT","TUNITS")])
  length_total_se = cbind(length_total_se[1],
                          length_total_se[2:(ncol(length_total_se)-1)]*length_total_se$TUNITS)
  results=list(dfLen = dfLen,
               length_by_set= length_by_set,
               length_by_strat_mean = length_by_strat_mean,
               length_by_strat_se = length_by_strat_se,
               length_total = length_total,
               length_total_se = length_total_se,
               lset = lset)
  
  return(results)
}
