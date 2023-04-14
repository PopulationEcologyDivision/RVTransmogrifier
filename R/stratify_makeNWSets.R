stratify_makeNWSets <- function(tblList = NULL){
  browser()

  taxa<- unique(dfRawCatch[!is.na(dfRawCatch[,t_field]), t_field])
  res<-list()
  for(t in 1:length(taxa)){
    thisSpecData <- dfRawCatch[dfRawCatch[,t_field] == taxa[t], ]
    thisTaxSets<-merge(tblList$GSINF, thisSpecData, all.x=T)
    thisTaxSets[which(is.na(thisTaxSets[,t_field])),t_field]<- taxa[t]
    thisTaxSets[which(is.na(thisTaxSets$TOTWGT)),c('TOTWGT','TOTNO')]<- 0
    thisTaxSets[which(is.na(thisTaxSets$TOTWGT_RAW)),c('TOTWGT_RAW')]<- 0
    thisTaxSets[which(is.na(thisTaxSets$TOTNO_RAW)),c('TOTNO_RAW')]<- 0
    #thisTaxSets$TOTNO_RAW <- thisTaxSets$TOTWGT_RAW <- NULL
    thisTaxSets$TOTWGT[which(!is.finite(thisTaxSets$TOTWGT))] <-1
    thisTaxSets$TOTNO[which(!is.finite(thisTaxSets$TOTNO))] <- 1
    thisTaxSets <- thisTaxSets[c('MISSION','SETNO','STRAT',setdiff(colnames(thisTaxSets), colnames(tblList$GSINF)))]
    names(thisTaxSets)[names(thisTaxSets) == "AREA"] <- "UNIT_AREA"
    thisTaxSets <- thisTaxSets[with(thisTaxSets,order(MISSION, SETNO)),]
    res[[paste0("sp_",taxa[t])]] <- thisTaxSets
  }
  return(res)
}