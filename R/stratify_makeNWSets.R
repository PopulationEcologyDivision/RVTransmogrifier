# stratify_makeNWSets <- function(tblList = NULL){
#   taxa<- unique(tblList$GSCAT[!is.na(tblList$GSCAT$TAXA_), "TAXA_"])
#   res<-list()
#   for(t in 1:length(taxa)){
#     thisSpecData <- tblList$GSCAT[tblList$GSCAT$TAXA_ == taxa[t], ]
#     thisTaxSets <- merge(tblList$GSINF, thisSpecData, all.x=T)
#     thisTaxSets[which(is.na(thisTaxSets$TAXA_)),"TAXA_"]<- taxa[t]
#     thisTaxSets[which(is.na(thisTaxSets$TOTWGT)),c('TOTWGT','TOTNO')]<- 0
#     thisTaxSets[which(is.na(thisTaxSets$TOTWGT_RAW)),c('TOTWGT_RAW')]<- 0
#     thisTaxSets[which(is.na(thisTaxSets$TOTNO_RAW)),c('TOTNO_RAW')]<- 0
#     #thisTaxSets$TOTNO_RAW <- thisTaxSets$TOTWGT_RAW <- NULL
#     thisTaxSets$TOTWGT[which(!is.finite(thisTaxSets$TOTWGT))] <-1
#     thisTaxSets$TOTNO[which(!is.finite(thisTaxSets$TOTNO))] <- 1
#     thisTaxSets <- thisTaxSets[c('MISSION','SETNO','STRAT',setdiff(colnames(thisTaxSets), colnames(tblList$GSINF)))]
#     names(thisTaxSets)[names(thisTaxSets) == "AREA"] <- "UNIT_AREA"
#     thisTaxSets <- thisTaxSets[with(thisTaxSets,order(MISSION, SETNO)),]
#     res[[paste0("sp_",taxa[t])]] <- thisTaxSets
#   }
#   return(res)
# }