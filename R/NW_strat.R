#' @title NW_strat
#' @description This function ...
#' @param tblList the default is \code{NULL}. This is a list populated with all RV dataframes. Prior 
#' to running this function they should all have been filtered via \code{propagateChanges()}.
#' @param dfNWSets  this is the output from \code{NW_sets()}.
#' @param ... other arguments passed to methods (i.e. 'debug' and 'quiet')
#' @returns ...
#' @author Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @importFrom dplyr %>%
#' @export
NW_strat<-function(tblList = NULL, dfNWSets= NULL, ...){
  args <- list(...)
  debug <- ifelse(is.null(args$debug), F, args$debug) 
  quiet <- ifelse(is.null(args$quiet), F, args$quiet)
  multiSp <- ifelse(is.null(args$multiSp), F, args$multiSp)
  
  tblList$GSSTRATUM <- addTUNITS(tblList$GSSTRATUM)
  tmp <- merge(dfNWSets,tblList$GSSTRATUM, by = 'STRAT')
  tmp$BIOMASS<-tmp$TOTWGT*tmp$TUNITS
  tmp$ABUND<-tmp$TOTNO*tmp$TUNITS
  if(!multiSp){
    tmp.cnt <- tmp %>%
      dplyr::group_by(STRAT, SPEC) %>%
      dplyr::summarise(COUNT = length(STRAT), 
                .groups = "keep")%>%
      as.data.frame()
    tmp.sum <- tmp %>%
      dplyr::group_by(STRAT, SPEC) %>%
      dplyr::summarise(TOT_WGT = sum(TOTWGT),
                TOT_NO = sum(TOTNO), 
                .groups = "keep")%>%
      as.data.frame()
    tmp.mean <- tmp %>%
      dplyr::group_by(STRAT, SPEC) %>%
      dplyr::summarise(MEAN_WGT = mean(TOTWGT),
                MEAN_NO = mean(TOTNO),
                BIOMASS = mean(BIOMASS),
                ABUND = mean(ABUND), 
                .groups = "keep")%>%
      as.data.frame()
    tmp.sterr <- tmp %>%
      dplyr::group_by(STRAT, SPEC) %>%
      dplyr::summarise(ST_ERR_WGT = Mar.utils::st_err(TOTWGT),
                ST_ERR_NO = Mar.utils::st_err(TOTNO),
                ST_ERR_BIOMASS = Mar.utils::st_err(BIOMASS),
                ST_ERR_ABUND = Mar.utils::st_err(ABUND), 
                .groups = "keep")%>%
      as.data.frame()
    nw<-merge(tmp.cnt,tmp.sum,by=c("STRAT","SPEC"))
    nw<-merge(nw, tmp.mean,by=c("STRAT","SPEC"))
    nw<-merge(nw, tmp.sterr,by=c("STRAT","SPEC"))
  }else{
    tmp.cnt <- tmp %>%
      dplyr::group_by(STRAT) %>%
      dplyr::summarise(COUNT = length(STRAT), 
                .groups = "keep")%>%
      as.data.frame()
    tmp.sum <- tmp %>%
      dplyr::group_by(STRAT) %>%
      dplyr::summarise(TOT_WGT = sum(TOTWGT),
                TOT_NO = sum(TOTNO), 
                .groups = "keep")%>%
      as.data.frame()
    tmp.mean <- tmp %>%
      dplyr::group_by(STRAT) %>%
      dplyr::summarise(MEAN_WGT = mean(TOTWGT),
                MEAN_NO = mean(TOTNO),
                BIOMASS = mean(BIOMASS),
                ABUND = mean(ABUND), 
                .groups = "keep")%>%
      as.data.frame()
    tmp.sterr <- tmp %>%
      dplyr::group_by(STRAT) %>%
      dplyr::summarise(ST_ERR_WGT = Mar.utils::st_err(TOTWGT),
                ST_ERR_NO = Mar.utils::st_err(TOTNO),
                ST_ERR_BIOMASS = Mar.utils::st_err(BIOMASS),
                ST_ERR_ABUND = Mar.utils::st_err(ABUND), 
                .groups = "keep")%>%
      as.data.frame()
    nw<-merge(tmp.cnt,tmp.sum,by=c("STRAT"))
    nw<-merge(nw, tmp.mean,by=c("STRAT"))
    nw<-merge(nw, tmp.sterr,by=c("STRAT"))
  }
  nw[is.na(nw)]<-0
  return(nw)
}