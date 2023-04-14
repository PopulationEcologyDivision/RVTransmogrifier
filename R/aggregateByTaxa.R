#' @title aggregateByTaxa
#' @description This function .
#' @param tblList the default is \code{NULL}. This is a list populated with all RV dataframes that
#' should have filtering applied to them.
#' @param code the default is \code{NULL}. If data should be limited to a particular species, enter 
#' the species code here.
#' @param aphiaid the default is \code{NULL}. If data should be limited to a particular aphiaid, 
#' enter the aphiaid here.
#' @param taxa the default is \code{NULL}. Any value found in any of "SCI_NAME", "KINGDOM", 
#' "PHYLUM", "CLASS", "ORDER", "FAMILY", or "GENUS" can be specified (e.g. \code{taxa=c("GADIDAE")})

#' @param ... other arguments passed to methods (e.g. 'debug' and 'quiet')
#' @returns #' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
aggregateByTaxa <- function(tblList = NULL, code=NULL, aphiaid=NULL, taxa = NULL, args = NULL){
  if(is.null(args))args<- list()
  if(!is.null(code)) args$code<- code
  if(!is.null(aphiaid)) args$aphiaid <- aphiaid
  if(!is.null(taxa)) args$taxa <- taxa
  args <- setDefaultArgs(args=args)
  
  #All data arranged by code, so no need to aggregate if code was specified
  if(!is.null(args$code))return(tblList)
  GSCAT_agg  <-  tblList$GSCAT
  dataLF_agg <-  tblList$dataLF
  dataDETS_agg <-  tblList$dataDETS
  #remove the spp code and collapse on the taxa_ and taxarank_
  GSCAT_agg   <- merge(GSCAT_agg, tblList$GSSPECIES[,c("CODE","TAXA_", "TAXARANK_","APHIA_ID", "SCI_NAME")], by.x="SPEC", by.y="CODE")
  dataLF_agg  <- merge(dataLF_agg, tblList$GSSPECIES[,c("CODE","TAXA_", "TAXARANK_","APHIA_ID", "SCI_NAME")], by.x="SPEC", by.y="CODE")
  dataDETS_agg <- merge(dataDETS_agg, tblList$GSSPECIES[,c("CODE","TAXA_", "TAXARANK_","APHIA_ID", "SCI_NAME")], by.x="SPEC", by.y="CODE")
  
  GSCAT_agg$SPEC <- dataLF_agg$SPEC <- dataDETS_agg$SPEC <- NULL
  if(!is.null(args$aphiaid)) GSCAT_agg$TAXA_ <- GSCAT_agg$TAXARANK_ <- dataLF_agg$TAXA_ <- dataLF_agg$TAXARANK_ <- dataDETS_agg$TAXA_ <- dataDETS_agg$TAXARANK_<- NULL
  if(!is.null(args$taxa)) GSCAT_agg$APHIA_ID <- GSCAT_agg$SCI_NAME <- dataLF_agg$APHIA_ID <- dataLF_agg$SCI_NAME <- dataDETS_agg$APHIA_ID <- dataDETS_agg$SCI_NAME <- NULL
  
  GSCAT_agg <- GSCAT_agg %>%
    dplyr::group_by(dplyr::across(c(-TOTNO, -TOTWGT))) %>%
    dplyr::summarise(TOTNO=sum(TOTNO),
                     TOTWGT=sum(TOTWGT),
                     .groups = "keep")%>%
    as.data.frame()
  
  dataLF_agg <- dataLF_agg %>%
    dplyr::group_by(dplyr::across(c(-CLEN))) %>%
    dplyr::summarise(CLEN=sum(CLEN),
                     .groups = "keep")%>%
    as.data.frame()
  
  tblList$GSCAT <- GSCAT_agg
  tblList$dataLF <- dataLF_agg
  tblList$dataDETS <- dataDETS_agg
  return(tblList)
}