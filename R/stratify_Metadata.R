stratify_Metadata <- function(...){
  argsFn <- as.list(environment())
  argsFn[["tblList"]] <- NULL
  argsUser <- list(...)
  args <- do.call(set_defaults, list(argsFn=argsFn, argsUser=argsUser))

  
  paramDf <- args
  paramDf <- replace(paramDf, sapply(paramDf, is.data.frame), "see <results>$params$fleet$...")
  paramDf[lengths(paramDf)>1]<- paste0(paramDf[lengths(paramDf)>1])
  paramDf <- replace(paramDf, sapply(paramDf, is.null), "<NULL>")
  paramDf <- data.frame(PARAMETER=names(paramDf), VALUE = unlist(paramDf), row.names = NULL)
  paramDf$SOURCE <- NA
  paramDf[is.na(paramDf$SOURCE),"SOURCE"] <- "default value (overwritable by user)"
  paramDf[paramDf$PARAMETER %in% names(argsUser),"SOURCE"] <- "user-supplied"
  paramDf[paramDf$PARAMETER %in% names(argsFn),"SOURCE"] <- "hardcoded for this fleet"

  
  toMatch <- c("TRUE", "FALSE","c\\(.*","^[0-9]*$")
  paramDf[!grepl(paste(toMatch, collapse = '|'),paramDf$VALUE),"VALUE"]<- paste0('"',paramDf[!grepl(paste(toMatch, collapse = '|'),paramDf$VALUE),"VALUE"],'"')
  paramDf <-  paramDf[with(paramDf,order(-rank(SOURCE), PARAMETER)),c( "SOURCE", "PARAMETER","VALUE")]
  paramDf$VALUE<- ifelse(nchar(paramDf$VALUE)>150,"<Too long to display>",paramDf$VALUE)
  paramDf <- rbind(paramDf, c("metadata","Date Run", format(Sys.Date(), "%Y-%m-%d")))
  if(all(is.na(utils::packageDescription("RVTransmogrifier")))){
    paramDf <- rbind(paramDf, c("metadata","RVTransmogrifier not installed"))
  }else{
    paramDf <- rbind(paramDf, c("metadata","RVTransmogrifier version", utils::packageDescription("RVTransmogrifier")$Version))
  }
}