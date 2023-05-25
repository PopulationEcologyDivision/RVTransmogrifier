where_now <- function (callstack = sys.calls()) 
{
  clean_where <- function(x) {
    val <- sapply(x, function(xt) {
      z <- strsplit(paste(xt, collapse = "\t"), "\t")[[1]]
      switch(z[1], lapply = z[3], sapply = z[3], do.call = z[2], 
             `function` = "FUN", source = "###", eval.with.vis = "###", 
             z[1])
    })
    val[grepl("\\<function\\>", val)] <- "FUN"
    val <- val[!grepl("(###|FUN)", val)]
    val <- utils::head(val, -1)
    paste(val, collapse = "|")
  }
  cs <- callstack
  cs <- clean_where(cs)
  return(cs)
}

st_err <- function (x = NULL) {
  stats::sd(x)/sqrt(length(x))
}
combine_lists <- function(primary = NULL, ancilliary = NULL){ 
  new <- ancilliary[setdiff(names(ancilliary),names(primary))]
  # discarded <- ancilliary[intersect(names(ancilliary),names(primary))]
  kept <- c(primary, new)
  return(kept)
}


sexifyNames <- function(df, desc=NULL){
  colnames(df) <- gsub(pattern = "^0_", paste0("UNKN_",desc,"_"), colnames(df))
  colnames(df) <- gsub(pattern = "^1_", paste0("MALE_",desc,"_"), colnames(df))
  colnames(df) <- gsub(pattern = "^2_", paste0("FEMALE_",desc,"_"), colnames(df))
  colnames(df) <- gsub(pattern = "^9_", paste0(desc,"_"), colnames(df))
  return(df)
}


unSexifyNames <- function(df, desc=NULL){
  colnames(df) <- gsub(pattern = paste0("^UNKN_",desc,"_"), paste0("0_"), colnames(df))
  colnames(df) <- gsub(pattern = paste0("^MALE_",desc,"_"), paste0("1_"), colnames(df))
  colnames(df) <- gsub(pattern = paste0("^FEMALE_",desc,"_"), paste0("2_"), colnames(df))
  colnames(df) <- gsub(pattern = paste0("^",desc,"_"), paste0("9_"), colnames(df))
  return(df)
}

set_defaults <- function(debug = FALSE, 
                         quiet = TRUE,
                         survey=NULL,
                         years= NULL,
                         type1TowsOnly = TRUE,
                         keep_nullsets= TRUE, 
                         towDist = 1.75, 
                         bySex=F, 
                         useBins = F,
                         code = NULL, 
                         aphiaid = NULL, 
                         taxa= NULL, 
                         taxaAgg = FALSE, ...){
  
  defaults <- as.list(environment())
  defaults[["tblList"]] <- NULL
  sentArgs <- list(...)
  #ensure hardcoded args take priority over user args
  submittedArgs <- combine_lists(primary = sentArgs$argsFn, ancilliary = sentArgs$argsUser)
  #ensure submitted args take priority over default args
  argg <- combine_lists(primary =  submittedArgs, ancilliary = defaults)
  # browser()
  # jakes <- setdiff(names(sentArgs),names(defaults))
  # if (length(jakes)>0){
  #    warning(paste0("This package does not understand the following parameter(s): ",paste0(jakes,collapse = ",")))
  # }
  
  return(argg)
}

binSizes <- function(bin, value){
  (floor(value/bin)*bin) + ((bin*.5)-0.5)  
}

expandDF <- function(templateDF = NULL, keyFields = NULL, expandField= NULL, expandVals = NULL){
  #This function takes a df and repeats some key fields for each value found in expandVals
  if (is.null(expandField))stop("expandField must have a value")
  newDF<- unique(templateDF[,keyFields])
  newDF <- merge(unique(newDF), expandVals)
  colnames(newDF)[colnames(newDF)=="y"] <- expandField
  return(newDF)
}

#' @title addTUNITS
#' @description This function .
#' @param stratum the default is \code{NULL}. This is a dataframe of strata, each with an AREA field
#' in square nautical miles.
#' @param towDist the default is \code{1.75}. 
#' @param wingspread the default is \code{41}. 
#' @returns the original \code{stratum} object, but with an additional \code{TUNITS} field.
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @note GSWARPOUT, GSCRUISELIST and GSSPEC can NOT be used to filter the other tables.
#' @export
addTUNITS<- function(stratum = NULL,towDist = 1.75, wingspread = 41){
  #calculate strat areas into tunits; US nautical mile is 6080.2ft
  stratum$TUNITS <- NA
  stratum$TUNITS <-stratum$AREA/(towDist * (wingspread/6080.2))
  return(stratum)
}
#' @title roundDD2Min
#' @description This function can be used to round decimal degrees to the nearest geographic minute.  
#' It can be set to round up or down, and is useful for generating plots with boundaries that make sense.
#' @param x this is value that should be rounded.
#' @param how the default is \code{"round"}, but values of \code{"ceiling"} and \code{"floor"} are also 
#' acceptable. \code{"round"} just rounds the value, while the others forcibly round it up or down 
#' (respectively) to the nearest minute.
#' @param nearestMin the default is \code{1}, but values between 1 and 60 make sense. 
#' @param digits the default is \code{4}.  This is how many decimal places the resultant data should be rounded to.
#' @returns the original value, but rounded.
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
roundDD2Min<-function(x=NULL, how = "round", nearestMin = 1, digits=4){
  minDD = 0.016666666666 #this is 1 min in DD
  base = nearestMin*minDD
  if (how =="round"){
    res <- base * round(x/base)
  }else if (how =="ceiling"){
    res <- base * ceiling(x/base)
  }else if (how=="floor"){
    res <- base * floor(x/base)
  }
  res <- round(res,digits)
  return(res)
}

getBbox <- function(filterField = "STRATA_ID", filterVals=NULL){
  if(all(filterVals %in% c(396:411))){
    strata <- RVSurveyData::strataMar4VSW_sf 
  }else{
    strata <- RVSurveyData::strataMar_sf
  }
  strata <-  sf::st_transform(strata, crs = 4326)
  if(!is.null(filterField)&& !is.null(filterVals)) strata<-strata[strata[[filterField]] %in% filterVals,]
  bbox <- sf::st_bbox(strata, na.rm=T)
  return(bbox)
}

ggBathy<- function(plotBathy = "FILL", bathyIntervals = 100){
  ggItems<- list()
  if (!is.null(plotBathy)){
    bathyBreaks <- seq(0,-6000, by=-bathyIntervals)
    depths <- grDevices::colorRampPalette(c("#EBF3FB","#215B93"))
    
    if (plotBathy == "FILL"){
      ggItems[["bathy"]] <- ggplot2::geom_contour_filled(data = RVSurveyData::maritimesBathy, ggplot2::aes(x=x, y=y, z=z), breaks=bathyBreaks,  show.legend = F)
      ggItems[["bathy"]] <- c(ggItems[["bathy"]],ggplot2::scale_fill_manual(values=depths(length(bathyBreaks))))
    }else if (plotBathy == "LINES"){
      ggItems[["bathy"]] <- ggplot2::geom_contour(data = RVSurveyData::maritimesBathy, ggplot2::aes(x=x, y=y, z=z), alpha= 2/3, breaks=seq(-bathyIntervals,-6000, by=-bathyIntervals), size=c(0.6), colour="steelblue", show.legend = T)
    }
  } else{
    ggItems[["bathy"]] <- NULL
  }
  return(ggItems)
}

ggNAFO <- function(plotNAFO = TRUE, plotLabels = TRUE, filter= NULL){
  ggItems<- list()
  nafo <- RVSurveyData::nafo_sf
  nafo <- sf::st_transform(nafo, crs = 4326)
  if (plotNAFO){
    ggItems[["nafo"]] <- ggplot2::geom_sf(data = nafo,fill=NA, color="#88888833")
    if(plotLabels) ggItems[["nafo"]] <- c(ggItems[["nafo"]],ggplot2::geom_sf_text(data = nafo,ggplot2::aes(label = NAFO), fun.geometry = sf::st_centroid, fontface = "italic", color="#88888880", size = 4))
  }else{
    ggItems[["nafo"]] <- NULL
  }
  #filter is not yet sorted out
  return(ggItems)
}

ggStrata <- function(plotStrata=TRUE, plotLabels=TRUE, filter=NULL){
  ggItems<- list()
  if(all(filter %in% c(396:411))){
    strata <- RVSurveyData::strataMar4VSW_sf 
  }else{
    strata <- RVSurveyData::strataMar_sf
  }
  strata <- sf::st_transform(strata, crs = 4326)
  if(!is.null(filter)) strata<-strata[strata$STRATA_ID %in% filter,]
  if (plotStrata){
    ggItems[["strata"]] <- ggplot2::geom_sf(data = strata,fill=NA, color="#00669933")
    if(plotLabels) {
      strata<- sf::st_set_geometry(strata, 'centroids')
      ggItems[["strata"]] <- c(ggItems[["strata"]],ggplot2::geom_sf_text(data = strata,ggplot2::aes(label = STRATA_ID), fontface = "italic", color="#88888880", size = 4))##00669933
    }
  }else{
    ggItems[["strata"]] <- NULL
  }
  return(ggItems)
}
ggCatchPts <- function(catchdata = NULL, sizeVar=NULL, colourVar = NULL, return=NULL){
  ggItems<- list()
  if (return=="CATCHES"){
    if(length(colourVar)>1){
      ggItems[["catchPts"]] <- ggplot2::geom_point(data=catchdata[!is.na(catchdata[,sizeVar]),], ggplot2::aes(x=SLONG_DD, y=SLAT_DD, size = sizeVar, colour=colourVar[1]), alpha = 0.6)
      ggItems[["catchPts"]] <- c(ggItems[["catchPts"]],ggplot2::scale_color_discrete(name=colourVar[1], labels=paste0(catchdata[!is.na(catchdata[,sizeVar]),colourVar[1]]," (",catchdata[!is.na(catchdata[,sizeVar]),colourVar[2]],")")))
    }else{
      ggItems[["catchPts"]] <- ggplot2::geom_point(data=catchdata[!is.na(catchdata[,sizeVar]),], ggplot2::aes(x=SLONG_DD, y=SLAT_DD, size = sizeVar, colour=colourVar), alpha = 0.6)
    }
    ggItems[["catchPts"]] <- c(ggItems[["catchPts"]], ggplot2::scale_size_continuous(range = c(2,8)))
  }
  if (return=="NULLSETS"){
    ggItems[["catchPts"]] <- ggplot2::geom_point(data=catchdata[is.na(catchdata[,sizeVar]),], ggplot2::aes(SLONG_DD, SLAT_DD),  shape=3, size= 2, color="black")
  }
  if (return=="ALLSETS"){
    ggItems[["catchPts"]] <- ggplot2::geom_point(data=catchdata, ggplot2::aes(SLONG_DD, SLAT_DD), size= 2, color="#333333CC")
  }
  return(ggItems)
}

ggStrataData <- function(catchStrataData = NULL, plotField = NULL, filter=NULL){
  if ("SPEC" %in% names(catchStrataData)){
    plotLabel <- paste0("SPEC: ",catchStrataData$SPEC[1])
  } else if ("TAXA_" %in% names(catchStrataData)){
    plotLabel <- catchStrataData$TAXA_[1]
  }
  if (is.null(filter)){
    strata <- RVSurveyData::strataMar_sf
  }
  if(all(filter %in% c(396:411))){
    strata <- RVSurveyData::strataMar4VSW_sf 
  }else{
    strata <- RVSurveyData::strataMar_sf
  }
  strata <- sf::st_transform(strata, crs = 4326)
  catchStrataData <- merge(strata, catchStrataData, by.x="STRATA_ID", by.y="STRAT")
  ggItems<- list()
  ggItems[["ggStratData"]] <- ggplot2::geom_sf(data = catchStrataData, ggplot2::aes_string(fill=plotField))
  ggItems[["ggStratData"]] <- c(ggItems[["ggStratData"]],ggplot2::scale_fill_continuous(name = paste0(plotField, "\n(",plotLabel,")"), direction = -1,type = "viridis",na.value = NA))
  return(ggItems)
}

#is.nan didn't have a method for data.frames
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))