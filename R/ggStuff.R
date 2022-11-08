getBbox <- function(area = RVSurveyData::strataMar_sf, filterField = "STRATA_ID", filterVals=NULL){
  area <-  sf::st_transform(area, crs = 4326)
  if(!is.null(filterField)&& !is.null(filterVals)) area<-area[area[[filterField]] %in% filterVals,]
  bbox <- st_bbox(area, na.rm=T)
  return(bbox)
}
ggBathy<- function(plotBathy = "FILL", bathyIntervals = 100){
  ggItems<- list()
  if (!is.null(plotBathy)){
    bathyBreaks <- seq(0,-6000, by=-bathyIntervals)
    depths <- colorRampPalette(c("#EBF3FB","#215B93"))
    
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
    if(plotLabels) ggItems[["nafo"]] <- c(ggItems[["nafo"]],ggplot2::geom_sf_text(data = RVSurveyData::nafo_sf,aes(label = NAFO), fun.geometry = sf::st_centroid, fontface = "italic", color="#88888880", size = 4))
  }else{
    ggItems[["nafo"]] <- NULL
  }
  #filter is not yet sorted out
  return(ggItems)
}

ggStrata <- function(plotStrata=TRUE, plotLabels=TRUE, filter=NULL){
  ggItems<- list()
  strata <- RVSurveyData::strataMar_sf
  strata <- sf::st_transform(strata, crs = 4326)
  if(!is.null(filter)) strata<-strata[strata$STRATA_ID %in% filter,]

  if (plotStrata){
    ggItems[["strata"]] <- ggplot2::geom_sf(data = strata,fill=NA, color="#00669933")
    if(plotLabels) ggItems[["strata"]] <- c(ggItems[["strata"]],ggplot2::geom_sf_text(data = strata,aes(label = STRATA_ID), fun.geometry = sf::st_centroid, fontface = "italic", color="#00669933", size = 4))
  }else{
    ggItems[["strata"]] <- NULL
  }
  return(ggItems)
}
ggCatchPts <- function(catchdata = NULL, sizeVar=NULL, colourVar = NULL, return=NULL){
  ggItems<- list()
  if (return=="CATCHES"){
    if(length(colourVar)>1){
      ggItems[["catchPts"]] <- ggplot2::geom_point(data=catchdata[!is.na(catchdata[,sizeVar]),], ggplot2::aes_string(x="SLONG_DD", y="SLAT_DD", size = sizeVar, colour=colourVar[1]), alpha = 0.6)
      ggItems[["catchPts"]] <- c(ggItems[["catchPts"]],ggplot2::scale_color_discrete(name=colourVar[1],labels=paste0(catchdata[!is.na(catchdata[,sizeVar]),colourVar[1]]," (",catchdata[!is.na(catchdata[,sizeVar]),colourVar[2]],")")))
    }else{
      ggItems[["catchPts"]] <- ggplot2::geom_point(data=catchdata[!is.na(catchdata[,sizeVar]),], ggplot2::aes_string(x="SLONG_DD", y="SLAT_DD", size = sizeVar, colour=colourVar), alpha = 0.6)
    }
    ggItems[["catchPts"]] <- c(ggItems[["catchPts"]], scale_size_continuous(range = c(2,8)))
  }
  if (return=="NULLSETS"){
    ggItems[["catchPts"]] <- ggplot2::geom_point(data=catchdata[is.na(catchdata[,sizeVar]),], ggplot2::aes(SLONG_DD, SLAT_DD),  shape=3, size= 2, color="black")
  }
  if (return=="ALLSETS"){
    ggItems[["catchPts"]] <- ggplot2::geom_point(data=catchdata, ggplot2::aes(SLONG_DD, SLAT_DD), size= 2, color="#333333CC")
  }
  return(ggItems)
}

ggStrataData <- function(catchStrataData = NULL, plotField = NULL){
  strata <- RVSurveyData::strataMar_sf
  strata <- sf::st_transform(strata, crs = 4326)
  catchStrataData <- merge(strata, catchStrataData, by.x="STRATA_ID", by.y="STRAT")
  ggItems<- list()
  ggItems[["ggStratData"]] <- ggplot2::geom_sf(data = catchStrataData, ggplot2::aes_string(fill=plotField))
  ggItems[["ggStratData"]] <- c(ggItems[["ggStratData"]],ggplot2::scale_fill_continuous(name = paste0(plotField, "\n(",unique(catchStrataData$TAXA_),")"), direction = -1,type = "viridis",na.value = NA))
  return(ggItems)
}