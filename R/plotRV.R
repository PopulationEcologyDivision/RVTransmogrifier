#' @title plotRV
#' @description This function can plot the list data resulting from \code{get_survey()} or from 
#' \code{propagateChanges()}.
#' @param tblList the default is \code{NULL}. This is a list populated with all RV dataframes that
#' should have filtering applied to them.
#' @param plotSets the default is \code{"TOTWGT"}, but can also be \code{"TOTWGT"}.  If set to \code{NULL},
#' no catch data points will be plotted.
#' @param plotNullSets   the default is \code{TRUE}.  If TRUE, sets where the specified species was not
#' caught will be plotted as '+'.  If FALSE, null sets will not be plotted.
#' @param plotCatchStrata the default is \code{"MEAN_WGT"}, but can be any of \code{"TOTNO"}, \code{"TOTWGT"}, 
#' \code{"MEAN_WGT"}, \code{"MEAN_NO"}, \code{"BIOMASS"}, or \code{"ABUND"}. If set to \code{NULL},
#' no strata will be plotted by catch.
#' @param catchStrataData the default is \code{NULL}. This is a stratified object resulting from running 
#' \code{stratify}.  \code{stratify()} can generate multiple dfs within a list, but this parameter should
#' be pointed to a single dataframe.
#' @param bkgdStrata the default is \code{TRUE}. Should strata lines be shown?
#' @param plotBathy the default is \code{"FILL"}, but can be any of \code{"FILL"}, \code{"LINES"} or \code{NULL}.  
#' FILL results in progressively darker blue areas as depth increases. 
#' LINES results in contour lines.
#' NULL does not show any bathymetric information.
#' @param bathyIntervals the default is \code{100} (m).  This is the difference in depth between contour lines.
#' @param ... other arguments passed to methods (e.g.'taxa', 'code', 'aphiaid', debug' and 'quiet')
#' @returns a ggplot object showing the positions of the data.  Null sets will be shown as + signs.
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
plotRV <- function(tblList = NULL, 
                       plotSets = "TOTWGT", plotNullSets = TRUE,
                       plotCatchStrata = "MEAN_WGT", catchStrataData = NULL,
                       plotStrata = TRUE,labelStrata=TRUE,
                       plotBathy = "FILL", bathyIntervals=200,
                       plotNAFO=FALSE, labelNAFO = FALSE,
                       ...){
  args <- list(...)
  debug <- ifelse(is.null(args$debug), F, args$debug) 
  quiet <- ifelse(is.null(args$quiet), F, args$quiet)


  
  if(!is.null(args$taxa)|!is.null(args$code)|!is.null(args$aphiaid)){
    tblList      <- filterSpecies(tblList, keep_nullsets = T,
                                  taxa = args$taxa,
                                  code = args$code,
                                  aphiaid = args$aphiaid)
    if (class(tblList)=="numeric")stop("Requested filter removed all species")
    tblList      <- aggregateByTaxa(tblList = tblList,
                                    taxa = args$taxa,
                                    code = args$code,
                                    aphiaid = args$aphiaid)
  }
  if (!is.null(tblList) & !is.null(plotSets)) {
    #strata are used to set plot bounds (filtered to sampled strata (from GSINF))
    limits1 <- sort(getBbox(filterVals = unique(tblList$GSINF$STRAT)))
  }
  if (!is.null(catchStrataData)) {
    if (class(catchStrataData)=="list" & length(catchStrataData)>1){
      message("catchStrataData can  multiple data frames, but only the first of these will be plotted (i.e.",names(catchStrataData[1]),").  To plot a different one, please set catchStrataData to the specific data frame you want to plot")
      catchStrataData <- catchStrataData[[1]]
    } else if (class(catchStrataData)=="list"){
      catchStrataData <- catchStrataData[[1]]
    }

    if (is.null(plotSets)) limits1 <- sort(getBbox(filterVals = unique(catchStrataData$STRAT)))
    #if we have stratified catch data, we can't have filled contours
    if(plotBathy == "FILL")plotBathy <- "LINES"
  }

  limits <- c(roundDD2Min(limits1[1],nearestMin=15, how = "floor"), 
              roundDD2Min(limits1[2],nearestMin=15, how = "ceiling"), 
              roundDD2Min(limits1[3],nearestMin=15, how = "floor"), 
              roundDD2Min(limits1[4],nearestMin=15, how = "ceiling"))

  #add plot elements to this list.
  ggItems <- list()
  sf::sf_use_s2(FALSE)
  #set up the basic plot
  p <-ggplot2::ggplot() + ggplot2::theme_bw()
  ggItems[["bathy"]]      <- ggBathy(plotBathy=plotBathy, bathyIntervals=bathyIntervals)
  ggItems[["bkgdStrata"]] <- ggStrata(plotStrata=plotStrata, plotLabels=labelStrata, filter=unique(tblList$GSINF$STRAT))
  ggItems[["bkgdNAFO"]]   <- ggNAFO(plotNAFO=plotNAFO, plotLabels=labelNAFO, filter=NULL)
  if (!is.null(catchStrataData) & plotCatchStrata %in% c("TOTNO", "TOTWGT", "MEAN_WGT", "MEAN_NO", "BIOMASS", "BIOMASS_T", "ABUND")){
    if (plotCatchStrata == "BIOMASS")plotCatchStrata="BIOMASS_T"
    #   #can't plot bkgrd strata if the strata are to be plotted by catch
    ggItems[["bkgdStrata"]] <- NULL
    ggItems[["gg_stratData"]] <- ggStrataData(catchStrataData = catchStrataData, plotField = plotCatchStrata)
  }else{
    ggItems[["gg_stratData"]]<- NULL
  } 
  
  #ensure specified variables are plottable
  if (is.null(plotSets)){
    #don't plot sets
    catLeg<- NULL
  }else if(!is.null(plotSets) & plotSets %in% c("TOTNO", "TOTWGT", "ALL")){
    if("GSCAT_agg" %in% names(tblList)){
      catches <- tblList$GSCAT_agg
    }else{
      catches <- tblList$GSCAT
      catches <- merge(catches, tblList$GSSPECIES[, c("CODE", "SCIENTIFICNAME","APHIAID")], by.x="SPEC", by.y="CODE")
    }
    catches <- merge(catches, tblList$GSINF[,c("MISSION", "SETNO",'SLONG_DD', 'SLAT_DD')], by=c("MISSION", "SETNO"),all.y=T)
    if ("TAXA_" %in% names(catches)){
      t_field <- "TAXA_"
    }else{
      #if 2 fields sent, the format will be "t_field1 (t_field2)"
      t_field <- c("SCIENTIFICNAME", "APHIAID")
    }
    if (plotSets %in% c("TOTNO", "TOTWGT")) {
      catLeg <- ifelse(plotSets == "TOTWGT", "Total Weight (kgs)","Total Number")
      ggItems[["catchPts"]] <- ggCatchPts(catchdata = catches, sizeVar=plotSets, colourVar = t_field, return="CATCHES")
      if (plotNullSets)  ggItems[["nullSets"]]   <- ggCatchPts(catchdata = catches, sizeVar="TOTNO", colourVar = t_field, return="NULLSETS")
      
    }else if (plotSets == "ALL"){
      catLeg <- NULL
      ggItems[["allPts"]] <- ggCatchPts(catchdata = catches, sizeVar="TOTNO", colourVar = NULL, return="ALLSETS")
    }
    ggItems[["catchLabels"]] <- ggplot2::guides(color = ggplot2::guide_legend(title = "Taxa",order=1), size = ggplot2::guide_legend(title = catLeg,order=2))
  }

  ggItems[["land"]]   <- ggplot2::geom_polygon(data = RVSurveyData::maritimesLand, ggplot2::aes(x = long, y = lat, group = group), fill = "darkgrey", color = NA) 
  ggItems[["extent"]] <- ggplot2::coord_sf(xlim = c(limits[1],limits[2]), ylim = c(limits[3], limits[4]))
  ggItems[["labels"]] <- ggplot2::labs(x="Longitude", y="Latitude")
  p<-p+ggItems
  return(p)
}