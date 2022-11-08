#' @title plotRV_onerous
#' @description This function .
#' @param tblList the default is \code{NULL}. This is a list populated with all RV dataframes that
#' should have filtering applied to them.
#' @param plotCatchPoints the default is \code{"TOTWGT"}, but can also be \code{"TOTWGT"}.  If set to \code{NULL},
#' no catch data points will be plotted.
#' @param plotNullsets   the default is \code{TRUE}.  If TRUE, sets where the specified species was not
#' caught will be plotted as '+'.  If FALSE, null sets will not be plotted.
#' @param plotCatchStrata the default is \code{"MEAN_WGT"}, but can be any of \code{"TOTNO"}, \code{"TOTWGT"}, 
#' \code{"MEAN_WGT"}, \code{"MEAN_NO"}, \code{"BIOMASS"}, or \code{"ABUND"}. If set to \code{NULL},
#' no strata will be plotted by catch.
#' @param bkgdStrata the default is \code{TRUE}. Should strata lines be shown?
#' @param plotBathy the default is \code{TRUE}. Should bathymetric contours be shown?
#' @param bathyIntervals the default is \code{100} (m).  This is the difference in depth between contour lines.
#' @param title  the default is \code{"auto"}. By default, if the plot will be titled by the species 
#' available in the data. Changing this value will result in a custom title.  Setting this to NULL 
#' will remove the title altogether.
#' @param ... other arguments passed to methods (e.g. 'taxa', 'code', 'aphiaid', 'debug' and 'quiet')
#' @returns a ggplot object showing the positions of the data.  Null sets will be shown as + signs.
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
plotRV_onerous <- function(tblList = NULL, 
                   plotCatchPoints = "TOTWGT", plotNullsets = TRUE,
                   plotCatchStrata = "MEAN_WGT",
                   plotBkgdStrata = TRUE,
                   plotBathy = TRUE, bathyIntervals=200,
                   # title ="auto", 
                   ...){
  args <- list(...)
  doFilt <- ifelse(length(names(args))>0 && names(args) %in% c("taxa", "code", "aphiaid"),T,F)
  debug <- ifelse(is.null(args$debug), F, args$debug) 
  quiet <- ifelse(is.null(args$quiet), F, args$quiet)
  
  #ensure specified variables are plottable
  if (!is.null(plotCatchPoints) && !plotCatchPoints %in% c("TOTNO", "TOTWGT")) stop("plotPts must be either 'TOTNO', 'TOTWGT' or NULL")
  if (!is.null(plotCatchStrata) && !plotCatchStrata %in% c("TOTNO", "TOTWGT", "MEAN_WGT", "MEAN_NO", "BIOMASS", "ABUND")) stop("plotCatchStrata must be either 'TOTNO', 'TOTWGT', 'MEAN_WGT', 'MEAN_NO', 'BIOMASS', 'ABUND' or NULL")
  #can't plot bkgrd strata if the strata are to be plotted by catch
  if (!is.null(plotCatchStrata))plotBkgdStrata <- FALSE
  
  #strata are used to set plot bounds (filtered to sampled strata)
  strat <- sf::st_transform(RVSurveyData::strataMar_sf, crs = 4326)
  bkgdStrata <- merge(strat,tblList$GSINF, by.x = "STRATA_ID", by.y="STRAT")
  limits <- c(range(bkgdStrata$SLONG_DD),
              range(bkgdStrata$SLAT_DD))
  
  if(doFilt){
    #if specified, source data is filtered by taxa (i.e. code, aphiaid or taxa) 
    tblList<- filterSpecies(tblList = tblList, 
                            taxa=args$taxa, 
                            code = args$code, 
                            aphiaid = args$aphiaid,
                            keep_nullsets = plotNullsets
    )  
  }
  if (!is.null(plotCatchPoints)){
    #if filtered by taxa, then the records for the varios spp comprising the taxa must be aggregated together
    #e.g. all the various "rajidae" spp should have their numbers and weights combined by catch
    if (!is.null(args$taxa)) {
      catches<-aggregateByTaxa(tblList=tblList, taxa = args$taxa)
    }else{
      catches <- tblList$GSCAT
      colnames(catches)[colnames(catches)=="SPEC"] <- "TAXA"
    }
    catches <- merge(tblList$GSINF[,c("MISSION", "SETNO",'SLONG_DD', 'SLAT_DD')], catches, by=c("MISSION", "SETNO"))
    catches <- catches[stats::complete.cases(catches[ , c('SLONG_DD', 'SLAT_DD')]), ]
  }
  
  if (plotNullsets){
    #create obj holding all set locations, and remove those that will be plotted with catch information
    nsets<- tblList$GSINF[,c("MISSION", "SETNO",'SLONG_DD', 'SLAT_DD')]
    if (!is.null(plotCatchPoints)) nsets <- nsets[!(paste0(nsets$MISSION,"_",nsets$SETNO) %in% paste0(catches$MISSION,"_",catches$SETNO)),]
  }
  
  if(!is.null(plotCatchStrata)){
    showStrata <- F
    if (!is.null(args$taxa)) {
      cat_obj<-aggregateByTaxa(tblList=tblList, taxa = args$taxa)
    }
    dfNWSets  <- nwSets(tblList = tblList,  taxa = args$taxa)
    
    stratData <-list()
    for(s in 1:length(dfNWSets)){
      thisDat   <- nwStrat(tblList = tblList, dfNWSets=dfNWSets[[s]])
      thisDat <- merge(strat, thisDat, by.x = "STRATA_ID", by.y="STRAT")
      if (plotCatchStrata == "BIOMASS") {
        thisDat$BIOMASS <- thisDat$BIOMASS/1000
        colnames(thisDat)[colnames(thisDat)=="BIOMASS"] <- "BIOMASS_T"
      }
      stratData[[s]] <- thisDat
    }
  }
  

  res <- list()
  
  if (!is.null(stratData)){
  for(g in 1:length(stratData)){
    
    if (plotCatchStrata=="BIOMASS") plotCatchStrata <- "BIOMASS_T"
    p <-ggplot2::ggplot() 
    # if (!is.null(theTitle)) p <- p + ggplot2::ggtitle(theTitle)
    p <- p + ggplot2::xlab("Longitude") + ggplot2::ylab("Latitude") 
    if(plotBathy){
      bathyBreaks <- seq(0,-6000, by=-bathyIntervals)
      depths <- colorRampPalette(c("#EBF3FB","#215B93"))
      if(!is.null(plotCatchStrata)){
        p <- p + ggplot2::geom_contour(data = RVSurveyData::maritimesBathy, ggplot2::aes(x=x, y=y, z=z), alpha= 2/3, breaks=seq(-bathyIntervals,-6000, by=-bathyIntervals), size=c(0.6), colour="steelblue", show.legend = T)
      }else{
        p <- p + ggplot2::geom_contour_filled(data = RVSurveyData::maritimesBathy, ggplot2::aes(x=x, y=y, z=z), breaks=bathyBreaks,  show.legend = F)+ scale_fill_manual(values=depths(length(bathyBreaks)))
      } 
    } 
    if (plotBkgdStrata | !is.null(plotCatchStrata)) {
      if(!is.null(plotCatchStrata)){
        p <- p + ggplot2::geom_sf(data = stratData[[g]], ggplot2::aes_string(fill=plotCatchStrata)) + ggplot2::scale_fill_continuous(name = paste0(plotCatchStrata, "\n(",unique(stratData[[g]]$TAXA),")"), direction = -1,type = "viridis",na.value = NA)
      }else{
        p <- p + ggplot2::geom_sf(data = bkgdStrata,fill=NA, color="darkgrey")
      }
    }
    p <- p + ggplot2::geom_polygon(data = RVSurveyData::maritimesLand, ggplot2::aes(x = long, y = lat, group = group), fill = "darkgrey", color = NA) 
    if (plotNullsets) p <- p + ggplot2::geom_point(data=nsets, ggplot2::aes(SLONG_DD, SLAT_DD),  shape=3, size= 2, color="black")
    if (!is.null(plotCatchPoints)) p <- p + ggplot2::geom_point(data=catches[!is.na(catches[,plotCatchPoints]),], ggplot2::aes_string(x="SLONG_DD", y="SLAT_DD", size = plotCatchPoints, colour="TAXA"), alpha = 0.6)
    p <- p + ggplot2::coord_sf(xlim = c(limits[1],limits[2]), ylim = c(limits[3], limits[4]))
    p <- p + ggplot2::theme_bw()
    res[[g]] <- p
    rm(p)
    if (plotCatchStrata=="BIOMASS_T") plotCatchStrata <- "BIOMASS"
  }
  }else{
    res<- NA
  }
  
  return(res)
}