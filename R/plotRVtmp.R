#' @title plotRV
#' @description This function .
#' @param tblList the default is \code{NULL}. This is a list populated with all RV dataframes that
#' should have filtering applied to them.
#' @param ptVar the default is \code{"TOTWGT"}. Can also be \code{"TOTWGT"}. 
#' @param strataVar the default is \code{"MEAN_WGT"}
#' @param plotPts the default is \code{TRUE}.
#' @param plotStrata the default is \code{TRUE}.
#' @param code the default is \code{NULL}. If data should be limited to a particular species, enter 
#' the species code here
#' @param aphiaid the default is \code{NULL}. If data should be limited to a particular aphiaid, 
#' enter the aphiaid here.
#' @param title  the default is \code{"auto"}. By default, if the plot will be titled by the species 
#' available in the data. Changing this value will result in a custom title.
#' @param hideTitle the default is \code{FALSE}. If the automatic title (i.e common names of species 
#' present in the data) is not desired, set this to TRUE (or change the default value of \code{title})
#' @param bathyRes  the default is \code{4}. This is the resolution of the bathymetric data, in 
#' minutes (sent directly to \code{marmap::getNOAA.bathy}).  If it is set to \code{NULL}, no 
#' bathymetric data will be plotted, and plots will be much faster.
#' @param showNullsets   the default is \code{TRUE}.  If TRUE, sets where the specified species was not
#' caught will be plotted as '+'.  If FALSE, null sets will not be plotted.
#' @param online the default is \code{FALSE}.
#' @param ... other arguments passed to methods (e.g. 'debug' and 'quiet')
#' @returns a ggplot object showing the positions of the data.  Null sets will be shown as + signs.
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @note GSWARPOUT, GSCRUISELIST and GSSPEC can NOT be used to filter the other tables.
#' @export
plotRV <- function(tblList = NULL, ptVar = "TOTWGT", strataVar = "MEAN_WGT",
                   plotPts =TRUE,
                   plotStrata = TRUE,
                   code= NULL, aphiaid=NULL, title ="auto", hideTitle = FALSE, 
                   bathyRes=4, showNullsets = TRUE, online=F,...){
  args <- list(...)
  doFilt <- ifelse(length(names(args))>0 && names(args) %in% c("KINGDOM", "PHYLUM","CLASS","ORDER","FAMILY","GENUS", "SPECIES"),T,F)
  debug <- ifelse(is.null(args$debug), F, args$debug) 
  multiSp <- ifelse(is.null(args$multiSp), F, args$multiSp)

  if (plotPts & !ptVar %in% c("TOTNO", "TOTWGT")) stop("plotVar must be either 'TOTNO' or 'TOTWGT'")
  if (plotStrata & !strataVar %in% c("TOTNO", "TOTWGT", "MEAN_WGT", "MEAN_NO", "BIOMASS", "ABUND")) stop("plotVar must be either 'TOTNO', 'TOTWGT', 'MEAN_WGT', 'MEAN_NO', 'BIOMASS', or 'ABUND'")
  
  if (title == "auto"){
    theTitle = ""
  }else{
    theTitle = title
  }
  
  if (!is.null(code) | !is.null(aphiaid) | doFilt){
    if (!is.null(args$KINGDOM)) tblList$GSSPECIES <- tblList$GSSPECIES[which(tblList$GSSPECIES$KINGDOM %in% c(args$KINGDOM)), ] 
    if (!is.null(args$PHYLUM))  tblList$GSSPECIES <- tblList$GSSPECIES[which(tblList$GSSPECIES$PHYLUM %in% c(args$PHYLUM)), ] 
    if (!is.null(args$CLASS))   tblList$GSSPECIES <- tblList$GSSPECIES[which(tblList$GSSPECIES$CLASS %in% c(args$CLASS)), ] 
    if (!is.null(args$ORDER))   tblList$GSSPECIES <- tblList$GSSPECIES[which(tblList$GSSPECIES$ORDER %in% c(args$ORDER)), ] 
    if (!is.null(args$FAMILY))  tblList$GSSPECIES <- tblList$GSSPECIES[which(tblList$GSSPECIES$FAMILY %in% c(args$FAMILY)), ] 
    if (!is.null(args$GENUS))   tblList$GSSPECIES <- tblList$GSSPECIES[which(tblList$GSSPECIES$GENUS %in% c(args$GENUS)), ] 
    if (!is.null(args$SPECIES)) tblList$GSSPECIES <- tblList$GSSPECIES[which(tblList$GSSPECIES$SPECIES %in% c(args$SPECIES)), ] 
    if (!is.null(code))         tblList$GSSPECIES <- tblList$GSSPECIES[which(tblList$GSSPECIES$CODE %in% code),] 
    if (!is.null(aphiaid))      tblList$GSSPECIES <- tblList$GSSPECIES[which(tblList$GSSPECIES$APHIAID %in% aphiaid),] 
    tblList <- propagateChanges(tblList, quiet=T, keep_nullsets = T)
    if (class(tblList)=="numeric")stop("No data found to plot")
  }
  
  bkgdStrata <- merge(RVSurveyData::strataMar_sf,tblList$GSINF, by.x = "STRATA_ID", by.y="STRAT")
  limits <- c(grDevices::extendrange(r=range(bkgdStrata$SLONG_DD),1),
              grDevices::extendrange(r=range(bkgdStrata$SLAT_DD),1))
  
  if (theTitle == "" & !hideTitle){
    theTitleSpecs <- sort(unique(tblList$GSCAT$SPEC))
    theTitle <- paste(tblList$GSSPECIES[tblList$GSSPECIES$CODE %in% theTitleSpecs,"COMM"], collapse = ", ")
  }
  
  if (plotPts){ 
    catches <- merge(tblList$GSINF, tblList$GSCAT, all.x=T, by=c("MISSION", "SETNO"))
    #plotting the data, so drop recs w/o valid coord pair
    catches <- catches[stats::complete.cases(catches[ , c('SLONG_DD', 'SLAT_DD')]), ]
  }
  
  if(plotStrata){
    showStrata <- F
    dfNWSets  <- NW_sets(tblList = tblList)
    specs <- unique(dfNWSets$SPEC)
    specs <- specs[!is.na(specs) & specs !=0]
    multiSp <- ifelse((length(specs)>1),T, F)
    dfNWAgg   <- NW_strat(tblList = tblList, dfNWSets=dfNWSets, multiSp = multiSp)
    if (length(specs)>1){theTitle = "Multispecies"}
    stratData <- merge(RVSurveyData::strataMar_sf, dfNWAgg, by.x = "STRATA_ID", by.y="STRAT")
    if (strataVar == "BIOMASS") {
      stratData$BIOMASS <- stratData$BIOMASS/1000
      colnames(stratData)[colnames(stratData)=="BIOMASS"] <- "BIOMASS_T"
      strataVar <- "BIOMASS_T"
    }
  }
  
  if (online){
    reg = ggplot2::map_data("world2Hires")
    reg = subset(reg, region %in% c('Canada', 'USA', 'France'))
    
    if (!is.null(bathyRes)){
      bathy <- marmap::getNOAA.bathy(lon1 = limits[1], limits[2], lat1 = limits[3], lat2 = limits[4], resolution = bathyRes)
      bathy <- marmap::fortify.bathy(bathy)
    }
  }
  p <-ggplot2::ggplot() 
  p <- p + ggplot2::ggtitle(theTitle)
  p <- p + ggplot2::xlab("Longitude") + ggplot2::ylab("Latitude") 
  if (online) p <- p + ggplot2::geom_polygon(data = reg, ggplot2::aes(x = long, y = lat, group = group), fill = "darkgrey", color = NA) 
  if (online & !is.null(bathyRes)) p <- p + ggplot2::geom_contour(data = bathy, ggplot2::aes(x=x, y=y, z=z), alpha= 2/3, breaks=seq(-50,-6000, by=-50), size=c(0.6), colour="steelblue")
  if (plotStrata) p <- p + ggplot2::geom_sf(data = stratData, ggplot2::aes_string(fill=strataVar)) + ggplot2::scale_fill_continuous(direction = -1,type = "viridis",na.value = NA)
  if (plotPts & showNullsets) p <- p + ggplot2::geom_point(data=catches[is.na(catches[,ptVar]),], ggplot2::aes(SLONG_DD, SLAT_DD),  shape=3, size= 1)
  if (plotPts) p <- p + ggplot2::geom_point(data=catches[!is.na(catches[,ptVar]),], ggplot2::aes_string(x="SLONG_DD", y="SLAT_DD", size = ptVar), alpha = 0.6)
  p <- p + ggplot2::theme_bw()
  
  return(p)
}