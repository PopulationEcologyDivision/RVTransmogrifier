#' @title plotRV
#' @description This function .
#' @param tblList the default is \code{NULL}. This is a list populated with all RV dataframes that
#' should have filtering applied to them.
#' @param plotVar the default is \code{"TOTNO"}. Can also be \code{"TOTWGT"}. 
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
#' @returns a ggplot object showing the positions of the data.  Null sets will be shown as + signs.
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @note GSWARPOUT, GSCRUISELIST and GSSPEC can NOT be used to filter the other tables.
#' @export
plotRV <- function(tblList = NULL, plotVar = "TOTNO", code= NULL, aphiaid=NULL, title ="auto", hideAutoTitle = FALSE, 
                   bathyRes=4, ...){
  args <- list(...)
  doFilt <- ifelse(length(names(args))>0 && names(args) %in% c("KINGDOM", "PHYLUM","CLASS","ORDER","FAMILY","GENUS", "SPECIES"),T,F)
  debug <- ifelse(is.null(args$debug), F, args$debug) 
  if (!plotVar %in% c("TOTNO", "TOTWGT")) stop("plotVar must be either 'TOTNO' or 'TOTWGT'")
  if (title == "auto"){
    theTitle = ""
  }else{
    theTitle = title
  }

  if (!is.null(code) | !is.null(aphiaid) | doFilt){
    if (!is.null(args$KINGDOM)) tblList$GSSPECIES_TAX <- tblList$GSSPECIES_TAX[which(tblList$GSSPECIES_TAX$KINGDOM %in% c(args$KINGDOM)), ] 
    if (!is.null(args$PHYLUM)) tblList$GSSPECIES_TAX <- tblList$GSSPECIES_TAX[which(tblList$GSSPECIES_TAX$PHYLUM %in% c(args$PHYLUM)), ] 
    if (!is.null(args$CLASS)) tblList$GSSPECIES_TAX <- tblList$GSSPECIES_TAX[which(tblList$GSSPECIES_TAX$CLASS %in% c(args$CLASS)), ] 
    if (!is.null(args$ORDER)) tblList$GSSPECIES_TAX <- tblList$GSSPECIES_TAX[which(tblList$GSSPECIES_TAX$ORDER %in% c(args$ORDER)), ] 
    if (!is.null(args$FAMILY)) tblList$GSSPECIES_TAX <- tblList$GSSPECIES_TAX[which(tblList$GSSPECIES_TAX$FAMILY %in% c(args$FAMILY)), ] 
    if (!is.null(args$GENUS)) tblList$GSSPECIES_TAX <- tblList$GSSPECIES_TAX[which(tblList$GSSPECIES_TAX$GENUS %in% c(args$GENUS)), ] 
    if (!is.null(args$SPECIES)) tblList$GSSPECIES_TAX <- tblList$GSSPECIES_TAX[which(tblList$GSSPECIES_TAX$SPECIES %in% c(args$SPECIES)), ] 
    
    if (!is.null(code))       tblList$GSSPECIES_20220624 <- tblList$GSSPECIES_20220624[which(tblList$GSSPECIES_20220624$CODE %in% code),] 
    if (!is.null(aphiaid))  tblList$GSSPECIES_20220624 <- tblList$GSSPECIES_20220624[which(tblList$GSSPECIES_20220624$APHIAID %in% aphiaid),] 
    tblList <- propagateChanges(tblList, quiet=T, keep_nullsets = T)
    if (class(tblList)=="numeric")stop("No data found to plot")
  }
  if (theTitle == "auto" & !hideAutoTitle){
    theTitleSpecs <- sort(unique(tblList$GSCAT$SPEC))
    theTitle <- paste(tblList$GSSPECIES_20220624[tblList$GSSPECIES_20220624$CODE %in% theTitleSpecs,"COMM"], collapse = ", ")
  }
  
  catches <- merge(tblList$GSINF, tblList$GSCAT, all.x=T, by=c("MISSION", "SETNO"))
  
  limits <- c(grDevices::extendrange(r=range(catches$SLONG_DD),1),grDevices::extendrange(r=range(catches$SLAT_DD),1))
  
  
  library(mapdata)
  reg = ggplot2::map_data("world2Hires")
  reg = subset(reg, region %in% c('Canada', 'USA', 'France'))
  
  if (!is.null(bathyRes)){
  bathy <- marmap::getNOAA.bathy(lon1 = limits[1], limits[2],
                                 lat1 = limits[3], lat2 = limits[4], resolution = bathyRes)
  bathy <- marmap::fortify.bathy(bathy)

    
  }
  p <-ggplot2::ggplot() + 
    ggplot2::ggtitle(theTitle)+
    ggplot2::xlab("Longitude") + ggplot2::ylab("Latitude") +
    ggplot2::geom_polygon(data = reg, ggplot2::aes(x = long, y = lat, group = group), 
                          fill = "darkgrey", color = NA) +
    {if (!is.null(bathyRes)) ggplot2::geom_contour(data = bathy, ggplot2::aes(x=x, y=y, z=z), alpha= 2/3, breaks=seq(-50,-6000, by=-50), size=c(0.6), colour="steelblue")}+
    ggplot2::geom_point(data=catches[!is.na(catches[,plotVar]),], ggplot2::aes_string(x="SLONG_DD", y="SLAT_DD", size = plotVar), alpha = 2/3)+
    ggplot2::geom_point(data=catches[is.na(catches[,plotVar]),], ggplot2::aes(SLONG_DD, SLAT_DD),  shape=3, size= 1)+ 
    ggplot2::coord_map(xlim = limits[1:2], ylim = limits[3:4])
  return(p)
}