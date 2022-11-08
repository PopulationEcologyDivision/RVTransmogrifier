#' created generously large area (based on the strata) 
#' for which to extract coastline and bathy
strataRng<- as.vector(st_bbox(RVSurveyData::strataMar_sf))
longs<- grDevices::extendrange(r=c(strataRng[1],strataRng[3]),10)
lats <- grDevices::extendrange(r=c(strataRng[2],strataRng[4]),10)

limits <- c(longs, lats)

library(mapdata)

theLand = ggplot2::map_data("world2Hires",region = c('Canada', 'USA', 'France'), wrap = c(-180,180))
theBathy <- marmap::fortify.bathy(marmap::getNOAA.bathy(lon1 = limits[1], 
                               lon2 = limits[2], 
                               lat1 = limits[3], 
                               lat2 = limits[4], 
                               resolution = 1))
p <-ggplot2::ggplot() 
p <- p + ggplot2::ggtitle("ggplot CRAPFEST 2022")
p <- p + ggplot2::xlab("Longitude") + ggplot2::ylab("Latitude") 
p <- p + ggplot2::coord_cartesian(xlim = c(limits[1],limits[2]), 
                                  ylim = c(limits[3], limits[4]),
                                  clip='on', expand= FALSE)
p <- p + ggplot2::geom_polygon(data = theLand, ggplot2::aes(x = long, y = lat, group = group), fill = "darkgrey", color = NA) 
p <- p + ggplot2::geom_contour(data = theBathy, ggplot2::aes(x=x, y=y, z=z), alpha= 2/3, breaks=seq(-50,-6000, by=-50), size=c(0.6), colour="steelblue")
p