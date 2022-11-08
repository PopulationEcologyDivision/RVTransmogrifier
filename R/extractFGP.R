#' @title extractFGP
#' @description This function generates the products (i.e. csvs and shapefiles)used by the FGP
#' services related to https://open.canada.ca/data/en/dataset/8ddcaeea-b806-4958-a79f-ba9ab645f53b.
#' @param survey the default is \code{NULL}. This specifies which survey should be extracted.  Valid
#' values are "SPRING", "SUMMER", "FALL", and "4VSW".  A value of NULL will result in products being
#' generated for all 4 different surveys.
#' @param years the default is \code{NULL}. This parameter allows you to generate datasets for one or
#' more specific years.  A value of NULL will result in products being generated for all years for
#' which data exists, and a vector of years will result in dataset that include the specified years.
#' @author  Mike McMahon, \email{Mike.McMahon@@dfo-mpo.gc.ca}
#' @export
extractFGP <- function(survey = NULL, years=NULL){
  ts <-  format(Sys.time(), "%Y")
  thisEnv = new.env()

  if (is.null(survey)){
    survey <- c("SPRING", "SUMMER", "FALL", "4VSW")
  }else{
    survey <- toupper(survey)
  }
  for (i in 1:length(survey)){
    if (is.null(years)){
      fn <- paste0(survey[i],"_",ts)
    }else{
      fn <- paste0(years[1],"_",survey[i],"_",ts)
    }

    this <- getSurvey(survey=survey[i], years = years)
    if (is.numeric(this))stop("Your query did not return valid results")

    # replace gear code with gear desc
    this$GSINF <- merge(this$GSINF, this$GSGEAR, all.x = T)
    this$GSINF$GEAR <- NULL

    # replace maturity code with maturity desc
    this$dataDETS <- merge(this$dataDETS, this$GSMATURITY, all.x = T, by.x="FMAT", by.y="CODE")
    colnames(this$dataDETS)[colnames(this$dataDETS)=="DESCRIPTION"] <- "MATURITY"

    # replace sex code with sex desc
    this$dataDETS <- merge(this$dataDETS, this$GSSEX, all.x = T, by.x="FSEX", by.y="CODE")
    colnames(this$dataDETS)[colnames(this$dataDETS)=="DESCRIPTION"] <- "SEX"

    # indicate and retain only desired fields
    this$GSMISSIONS <- this$GSMISSIONS[,c("MISSION", 	"VESEL",	"CRUNO",	"YEAR",	"SEASON")]
    this$GSINF      <- this$GSINF[,c("MISSION",	"SETNO",	"SDATE",	"TIME", "STRAT",	"SLAT_DD",	"SLONG_DD",	"ELAT_DD",	"ELONG_DD",	"DUR",	"DIST",	"SPEED",	"DEPTH_M",	"SURFACE_TEMPERATURE",	"BOTTOM_TEMPERATURE",	"BOTTOM_SALINITY", "GEARDESC")]
    this$GSCAT      <- this$GSCAT[,c("MISSION",	"SETNO",	"SPEC",	"TOTWGT",	"TOTNO")]
    this$dataDETS      <- this$dataDETS[,c("MISSION",	"SETNO",	"SPEC",	"FLEN",	"FWT", "MATURITY",	"SEX", "AGE",	"SPECIMEN_ID")]
    this$GSSPECIES  <- this$GSSPECIES[,c("SCIENTIFICNAME",	"COMM",	"CODE")]#,	"TSN")]

    # drop time from date
    this$GSINF$SDATE <- as.Date(this$GSINF$SDATE)

    #rename fields as necess
    colnames(this$GSINF)[colnames(this$GSINF)=="SLAT_DD"] <- "SLAT"
    colnames(this$GSINF)[colnames(this$GSINF)=="SLONG_DD"] <- "SLONG"
    colnames(this$GSINF)[colnames(this$GSINF)=="ELAT_DD"] <- "ELAT"
    colnames(this$GSINF)[colnames(this$GSINF)=="ELONG_DD"] <- "ELONG"
    colnames(this$GSINF)[colnames(this$GSINF)=="SURFACE_TEMPERATURE"] <- "SURF_TEMP"
    colnames(this$GSINF)[colnames(this$GSINF)=="BOTTOM_TEMPERATURE"] <- "BOTT_TEMP"
    colnames(this$GSINF)[colnames(this$GSINF)=="BOTTOM_SALINITY"] <- "BOTT_SAL"
    colnames(this$GSINF)[colnames(this$GSINF)=="DEPTH_M"] <- "DEPTH"
    # make the shapefile
    Mar.utils::df_to_shp(df=this$GSINF,filename = fn,lat.field = "SLAT",lon.field = "SLONG")

    utils::write.csv(this$GSMISSIONS, file = paste0(fn,"_GSMISSIONS.csv"), row.names = F)
    utils::write.csv(this$GSINF, file = paste0(fn,"_GSINF.csv"), row.names = F)
    utils::write.csv(this$GSCAT, file = paste0(fn,"_GSCAT.csv"), row.names = F)
    utils::write.csv(this$GSDET, file = paste0(fn,"_GSDET.csv"), row.names = F)
    utils::write.csv(this$GSSPECIES, file = paste0(fn,"_GSSPECIES.csv"), row.names = F)

  }
  return(this)
}
