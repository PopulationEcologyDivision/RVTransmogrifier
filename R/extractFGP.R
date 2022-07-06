#' @title extractFGP
#' @description This function generates the products (i.e. csvs and shapefiles)used by the FGP
#' services related to https://open.canada.ca/data/en/dataset/8ddcaeea-b806-4958-a79f-ba9ab645f53b.
#' @param survey the default is \code{NULL}. This specifies which survey should be extracted.  Valid
#' values are "SPRING", "SUMMER", "FALL", and "4VSW".  A value of NULL will result in products being
#' generated for all 4 different surveys.
#' @param years the default is \code{NULL}. This paramter allows you to generate datasets for one or
#' more specific years.  A value of NULL will result in products being generated for all years for
#' which data exists, and a vector of years will result in dataset that include the specified years.
#' @param path default is \code{NULL}.  This identifies a location where the resulting products
#' should be saved.  A value of NULL will result in products being generated within the current
#' working directory.
#' @author Mike McMahon
#' @export
extractFGP <- function(survey = NULL, years=NULL, path = NULL ){
  ts <-  format(Sys.time(), "%Y")
  thisEnv = new.env()
  if (!is.null(path))setwd(path)

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

    months <- switch(survey[i],
                     "SPRING" = c(1,2,3,4),
                     "SUMMER" = c(5,6,7,8),
                     "FALL" = c(9,10,11,12),
                     "4VSW" = -1
    )

    thisList <- loadRVData()

# #     invisible(sapply(allTbls, loadData))
# # browser()
#     thisList <- list()
#     thisList$GSXTYPE   <- RVSurveyData::GSXTYPE
#     thisList$GSINF     <- RVSurveyData::GSINF
#     thisList$GSGEAR    <- RVSurveyData::GSGEAR
#     thisList$GSMATURITY<- RVSurveyData::GSMATURITY
#     thisList$GSSEX     <- RVSurveyData::GSSEX
#     thisList$GSMISSIONS<- RVSurveyData::GSMISSIONS
#     thisList$GSCAT     <- RVSurveyData::GSCAT
#     thisList$GSDET     <- RVSurveyData::GSDET
#     thisList$GSSPECIES <- RVSurveyData::GSSPECIES

    thisList$GSINF <- thisList$GSINF[thisList$GSINF$TYPE==1,]
    if (!is.null(years)){
      thisList$GSMISSIONS <- thisList$GSMISSIONS[thisList$GSMISSIONS$YEAR %in% c(years),]
    }
    if (survey[i] != "4VSW"){
      #get rid of 4VSW cod data
      thisList$GSINF <- thisList$GSINF[-which(thisList$GSINF$STRAT %in% c(396:411)
                                            & lubridate::month(thisList$GSINF$SDATE) %in% c(1,2,3,4)),]
      #retain appropriate months
      thisList$GSINF <- thisList$GSINF[which(lubridate::month(thisList$GSINF$SDATE) %in% months),]
    }else{
      thisList$GSINF <- thisList$GSINF[which(thisList$GSINF$STRAT %in% c(396:411)
                                           & lubridate::month(thisList$GSINF$SDATE) %in% c(1,2,3,4)),]
    }
    this <- filternator(thisList)
    if (is.numeric(this))stop("Your query did not return valid results")

    # replace gear code with gear desc
    this$GSINF <- merge(this$GSINF, this$GSGEAR, all.x = T)
    this$GSINF$GEAR <- NULL

    # replace maturity code with maturity desc
    this$GSDET <- merge(this$GSDET, this$GSMATURITY, all.x = T, by.x="FMAT", by.y="CODE")
    colnames(this$GSDET)[colnames(this$GSDET)=="DESCRIPTION"] <- "MATURITY"

    # replace sex code with sex desc
    this$GSDET <- merge(this$GSDET, this$GSSEX, all.x = T, by.x="FSEX", by.y="CODE")
    colnames(this$GSDET)[colnames(this$GSDET)=="DESCRIPTION"] <- "SEX"

    # indicate and retain only desired fields
    this$GSMISSIONS <- this$GSMISSIONS[,c("MISSION", 	"VESEL",	"CRUNO",	"YEAR",	"SEASON")]
    this$GSINF      <- this$GSINF[,c("MISSION",	"SETNO",	"SDATE",	"TIME", "STRAT",	"SLAT_DD",	"SLONG_DD",	"ELAT_DD",	"ELONG_DD",	"DUR",	"DIST",	"SPEED",	"DEPTH_M",	"SURFACE_TEMPERATURE",	"BOTTOM_TEMPERATURE",	"BOTTOM_SALINITY", "GEARDESC")]
    this$GSCAT      <- this$GSCAT[,c("MISSION",	"SETNO",	"SPEC",	"TOTWGT",	"TOTNO")]
    this$GSDET      <- this$GSDET[,c("MISSION",	"SETNO",	"SPEC",	"FLEN",	"FWT", "MATURITY",	"SEX", "AGE",	"SPECIMEN_ID")]
    this$GSSPECIES  <- this$GSSPECIES[,c("SPEC",	"COMM",	"CODE",	"TSN")]

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

    write.csv(this$GSMISSIONS, file = paste0(fn,"_GSMISSIONS.csv"), row.names = F)
    write.csv(this$GSINF, file = paste0(fn,"_GSINF.csv"), row.names = F)
    write.csv(this$GSCAT, file = paste0(fn,"_GSCAT.csv"), row.names = F)
    write.csv(this$GSDET, file = paste0(fn,"_GSDET.csv"), row.names = F)
    write.csv(this$GSSPECIES, file = paste0(fn,"_GSSPECIES.csv"), row.names = F)

  }
}

# extractFGP("SPRING")
# extractFGP("SUMMER")
# extractFGP("FALL")
# extractFGP("4VSW")
# tt <- extractFGP(survey = NULL, )

