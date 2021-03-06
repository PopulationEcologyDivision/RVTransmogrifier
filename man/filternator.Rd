% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filternator.R
\name{filternator}
\alias{filternator}
\title{filternator}
\usage{
filternator(tblList = NULL, keep_nullsets = T, ...)
}
\arguments{
\item{tblList}{the default is \code{NULL}. This is a list populated with all RV dataframes that
should have filtering applied to them.}

\item{keep_nullsets}{the default is \code{T}. This is used to control whether or not the 
presence/absence of species changes the returned set locations.}

\item{...}{other arguments passed to methods (i.e. 'debug' and 'quiet')}
}
\value{
a list of filtered versions of the dataframes passed as \code{tblList}.  If the
filtering fails, a value of -1 will be returned.  For example, if data is filtered for a year
where data was not collected, a strata that doesn't exist, or a species that was not observed
would all result in a value of -1 being returned.
}
\description{
This function is used to ensure that filtering of a single RV data object cascades
to all of the other known RV data objects.  For example, if GSSPECIES is filtered to only
"haddock", and \code{filternator} is run, then all of the various RV tables will be filtered to
the point where they relate directly to haddock. All remaining GSINF records would be sets that
caught haddock, all GSCAT records would be limited to haddock, all GSDET records would be limited
to haddock, etc.  Filtering is not limited to species, but any value that exists in any field in
any of the tables present in RVSurveyData package.
}
\note{
GSWARPOUT, GSCRUISELIST and GSSPEC can NOT be used to filter the other tables.
}
\author{
Mike McMahon
}
