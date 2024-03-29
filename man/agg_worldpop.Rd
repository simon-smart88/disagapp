% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/agg_worldpop.R
\name{agg_worldpop}
\alias{agg_worldpop}
\title{agg_worldpop}
\usage{
agg_worldpop(country_code, method, resolution, year, logger = NULL)
}
\arguments{
\item{country_code}{character. ISO3 code of the country.}

\item{method}{character. The method used to estimate population. Either \code{Constrained} or \code{Unconstrained}.}

\item{resolution}{character. The resolution of the returned raster. Either \verb{100m} or \verb{1km}.}

\item{year}{numeric. The year to obtain data for. Either 2000 to 2020 when \code{method = Unconstrained} or only 2020 when \code{method = Constrained}}

\item{logger}{Stores all notification messages to be displayed in the Log
Window. Insert the logger reactive list here for running in
shiny, otherwise leave the default NULL}
}
\value{
a SpatRaster object
}
\description{
This function is called by the agg_worldpop module and downloads data on
population density for a single country from worldpop
}
\author{
Simon Smart \href{mailto:simon.smart@cantab.net}{simon.smart@cantab.net}
}
