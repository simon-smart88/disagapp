% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mapping_functions.R
\name{replot_raster_map}
\alias{replot_raster_map}
\title{replot_raster_map}
\usage{
replot_raster_map(map, common, covariates, aggregation, selected_layer)
}
\arguments{
\item{map}{The leafletProxy object to replot the rasters on}

\item{common}{The common data structure}

\item{covariates}{character. The name of the item in common containing
the covariates to replot}

\item{aggregation}{character. The name of the item in common containing
the aggregation raster to replot}

\item{selected_layer}{character. The currently selected layer}
}
\description{
For internal use. Replot covariate data on the leaflet map after
changes in the selected set
}
\keyword{internal}
