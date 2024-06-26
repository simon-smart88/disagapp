% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prep_scale.R
\name{prep_scale}
\alias{prep_scale}
\title{prep_scale}
\usage{
prep_scale(covs)
}
\arguments{
\item{covs}{SpatRaster. The covariates to be scaled.}
}
\value{
a list containing the scaled SpatRaster and a dataframe with the
parameters used for scaling
}
\description{
This function is called by the prep_scale module and scales
the values of a SpatRaster using a method equivalent to \code{terra::scale}
}
\author{
Simon Smart \href{mailto:simon.smart@cantab.net}{simon.smart@cantab.net}
}
