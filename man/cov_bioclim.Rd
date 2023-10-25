% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cov_bioclim.R
\name{cov_bioclim}
\alias{cov_bioclim}
\title{cov_bioclim}
\usage{
cov_bioclim(country_code, variables)
}
\arguments{
\item{country_code}{character. ISO3 code of the country.}

\item{variables}{vector. List of the bioclimatic variables to be returned}
}
\value{
a list of SpatRaster objects
}
\description{
This function is called by the cov_bioclim module and downloads bioclimatic
data on from Bioclim via geodata. It returns a list of SpatRasters for the
selected variables
}
\author{
Simon Smart <simon.smart@cantab.net>
}