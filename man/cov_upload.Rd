% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cov_upload_f.R
\name{cov_upload}
\alias{cov_upload}
\title{Upload covariates}
\usage{
cov_upload(shape, path_df, logger = NULL)
}
\arguments{
\item{shape}{sf. sf object containing the area of interest}

\item{path_df}{data.frame. Containing \code{datapath} and \code{name} columns of the
file(s) to be uploaded.}

\item{logger}{Stores all notification messages to be displayed in the Log
Window. Insert the logger reactive list here for running in
shiny, otherwise leave the default NULL}
}
\value{
a list containing SpatRaster objects
}
\description{
This function is called by the cov_upload module. It loads raster images,
checks their projection and transforms them if necessary, checks that they
overlap with the area of interest and crops and masks them to the area of
interest.
}
\examples{
cov_df <- data.frame(datapath = list.files(system.file("extdata", "covariates",
                                package="disagapp"), full.names = TRUE),
                    name = list.files(system.file("extdata", "covariates",
                           package="disagapp")))
shp_file <- list.files(system.file("extdata", "shapes", package="disagapp"),
                       pattern = ".shp", full.names = TRUE)
shape <- sf::st_read(shp_file, quiet = TRUE)
rasters <- cov_upload(shape = shape, path_df = cov_df)
}
\author{
Simon Smart \href{mailto:simon.smart@cantab.net}{simon.smart@cantab.net}
}
