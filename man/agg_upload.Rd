% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/agg_upload_f.R
\name{agg_upload}
\alias{agg_upload}
\title{Upload an aggregation raster}
\usage{
agg_upload(shape, path, logger = NULL)
}
\arguments{
\item{shape}{sf. sf object containing the area of interest}

\item{path}{character. The location of the file to be loaded.}

\item{logger}{Stores all notification messages to be displayed in the Log
Window. Insert the logger reactive list here for running in
shiny, otherwise leave the default NULL}
}
\value{
a SpatRaster object
}
\description{
This function is called by the agg_upload module and loads a
raster image.
}
\examples{
path <- list.files(system.file("extdata/aggregation",
        package="disagapp"), full.names = TRUE)
shp_file <- list.files(system.file("extdata/shapes",
        package="disagapp"), pattern = ".shp", full.names = TRUE)
shape <- sf::st_read(shp_file, quiet = TRUE)
raster <- agg_upload(shape = shape, path = path)

}
\author{
Simon Smart \href{mailto:simon.smart@cantab.net}{simon.smart@cantab.net}
}
