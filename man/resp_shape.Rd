% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/resp_shape.R
\name{resp_shape}
\alias{resp_shape}
\title{resp_shape}
\usage{
resp_shape(shpdf)
}
\arguments{
\item{shpdf}{dataframe. As produced by \code{shiny::fileInput}, containing \code{name} and
\code{datapath} columns}
}
\value{
an sf object
}
\description{
This function is called by the resp_shape module and loads a
shapefile into an sf object. Inspired by a function written by Paula Moraga
here: https://www.paulamoraga.com/book-geospatial/sec-shinyexample.html#uploading-data
}
\author{
Simon Smart \href{mailto:simon.smart@cantab.net}{simon.smart@cantab.net}

Paula Moraga
}
