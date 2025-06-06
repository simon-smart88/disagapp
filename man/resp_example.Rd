% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/resp_example_f.R
\name{resp_example}
\alias{resp_example}
\title{Load example datasets}
\usage{
resp_example(dataset, logger = NULL)
}
\arguments{
\item{dataset}{character. The dataset to load. Either \code{mad}, \code{nys} or \code{scot}}

\item{logger}{Stores all notification messages to be displayed in the Log
Window. Insert the logger reactive list here for running in
shiny, otherwise leave the default NULL}
}
\value{
an sf object
}
\description{
This function is called by the resp_example module and loads example datasets
}
\examples{
shape <- resp_example("mad")

}
\author{
Simon Smart \href{mailto:simon.smart@cantab.net}{simon.smart@cantab.net}
}
