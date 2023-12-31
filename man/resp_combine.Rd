% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/resp_combine.R
\name{resp_combine}
\alias{resp_combine}
\title{resp_combine}
\usage{
resp_combine(
  df,
  df_area_column,
  df_resp_column,
  shape,
  shape_area_column,
  logger = NULL
)
}
\arguments{
\item{df}{dataframe. Containing the response data and the name of the administrative area}

\item{df_area_column}{character. The column name of the dataframe containing the administrative areas}

\item{df_resp_column}{character. The column name of the dataframe containing the response data}

\item{shape}{sf. Shapefile containing the boundary data}

\item{shape_area_column}{character. The column name of the shapefile containing the administrative areas}

\item{logger}{Stores all notification messages to be displayed in the Log
Window. Insert the logger reactive list here for running in
shiny, otherwise leave the default NULL}
}
\value{
an sf object
}
\description{
This function is called by the resp_combine module and merges response data
from a spreadsheet with boundary data from a shapefile into an sf object
}
\author{
Simon Smart \href{mailto:simon.smart@cantab.net}{simon.smart@cantab.net}
}
