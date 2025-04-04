% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pred_pred_f.R
\name{pred_pred}
\alias{pred_pred}
\title{Get predictions from a disaggregation model}
\usage{
pred_pred(
  fit,
  aggregation,
  cases,
  predict_iid,
  uncertain = FALSE,
  N = NULL,
  CI = NULL,
  async = FALSE
)
}
\arguments{
\item{fit}{disag_model. Object returned by disag_model function that
contains all the necessary objects for generating predictions.}

\item{aggregation}{SpatRaster. The aggregation raster}

\item{cases}{logical. Whether to predictions of cases}

\item{predict_iid}{logical. Whether to generate predictions including the iid effect}

\item{uncertain}{logical. Whether or not to generate upper and lower credible
intervals}

\item{N}{numeric. The number of realisations to use when generating uncertainty
predictions}

\item{CI}{numeric. The credible interval to user when generating uncertainty
predictions.}

\item{async}{logical. Whether or not the function is being used asynchronously}
}
\value{
a list containing the predictions
}
\description{
This function is called by the pred_pred module and calls
disaggregation::predict_model() and disaggregation::predict_uncertainty() and
also generates predicted cases. SpatRasters are wrapped to enable asynchronous
operation.
}
\author{
Simon Smart \href{mailto:simon.smart@cantab.net}{simon.smart@cantab.net}
}
