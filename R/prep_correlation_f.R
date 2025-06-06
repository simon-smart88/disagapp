#' @title Produce a correlation matrix of covariates
#' @description
#' Produce a correlation matrix of covariates
#'
#' @param covariates SpatRaster. Multi-layered SpatRaster
#' @param logger Stores all notification messages to be displayed in the Log
#' Window. Insert the logger reactive list here for running in
#' shiny, otherwise leave the default NULL
#' @return a matrix containing correlation coefficients
#' @author Simon Smart <simon.smart@@cantab.net>
#' @examples
#' covariate_files <- list.files(system.file("extdata", "covariates",
#'                               package = "disagapp"), full.names = TRUE)
#' covariate_list <- lapply(covariate_files, terra::rast)
#' covariates <- terra::rast(covariate_list)
#' correlation_matrix <- prep_correlation(covariates = covariates)
#'
#' @export

prep_correlation <- function(covariates, logger = NULL){

  # strictly speaking, not required here, but used downstream
  if (!requireNamespace("corrplot", quietly = TRUE)){
    logger |> writeLog(type = "error", 'This module requires the corrplot package to be installed. Close the app, run install.packages("corrplot") and try again')
    return()
  }

  if (!inherits(covariates, "SpatRaster")){
    logger |> writeLog(type = "error", "covariates must be a SpatRaster")
    return()
  }

  if (terra::nlyr(covariates) < 2){
    logger |> writeLog(type = "error", "You must have more than one covariate to plot a correlation matrix")
    return()
  }

  corr_matrix <- terra::layerCor(covariates, fun = "cor", use = "complete.obs")

  corr_matrix <- as.matrix(as.data.frame(corr_matrix$correlation))

  return(corr_matrix)
}
