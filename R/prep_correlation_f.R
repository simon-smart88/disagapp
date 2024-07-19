#' @title prep_correlation
#' @description
#' Produce a correlation matrix of covariates
#'
#' @param covariates SpatRaster. Multi-layered SpatRaster
#' @param logger Stores all notification messages to be displayed in the Log
#' Window. Insert the logger reactive list here for running in
#' shiny, otherwise leave the default NULL
#' @return a matrix containing correlation coefficients
#' @author Simon Smart <simon.smart@@cantab.net>
#' @export

prep_correlation <- function(covariates, logger = NULL){

  if (terra::nlyr(covariates) < 2){
    logger |> writeLog(type = "error", "You must have more than one covariate to plot a correlation matrix")
    return()
  }

  corr_matrix <- terra::layerCor(covariates, fun="cor", use = "complete.obs")

  corr_matrix <- as.matrix(as.data.frame(corr_matrix$correlation))

  return(corr_matrix)
}
