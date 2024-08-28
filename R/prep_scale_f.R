#' @title Scale the covariates
#' @description This function is called by the prep_scale module and scales
#' the values of a SpatRaster using a method equivalent to `terra::scale`
#' @param covs SpatRaster. The covariates to be scaled.
#' @param logger Stores all notification messages to be displayed in the Log
#'   Window. Insert the logger reactive list here for running in
#'   shiny, otherwise leave the default NULL
#' @return a list containing the scaled SpatRaster and a dataframe with the
#' parameters used for scaling
#' @author Simon Smart <simon.smart@@cantab.net>
#' @examples
#' covariate_files <- list.files(system.file("extdata", "covariates",
#'                               package = "disagapp"), full.names = TRUE)
#' covariate_list <- lapply(covariate_files, terra::rast)
#' covariates <- terra::rast(covariate_list)
#' result <- prep_scale(covs = covariates)
#' scaled_covariates <- result$covariates
#' @export

prep_scale <- function(covs, logger = NULL){

  if (!inherits(covs, "SpatRaster")){
    logger |> writeLog(type = "error", "covs must be a SpatRaster")
    return()
  }

  if (terra::nlyr(covs) < 2){
    logger |> writeLog(type = "error", "You must have more than one covariate to plot a correlation matrix")
    return()
  }

  mean_values <- terra::global(covs, "mean", na.rm = TRUE)
  residual <- covs - mean_values[,1]
  rms_values <- terra::global(residual, "rms", na.rm = TRUE)
  scaled_covs <- residual / rms_values[,1]
  scaling_parameters <- cbind(mean_values, rms_values)

  return(list(
    covariates = scaled_covs,
    parameters = scaling_parameters
  ))
}
