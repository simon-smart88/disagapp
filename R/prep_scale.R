#' @title prep_scale
#' @description This function is called by the prep_scale module and scales
#' the values of a SpatRaster using a method equivalent to `terra::scale`
#' @param covs SpatRaster. The covariates to be scaled.
#' @return a list containing the scaled SpatRaster and a dataframe with the
#' parameters used for scaling
#' @author Simon Smart <simon.smart@@cantab.net>
#' @export

prep_scale <- function(covs){
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
