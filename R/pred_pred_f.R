#' @title Get predictions from a disaggregation model
#' @description
#' This function is called by the pred_pred module and calls
#' disaggregation::predict_model() and disaggregation::predict_uncertainty() and
#' also generates predicted cases. SpatRasters are wrapped to enable asynchronous
#' operation.
#'
#' @param fit disag_model. Object returned by disag_model function that
#' contains all the necessary objects for the model fitting
#' @param aggregation SpatRaster. The aggregation raster
#' @param predict_iid logical. Whether to generate predictions including the iid effect
#' @param uncertain logical. Whether or not to generate upper and lower credible
#' intervals
#' @param N. numeric. The number of realisations to use when generating uncertainty
#' predictions
#' @param CI numeric. The credible interval to user when generating uncertainty
#' predictions.
#' @param async logical. Whether or not the function is being used asynchronously
#' @return a list containing the predictions
#' @author Simon Smart <simon.smart@@cantab.net>
#' @export

pred_pred <- function(fit, aggregation, predict_iid, uncertain = FALSE, N = NULL, CI = NULL, async = FALSE){

  if (async){
    fit$data$covariate_rasters <- terra::unwrap(fit$data$covariate_rasters)
    aggregation <- terra::unwrap(aggregation)
  }

  prediction <- disaggregation::predict_model(fit, predict_iid = predict_iid)
  prediction$cases <- prediction$prediction * aggregation

  if (!is.null(prediction$field)){
    terra::crs(prediction$field) <- terra::crs(fit$data$covariate_rasters[[1]])
    prediction$field <- terra::mask(prediction$field, fit$data$covariate_rasters[[1]])
  }

  if (uncertain){
    prediction$uncertainty <- disaggregation::predict_uncertainty(fit, predict_iid = predict_iid, N = N, CI = CI)
  }

  names(prediction)[which(names(prediction) == "prediction")] <- "prediction (rate)"
  names(prediction)[which(names(prediction) == "cases")] <- "prediction (cases)"

  if (async){
    prediction$`prediction (rate)` <- terra::wrap(prediction$`prediction (rate)`)
    prediction$`prediction (cases)` <- terra::wrap(prediction$`prediction (cases)`)
    prediction$covariates <- terra::wrap(prediction$covariates)

    if (!is.null(prediction$field)){
      prediction$field <- terra::wrap(prediction$field)
    }
    if (predict_iid){
      prediction$iid <- terra::wrap(prediction$iid)
    }
    if (uncertain){
      prediction$uncertainty$predictions_ci$`lower CI` <- terra::wrap(prediction$uncertainty$predictions_ci$`lower CI`)
      prediction$uncertainty$predictions_ci$`upper CI` <- terra::wrap(prediction$uncertainty$predictions_ci$`upper CI`)
    }
  }

  prediction

}
