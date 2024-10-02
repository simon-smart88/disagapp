#' @title Get predictions from a disaggregation model
#' @description
#' This function is called by the pred_pred module and calls
#' disaggregation::predict_model() and disaggregation::predict_uncertainty() and
#' also generates predicted cases. SpatRasters are wrapped to enable asynchronous
#' operation.
#'
#' @param fit disag_model. Object returned by disag_model function that
#' contains all the necessary objects for the model fitting
#' @param aggregation SpatRaster. List of prior values
#' @param predict_iid logical. Whether to include an iid effect
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
  fit$data$covariate_rasters <- unwrap_terra(fit$data$covariate_rasters)
  aggregation <- unwrap_terra(aggregation)

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
    prediction$`prediction (rate)` <- wrap_terra(prediction$`prediction (rate)`)
    prediction$`prediction (cases)` <- wrap_terra(prediction$`prediction (cases)`)
    prediction$covariates <- wrap_terra(prediction$covariates)
    prediction$field <- wrap_terra(prediction$field)
    prediction$iid <- wrap_terra(prediction$iid)
    prediction$uncertainty$predictions_ci$`lower CI` <- wrap_terra(prediction$uncertainty$predictions_ci$`lower CI`)
    prediction$uncertainty$predictions_ci$`upper CI` <- wrap_terra(prediction$uncertainty$predictions_ci$`upper CI`)
  }

  prediction

}
