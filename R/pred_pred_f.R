#' @title Get predictions from a disaggregation model
#' @description
#' This function is called by the pred_pred module and calls
#' disaggregation::predict_model() and disaggregation::predict_uncertainty() and
#' also generates predicted cases. SpatRasters are wrapped to enable asynchronous
#' operation.
#'
#' @param fit disag_model. Object returned by disag_model function that
#' contains all the necessary objects for generating predictions.
#' @param cases logical. Whether to predictions of cases. Default `FALSE`
#' @param aggregation SpatRaster. The aggregation raster. Default `NULL`. Must be
#' supplied if `cases` is `TRUE`.
#' @param predict_iid logical. Whether to generate predictions including the iid effect
#' @param uncertain logical. Whether or not to generate upper and lower credible
#' intervals
#' @param N numeric. The number of realisations to use when generating uncertainty
#' predictions. Default `100`
#' @param CI numeric. The credible interval to user when generating uncertainty
#' predictions. Default `0.95`
#' @param async logical. Whether or not the function is being used asynchronously
#' @return a list containing the predictions
#' @examples
#' common <- readRDS(system.file("extdata", "fit-minimal.rds", package = "disagapp"))
#' common$fit$data$covariate_rasters <- unwrap_terra(common$fit$data$covariate_rasters)
#' prediction <- pred_pred(common$fit)
#' @author Simon Smart <simon.smart@@cantab.net>
#' @export

pred_pred <- function(fit, cases = FALSE, aggregation = NULL, predict_iid = FALSE, uncertain = FALSE, N = 100, CI = 0.95, async = FALSE){

  if (!inherits(fit, "disag_model")){
    async |> asyncLog(type = "error", "fit must be a disag_model object")
  }

  if (!inherits(cases, "logical")){
    async |> asyncLog(type = "error", "cases must be either TRUE or FALSE")
  }

  if (!inherits(predict_iid, "logical")){
    async |> asyncLog(type = "error", "predict_iid must be either TRUE or FALSE")
  }

  if (!inherits(uncertain, "logical")){
    async |> asyncLog(type = "error", "uncertainty must be either TRUE or FALSE")
  }

  if (cases && is.null(aggregation)){
    async |> asyncLog(type = "error", "You must supply an aggregation raster when cases is TRUE")
  }

  if (!async && cases && !inherits(aggregation, "SpatRaster")){
    async |> asyncLog(type = "error", "aggregation must be a SpatRaster")
  }

  if (async){
    fit$data$covariate_rasters <- terra::unwrap(fit$data$covariate_rasters)
    if (cases && !inherits(aggregation, "PackedSpatRaster")){
      async |> asyncLog(type = "error", "aggregation must be a PackedSpatRaster")
    } else {
      aggregation <- terra::unwrap(aggregation)
    }
  }

  if (uncertain){
    if (!inherits(N, "numeric")){
      async |> asyncLog(type = "error", "N must be numeric")
    }
    if (!inherits(CI, "numeric")){
      async |> asyncLog(type = "error", "CI must be numeric")
    }
    if (CI > 1){
      async |> asyncLog(type = "error", "CI must 1 or less")
    }
  }

  prediction <- disaggregation::predict_model(fit, predict_iid = predict_iid)

  if (cases){
    prediction$cases <- prediction$prediction * aggregation
  }

  if (!is.null(prediction$field)){
    terra::crs(prediction$field) <- terra::crs(fit$data$covariate_rasters[[1]])
    prediction$field <- terra::mask(prediction$field, fit$data$covariate_rasters[[1]])
  }

  if (!is.null(prediction$iid)){
    prediction$iid <- terra::mask(prediction$iid, fit$data$covariate_rasters[[1]])
  }

  if (uncertain){
    prediction$uncertainty <- disaggregation::predict_uncertainty(fit, predict_iid = predict_iid, N = N, CI = CI)
    prediction$uncertainty_lower <- prediction$uncertainty$predictions_ci$`lower CI`
    prediction$uncertainty_upper <- prediction$uncertainty$predictions_ci$`upper CI`
    prediction$uncertainty <- NULL
  }

  names(prediction)[which(names(prediction) == "prediction")] <- "prediction (rate)"
  if (cases){
    names(prediction)[which(names(prediction) == "cases")] <- "prediction (cases)"
  }


  if (async){
    prediction$`prediction (rate)` <- terra::wrap(prediction$`prediction (rate)`)
    if (cases){
      prediction$`prediction (cases)` <- terra::wrap(prediction$`prediction (cases)`)
    }
    prediction$covariates <- terra::wrap(prediction$covariates)

    if (!is.null(prediction$field)){
      prediction$field <- terra::wrap(prediction$field)
    }
    if (predict_iid){
      prediction$iid <- terra::wrap(prediction$iid)
    }
    if (uncertain){
      prediction$uncertainty_lower <- terra::wrap(prediction$uncertainty_lower)
      prediction$uncertainty_upper <- terra::wrap(prediction$uncertainty_upper)
    }
  }

  prediction

}
