#' @title Transfer predictions to a new country
#' @description
#' This function is called by the pred_transfer module and can transfer predictions
#' generated from a model to a new area of interest.
#'
#' @param country character. ISO3 code of the country.
#' @param fit disag_model. Object returned by disag_model function that
#' contains all the necessary objects for generating predictions.
#' @param meta list. The metadata object
#' @param covdf dataframe. As produced by `fileInput`, containing `name` and
#' `datapath` columns of covariates
#' @param aggdf dataframe. As produced by `fileInput`, containing `name` and
#' `datapath` columns of the aggregation raster
#' @param async logical. Whether or not the function is being used asynchronously
#' @return a list of SpatRaster objects
#' @examples
#' common <- readRDS(system.file("extdata", "fit-minimal.rds", package = "disagapp"))
#' common$fit$data$covariate_rasters <- unwrap_terra(common$fit$data$covariate_rasters)
#' # transfer to Malawi
#' transfer <- pred_transfer("MWI", common$fit, common$meta)
#'
#' @author Simon Smart <simon.smart@@cantab.net>
#' @export

pred_transfer <- function(country, fit, meta, covdf = NULL, aggdf = NULL, async = FALSE) {

  if (async){
    fit$data$covariate_rasters <- terra::unwrap(fit$data$covariate_rasters)
  }

  # fetch boundary
  url <- glue::glue("https://www.geoboundaries.org/api/current/gbOpen/{country}/ADM1/")
  req <- httr2::request(url) |> httr2::req_perform()
  cont <- httr2::resp_body_json(req)
  new_shape <- sf::st_read(cont$gjDownloadURL, quiet = TRUE)

  # find which covariates were used and download versions for new shape
  covs <- list()
  cov_modules <- names(meta)[grepl("cov_", names(meta))]

  if ("cov_access" %in% cov_modules){
    covs[[meta$cov_access$layer]] <- cov_access(new_shape, meta$cov_access$layer)
  }

  if ("cov_bioclim" %in% cov_modules){
    covs <- append(covs, cov_bioclim(new_shape, country, meta$cov_bioclim$variables))
  }
  if ("cov_nightlight" %in% cov_modules){
    covs[["Nighttime light"]] <- cov_nightlight(new_shape, country, meta$cov_nightlight$year)
  }

  if ("cov_water" %in% cov_modules){
    covs[["Distance to water"]] <-  cov_water(new_shape, country)
  }

  if ("cov_upload" %in% cov_modules){
    covs <- append(covs, cov_upload(new_shape, covdf))
  }

  # fetch aggregation
  agg_modules <- names(meta)[grepl("agg_", names(meta))]

  if ("agg_worldpop" %in% agg_modules){
    agg <- agg_worldpop(new_shape, country, meta$agg_worldpop$method,
                        meta$agg_worldpop$resolution,
                        meta$agg_worldpop$year)
  }

  if ("agg_upload" %in% agg_modules){
    agg <- agg_upload(aggdf$path, new_shape)
  }

  if ("agg_uniform" %in% agg_modules){
    agg <- agg_uniform(covs[[1]])
  }

  # resample
  covs_prep <- lapply(covs, terra::resample, covs[[meta$prep_summary$resample_target]])
  agg <- terra::resample(agg, covs[[meta$prep_summary$resample_target]], method = "sum")

  # scale covariates using original parameters
  if (!is.null(meta$prep_scale$used)){
    cov_names <- names(covs_prep)
    scaling_parameters <- meta$prep_scale$parameters

    for (cov_layer in cov_names){
      layer_mean <- scaling_parameters$mean[row.names(scaling_parameters) == cov_layer]
      layer_rms <- scaling_parameters$rms[row.names(scaling_parameters) == cov_layer]
      residual <- covs_prep[[cov_layer]] - layer_mean
      covs_prep[[cov_layer]] <- residual / layer_rms
    }
  }

  # list to stack
  covs_prep <- terra::rast(covs_prep)

  if (meta$prep_final$resolution == "High resolution"){

    # generate the prediction
    transfer <- disaggregation::predict_model(fit, new_data = covs_prep)

    # convert to cases
    transfer$agg <- agg
    transfer$cases <- transfer$prediction * transfer$agg
    transfer$covariates <- covs_prep
  }

  if (meta$prep_final$resolution == "Low resolution"){

    # resolution
    covs_prep_lores <- terra::aggregate(covs_prep, fact = meta$prep_resolution$factor, fun = "mean")
    agg_lores <- terra::aggregate(agg, fact = meta$prep_resolution$factor, fun = "sum")

    # generate the prediction
    transfer <- disaggregation::predict_model(fit, new_data = covs_prep_lores)

    # convert to cases
    transfer$agg <- agg_lores
    transfer$cases <- transfer$prediction * transfer$agg
    transfer$covariates <- covs_prep_lores
  }

  if (async){
    transfer$agg <- terra::wrap(transfer$agg)
    transfer$cases <- terra::wrap(transfer$cases)
    transfer$prediction <- terra::wrap(transfer$prediction)
    transfer$covariates <- terra::wrap(transfer$covariates)
  }

  transfer
}
