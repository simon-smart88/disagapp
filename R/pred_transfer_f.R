#' @title Transfer predictions to a new country
#' @description
#' This function is called by the pred_transfer module and can transfer predictions
#' generated from a model to a new area of interest.
#'
#' @param country character. ISO3 code of the country.
#' @param common The common data structure.
#' @param covdf dataframe. As produced by `shiny::fileInput`, containing `name` and
#' `datapath` columns of covariates
#' @param aggdf dataframe. As produced by `shiny::fileInput`, containing `name` and
#' `datapath` columns of the aggregation raster
#' @param logger Stores all notification messages to be displayed in the Log
#' Window. Insert the logger reactive list here for running in
#' shiny, otherwise leave the default NULL
#' @return a list of SpatRaster objects
#' @author Simon Smart <simon.smart@@cantab.net>
#' @export

pred_transfer <- function(country, common, covdf = NULL, aggdf = NULL, logger = NULL) {

# fetch boundary
url <- glue::glue("https://www.geoboundaries.org/api/current/gbOpen/{country}/ADM1/")
req <- httr2::request(url) |> httr2::req_perform()
cont <- httr2::resp_body_json(req)
new_shape <- sf::st_read(cont$gjDownloadURL, quiet = TRUE)

# find which covariates were used and download versions for new shape
covs <- list()
cov_modules <- names(common$meta)[grepl("cov_", names(common$meta))]

if ("cov_access" %in% cov_modules){
  covs[[common$meta$cov_access$layer]] <- cov_access(new_shape, common$meta$cov_access$layer)
}

if ("cov_bioclim" %in% cov_modules){
  covs <- append(covs, cov_bioclim(country, common$meta$cov_bioclim$variables, new_shape))
}

if ("cov_landuse" %in% cov_modules){
  covs <- append(covs, cov_landuse(new_shape, common$meta$cov_landuse$year, common$meta$cov_landuse$uses))
}

if ("cov_nightlight" %in% cov_modules){
  if (Sys.getenv("NASA_bearer") != ""){
    bearer = Sys.getenv("NASA_bearer")
  } else {
    bearer = common$meta$cov_nightlight$bearer
  }
  covs[["Nighttime light"]] <- cov_nightlight(new_shape, common$meta$cov_nightlight$year, bearer)
}

if ("cov_water" %in% cov_modules){
  if (Sys.getenv("ARCGIS_CLIENT") != ""){
    token <- arcgisutils::auth_client()
  } else {
    token <- httr2::oauth_token(common$meta$cov_water$token, arcgis_host = arcgisutils::arc_host())
  }
  covs[["Distance to water"]] <-  cov_water(new_shape, token)
}

if ("cov_upload" %in% cov_modules){
  covs <- append(covs, cov_upload(covdf, new_shape))
}

# fetch aggregation
agg_modules <- names(common$meta)[grepl("agg_", names(common$meta))]

if ("agg_worldpop" %in% agg_modules){
  agg <- agg_worldpop(new_shape, country, common$meta$agg_worldpop$method,
                      common$meta$agg_worldpop$resolution,
                      common$meta$agg_worldpop$year)
}

if ("agg_upload" %in% agg_modules){
  agg <- agg_upload(aggdf$path, new_shape)
}

if ("agg_uniform" %in% agg_modules){
  agg <- agg_uniform(covs[[1]])
}

# resample
covs_prep <- lapply(covs, terra::resample, covs[[common$meta$prep_summary$resample_target]])
agg <- terra::resample(agg, covs[[common$meta$prep_summary$resample_target]], method = "sum")

# scale covariates using original parameters
if (!is.null(common$meta$prep_scale$used)){
  cov_names <- names(covs_prep)
  scaling_parameters <- common$meta$prep_scale$parameters

  for (cov_layer in cov_names){
    layer_mean <- scaling_parameters$mean[row.names(scaling_parameters) == cov_layer]
    layer_rms <- scaling_parameters$rms[row.names(scaling_parameters) == cov_layer]
    residual <- covs_prep[[cov_layer]] - layer_mean
    covs_prep[[cov_layer]] <- residual / layer_rms
  }
}

# list to stack
covs_prep <- terra::rast(covs_prep)

if (common$meta$prep_final$resolution == "High resolution"){

  # generate the prediction
  transfer <- disaggregation::predict_model(common$fit, new_data = covs_prep)

  # convert to cases
  transfer$agg <- agg
  transfer$cases <- transfer$prediction * transfer$agg
  transfer$covariates <- covs_prep
}

if (common$meta$prep_final$resolution == "Low resolution"){

  # resolution
  covs_prep_lores <- terra::aggregate(covs_prep, fact = common$meta$prep_resolution$factor, fun = "mean")
  agg_lores <- terra::aggregate(agg, fact = common$meta$prep_resolution$factor, fun = "sum")

  # generate the prediction
  transfer <- disaggregation::predict_model(common$fit, new_data = covs_prep_lores)

  # convert to cases
  transfer$agg <- agg_lores
  transfer$cases <- transfer$prediction * transfer$agg
  transfer$covariates <- covs_prep_lores
}


return(transfer)
}
