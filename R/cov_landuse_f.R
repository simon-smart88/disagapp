#' @title Download land use data from Copernicus Global Land Service
#' @description
#' This function is called by the cov_landuse module and downloads data on the
#' percentage of land used for each category selected. Data is obtained at a
#' 100 m resolution from the Copernicus Global Land Service e.g.
#' https://zenodo.org/records/3939038 as documented here:
#' https://zenodo.org/records/4723921
#'
#' @param shape sf. sf object containing the area of interest
#' @param year numeric. The requested year (2015-2019 only)
#' @param landuses vector. List of the requested land use types from the
#' following options: `Bare`, `BuiltUp`, `Crops`, `Grass`, `MossLichen`,
#' `PermanentWater`, `SeasonalWater`, `Shrub`, `Snow`, `Tree`
#' @param async logical. Whether or not the function is being used asynchronously
#' @return a list of containing an item for each land use, either SpatRaster
#' objects when `async` is `FALSE` or PackedSpatRaster objects when `async` is
#' `TRUE`
#' @author Simon Smart <simon.smart@@cantab.net>
#' @examples
#' x_min <- 0
#' x_max <- 0.5
#' y_min <- 52
#' y_max <- 52.5
#' poly_matrix <- matrix(c(x_min, x_min, x_max, x_max, x_min,
#'                         y_min, y_max, y_max, y_min, y_min), ncol = 2)
#' poly <- sf::st_polygon(list(poly_matrix))
#' shape <- sf::st_sf(1, geometry = list(poly))
#' sf::st_crs(shape) = 4326
#' raster <- cov_landuse(shape = shape,
#'                       year = 2019,
#'                       landuses = "Crops")
#'
#' @export

cov_landuse <- function(shape, year, landuses, async = FALSE) {

  if (!inherits(shape, "sf")){
    return(async |> asyncLog(type = "error", "shape must be an sf object"))
  }

  if (!inherits(year, "numeric")){
    return(async |> asyncLog(type = "error", "year must be a number"))
  }

  if (year > 2019 | year < 2015){
    return(async |> asyncLog(type = "error", "Land use data is only available between 2015 and 2019"))
  }

  valid_uses <- c("Bare", "BuiltUp", "Crops", "Grass", "MossLichen",
  "PermanentWater", "SeasonalWater", "Shrub", "Snow", "Tree")
  invalid_uses <- landuses[(!landuses %in% valid_uses)]
  if (length(invalid_uses) > 0){
    return(async |> asyncLog(type = "error", glue::glue("{invalid_uses} is not a valid land use type. ")))
  }

  if (!check_url("https://zenodo.org/records/3939038")){
    return(async |> asyncLog(type = "error", "Sorry the land use data source is currently offline"))
  }

  # increase chunk size and reset on exit
  terra::setGDALconfig("CPL_VSIL_CURL_CHUNK_SIZE",  10 * 1024 * 1024)
  on.exit(terra::setGDALconfig("CPL_VSIL_CURL_CHUNK_SIZE",  ""))

  # id for each year from 2015:2019
  zenodo_ids <- c("3939038", "3518026", "3518036", "3518038", "3939050")
  zenodo_id <- zenodo_ids[year - 2014]

  # part of file name
  file_names <- c("base", rep("conso", 3), "nrt")
  file_name <- file_names[year - 2014]

  # request each tile
  raster_layers <- list()

  tryCatch({
    message <- NULL

    for (l in landuses){
      url <- glue::glue("/vsicurl/https://zenodo.org/records/{zenodo_id}/files/PROBAV_LC100_global_v3.0.1_{year}-{file_name}_{l}-CoverFraction-layer_EPSG-4326.tif")
      layer <- terra::rast(url, win = terra::ext(shape))
      resampled_layer <- terra::aggregate(layer, fact = 10, fun = "mean")
      layer_name <- paste0(l," land use")
      resampled_layer <- terra::crop(resampled_layer, shape)
      resampled_layer <- terra::mask(resampled_layer, shape)
      raster_layers[[layer_name]] <- resampled_layer
      names(raster_layers[[layer_name]]) <- layer_name
    }
  },
  error = function(x){
    message <<- paste0("An error occurred whilst trying to download land use data: ", x$message)
    NULL},
  warning = function(x){
    message <<- paste0("An error occurred whilst trying to download land use data: ", x$message)
    NULL}
  )

  if (length(raster_layers) != length(landuses)){
    return(async |> asyncLog(type = "error", message))
  } else {
  if (async){
    raster_layers <- wrap_terra(raster_layers)
  }
  return(raster_layers)
  }
}
