#' @title cov_landuse
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
#' @export

cov_landuse <- function(shape, year, landuses, async = FALSE) {

  #determine which 20 degree tiles are required
  bbx <- sf::st_bbox(shape)/20
  min_x_tile <- floor(bbx["xmin"]) * 20
  max_x_tile <- (ceiling(bbx["xmax"]) * 20) - 20
  min_y_tile <- (floor(bbx["ymin"]) * 20) + 20
  max_y_tile <- (ceiling(bbx["ymax"]) * 20)
  x_all <- seq(from = min_x_tile, to = max_x_tile, 20)
  y_all <- seq(from = min_y_tile, to = max_y_tile, 20)
  tiles <- expand.grid(x_all,y_all)
  colnames(tiles) <- c("x", "y")
  tiles$x_str <- formatC(tiles$x, width = 4, format = "d", flag = "0+")
  tiles$y_str <- formatC(tiles$y, width = 3, format = "d", flag = "0+")
  tiles$y_str <- gsub("-", "S", tiles$y_str)
  tiles$y_str <- gsub("\\+", "N", tiles$y_str)
  tiles$x_str <- gsub("-", "W", tiles$x_str)
  tiles$x_str <- gsub("\\+", "E", tiles$x_str)
  tiles$url <- paste0(tiles$x_str, tiles$y_str,"/", tiles$x_str, tiles$y_str)

  #request each tile
  raster_layers <- list()

  tryCatch({
    for (l in landuses){
      raster_tiles <- NULL
      for (t in tiles$url){
        url <- glue::glue("https://s3-eu-west-1.amazonaws.com/vito.landcover.global/v3.0.1/{year}/{t}_PROBAV_LC100_global_v3.0.1_2019-nrt_{l}-CoverFraction-layer_EPSG-4326.tif")
        ras <- terra::rast(url)
        if (is.null(raster_tiles)){
          raster_tiles <- ras
        } else {
          raster_tiles <- terra::merge(raster_tiles, ras)
        }
      }
      tile_name <- paste0(l," land use")
      raster_tiles <- terra::crop(raster_tiles, shape)
      raster_tiles <- terra::mask(raster_tiles, shape)
      raster_tiles <- terra::aggregate(raster_tiles, fact = 10, fun = "mean")
      raster_layers[[tile_name]] <- raster_tiles
      names(raster_layers[[tile_name]]) <- tile_name
    }
  },
  error = function(x){
    message <- paste0("An error occurred whilst trying to download land use data: ", x)
    NULL},
  warning = function(x){
    message <- paste0("An error occurred whilst trying to download land use data: ", x)
    NULL}
  )

  if (length(raster_layers) != length(landuses)){
    if (async){
      return(message)
    } else {
      stop(message)
    }
  } else {
  if (async){
    raster_layers <- wrap_terra(raster_layers)
  }
  return(raster_layers)
  }
}
