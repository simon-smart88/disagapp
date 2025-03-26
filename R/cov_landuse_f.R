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
    return(async %>% asyncLog(type = "error", "shape must be an sf object"))
  }

  if (year > 2019 | year < 2015){
    return(async %>% asyncLog(type = "error", "Land use data is only available between 2015 and 2019"))
  }

  valid_uses <- c("Bare", "BuiltUp", "Crops", "Grass", "MossLichen",
  "PermanentWater", "SeasonalWater", "Shrub", "Snow", "Tree")
  invalid_uses <- landuses[(!landuses %in% valid_uses)]
  if (length(invalid_uses) > 0){
    return(async %>% asyncLog(type = "error", glue::glue("{invalid_uses} is not a valid land use type. ")))
  }

  # determine 20 degree tiles covering entire shape
  bbx <- sf::st_bbox(shape) / 20
  min_x_tile <- floor(bbx["xmin"]) * 20
  max_x_tile <- (ceiling(bbx["xmax"]) * 20) - 20
  min_y_tile <- (floor(bbx["ymin"]) * 20) + 20
  max_y_tile <- (ceiling(bbx["ymax"]) * 20)
  x_all <- seq(from = min_x_tile, to = max_x_tile, 20)
  y_all <- seq(from = min_y_tile, to = max_y_tile, 20)
  tiles <- expand.grid(x_all, y_all)
  colnames(tiles) <- c("x", "y")
  tiles$x_str <- formatC(tiles$x, width = 4, format = "d", flag = "0+")
  tiles$y_str <- formatC(tiles$y, width = 3, format = "d", flag = "0+")
  tiles$y_str <- gsub("-", "S", tiles$y_str)
  tiles$y_str <- gsub("\\+", "N", tiles$y_str)
  tiles$x_str <- gsub("-", "W", tiles$x_str)
  tiles$x_str <- gsub("\\+", "E", tiles$x_str)
  tiles$url <- paste0(tiles$x_str, tiles$y_str,"/", tiles$x_str, tiles$y_str)

  # check whether each tile contains part of the region of interest
  # needed in case of multiple countries / empty tiles
  valid_tiles <- NA

  for (r in 1:nrow(tiles)){
    t <- tiles[r,]

    tile_coords <- matrix(c(
      t$x, t$y - 20,
      t$x + 20, t$y - 20,
      t$x + 20, t$y,
      t$x, t$y,
      t$x, t$y - 20
    ), ncol = 2, byrow = TRUE)

    tile_polygon <- sf::st_polygon(list(tile_coords))
    tile_sf <- sf::st_sfc(tile_polygon, crs = 4326)
    tile_sf <- sf::st_sf(geometry = tile_sf)
    valid_tiles[r] <- sum(sf::st_intersects(shape, tile_sf) |> lengths())
  }

  tiles <- tiles[valid_tiles > 0,]

  # request each tile
  raster_layers <- list()

  tryCatch({
    for (l in landuses){
      raster_tiles <- NULL
      for (t in tiles$url){
        url <- glue::glue("/vsicurl/https://s3-eu-west-1.amazonaws.com/vito.landcover.global/v3.0.1/{year}/{t}_PROBAV_LC100_global_v3.0.1_2019-nrt_{l}-CoverFraction-layer_EPSG-4326.tif")
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
    return(async %>% asyncLog(type = "error", message))
  } else {
  if (async){
    raster_layers <- wrap_terra(raster_layers)
  }
  return(raster_layers)
  }
}
