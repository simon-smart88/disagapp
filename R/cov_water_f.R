#' @title Download data on the distance to water from ArcGIS
#' @description
#' This function is called by the cov_water module and downloads data on the
#' distance to surface water from ArcGIS
#'
#' @param shape sf. sf object containing the area of interest
#' @param token httr2_token. ArcGIS access token.
#' \href{https://developers.arcgis.com/sign-up/}{Click here} to obtain yours
#' and either provide that or set the `ARCGIS_CLIENT` and `ARCGIS_SECRET`
#' environmental variables to automate it as
#' \href{https://r.esri.com/r-bridge-site/location-services/connecting-to-a-portal.html}{explained here}.
#' @param async Whether or not the function is being used asynchronously. When
#' `TRUE` the returned object is a wrapped SpatRaster.
#' @return a SpatRaster object when `async` is `FALSE` or a PackedSpatRaster
#' when `async` is `TRUE`.
#' @author Simon Smart <simon.smart@@cantab.net>
#' @examples
#' \dontrun{
#' x_min <- 0
#' x_max <- 0.5
#' y_min <- 52
#' y_max <- 52.5
#' poly_matrix <- matrix(c(x_min, x_min, x_max, x_max, x_min,
#'                         y_min, y_max, y_max, y_min, y_min), ncol = 2)
#' poly <- sf::st_polygon(list(poly_matrix))
#' shape <- sf::st_sf(1, geometry = list(poly))
#' sf::st_crs(shape) = 4326
#' raster <- cov_water(shape = shape, token = arcgisutils::auth_client())
#' }
#'
#' @export

cov_water <- function(shape, token, async = FALSE) {

  message <- NULL

  if (!requireNamespace("arcgislayers", quietly = TRUE)){
    return(async |> asyncLog(type = "error", 'This module requires the arcgislayers package to be installed. Close the app, run install.packages("arcgislayers") and try again'))
  }

  if (!inherits(shape, "sf")){
    return(async |> asyncLog(type = "error", "Shape must be an sf object"))
  }

  if (!inherits(token, "httr2_token")){
    return(async |> asyncLog(type = "error", "Token must be an httr2_token"))
  }

  if (!check_url("https://landscape6.arcgis.com")){
    return(async |> asyncLog(type = "error", "Sorry the distance to water data source is currently offline"))
  }

  arcgisutils::set_arc_token(token)

  furl <- "https://landscape6.arcgis.com/arcgis/rest/services/World_Distance_to_Surface_Water/ImageServer"

  flayer <- tryCatch({arcgislayers::arc_open(furl)},
                     error = function(x){
                     message <- paste0("An error occurred whilst trying to download distance to water data: ", x)
                     NULL},
                     warning = function(x){
                     message <- paste0("An error occurred whilst trying to download distance to water data: ", x)
                     NULL}
                     )

  if (is.null(flayer)){
    return(async |> asyncLog(type = "error", message))
  }

  if (!is.null(flayer)){

    # check size of shape and split into tiles if any dimension is over 3000 km
    contiguous_shape <- shape |> sf::st_union() |> sf::st_sf()
    shape_metres <- sf::st_transform(contiguous_shape, crs = 5070)
    bbox_metres <- sf::st_bbox(shape_metres)
    if (abs(bbox_metres[[1]]-bbox_metres[[3]]) > 3000000 || abs(bbox_metres[[2]]-bbox_metres[[4]]) > 3000000){
      grid <- sf::st_make_grid(shape_metres, cellsize = c(3000000, 3000000), what = "polygons") |>  sf::st_sf()
      chunk_metres <- sf::st_intersection(shape_metres, grid)
      chunks <- sf::st_transform(chunk_metres, crs = 4326)
    } else {
      chunks <- contiguous_shape
    }

    ras <- NULL

    for (i in 1:nrow(chunks)) {
      current_chunk <- chunks[i, ]

      bbox <- sf::st_bbox(current_chunk)
      ras_chunk <- arcgislayers::arc_raster(flayer, xmin = bbox[[1]], xmax = bbox[[3]],
                                      ymin = bbox[[2]], ymax = bbox[[4]],
                                      crs = sf::st_crs(shape))
      if (is.null(ras)){
        ras <- ras_chunk
      } else {
        ras <- terra::merge(ras, ras_chunk)
      }
    }

    ras <- terra::clamp(ras, upper = 60000, value = FALSE)
    ras <- ras / 4 # convert to distance in km
    ras <- terra::project(ras, "+proj=longlat +datum=WGS84")
    ras <- terra::crop(ras, shape, mask = TRUE)

    names(ras) <- "Distance to water"
    if (async){
      ras <- wrap_terra(ras)
    }
    return(ras)
  }



}
