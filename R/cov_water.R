#' @title cov_water
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
#' @export

cov_water <- function(shape, token, async = FALSE) {

  message <- NULL

  if (!("sf" %in% class(shape))){
    message <- "Shape must be an sf object"
    if (async){
      return(message)
    } else {
      stop(message)
    }
  }

  if (!("httr2_token" %in% class(token))){
    message <- "Token must be an httr2_token"
    if (async){
      return(message)
    } else {
      stop(message)
    }
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
    if (async){
      return(message)
    } else {
      stop(message)
    }
  }

  if (!is.null(flayer)){
    bbox <- sf::st_bbox(shape)

    ras <- arcgislayers::arc_raster(flayer, xmin = bbox[[1]], xmax = bbox[[3]],
                                    ymin = bbox[[2]], ymax = bbox[[4]],
                                    crs = sf::st_crs(shape))

    ras <- terra::clamp(ras, upper = 60000, value = FALSE)
    ras <- ras/250 #convert to distance in km
    ras <- terra::project(ras, "+proj=longlat +datum=WGS84")
    ras <- terra::crop(ras, shape, mask = TRUE)

    names(ras) <- "Distance to water"
    if (async){
      ras <- wrap_terra(ras)
    }
    return(ras)
  }



}
