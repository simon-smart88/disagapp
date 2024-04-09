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
#' @param logger Stores all notification messages to be displayed in the Log
#' Window. Insert the logger reactive list here for running in
#' shiny, otherwise leave the default NULL
#' @return a SpatRaster object
#' @author Simon Smart <simon.smart@@cantab.net>
#' @export

cov_water <- function(shape, token, logger = NULL) {

  if (!("sf" %in% class(shape))){
    logger %>% writeLog(type = "error", "Shape must be an sf object")
    return()
  }

  arcgisutils::set_arc_token(token)

  furl <- "https://landscape6.arcgis.com/arcgis/rest/services/World_Distance_to_Surface_Water/ImageServer"

  flayer <- arcgislayers::arc_open(furl)

  bbox <- sf::st_bbox(shape)

  ras <- arcgislayers::arc_raster(flayer, xmin = bbox[[1]], xmax = bbox[[3]],
                                  ymin = bbox[[2]], ymax = bbox[[4]],
                                  crs = sf::st_crs(shape))

  ras <- terra::clamp(ras, upper = 60000, value = FALSE)
  ras <- ras/250 #convert to distance in km
  ras <- terra::project(ras, "+proj=longlat +datum=WGS84")
  ras <- terra::crop(ras, shape, mask = TRUE)

  names(ras) <- "Distance to water"

  return(ras)
}
