#' @title Simplify geometry of boundary data
#' @description
#' This function is called by the resp_simplify module and simplifies the
#' geometry data in an sf object
#'
#' @param shape sf. The sf object to be simplified
#' @param distance numeric. The distance in meters to simplify the geometry by
#' @param logger Stores all notification messages to be displayed in the Log
#' Window. Insert the logger reactive list here for running in
#' shiny, otherwise leave the default NULL
#' @return an sf object
#' @author Simon Smart <simon.smart@@cantab.net>
#' @examples
#' poly <- sf::st_polygon(list(rbind(
#'         c(0, 0), c(1, 0.25),
#'         c(2, 0), c(2.25, 1),
#'         c(2, 2), c(1, 2.25),
#'         c(0, 2), c(0.25, 1),
#'         c(0, 0))))
#' shape <- sf::st_sf(geometry = sf::st_sfc(poly))
#' simplified_shape <- resp_simplify(shape, 0.5)
#'
#' @export
#'
resp_simplify <- function(shape, distance, logger = NULL) {

  if (!inherits(shape, "sf")){
    logger |> writeLog(type = "error", "shape must be an sf object")
    return()
  }

  if (!inherits(distance, "numeric")){
    logger |> writeLog(type = "error", "distance must be numeric")
    return()
  }

  shape <- sf::st_transform(shape, 25832)
  shape <- sf::st_simplify(shape, preserveTopology = TRUE, dTolerance = distance)
  shape <- sf::st_transform(shape, 4326)

  return(shape)

}



