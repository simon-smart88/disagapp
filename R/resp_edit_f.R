#' @title Remove polygons from response data
#' @description
#' This function is called by the resp_edit module and removes polygons from the
#' response data depending on whether they are inside or outside of a polygon.
#'
#' @param shape sf. The sf object to be edited
#' @param poly matrix. Containing latitude and longitude columns
#' @param type character. Either `inside` or `outside` to determine whether the
#' polygons to keep are those inside or outside of the polygon
#' @param logger Stores all notification messages to be displayed in the Log
#'   Window. Insert the logger reactive list here for running in
#'   shiny, otherwise leave the default NULL
#' @return an sf object
#' @author Simon Smart <simon.smart@@cantab.net>
#' @examples
#' polygons <- list()
#' for(i in 1:3) {
#'   row <- ceiling(i/10)
#'   col <- ifelse(i %% 10 != 0, i %% 10, 10)
#'   xmin = 2*(col - 1); xmax = 2*col; ymin = 2*(row - 1); ymax = 2*row
#'   polygons[[i]] <- list(cbind(c(xmin, xmax, xmax, xmin, xmin),
#'                              c(ymax, ymax, ymin, ymin, ymax)))
#' }
#'
#' polys <- lapply(polygons, sf::st_polygon)
#' shape <- sf::st_sf(data.frame(area = c("A", "B", "C")), geometry = polys)
#'
#' x_min <- 3
#' x_max <- 7
#' y_min <- -1
#' y_max <- 3
#' crop_matrix <- matrix(c(x_min, x_min, x_max, x_max, x_min,
#'                         y_min, y_max, y_max, y_min, y_min), ncol = 2)
#'
#' sf::st_crs(shape) <- 4326
#' outside_shape <- resp_edit(shape, crop_matrix, "outside")
#' inside_shape <- resp_edit(shape, crop_matrix, "inside")
#'
#' @export
#'
resp_edit <- function(shape, poly, type, logger = NULL) {

  if (!inherits(shape, "sf")){
    logger |> writeLog(type = "error", "shape must be an sf object")
    return()
  }

  if (!inherits(poly, "matrix")){
    logger |> writeLog(type = "error", "poly must be a matrix")
    return()
  }

  if (!(type %in% c("inside", "outside"))){
    logger |> writeLog(type = "error", "type must be either inside or outside")
    return()
  }

  crop_poly <- sf::st_polygon(list(poly))
  crop_sf <- sf::st_sf(data.frame("name" = "crop"), geometry = sf::st_sfc(crop_poly))
  sf::st_crs(crop_sf) <- 4326

  if (type == "outside"){
    shape <- shape[sf::st_covered_by(shape, crop_sf) |> lengths() == 0,]
  }
  if (type == "inside"){
    shape <- shape[sf::st_covered_by(shape, crop_sf) |> lengths() > 0,]
  }

  return(shape)

}



