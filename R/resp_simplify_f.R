#' @title resp_simplify
#' @description
#' This function is called by the resp_simplify module and simplifies the
#' geometry data in an sf object
#'
#' @param shape sf. The sf object to be simplified
#' @param distance numeric. The distance
#' @return an sf object
#' @author Simon Smart <simon.smart@@cantab.net>
#' @export
#'
resp_simplify <- function(shape, distance) {

  shape <- sf::st_simplify(shape, preserveTopology = TRUE, dTolerance = distance)

  return(shape)

}



