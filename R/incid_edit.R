#' @title incid_edit
#' @description
#' This function is called by the incid_combo module and merges incidence data
#'  from a spreadsheet with boundary data into an sf object
#'
#' @param shape sf. The sf object to be edited
#' @param poly matrix. Containing latitude and longitude columns
#' @param type character. Either inside or outside to determine whether the
#' polygons to keep are those inside or outside of the polygon
#' @param logger Stores all notification messages to be displayed in the Log
#'   Window. Insert the logger reactive list here for running in
#'   shiny, otherwise leave the default NULL
#' @return an sf object
#' @author Simon Smart <simon.smart@@cantab.net>
#' @export
#'
incid_edit <- function(shape, poly, type, logger = NULL) {

  crop_poly <- sf::st_polygon(list(poly))
  crop_sf <- sf::st_sf(data.frame("name" = "crop"), geometry = sf::st_sfc(crop_poly))
  sf::st_crs(crop_sf) <- 4326

  if (type == "outside"){
  shape <- shape[sf::st_covered_by(shape, crop_sf) %>% lengths == 0,]
  }
  if (type == "inside"){
    shape <- shape[sf::st_covered_by(shape, crop_sf) %>% lengths > 0,]
  }

  return(shape)

}



