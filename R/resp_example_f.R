#' @title Load example datasets
#' @description
#' This function is called by the resp_example module and loads example datasets
#'
#' @param dataset character. The dataset to load. Either `mad`, `nys` or `scot`
#' @param logger Stores all notification messages to be displayed in the Log
#' Window. Insert the logger reactive list here for running in
#' shiny, otherwise leave the default NULL
#' @return an sf object
#' @author Simon Smart <simon.smart@@cantab.net>
#' @examples
#' shape <- resp_example("mad")
#'
#' @export
resp_example <- function(dataset, logger = NULL){

  if (!inherits(dataset, "character")){
    logger |> writeLog(type = "error", "dataset must be a character string")
    return()
  }

  if (!(dataset %in% c("mad", "nys", "scot"))){
    logger |> writeLog(type = "error", "dataset must either 'mad', 'nys' or 'scot'")
    return()
  }

  switch(dataset,
         "mad" = {
           shape <- sf::st_read(system.file("extdata", "shapes", "mdg_shapes.shp", package="disagapp"))
         },
         "nys" = {
           shape <- SpatialEpi::NYleukemia_sf
         },
         "scot" = {
           shape <- SpatialEpi::scotland_sf
           shape$geometry <- shape$geometry * 1000
           shape <- sf::st_set_crs(shape, 27700)
           shape <- sf::st_transform(shape, crs = 4326)
         }
  )

  shape
}

