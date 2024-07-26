#' @title agg_uniform
#' @description
#' This function is called by the agg_uniform module and creates a
#' uniform aggregation raster.
#'
#' @param template SpatRaster. Raster image to use as a template
#' @param logger Stores all notification messages to be displayed in the Log
#'   Window. Insert the logger reactive list here for running in
#'   shiny, otherwise leave the default NULL
#' @return a SpatRaster object
#' @author Simon Smart <simon.smart@@cantab.net>
#'
#'@examples
#' raster <- terra::rast(nrow = 10, ncol = 10)
#' raster[] <- 1:100
#' uniform_raster <- agg_uniform(template = raster)
#' @export

agg_uniform <- function(template, logger = NULL){

  if (!inherits(template, "SpatRaster")){
    logger |> writeLog(type = "error", "template must be a SpatRaster")
    return()
  }

  aggregation_raster <- terra::setValues(template, rep(1, terra::ncell(template)))
  names(aggregation_raster) <- 'aggregation_raster'
  return(aggregation_raster)
}
