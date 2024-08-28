#' @title Upload an aggregation raster
#' @description
#' This function is called by the agg_upload module and loads a
#'  raster image.
#'
#' @param shape sf. sf object containing the area of interest
#' @param path character. The location of the file to be loaded.
#' @param logger Stores all notification messages to be displayed in the Log
#' Window. Insert the logger reactive list here for running in
#' shiny, otherwise leave the default NULL
#' @return a SpatRaster object
#' @author Simon Smart <simon.smart@@cantab.net>
#' @examples
#' path <- list.files(system.file("extdata/aggregation",
#'         package="disagapp"), full.names = TRUE)
#' shp_file <- list.files(system.file("extdata/shapes",
#'         package="disagapp"), pattern = ".shp", full.names = TRUE)
#' shape <- sf::st_read(shp_file, quiet = TRUE)
#' raster <- agg_upload(shape = shape, path = path)
#'
#' @export

agg_upload <- function(shape, path, logger = NULL) {

  if (!inherits(path, "character")){
    logger |> writeLog(type = "error", "path must be a character string")
    return()
  }

  if (!(tools::file_ext(path) %in% c("tif", "tiff"))){
    logger |> writeLog(type = "error", "path must a tif or tiff file")
    return()
  }

  if (!inherits(shape, "sf")){
    logger |> writeLog(type = "error", "shape must be an sf object")
    return()
  }

  agg <- terra::rast(path)

  #check crs and reproject if necessary
  ras_crs <- terra::crs(agg, describe = TRUE)

  if (is.na(ras_crs$code)){
    logger |> writeLog(type = "error", "The uploaded file does not have a coordinate reference system")
    return()
  }

  if (ras_crs$code != "4326"){
    agg <- terra::project(agg, "EPSG:4326")
  }

  #check that raster overlaps with shape
  check_overlap <- terra::is.related(agg, terra::vect(shape), "intersects")
  if (check_overlap == FALSE){
    logger |> writeLog(type = "error", "The uploaded file does not overlap with the response data")
    return()
  }

  # crop and mask
  agg <- terra::crop(agg, shape)
  agg <- terra::mask(agg, shape)

  return(agg)
}
