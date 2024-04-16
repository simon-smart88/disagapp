#' @title agg_upload
#' @description
#' This function is called by the agg_upload module and loads a
#'  raster image.
#'
#' @param path character. The location of the file to be loaded.
#' @param shape sf. sf object containing the area of interest
#' @param logger Stores all notification messages to be displayed in the Log
#' Window. Insert the logger reactive list here for running in
#' shiny, otherwise leave the default NULL
#' @return a SpatRaster object
#' @author Simon Smart <simon.smart@@cantab.net>
#' @export

 agg_upload <- function(path, shape, logger = NULL) {
  agg <- terra::rast(path)

  #check crs and reproject if necessary
  ras_crs <- terra::crs(agg, describe = TRUE)

  if (is.na(ras_crs$code)){
    logger %>% writeLog(type = "error", "The uploaded file does not have a coordinate reference system")
    return(NULL)
  }

  if (ras_crs$code != "4326"){
    agg <- terra::project(agg, "EPSG:4326")
  }

  #check that raster overlaps with shape
  check_overlap <- terra::is.related(agg, terra::vect(shape), "intersects")
  if (check_overlap == FALSE){
    logger %>% writeLog(type = "error", "The uploaded file does not overlap with the response data")
    return(NULL)
  }

  # crop and mask
  agg <- terra::crop(agg, shape)
  agg <- terra::mask(agg, shape)

  return(agg)
}
