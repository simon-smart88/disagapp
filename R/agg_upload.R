#' @title agg_upload
#' @description
#' This function is called by the agg_upload module and loads a
#'  raster image.
#'
#' @param path character. The location of the file to be loaded.
#' @return a SpatRaster object
#' @author Simon Smart <simon.smart@@cantab.net>
#' @export

 agg_upload <- function(path) {
  agg <- terra::rast(path)
  agg
}
