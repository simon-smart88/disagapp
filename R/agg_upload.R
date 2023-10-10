#' @title agg_upload
#' @description
#' This function is called by the agg_upload module and loads a
#'  raster image.
#'
#' @param ras_path character. Path to file to be loaded
#' @return a SpatRaster object
#' @author Simon Smart <simon.smart@@cantab.net>
#' @export

 agg_upload <- function(aggdf) {
  agg <- terra::rast(aggdf$datapath)
  agg
}
