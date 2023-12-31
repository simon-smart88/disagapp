#' @title agg_uniform
#' @description
#' This function is called by the agg_uniform module and creates a
#' uniform aggregation raster.
#'
#' @param template SpatRaster. Raster image to use as a template
#' @return a SpatRaster object
#' @author Simon Smart <simon.smart@@cantab.net>
#' @export


agg_uniform <- function(template){
aggregation_raster <- terra::setValues(template, rep(1, terra::ncell(template)))
names(aggregation_raster) <- 'aggregation_raster'
return(aggregation_raster)
}
