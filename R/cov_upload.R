#' @title cov_upload
#' @description
#' This function is called by the cov_upload module and loads a
#'  raster image.
#'
#' @param ras_path character. Path to file to be loaded
#' @return a SpatRaster object
#' @author Simon Smart <simon.smart@@cantab.net>
#' @export

 cov_upload <- function(covdf) {
  covs <- lapply(covdf$datapath,terra::rast)
  names(covs) <- as.vector(covdf$name)
  return(covs)
}
