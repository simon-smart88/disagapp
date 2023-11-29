#' @title cov_upload
#' @description
#' This function is called by the cov_upload module and loads a
#'  raster image.
#'
#' @param path_df data.frame. Containing `datapath` and `name` columns of the
#' file(s) to be uploaded.
#' @return a list containing SpatRaster objects
#' @author Simon Smart <simon.smart@@cantab.net>
#' @export

 cov_upload <- function(path_df) {
  covs <- lapply(path_df$datapath,terra::rast)
  names(covs) <- as.vector(path_df$name)
  return(covs)
}
