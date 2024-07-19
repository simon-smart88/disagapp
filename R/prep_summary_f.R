#' @title prep_summary
#' @description
#' Summarises the properties of a list of SpatRasters
#'
#' @param covs list. List of SpatRasters
#' @param remove logical. Whether to remove columns where all values are equal
#' @param logger Stores all notification messages to be displayed in the Log
#'   Window. Insert the logger reactive list here for running in
#'   shiny, otherwise leave the default NULL
#' @return a dataframe containing columns for the resolution, origin, min and
#' max coordinates, coordinate reference system and number of cells.
#' @author Simon Smart <simon.smart@@cantab.net>
#' @examples
#' covariate_files <- list.files(system.file("extdata/covariates",
#'                               package = "disagapp"), full.names = TRUE)
#' covariate_list <- lapply(covariate_files, terra::rast)
#' covariates <- terra::rast(covariate_list)
#' covariate_summary <- prep_summary(covs = covariates)
#' @export

prep_summary <- function(covs, remove = FALSE, logger = NULL){

  if (!("SpatRaster" %in% class(covs))){
    logger |> writeLog(type = "error", "covs must be a SpatRaster")
    return()
  }

  cov_res <- lapply(covs, terra::res)
  x_res <- unlist(cov_res)[seq(1,length(covs)*2, 2)]
  y_res <- unlist(cov_res)[seq(2,length(covs)*2, 2)]

  cov_ext <- lapply(covs, terra::ext)
  cov_ext <- lapply(cov_ext,as.vector)
  cov_ext <- lapply(cov_ext,unlist)
  cov_ext <- as.data.frame(t(as.data.frame(cov_ext)))

  cov_origin <- lapply(covs, terra::origin)
  x_origin <- format(unlist(cov_origin)[seq(1, length(covs)*2, 2)], scientific = TRUE)
  y_origin <- format(unlist(cov_origin)[seq(2, length(covs)*2, 2)], scientific = TRUE)

  cov_crs <- unlist(lapply(covs, terra::crs, proj = TRUE))
  crs_proj <- sub("^.*\\bproj=([^\\s]+).*", "\\1", cov_crs, perl=TRUE)
  crs_datum <- sub("^.*\\bdatum=([^\\s]+).*", "\\1", cov_crs, perl=TRUE)

  cov_ncell <- lapply(covs, terra::ncell)

  cov_df <- data.frame(x_res, y_res,
                       x_origin, y_origin,
                       crs_proj, crs_datum,
                       number_of_cells = unlist(cov_ncell))
  row.names(cov_df) <- names(covs)
  cov_df <- cbind(cov_df, cov_ext)

  colnames(cov_df) <- c("X resolution", "Y resolution",
                        "X origin", "Y origin",
                        "CRS projection", "CRS datum",
                        "Number of pixels",
                        "X minimum", "X maximum",
                        "Y minimum", "Y maximum"
                        )

  if (remove == TRUE){
    # remove columns with the same values
    cov_df <- cov_df[vapply(cov_df, function(x) length(unique(x)) > 1, logical(1L))]
  }
  return(t(cov_df))
}
