#' @title Upload covariates
#' @description
#' This function is called by the cov_upload module. It loads raster images,
#' checks their projection and transforms them if necessary, checks that they
#' overlap with the area of interest and crops and masks them to the area of
#' interest.
#'
#' @param shape sf. sf object containing the area of interest
#' @param path_df data.frame. Containing `datapath` and `name` columns of the
#' file(s) to be uploaded.
#' @param logger Stores all notification messages to be displayed in the Log
#' Window. Insert the logger reactive list here for running in
#' shiny, otherwise leave the default NULL
#' @return a list containing SpatRaster objects
#' @author Simon Smart <simon.smart@@cantab.net>
#' @examples
#' cov_df <- data.frame(datapath = list.files(system.file("extdata", "covariates",
#'                                 package="disagapp"), full.names = TRUE),
#'                     name = list.files(system.file("extdata", "covariates",
#'                            package="disagapp")))
#' shp_file <- list.files(system.file("extdata", "shapes", package="disagapp"),
#'                        pattern = ".shp", full.names = TRUE)
#' shape <- sf::st_read(shp_file, quiet = TRUE)
#' rasters <- cov_upload(shape = shape, path_df = cov_df)
#' @export

 cov_upload <- function(shape, path_df, logger = NULL) {

   # check inputs
   if (!inherits(path_df, "data.frame")){
     logger |> writeLog(type = "error", "path_df must be a data.frame")
     return()
   }

   df_columns <- c("datapath", "name")
   if (!all(df_columns %in% colnames(path_df))){
     missing_column <- df_columns[!(df_columns %in% colnames(path_df))]
     missing_column <- paste(missing_column, collapse = ",")
     logger |> writeLog(type = "error", glue::glue("path_df must contain the column(s): {missing_column}"))
     return()
   }

   if (!inherits(shape, "sf")){
     logger |> writeLog(type = "error", "Shape must be an sf object")
     return()
   }

  # load the data
  covs <- lapply(path_df$datapath, terra::rast)
  names(covs) <- as.vector(path_df$name)

  # check crs and reproject if necessary
  ras_crs <- lapply(covs, terra::crs, describe = TRUE)
  ras_crs <- dplyr::bind_rows(ras_crs, .id = "column_label")

  no_crs <- ras_crs[is.na(ras_crs$code),]
  if (nrow(no_crs) > 0){
    logger |> writeLog(type = "error", "Some files do not have a coordinate reference system")
    for (file in not_overlapping$column_label){
      logger |> writeLog(glue::glue("{file} does not have a coordinate reference system"))
    }
  }

  require_transform <- ras_crs[ras_crs$code != "4326",]
  if (nrow(require_transform) > 0){
    for (file in require_transform$column_label){
      covs[[file]] <- terra::project(covs[[file]], "EPSG:4326")
    }
  }

  # check that rasters overlap with shape
  check_overlap <- as.vector(unlist(lapply(covs, terra::is.related, terra::vect(shape), "intersects")))
  if (any(check_overlap) == FALSE){
    logger |> writeLog(type = "error", "Some files do not overlap with the response data")
    not_overlapping <- names(covs)[!check_overlap]
    for (file in not_overlapping){
      logger |> writeLog(glue::glue("{file} does not overlap with the response data"))
    }
    return(NULL)
  }

  # crop and mask
  covs <- lapply(covs, terra::crop, shape)
  covs <- lapply(covs, terra::mask, shape)

  return(covs)
}
