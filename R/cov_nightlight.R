#' @title cov_nightlight
#' @description
#' This function is called by the cov_nightlight module and downloads annual
#' data on night time illumination from NASA (product ID VNP46A4) using the
#' blackmarbler package. You must obtain and a token from NASA to use this
#' function and set an environmental variable called `NASA_bearer` to contain it.
#' It returns a SpatRaster for the selected area and year.
#'
#' @param shape sf. sf object containing the area of interest
#' @param year numeric. Year for which to download the data. Limited to 2012-2022
#' @param bearer character. NASA bearer token. \href{https://cran.r-project.org/web/packages/blackmarbler/readme/README.html#token}{Click here}
#' for details of how to obtain one.
#' @param async Whether or not the function is being used asynchronously. When
#' `TRUE` the returned object is a wrapped SpatRaster.
#' @return a SpatRaster object when `async` is `FALSE` or a PackedSpatRaster
#' when `async` is `TRUE`.
#' @author Simon Smart <simon.smart@@cantab.net>
#' @export

cov_nightlight <- function(shape, year, bearer, async = FALSE) {

  message <- NULL

  if (!("sf" %in% class(shape))){
    message <- "Shape must be an sf object"
    if (async){
      return(message)
    } else {
      stop(message)
    }
  }

  if (year > 2022 | year < 2012){
    message <- "Nighttime data is only available between 2012 and 2022"
    if (async){
      return(message)
    } else {
      stop(message)
    }
  }

  if (nchar(bearer) < 200){
    message <- "That doesn't look like a valid NASA bearer token"
    if (async){
      return(message)
    } else {
      stop(message)
    }
  }

ras <- tryCatch({blackmarbler::bm_raster(roi_sf = shape,
                        product_id = "VNP46A4",
                        date = year,
                        bearer = bearer,
                        quiet = TRUE)},
                error = function(x){
                message <- paste0("An error occurred whilst trying to download night light data: ", x)
                NULL},
                warning = function(x){
                message <- paste0("An error occurred whilst trying to download night light data: ", x)
                NULL})

if (is.null(ras)){
  if (is.null(message)){
    message <- paste0("An error occurred whilst trying to download night light data")
  }
  if (async){
    return(message)
  } else {
    stop(message)
  }
} else {
  ras <- terra::rast(ras)
  names(ras) <- "Nighttime light"
  ras <- terra::crop(ras, shape, mask = TRUE )
  if (async){ ras <- terra::wrap(ras) }
  return(ras)
}


}
