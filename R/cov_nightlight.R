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
#' @param logger Stores all notification messages to be displayed in the Log
#' Window. Insert the logger reactive list here for running in
#' shiny, otherwise leave the default NULL
#' @return a SpatRaster object
#' @author Simon Smart <simon.smart@@cantab.net>
#' @export

cov_nightlight <- function(shape, year, bearer, logger = NULL) {

  if (!("sf" %in% class(shape))){
    logger %>% writeLog(type = "error", "Shape must be an sf object")
    return()
  }

  if (year > 2022 | year < 2012){
    logger %>% writeLog(type = "error", "Night time illumination data is only available between 2012 and 2022")
    return()
  }

  if (nchar(bearer) < 200){
    logger %>% writeLog(type = "error", "That doesn't look like a valid NASA bearer token")
    return()
  }

ras <- blackmarbler::bm_raster(roi_sf = shape,
                        product_id = "VNP46A4",
                        date = year,
                        bearer = bearer,
                        quiet = TRUE)
if (is.null(ras)){
  logger %>% writeLog(type = "error", "An error occurred whilst trying to download the data")
  return()
} else {
  ras <- terra::rast(ras)
  names(ras) <- "Nighttime light"
  ras <- terra::crop(ras, shape, mask = TRUE )
  return(ras)
}



}
