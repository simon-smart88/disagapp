#' @title Download accessibility data from Malaria Atlas Project
#' @description
#' This function is called by the cov_access module and downloads data on
#' accessibility using data from the Malaria Atlas Project.
#'
#' @param shape sf. sf object containing the area of interest
#' @param layer character. The layer to be returned - one of:
#' `Travel Time to Cities (2015)`, `Motorized Travel Time to Healthcare (2020)`
#' or `Walking Only Travel Time to Healthcare (2020)`
#' @param async logical. Whether or not the function is being used asynchronously
#' @return a SpatRaster object
#' @author Simon Smart <simon.smart@@cantab.net>
#' @examples
#' x_min <- 0
#' x_max <- 0.5
#' y_min <- 52
#' y_max <- 52.5
#' poly_matrix <- matrix(c(x_min, x_min, x_max, x_max, x_min,
#'                         y_min, y_max, y_max, y_min, y_min), ncol = 2)
#' poly <- sf::st_polygon(list(poly_matrix))
#' shape <- sf::st_sf(1, geometry = list(poly))
#' sf::st_crs(shape) = 4326
#' raster <- cov_access(shape = shape,
#'                      layer = "Travel Time to Cities (2015)")
#'
#' @export

cov_access <- function(shape, layer, async = FALSE) {

  message <- NULL

  if (!requireNamespace("malariaAtlas", quietly = TRUE)){
    return(async |> asyncLog(type = "error", 'This module requires the malariaAtlas package to be installed. Close the app, run install.packages("malariaAtlas") and try again'))
  }

  if (!inherits(shape, "sf")){
    return(async |> asyncLog(type = "error", "Shape must be an sf object"))
  }

  if (!inherits(layer, "character")){
    return(async |> asyncLog(type = "error", "layer must be a character string"))
  }

  if (!(layer %in% c("Travel Time to Cities (2015)",
                     "Motorized Travel Time to Healthcare (2020)",
                     "Walking Only Travel Time to Healthcare (2020)"))){
    return(async |> asyncLog(type = "error", glue::glue("{layer} is not a valid accessibility layer")))
  }

  if (!check_url("https://data.malariaatlas.org/")){
    return(async |> asyncLog(type = "error", "Sorry the accessibility data source is currently offline"))
  }

  datasets <- list(`Travel Time to Cities (2015)` = "Accessibility__201501_Global_Travel_Time_to_Cities",
                   `Motorized Travel Time to Healthcare (2020)` = "Accessibility__202001_Global_Motorized_Travel_Time_to_Healthcare",
                   `Walking Only Travel Time to Healthcare (2020)` = "Accessibility__202001_Global_Walking_Only_Travel_Time_To_Healthcare")

  acc <- tryCatch({malariaAtlas::getRaster(dataset_id = datasets[[layer]], shp = shape)},
                  error = function(x){
                  message <- paste0("An error occurred whilst trying to download accessibility data: ", x)
                  NULL},
                  warning = function(x){
                  message <- paste0("An error occurred whilst trying to download accessibility data: ", x)
                  NULL}
  )

  if (is.null(acc)){
    if (is.null(message)){
      message <- "An error occurred whilst trying to download accessibility data"
    }
    return(async |> asyncLog(type = "error", message))
  }

  if (async){
    acc <- wrap_terra(acc)
  }
    return(acc)
}


