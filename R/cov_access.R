#' @title cov_access
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
#' @export

cov_access <- function(shape, layer, async = FALSE) {

  message <- NULL

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
      message <- paste0("An error occurred whilst trying to download accessibility data")
    }
    if (async){
      return(message)
    } else {
      stop(message)
    }
  }

  if (async){
    acc <- wrap_terra(acc)
  }
    return(acc)
}


