#' @title cov_access
#' @description
#' This function is called by the cov_access module and downloads data on
#' accessibility using data from the Malaria Atlas Project.
#'
#' @param shape sf. sf object containing the area of interest
#' @param layer character. The layer to be returned - one of: Travel Speed
#' Friction Surface (2015), Travel Time to Cities (2015), Motorized Friction
#' Surface (2020), Walking Only Friction Surface (2020), Motorized Travel Time
#' to Healthcare (2020) or Walking Only Travel Time to Healthcare (2020)
#' @return a SpatRaster object
#' @author Simon Smart <simon.smart@@cantab.net>
#' @export

cov_access <- function(shape, layer) {

  datasets <- list(`Travel Time to Cities (2015)` = "Accessibility__201501_Global_Travel_Time_to_Cities",
                   `Motorized Travel Time to Healthcare (2020)` = "Accessibility__202001_Global_Motorized_Travel_Time_to_Healthcare",
                   `Walking Only Travel Time to Healthcare (2020)` = "Accessibility__202001_Global_Walking_Only_Travel_Time_To_Healthcare")

  acc <- malariaAtlas::getRaster(dataset_id = datasets[[layer]], shp = shape)
  return(acc)
}


