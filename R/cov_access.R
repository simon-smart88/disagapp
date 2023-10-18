#' @title cov_access
#' @description
#' This function is called by the cov_access module and downloads data on the
#' travel time to cities for 2015 as quantified by the Malaria Atlas Project.
#'
#' @param shape sf. sf object containing the area of interest
#' @return a SpatRaster object
#' @author Simon Smart <simon.smart@@cantab.net>
#' @export

cov_access <- function(shape) {
  acc <- malariaAtlas::getRaster(dataset_id ='A global map of travel time to
                                 cities to assess inequalities in accessibility in 2015',
                                 shp=shape)
  return(acc)
}
