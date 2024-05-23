#' @title cov_bioclim
#' @description
#' This function is called by the cov_bioclim module and downloads bioclimatic
#' data on from Bioclim via geodata. It returns a list of SpatRasters for the
#' selected variables
#'
#' @param country_code character. ISO3 code of the country.
#' @param variables vector. List of the bioclimatic variables to be returned.
#' Options are: `Mean temperature`, `Mean diurnal range`, `Isothermality`,
#' `Temperature seasonality`, `Maximum temperature warmest month`,
#' `Minimum temperature coldest month`, `Temperature range`,
#' `Mean temperature wettest quarter`, `Mean temperature driest quarter`,
#' `Mean temperature warmest quarter`, `Mean temperature coldest quarter`,
#' `Total precipitation`, `Precipitation wettest month`,
#' `Precipitation driest month`, `Precipitation seasonality`,
#' `Precipitation wettest quarter`, `Precipitation driest quarter`,
#' `Precipitation warmest quarter`, `Precipitation coldest quarter`
#' @param shape sf. sf object containing the area of interest
#' @param logger Stores all notification messages to be displayed in the Log
#' Window. Insert the logger reactive list here for running in
#' shiny, otherwise leave the default NULL
#' @return a list of SpatRaster objects
#' @author Simon Smart <simon.smart@@cantab.net>
#' @export

cov_bioclim <- function(country_code, variables, shape, async = FALSE) {

  layers  <-  c("Mean temperature",
                "Mean diurnal range",
                "Isothermality",
                "Temperature seasonality",
                "Maximum temperature warmest month",
                "Minimum temperature coldest month",
                "Temperature range",
                "Mean temperature wettest quarter",
                "Mean temperature driest quarter",
                "Mean temperature warmest quarter",
                "Mean temperature coldest quarter",
                "Total precipitation",
                "Precipitation wettest month",
                "Precipitation driest month",
                "Precipitation seasonality",
                "Precipitation wettest quarter",
                "Precipitation driest quarter",
                "Precipitation warmest quarter",
                "Precipitation coldest quarter")

  for (v in variables){
  if (!(v %in% layers)){
    stop(glue::glue("{v} is not a valid bioclim variable"))
  }}

  bioclim_ras <- terra::rast(glue::glue("https://geodata.ucdavis.edu/climate/worldclim/2_1/tiles/iso/{country_code}_wc2.1_30s_bio.tif"))
  bioclim_ras <- terra::crop(bioclim_ras, shape, mask = TRUE )
  names(bioclim_ras) <- layers
  bioclim_ras <- as.list(bioclim_ras[[variables]])
  names(bioclim_ras) <- variables
  if (async){
    bioclim_ras <- wrap_terra(bioclim_ras)
  }
  return(bioclim_ras)
}
