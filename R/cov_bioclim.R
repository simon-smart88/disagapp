#' @title cov_bioclim
#' @description
#' This function is called by the cov_bioclim module and downloads bioclimatic
#' data on from Bioclim via geodata. It returns a list of SpatRasters for the
#' selected variables
#'
#' @param country_code character. ISO3 code of the country.
#' @param variables vector. List of the bioclimatic variables to be returned
#' @return a list of SpatRaster objects
#' @author Simon Smart <simon.smart@@cantab.net>
#' @export

cov_bioclim <- function(country_code, variables) {
  bioclim_ras <- terra::rast(glue::glue("https://geodata.ucdavis.edu/climate/worldclim/2_1/tiles/iso/{country_code}_wc2.1_30s_bio.tif"))

  names(bioclim_ras) <- c("Mean temperature",
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

  bioclim_ras <- as.list(bioclim_ras[[variables]])

  return(bioclim_ras)
}
