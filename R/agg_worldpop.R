#' @title agg_worldpop
#' @description
#' This function is called by the agg_worldpop module and downloads data on
#' population density for a single country from worldpop
#'
#' @param country_code character. ISO3 code of the country.
#' @return a SpatRaster object
#' @author Simon Smart <simon.smart@@cantab.net>
#' @export

agg_worldpop <- function(country_code) {
temp <- tempfile()
download.file(glue::glue('https://data.worldpop.org/GIS/Population/Global_2000_2020_1km/2018/{country_code}/{tolower(country_code)}_ppp_2018_1km_Aggregated.tif'), temp)
pop_ras <- terra::rast(temp)
names(pop_ras) <- 'Population'
return(pop_ras)
}
