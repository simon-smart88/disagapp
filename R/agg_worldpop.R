#' @title agg_worldpop
#' @description
#' This function is called by the agg_worldpop module and downloads data on
#' population density for a single country from worldpop
#'
#' @param country_code character. ISO3 code of the country.
#' @param method character. The method used to estimate population. Either Constrained or Unconstrained.
#' @param resolution character. The resolution of the returned raster. Either 100m ior 1km.
#' @return a SpatRaster object
#' @author Simon Smart <simon.smart@@cantab.net>
#' @export

agg_worldpop <- function(country_code, method, resolution) {
temp <- tempfile()
if (method == "Unconstrained" & resolution == "1km"){
download.file(glue::glue('https://data.worldpop.org/GIS/Population/Global_2000_2020_1km/2020/{country_code}/{tolower(country_code)}_ppp_2018_1km_Aggregated.tif'), temp)
}
if (method == "Unconstrained" & resolution == "100m"){
  download.file(glue::glue('https://data.worldpop.org/GIS/Population/Global_2000_2020/2020/{country_code}/{tolower(country_code)}_ppp_2020.tif'), temp)
}
if (method == "Constrained"){
  download.file(glue::glue('https://data.worldpop.org/GIS/Population/Global_2000_2020_Constrained/2020/maxar_v1/{country_code}/{tolower(country_code)}_ppp_2020_constrained.tif'), temp)
}
pop_ras <- terra::rast(temp)

#aggregate unconstrained as only available at 100m
if (method == "Constrained" & resolution == "1km"){
pop_ras <- terra::aggregate(pop_ras, fact = 10, fun = "sum")
}

names(pop_ras) <- "Population"
return(pop_ras)
}
