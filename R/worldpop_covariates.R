#' @title Fetch covariate data from worldpop
#' @description
#' This function is used internally by the cov_nightlight and cov_water modules to
#' fetch data from worldpop
#'
#' @param covariate character. Either `mean_temperature`, `total_precipitation`, `distance_to_inland_water` or `nightlights`
#' @param country_code character
#' @param year numeric. The requested year (2015-2023 only)
#' @return a SpatRaster for the requested country
#' @author Simon Smart <simon.smart@@cantab.net>

get_worldpop_covariate <- function(covariate, country_code, year){
  df <- structure(list(covariate = c("mean_temperature", "total_precipitation", "distance_to_inland_water", "nightlights"),
                       location = c("Climate/Temperature/TerraLST/v1/",
                                    "Climate/Precipitation/TerraClim/v1/",
                                    "Inland_water/esa_worldcover/v1/Dist",
                                    "VIIRS/v1/fvf/"),
                       file1 = c("tavg", "ppt", "dist_inland_water_100m_esa", "viirs_fvf"),
                       file2 = c("tlst_100m_v1.tif", "yravg_tc_100m_v1.tif", "v1.tif", "100m_v1.tif")
  ), class = "data.frame")

  if (year < 2015 || year > 2023){
    stop("data is only available from 2015 to 2023")
  }

  if (!covariate %in% df$covariate){
    stop("covariate must be either mean_temperature, total_precipitation, distance_to_inland_water or nightlights")
  }

  if (covariate == "distance_to_inland_water" && year != 2021){
    warning("distance_to_inland_water data is only available for 2021")
    year = 2021
  }

  cdf <- df[df$covariate == covariate,]

  base_url <- "https://data.worldpop.org/GIS/Covariates/Global_2015_2030"

  file_url <- glue::glue("{base_url}/{country_code}/{cdf$location}/{tolower(country_code)}_{cdf$file1}_{year}_{cdf$file2}")

  raster <- terra::rast(file_url)

  # see e.g. https://hub.worldpop.org/geodata/summary?id=62270
  # the data returned is a monthly average
  if (covariate == "total_precipiation"){
    raster <- raster * 0.1 * 12
  }

  # convert kelvin to celsius
  if (covariate == "mean_temperature"){
    raster <- raster - 273.5
  }

  # convert to sq km
  raster <- terra::aggregate(raster, fact = 10, fun = "mean")

  raster
}
