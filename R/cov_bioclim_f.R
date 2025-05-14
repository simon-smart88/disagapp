#' @title Download bioclimatic data from Bioclim
#' @description
#' This function is called by the cov_bioclim module and downloads bioclimatic
#' data on from Bioclim via geodata. It returns a list of SpatRasters for the
#' selected variables
#'
#' @param shape sf. sf object containing the area of interest
#' @param country_code vector. ISO3 code of the country or countries.
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
#' @param async logical. Whether or not the function is being used asynchronously
#' @return a list of containing an item for each variable, either SpatRaster
#' objects when `async` is `FALSE` or PackedSpatRaster objects when `async` is
#' `TRUE`
#' @author Simon Smart <simon.smart@@cantab.net>
#' @examples
#' x_min <- 9.47
#' x_max <- 9.63
#' y_min <- 47.05
#' y_max <- 47.27
#' poly_matrix <- matrix(c(x_min, x_min, x_max, x_max, x_min,
#'                         y_min, y_max, y_max, y_min, y_min), ncol = 2)
#' poly <- sf::st_polygon(list(poly_matrix))
#' shape <- sf::st_sf(1, geometry = list(poly))
#' sf::st_crs(shape) = 4326
#' raster <- cov_bioclim(shape = shape,
#'                       country_code = "LIE",
#'                       variables = c("Mean temperature", "Mean diurnal range"))
#'
#'
#' @export

cov_bioclim <- function(shape, country_code, variables,  async = FALSE) {

  valid_countries <- utils::read.csv(system.file("extdata", "countries.csv", package = "disagapp"))$boundaryISO

  invalid_countries <- country_code[(!country_code %in% valid_countries)]
  if (length(invalid_countries) > 0){
    return(async |> asyncLog(type = "error", glue::glue("{invalid_countries} is not a valid IS03 country code. ")))
  }

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

  invalid_layers <- variables[(!variables %in% layers)]
  if (length(invalid_layers) > 0){
    return(async |> asyncLog(type = "error", glue::glue("{invalid_layers} is not a valid bioclim variable. ")))
  }

  if (!inherits(shape, "sf")){
    return(async |> asyncLog(type = "error", "Shape must be an sf object"))
  }

  if (!check_url("https://geodata.ucdavis.edu/climate/worldclim")){
    return(async |> asyncLog(type = "error", "Sorry the bioclim data source is currently offline"))
  }

  bioclim_ras <- NULL

  for (c in country_code){
    country_ras <- tryCatch({terra::rast(glue::glue("/vsicurl/https://geodata.ucdavis.edu/climate/worldclim/2_1/tiles/iso/{c}_wc2.1_30s_bio.tif"))},
                            error = function(x){
                              message <- paste0("An error occurred whilst trying to download bioclim data: ", x)
                              NULL},
                            warning = function(x){
                              message <- paste0("An error occurred whilst trying to download bioclim data: ", x)
                              NULL}
    )
    if (!(is.null(country_ras))){
      if (is.null(bioclim_ras)){
        bioclim_ras <- country_ras
      } else {
        bioclim_ras <- terra::merge(bioclim_ras, country_ras)
      }
    }
  }


  if (is.null(bioclim_ras)){
    return(async |> asyncLog(type = "error", message))
  } else {

    # check that raster overlaps with shape
    check_overlap <- terra::is.related(bioclim_ras, terra::vect(shape), "intersects")
    if (check_overlap == FALSE){
      return(async |> asyncLog(type = "error", "The downloaded bioclim data does not overlap with the response data - check the selected country"))
    }

    bioclim_ras <- terra::crop(bioclim_ras, shape, mask = TRUE )
    names(bioclim_ras) <- layers
    bioclim_ras <- as.list(bioclim_ras[[variables]])
    names(bioclim_ras) <- variables
  if (async){
    bioclim_ras <- wrap_terra(bioclim_ras)
  }
  return(bioclim_ras)
  }
}
