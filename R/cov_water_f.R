#' @title Download distance to inland water data from Worldpop
#' @description
#' This function is called by the cov_water module and downloads distance to
#' inland water data from Worldpop
#' @param shape sf. sf object containing the area of interest
#' @param country_code vector. ISO3 code of the country or countries.
#' @param async Whether or not the function is being used asynchronously. When
#' `TRUE` the returned object is a wrapped SpatRaster.
#' @return a SpatRaster object when `async` is `FALSE` or a PackedSpatRaster
#' when `async` is `TRUE`.
#' @author Simon Smart <simon.smart@@cantab.net>
#' @examples
#' \dontrun{
#' df_path <- system.file("extdata", "lie.csv", package = "disagapp")
#' df <- read.csv(df_path)
#' lie_shape <- resp_download(df, "area", "response", "LIE", "ADM1")
#' raster <- cov_water(lie_shape, "LIE")
#' }
#'
#' @export

cov_water <- function(shape, country_code, async = FALSE) {

  message <- NULL

  if (!inherits(shape, "sf")){
    return(async |> asyncLog(type = "error", "Shape must be an sf object"))
  }

  valid_countries <- utils::read.csv(system.file("extdata", "countries.csv", package = "disagapp"))$boundaryISO
  invalid_countries <- country_code[(!country_code %in% valid_countries)]
  if (length(invalid_countries) > 0){
    return(async |> asyncLog(type = "error", glue::glue("{invalid_countries} is not a valid IS03 country code. ")))
  }

  if (!check_url("https://data.worldpop.org/")){
    return(async |> asyncLog(type = "error", "Sorry the distance to water data source is currently offline"))
  }

  combined_ras <- NULL

  for (c in country_code){
    country_ras <- tryCatch({get_worldpop_covariate("distance_to_inland_water", c, 2021)},
                            error = function(x){
                              message <- paste0("An error occurred whilst trying to download night light data: ", x)
                              NULL}
    )
    if (!(is.null(country_ras))){
      if (is.null(combined_ras)){
        combined_ras <- country_ras
      } else {
        combined_ras <- suppressWarnings(terra::merge(combined_ras, country_ras))
      }
    }
  }

  if (is.null(combined_ras)){
    if (is.null(message)){
      message <- "An error occurred whilst trying to download distance to water data"
    }
    return(async |> asyncLog(type = "error", message))

  } else {

    # check that raster overlaps with shape
    check_overlap <- terra::is.related(combined_ras, terra::vect(shape), "intersects")
    if (check_overlap == FALSE){
      return(async |> asyncLog(type = "error", "The downloaded  distance to water data does not overlap with the response data - check the selected country"))
    }

    names(combined_ras) <- "Distance to inland water"
    combined_ras <- terra::crop(combined_ras, shape, mask = TRUE )
    if (async){
      combined_ras <- terra::wrap(combined_ras)
    }
  }
  combined_ras
}
