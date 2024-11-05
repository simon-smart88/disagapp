#' @title Download an aggregation raster from WorldPop
#' @description
#' This function is called by the agg_worldpop module and downloads data on
#' population density for a single country from WorldPop
#'
#' @param shape sf. sf object containing the area of interest
#' @param country_code character. ISO3 code of the country.
#' @param method character. The method used to estimate population. Either `Constrained` or `Unconstrained`.
#' @param resolution character. The resolution of the returned raster. Either `100m` or `1km`.
#' @param year numeric. The year to obtain data for. Either 2000 to 2020 when `method = Unconstrained` or only 2020 when `method = Constrained`
#' @param async logical. Whether or not the function is being used asynchronously
#' @importFrom rlang .data
#' @return a SpatRaster object when `async` is `FALSE` or a PackedSpatRaster
#' when `async` is `TRUE`.
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
#' raster <- disagapp::agg_worldpop(shape = shape,
#'                                  country_code ="LIE",
#'                                  method = "Constrained",
#'                                  resolution = "1km",
#'                                  year = 2020)
#' @export

agg_worldpop <- function(shape, country_code, method, resolution, year, async = FALSE) {

message <- NULL
pop_ras <- NULL

if (!inherits(shape, "sf")){
  message <- "shape must be an sf object"
}

character_variables = list("country_code" = country_code,
                           "method" = method,
                           "resolution" = resolution)
for (i in names(character_variables)){
  if (!inherits(character_variables[[i]], "character")){
    message <- glue::glue("{i} must be a character string")
  }
}

if (!inherits(year, "numeric")){
  message <- "year must be numeric"
}

valid_countries <- rgeoboundaries::gb_metadata("all", adm_lvl = 0)$boundaryISO
invalid_countries <- country_code[(!country_code %in% valid_countries)]
if (length(invalid_countries) > 0){
  message <- glue::glue("{invalid_countries} is not a valid IS03 country code.")
}

if (is.null(message)){
  if (!(method %in% c("Unconstrained", "Constrained"))){
    message <-"Method must be either \"Constrained\" or \"Unconstrained\""
  }

  if (!(resolution %in% c("100m", "1km"))){
    message <- "Resolution must be either \"100m\" or \"1km\""
  }

  if (method == "Unconstrained" & (year > 2020| year < 2000)){
    message <- "Unconstrained data is only available between 2000 and 2020"
  }

  if (method == "Constrained" & year != 2020){
    message <- "Constrained population data is only available for 2020"
  }
}

if (is.null(message)){
  base_url <- "https://hub.worldpop.org/rest/data/pop/"

  # select the product url
  if (method == "Unconstrained" & resolution == "1km"){
    product <- "wpic1km"
  }
  if (method == "Unconstrained" & resolution == "100m"){
    product <- "wpgp"
  }
  if (method == "Constrained"){
    product <- "cic2020_100m"
  }
}

if (is.null(message)){
  for (c in country_code){
    # call the API and return error if it doesn't work
    api_url <- glue::glue("{base_url}{product}?iso3={c}")
    req <- httr2::request(api_url) |> httr2::req_perform()
    if (req$status_code != 200){
      message <- "The requested data could not be found"
    }

    # fetch the API call content and return an error if it is empty
    cont <- httr2::resp_body_json(req)
    if (length(cont$data) == 0){
      message <- "The requested data could not be found"
    }
    # select the file_url and download the raster
    data <- dplyr::bind_rows(cont$data) |> dplyr::filter(.data$popyear == as.character(year) & grepl(".tif", .data$files)) |> dplyr::select("files")
    tryCatch({
      country_ras <- terra::rast(data$files[[1]])
    },
    error = function(x){
      message <- paste0("An error occurred whilst trying to download Worldpop data: ", x)
      NULL},
    warning = function(x){
      message <- paste0("An error occurred whilst trying to download Worldpop data: ", x)
      NULL}
    )
    if (!(is.null(country_ras))){
      if (is.null(pop_ras)){
        pop_ras <- country_ras
      } else {
        pop_ras <- terra::merge(pop_ras, country_ras)
      }
    }
  }
}

if (is.null(pop_ras)){
  if (async){
    return(message)
  } else {
    stop(message)
  }
} else {
  # aggregate unconstrained as only available at 100m
  if (method == "Constrained" & resolution == "1km"){
    pop_ras <- terra::aggregate(pop_ras, fact = 10, fun = "sum", na.rm = T)
  }

  #check that raster overlaps with shape
  check_overlap <- terra::is.related(pop_ras, terra::vect(shape), "intersects")
  if (check_overlap == FALSE){
    message <- "The downloaded Worldpop data does not overlap with the response data - check the selected country"
    if (async){
      return(message)
    } else {
      stop(message)
    }
  }

  # convert NAs to zero
  pop_ras <- terra::subst(pop_ras, NA, 0)

  pop_ras <- terra::crop(pop_ras, shape)
  pop_ras <- terra::mask(pop_ras, shape)
  names(pop_ras) <- "Population"

  if (async){
    pop_ras <- wrap_terra(pop_ras)
  }
  return(pop_ras)
}


}
