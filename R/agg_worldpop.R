#' @title agg_worldpop
#' @description
#' This function is called by the agg_worldpop module and downloads data on
#' population density for a single country from worldpop
#'
#' @param shape sf. sf object containing the area of interest
#' @param country_code character. ISO3 code of the country.
#' @param method character. The method used to estimate population. Either `Constrained` or `Unconstrained`.
#' @param resolution character. The resolution of the returned raster. Either `100m` or `1km`.
#' @param year numeric. The year to obtain data for. Either 2000 to 2020 when `method = Unconstrained` or only 2020 when `method = Constrained`
#' @param logger Stores all notification messages to be displayed in the Log
#'   Window. Insert the logger reactive list here for running in
#'   shiny, otherwise leave the default NULL
#' @importFrom rlang .data
#' @return a SpatRaster object
#' @author Simon Smart <simon.smart@@cantab.net>
#' @export

agg_worldpop <- function(shape, country_code, method, resolution, year, logger = NULL) {

if (!(method %in% c("Unconstrained", "Constrained"))){
  logger %>% writeLog(type = "error", "Method must be either \"Constrained\" or \"Unconstrained\"")
  return()
}

if (!(resolution %in% c("100m", "1km"))){
  logger %>% writeLog(type = "error", "Resolution must be either \"100m\" or \"1km\"")
  return()
}

if (method == "Unconstrained" & (year > 2020| year < 2000)){
  logger %>% writeLog(type = "error", "Unconstrained data is only available between 2000 and 2020")
  return()
}

if (method == "Constrained" & year != 2020){
  logger %>% writeLog(type = "error", "Constrained population data is only available for 2020")
  return()
}

base_url <- "https://hub.worldpop.org/rest/data/pop/"

#select the product url
if (method == "Unconstrained" & resolution == "1km"){
  product <- "wpic1km"
}
if (method == "Unconstrained" & resolution == "100m"){
  product <- "wpgp"
}
if (method == "Constrained"){
  product <- "cic2020_100m"
}

#call the API and return error if it doesn't work
api_url <- glue::glue("{base_url}{product}?iso3={country_code}")
req <- httr::GET(api_url)
if (req$status_code != 200){
  logger %>% writeLog(type="error", "The requested data could not be found")
  return()
}

#fetch the API call content and return an error if it is empty
cont <- httr::content(req)
if (length(cont$data) == 0){
  logger %>% writeLog(type="error", "The requested data could not be found")
  return()
}
# select the file_url and download the raster
data <- dplyr::bind_rows(cont$data) %>% dplyr::filter(.data$popyear == as.character(year) & grepl(".tif", .data$files)) %>% dplyr::select("files")
pop_ras <- terra::rast(data$files[[1]])

#aggregate unconstrained as only available at 100m
if (method == "Constrained" & resolution == "1km"){
pop_ras <- terra::aggregate(pop_ras, fact = 10, fun = "sum", na.rm = T)
}

pop_ras <- terra::crop(pop_ras, shape)
pop_ras <- terra::mask(pop_ras, shape)

names(pop_ras) <- "Population"
return(pop_ras)
}
