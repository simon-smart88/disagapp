#' @title Merge response data with a downloaded shapefile
#' @description
#' This function is called by the resp_download module and merges response data
#'  from a spreadsheet with boundary data into an sf object
#'
#' @param df dataframe. Containing the response data and the name of the administrative area
#' @param area_column character. The column name of the dataframe containing the administrative areas
#' @param resp_column character. The column name of the dataframe containing the response data
#' @param country_code character. ISO3 code of the country.
#' @param admin_level character. The administrative level requested e.g. ADM1 or ADM2
#' @param logger Stores all notification messages to be displayed in the Log
#'   Window. Insert the logger reactive list here for running in
#'   shiny, otherwise leave the default NULL
#' @return an sf object
#' @author Simon Smart <simon.smart@@cantab.net>
#' @examples
#' df <- data.frame("area" = c("Triesen", "Schellenberg", "Gamprin", "Triesenberg",
#'                             "Eschen", "Ruggell", "Mauren", "Schaan", "Balzers",
#'                              "Planken","Vaduz"), "response" = 1:11)
#' shape <- resp_download(df, "area", "response", "LIE", "ADM1")
#' @export
#'
resp_download <- function(df, area_column, resp_column, country_code, admin_level, logger = NULL) {

  # check inputs
  if (!inherits(df, "data.frame")){
    logger |> writeLog(type = "error", "df must be a data.frame")
    return()
  }

  character_variables = list("area_column" = area_column,
                             "resp_column" = resp_column,
                             "country_code" = country_code,
                             "admin_level" = admin_level)
  for (i in names(character_variables)){
    if (!inherits(character_variables[[i]], "character")){
      logger |> writeLog(type = "error", glue::glue("{i} must be a character string"))
      return()
    }
  }

  df_columns <- c(area_column, resp_column)
  if (!all(df_columns %in% colnames(df))){
    missing_column <- df_columns[!(df_columns %in% colnames(df))]
    missing_column <- paste(missing_column, collapse = ",")
    logger |> writeLog(type = "error", glue::glue("df does not contain the column(s): {missing_column}"))
  }

  valid_countries <- readRDS(system.file("ex", "countries.rds", package = "geodata"))$ISO3
  invalid_countries <- country_code[(!country_code %in% valid_countries)]
  if (length(invalid_countries) > 0){
    logger |> writeLog(type = "error", glue::glue("{invalid_countries} is not a valid IS03 country code"))
    return()
  }

  if (!(admin_level %in% c("ADM1", "ADM2"))){
    logger |> writeLog(type = "error", "admin_level must be either ADM1 or ADM2")
    return()
  }

  shape <- NULL

  for (c in country_code){

    url <- glue::glue("https://www.geoboundaries.org/api/current/gbOpen/{c}/{admin_level}/")
    req <- httr2::request(url)
    resp <- tryCatch(
      req |> httr2::req_perform(),
      httr2_http_404 = function(cnd){NULL}
    )

    if (is.null(resp) || resp$status_code != 200){
      logger |> writeLog(type = "error", "The requested boundaries could not be downloaded, the requested admin level may be unavailable")
      return()
    } else {
      cont <- httr2::resp_body_json(resp)
      c_shape <- sf::st_read(cont$gjDownloadURL, quiet = TRUE)
      if (is.null(shape)){
        shape <- c_shape
      } else {
        shape <- rbind(shape, c_shape)
      }
    }
  }
    shape <- shape |>
      dplyr::full_join(df, by = stats::setNames(area_column, "shapeName"))

    #look for any NA in merged shapes, raise a warning if any found
    if (any(c(any(is.na(shape[[resp_column]]))),(any(is.na(shape$shapeISO))))){
      logger |> writeLog(type = "warning", "Some areas could not be matched with the response data - check the log")
    }

    #log the individual errors
    if (any(is.na(shape[[resp_column]]))){
      missing <- shape$shapeName[is.na(shape[[resp_column]])]
      for (m in missing){
        logger |> writeLog(glue::glue("Area data for {m} could not be matched with response data"))
      }
    }

    if (any(is.na(shape$shapeISO))){
      missing <- shape$shapeName[is.na(shape$shapeISO)]
      for (m in missing){
        logger |> writeLog(glue::glue("Response data for {m} could not be matched with an area"))
      }
    }
    return(shape)

}
