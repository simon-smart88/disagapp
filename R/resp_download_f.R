#' @title resp_download
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

  shape <- NULL

  for (c in country_code){

    url <- glue::glue("https://www.geoboundaries.org/api/current/gbOpen/{c}/{admin_level}/")
    req <- httr2::request(url)
    resp <- tryCatch(
      req |> httr2::req_perform(),
      httr2_http_404 = function(cnd){NULL}
    )

    if (is.null(resp) || resp$status_code != 200){
      logger |> writeLog(type = "error", "The requested boundaries could not be downloaded")
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
