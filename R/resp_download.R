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
#' @export
#'
resp_download <- function(df, area_column, resp_column, country_code, admin_level, logger = NULL) {

  #Runfola, D. et al. (2020) geoBoundaries: A global database of political administrative boundaries. PLoS ONE 15(4): e0231866. https://doi.org/10.1371/journal.pone.0231866.

  url <- glue::glue("https://www.geoboundaries.org/api/current/gbOpen/{country_code}/{admin_level}/")
  req <- httr::GET(url)
  if (req$status_code != 200){
    logger %>% writeLog("The requested boundaries could not be downloaded")
    return()
  } else {

    cont <- httr::content(req)
    shape <- sf::st_read(cont$gjDownloadURL, quiet = TRUE)

    shape <- shape %>%
      dplyr::full_join(df, by = stats::setNames(area_column, "shapeName"))

    #look for any NA in merged shapes, raise a warning if any found
    if (any(c(any(is.na(shape[[resp_column]]))),(any(is.na(shape$shapeISO))))){
      logger %>% writeLog(type = "warning")
      }

    #log the individual errors
    if (any(is.na(shape[[resp_column]]))){
      missing <- shape$shapeName[is.na(shape[[resp_column]])]
      for (m in missing){
      logger %>% writeLog(glue::glue("Area data for {m} could not be matched with response data"))
      }
    }

    if (any(is.na(shape$shapeISO))){
      missing <- shape$shapeName[is.na(shape$shapeISO)]
      for (m in missing){
        logger %>% writeLog(glue::glue("Response data for {m} could not be matched with an area"))
      }
    }
    return(shape)
  }

}



