#' @title incid_combo
#' @description
#' This function is called by the incid_combo module and merges incidence data
#'  from a spreadsheet with boundary data into an sf object
#'
#' @param spdf dataframe. As produced by shiny::fileInput, containing name and
#' datapath columns
#' @param country_code character. ISO3 code of the country.
#' @param admin_level character. The administrative level requested e.g. ADM1 or ADM2
#' @return an sf object
#' @author Simon Smart <simon.smart@@cantab.net>
#' @export
#'
incid_combo <- function(spdf, country_code, admin_level) {

  file_format <- tools::file_ext(spdf$datapath[1])
  if (file_format == "csv"){
    df <- read.csv(spdf$datapath[1])
  } else if (file_format == "xlsx"){
    df <- openxlsx::read.xlsx(spdf$datapath[1])
  } else {
    common$logger %>% writeLog("The uploaded file was not a .csv or .xlsx")
    return()
  }

  #Runfola, D. et al. (2020) geoBoundaries: A global database of political administrative boundaries. PLoS ONE 15(4): e0231866. https://doi.org/10.1371/journal.pone.0231866.

  url <- glue::glue("https://www.geoboundaries.org/api/current/gbOpen/{country_code}/{admin_level}/")
  req <- httr::GET(url)
  cont <- httr::content(req)
  shape <- sf::st_read(cont$gjDownloadURL)

  shape %>%
    dplyr::full_join(df)

  return(shape)
}


