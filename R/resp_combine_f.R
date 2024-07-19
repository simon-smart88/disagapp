#' @title resp_combine
#' @description
#' This function is called by the resp_combine module and merges response data
#'  from a spreadsheet with boundary data from a shapefile into an sf object
#'
#' @param df dataframe. Containing the response data and the name of the administrative area
#' @param df_area_column character. The column name of the dataframe containing the administrative areas
#' @param df_resp_column character. The column name of the dataframe containing the response data
#' @param shape sf. Shapefile containing the boundary data
#' @param shape_area_column character. The column name of the shapefile containing the administrative areas
#' @param logger Stores all notification messages to be displayed in the Log
#'   Window. Insert the logger reactive list here for running in
#'   shiny, otherwise leave the default NULL
#' @return an sf object
#' @author Simon Smart <simon.smart@@cantab.net>
#' @export
#'
resp_combine <- function(df, df_area_column, df_resp_column, shape, shape_area_column, logger = NULL) {

    #ensure columns to merge on are both characters
    df[[df_area_column]] <- as.character(df[[df_area_column]])
    shape[[shape_area_column]] <- as.character(shape[[shape_area_column]])

    #merge data
    shape <- shape |>
      dplyr::full_join(df, by = stats::setNames(df_area_column, shape_area_column))

    #look for any NA in merged shapes, raise a warning if any found
    if (any(c(any(is.na(shape[[df_resp_column]]))),(any(is.na(shape[[shape_area_column]]))))){
      logger |> writeLog(type = "warning")
      }

    #log the individual errors
    if (any(is.na(shape[[df_resp_column]]))){
      missing <- shape[[shape_area_column]][is.na(shape[[df_resp_column]])]
      for (m in missing){
      logger |> writeLog(glue::glue("Area data for {m} could not be matched with response data"))
      }
    }

    if (any(is.na(shape[[shape_area_column]]))){
      missing <- shape[[df_resp_column]][is.na(shape[[shape_area_column]])]
      for (m in missing){
        logger |> writeLog(glue::glue("Response data for {m} could not be matched with an area"))
      }
    }
    return(shape)

}



