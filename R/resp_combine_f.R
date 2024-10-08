#' @title Merge response data with a shapefile
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
#' @examples
#' poly_names <- c("A", "B", "C")
#' df <- data.frame("area" = poly_names, "response" = 1:3)
#' polygons <- list()
#'  for(i in 1:3) {
#'    row <- ceiling(i/10)
#'    col <- ifelse(i %% 10 != 0, i %% 10, 10)
#'    xmin = 2*(col - 1); xmax = 2*col; ymin = 2*(row - 1); ymax = 2*row
#'    polygons[[i]] <- list(cbind(c(xmin, xmax, xmax, xmin, xmin),
#'                                c(ymax, ymax, ymin, ymin, ymax)))
#'  }
#'
#'  polys <- lapply(polygons, sf::st_polygon)
#'  shape <- sf::st_sf(data.frame(area = poly_names ), geometry = polys)
#'  combined_shape <- resp_combine(df = df,
#'                                 df_area_column = "area",
#'                                 df_resp_column = "response",
#'                                 shape = shape,
#'                                 shape_area_column = "area")
#'
#' @export
#'
resp_combine <- function(df, df_area_column, df_resp_column, shape, shape_area_column, logger = NULL) {

  # check inputs
  if (!inherits(df, "data.frame")){
    logger |> writeLog(type = "error", "df must be a data.frame")
    return()
  }

  df_columns <- c(df_area_column, df_resp_column)
  if (!all(df_columns %in% colnames(df))){
    missing_column <- df_columns[!(df_columns %in% colnames(df))]
    missing_column <- paste(missing_column, collapse = ",")
    logger |> writeLog(type = "error", glue::glue("df does not contain the column(s): {missing_column}"))
    return()
  }

  if (!inherits(shape, "sf")){
    logger |> writeLog(type = "error", "shape must be an sf object")
    return()
  }

  if (!(shape_area_column %in% colnames(shape))){
    logger |> writeLog(type = "error", glue::glue("shape does not contain a {shape_area_column} column"))
    return()
  }

  # ensure columns to merge on are both characters
  df[[df_area_column]] <- as.character(df[[df_area_column]])
  shape[[shape_area_column]] <- as.character(shape[[shape_area_column]])

  # merge data
  shape <- shape |>
    dplyr::full_join(df, by = stats::setNames(df_area_column, shape_area_column))

  # look for any NA in merged shapes, raise a warning if any found
  if (any(c(any(is.na(shape[[df_resp_column]]))),(any(is.na(shape[[shape_area_column]]))))){
    logger |> writeLog(type = "warning", "Some data could not be merged - check the log")
    }

  # log the individual errors
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



