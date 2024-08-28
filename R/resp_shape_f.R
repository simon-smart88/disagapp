#' @title Upload response data from a shapefile
#' @description
#' This function is called by the resp_shape module and loads a
#'  shapefile into an sf object. Inspired by a function written by Paula Moraga
#'  here: https://www.paulamoraga.com/book-geospatial/sec-shinyexample.html#uploading-data
#'
#' @param shpdf dataframe. As produced by `shiny::fileInput`, containing `name` and
#' `datapath` columns
#' @param logger Stores all notification messages to be displayed in the Log
#' Window. Insert the logger reactive list here for running in
#' shiny, otherwise leave the default NULL
#' @return an sf object
#' @author Simon Smart <simon.smart@@cantab.net>
#' @author Paula Moraga
#' @examples
#' shpdf <- data.frame(
#' datapath = list.files(system.file("extdata", "shapes", package = "disagapp"), full.names = TRUE),
#' name = list.files(system.file("extdata", "shapes", package = "disagapp")))
#' shape <- resp_shape(shpdf)
#' @export
#
resp_shape <- function(shpdf, logger = NULL) {

  if (!inherits(shpdf, "data.frame")){
    logger |> writeLog(type = "error", "shpdf must be a data.frame")
    return()
  }

  df_columns <- c("datapath", "name")
  if (!all(df_columns %in% colnames(shpdf))){
    missing_column <- df_columns[!(df_columns %in% colnames(shpdf))]
    missing_column <- paste(missing_column, collapse = ",")
    logger |> writeLog(type = "error", glue::glue("shpdf must contain the column(s): {missing_column}"))
    return()
  }

  if (nrow(shpdf) != 4){
    logger |> writeLog(type = "error", "shpdf must contain four rows")
    return()
  }

  tempdirname <- dirname(shpdf$datapath[1])
  for (i in 1:nrow(shpdf)) {
    file.rename(
      shpdf$datapath[i],
      paste0(tempdirname, "/", shpdf$name[i])
    )
  }

  shape_file_path <- shpdf$name[grep(pattern = "*.shp$", shpdf$name)]
  shape <- sf::st_read(paste(tempdirname, shape_file_path, sep = "/"), quiet = TRUE)
  crs <- sf::st_crs(shape)
  if (crs$input != "EPSG:4326"){
    shape <- sf::st_transform(shape, crs = 4326)
  }

  return(shape)
}
