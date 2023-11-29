#' @title resp_shape
#' @description
#' This function is called by the resp_shape module and loads a
#'  shapefile into an sf object. Inspired by a function written by Paula Moraga
#'  here: https://www.paulamoraga.com/book-geospatial/sec-shinyexample.html#uploading-data
#'
#' @param shpdf dataframe. As produced by `shiny::fileInput`, containing `name` and
#' `datapath` columns
#' @return an sf object
#' @author Simon Smart <simon.smart@@cantab.net>
#' @author Paula Moraga
#' @export
#'
#
resp_shape <- function(shpdf) {
  tempdirname <- dirname(shpdf$datapath[1])
  for (i in 1:nrow(shpdf)) {
    file.rename(
      shpdf$datapath[i],
      paste0(tempdirname, "/", shpdf$name[i])
    )
  }
  if (nrow(shpdf) == 4){
    shape_file_path <- shpdf$name[grep(pattern = "*.shp$", shpdf$name)]
    shape <- sf::st_read(paste(tempdirname, shape_file_path, sep = "/"), quiet = TRUE)
    crs <- sf::st_crs(shape)
    if (crs$input != "EPSG:4326"){
      shape <- sf::st_transform(shape, crs = 4326)
    }
  }

  #else raise log

  return(shape)
}
