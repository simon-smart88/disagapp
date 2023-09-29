
# https://www.paulamoraga.com/book-geospatial/sec-shinyexample.html#uploading-data
incid_user <- function(shpdf) {
  tempdirname <- dirname(shpdf$datapath[1])
  for (i in 1:nrow(shpdf)) {
    file.rename(
      shpdf$datapath[i],
      paste0(tempdirname, "/", shpdf$name[i])
    )
  }
  if (nrow(shpdf) == 4){
    shape_file_path <- shpdf$name[grep(pattern = "*.shp$", shpdf$name)]
    shape <- sf::st_read(paste(tempdirname,shape_file_path,sep = "/"))
  }

  #else raise log

  return(shape)
}
