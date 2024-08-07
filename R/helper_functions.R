####################### #
# MISC #
####################### #
#' @title printVecAsis
#' @description For internal use. Print vector as character string
#' @param x vector
#' @param asChar exclude c notation at the beginning of string
#' @keywords internal
#' @export
printVecAsis <- function(x, asChar = FALSE) {
  if (is.character(x)) {
    if (length(x) == 1) {
      return(paste0("\'", x, "\'"))
    } else {
      if (asChar == FALSE) {
        return(paste0("c(", paste(sapply(x, function(a) paste0("\'", a, "\'")),
                                  collapse = ", "), ")"))
      } else {
        return(paste0("(", paste(sapply(x, function(a) paste0("\'", a, "\'")),
                                 collapse = ", "), ")"))
      }
    }
  } else {
    if (length(x) == 1) {
      return(x)
    } else {
      if (asChar == FALSE) {
        return(paste0("c(", paste(x, collapse = ", "), ")"))
      } else {
        return(paste0("(", paste(x, collapse = ", "), ")"))
      }
    }
  }
}

#' @title Spurious package call to avoid note of functions outside R folder
#' @description For internal use.
#' @param x x
#' @keywords internal
#' @import gargoyle leaflet.extras
#' @export
spurious <- function(x) {
  curl::curl(x)
  DT::renderDataTable(x)
  RColorBrewer::brewer.pal(x)
  R6::R6Class(x)
  corrplot::corrplot(x)
  disaggregation::build_mesh(x)
  fmesher::fm_as_sfc(x)
  geodata::worldclim_country(x)
  geosphere::centroid(x)
  leafem::addMouseCoordinates(x)
  markdown::html_format(x)
  openxlsx::read.xlsx(x)
  plotly::plot_ly(x)
  promises::promise(x)
  renv::activate(x)
  rintrojs::introjs(x)
  rmarkdown::github_document(x)
  shinyWidgets::pickerInput(x)
  shinyjs::disable(x)
  SpatialEpi::bayes_cluster(x)
  zip::zipr(x)
  return()
}

#' @title reset_data_ui
#' @description For internal use. UI component that appears once response data has been loaded
#' @keywords internal
#' @param session The session object passed to function given to shinyServer.
#' @param common The common data structure
#' @export
reset_data_ui <- function(session, common){
  gargoyle::watch("resp_shape")
  gargoyle::watch("resp_combine")
  gargoyle::watch("resp_download")
  gargoyle::watch("resp_example")
  if (!is.null(common$shape)){
    shinyWidgets::materialSwitch(session$ns("reset"), "Delete existing data?", FALSE, status = "success")
  }
}


####################### #
# SHINY LOG #
####################### #

#' @title writeLog
#' @description For internal use. Add text to a logger
#' @param logger The logger to write the text to. Can be NULL or a function
#' @param ... Messages to write to the logger
#' @param type One of "default", "error", "warning"
#' @keywords internal
#' @export
writeLog <- function(logger, ..., type = "default") {
  if (is.null(logger)) {
    if (type == "error") {
      stop(paste0(..., collapse = ""), call. = FALSE)
    } else if (type == "warning") {
      warning(paste0(..., collapse = ""), call. = FALSE)
    } else {
      message(paste0(..., collapse = ""))
    }
  } else if (is.function(logger)) {
    if (type == "default") {
      pre <- "> "
    } else if (type == "starting") {
      pre <- paste0(icon("clock", class = "log_start"), " ")
    } else if (type == "complete") {
      pre <- paste0(icon("check", class = "log_end"), " ")
    } else if (type == "info") {
      if (nchar(...) < 80){
        shinyalert::shinyalert(..., type = "info")
      } else {
        shinyalert::shinyalert("Please, check Log window for more information ",
                               type = "info")
      }
      pre <- paste0(icon("info", class = "log_info"), " ")
    } else if (type == "error") {
      if (nchar(...) < 80){
        shinyalert::shinyalert(...,
                               type = "error")
      } else {
        shinyalert::shinyalert("Please, check Log window for more information ",
                               type = "error")
      }
      pre <- paste0(icon("xmark", class = "log_error"), " ")
    } else if (type == "warning") {
      if (nchar(...) < 80){
        shinyalert::shinyalert(...,
                               type = "warning")
      } else {
      shinyalert::shinyalert("Please, check Log window for more information ",
                             type = "warning")

      pre <- paste0(icon("triangle-exclamation", class = "log_warn"), " ")
      }
    }
    newEntries <- paste0("<br>", pre, ..., collapse = "")
    logger(paste0(logger(), newEntries))
  } else {
    warning("Invalid logger type")
  }
  invisible()
}

####################### #
# LOADING MODAL #
####################### #

#' @title show_loading_modal
#' @description For internal use. Show a modal when something is loading
#' @param type One of "data", "fitting"
#' @keywords internal
#' @export

show_loading_modal <- function(message){
  shinybusy::show_modal_spinner(
    spin = "self-building-square",
    color = "#446e9b",
    text = message
  )
}

#' @title close_loading_modal
#' @description For internal use. Close the modal once loading is complete
#' @param session The session object passed to function given to shinyServer.
#' @keywords internal
#' @export

close_loading_modal <- function (session = getDefaultReactiveDomain())
{
  session$sendModal("remove", NULL)
}

####################### #
# SELECTED COUNTRIES #
####################### #

#' @title country_out
#' @description For internal use. Produce a drop down list of countries and update all inputs once one country has been selected.
#' @param session The session object passed to function given to shinyServer.
#' @param common The common data structure
#' @keywords internal
#' @export
country_out <- function(session, common){
  gargoyle::init("country_out")
  renderUI({
    gargoyle::watch("country_out")
    if (is.null(common$selected_country)){
      selectInput(session$ns("country"), "Select country", c('',common$countries$NAME), multiple = TRUE)
    } else {
      selectInput(session$ns("country"), "Select country", common$countries$NAME, selected = common$selected_country, multiple = TRUE)
    }
  })
}

####################### #
# MAPPING FUNCTIONS #
####################### #

#' @title shape_map
#' @description For internal use. Plot response data on the leaflet map
#' @param map The leafletProxy object to add the shape to
#' @param common The common data structure
#' @keywords internal
#' @export
shape_map <- function(map, common){
  response <- as.numeric(common$shape[[common$response_name]])
  ex <- as.vector(terra::ext(common$shape))
  common$add_map_layer("Response")
  pal <- colorBin("viridis", domain = response, bins = 9, na.color ="#00000000")
  map |>
    clearControls() |>
    removeLayersControl() |>
    clearGroup("Response") |>
    removeControl("Response") |>
    addPolygons(data = common$shape, fillColor = ~pal(response), color = "black", fillOpacity = 0.7, weight = 2, group = "Response", popup = ~as.character(round(response,0))) |>
    fitBounds(lng1 = ex[[1]], lng2 = ex[[2]], lat1 = ex[[3]], lat2 = ex[[4]]) |>
    addLegend(position = "bottomright", pal = pal, values = response, group = "Response", title = "Response", layerId = "Response") |>
    addLayersControl(overlayGroups = common$map_layers, options = layersControlOptions(collapsed = FALSE)) |>
    hideGroup(common$map_layers) |>
    showGroup("Response")
}

#' @title raster_map
#' @description For internal use. Plot covariate data on the leaflet map
#' @param map The leafletProxy object to add the shape to
#' @param common The common data structure
#' @param raster The SpatRaster to plot
#' @param name The name of the covariate
#' @param log Whether to plot the raster using a log scale
#' @param selected The layer to display
#' @keywords internal
#' @export
raster_map <- function(map, common, raster, name, log = FALSE, selected = NULL){
  common$add_map_layer(name)
  if (log == TRUE){
    raster[terra::values(raster) == 0] <- NA
    raster = log10(raster)
    title = paste0(name, " (log 10)")
  } else {
    title = name
  }

  domain <- c(min(terra::values(raster), na.rm = T), max(terra::values(raster), na.rm = T))
  pal <- colorBin("plasma", domain = domain, bins = 9, na.color = "#00000000")

  map |>
    clearControls() |>
    removeLayersControl() |>
    clearGroup(name) |>
    removeControl(name) |>
    addRasterImage(raster, group = name, colors = pal) |>
    addLegend(position = "bottomleft", pal = pal, values = terra::values(raster), group = name, title = title, layerId = name) |>
    addLayersControl(overlayGroups = common$map_layers, options = layersControlOptions(collapsed = FALSE))

    if (is.null(selected)){
      map |>
        hideGroup(common$map_layers) |>
        showGroup(name)
    } else {
      map |>
        hideGroup(common$map_layers) |>
        showGroup(selected)
    }
}


#' @title mesh_map
#' @description For internal use. Plot spatial mesh on the leaflet map
#' @param map The leafletProxy object to add the mesh to
#' @param common The common data structure
#' @keywords internal
#' @export
mesh_map <- function(map, common){
#convert the inla mesh to a format which leaflet can handle
sf_mesh <- common$mesh |> fmesher::fm_as_sfc() |>  sf::st_as_sf() |> sf::st_zm()
bbox <- sf::st_bbox(sf_mesh)

common$add_map_layer("Mesh")

map |>
  clearControls() |>
  removeLayersControl() |>
  clearGroup("Mesh") |>
  addPolylines(data = sf_mesh, stroke = "black", weight = 2 , fill = FALSE, group = "Mesh") |>
  fitBounds(lng1 = bbox[[1]], lng2 = bbox[[3]], lat1 = bbox[[2]], lat2 = bbox[[4]]) |>
  addLayersControl(overlayGroups = common$map_layers, options = layersControlOptions(collapsed = FALSE)) |>
  hideGroup(common$map_layers) |>
  showGroup("Mesh")
}


####################### #
# CHANGING TABS #
####################### #

#' @title show_map
#' @description For internal use. Switches the view to the Map tab
#' @param parent_session Session object of the main server function
#' @keywords internal
#' @export
show_map <- function(parent_session){
  updateTabsetPanel(parent_session, "main", selected = "Map")
}

#' @title show_results
#' @description For internal use. Switches the view to the Results tab
#' @param parent_session Session object of the main server function
#' @keywords internal
#' @export
show_results <- function(parent_session){
  updateTabsetPanel(parent_session, "main", selected = "Results")
}


####################### #
# WRAP/UNWRAP TERRA #
####################### #

#' @title wrap_terra
#' @description For internal use. Flexible function for wrapping SpatRasters or
#' lists of SpatRasters but only when they exist.
#' @param object The SpatRaster or the list of SpatRasters to pack
#' @keywords internal
#' @export
wrap_terra <- function(object){
  if (!is.null(object) & (length(object) > 0)){
    if (class(object)[1] == "list") {
      if (class(object[[1]])[1] == "SpatRaster"){
        object <- lapply(object, terra::wrap)
    }}
    if (class(object)[1] == "SpatRaster"){
      object <- terra::wrap(object)
    }
  }
  return(object)
}

#' @title unwrap_terra
#' @description For internal use. Flexible function for unwrapping SpatRasters or
#' lists of SpatRasters but only when they exist.
#' @param object The SpatRaster or the list of SpatRasters to pack
#' @keywords internal
#' @export
unwrap_terra <- function(object){
  if (!is.null(object) & (length(object) > 0)){
    if (class(object)[1] == "list"){
      if (class(object[[1]])[1] == "PackedSpatRaster"){
        object <- lapply(object, terra::unwrap)
    }}
    if (class(object)[1] == "PackedSpatRaster"){
      object <- terra::unwrap(object)
    }
  }
  return(object)
}

