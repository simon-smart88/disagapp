####################### #
# MISC #
####################### #
#' @title printVecAsis
#' @description For internal use. Print objects as character string
#' @param x object to print
#' @return A character string to reproduce the object
#' @keywords internal
#' @export
printVecAsis <- function(x) {
  if (is.numeric(x) && length(x) == 1){
    return(x)
  } else {
    utils::capture.output(dput(x))
  }
}

#' @title check_url
#' @description For internal use. Checks whether a URL is live
#' @param url character. The URL to check
#' @returns TRUE if url is live, FALSE if not
#' @keywords internal
#' @export
check_url <- function(url){
  req <- httr2::request(url)
  resp <- tryCatch(
    req %>% httr2::req_method("HEAD") %>% httr2::req_perform(),
    httr2_http_404 = function(cnd){NULL},
    httr2_failure = function(cnd){NULL},
    httr2_error = function(cnd){NULL}
  )
  if (is.null(resp)){
    return(FALSE)
  } else {
    return(TRUE)
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
  leaflet.extras::removeDrawToolbar(x)
  markdown::html_format(x)
  openxlsx::read.xlsx(x)
  parallelly::as.cluster(x)
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
  watch("resp_shape")
  watch("resp_combine")
  watch("resp_download")
  watch("resp_example")
  if (!is.null(common$shape)){
    shinyWidgets::materialSwitch(session$ns("reset"), "Delete existing data?", FALSE, status = "success")
  }
}

#' @title reset_data
#' @description For internal use. Clears the common structure of data, resets the map and any plots
#' @keywords internal
#' @param common The common data structure
#' @export
reset_data <- function(common){
  modules <- names(common$meta)
  common$reset()
  trigger("clear_map")
  for (module in modules){
    trigger(module)
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
      if (nchar(...) < 350){
        shinyalert::shinyalert(..., type = "info")
      } else {
        shinyalert::shinyalert("Please, check Log window for more information ",
                               type = "info")
      }
      pre <- paste0(icon("info", class = "log_info"), " ")
    } else if (type == "error") {
      if (nchar(...) < 350){
        shinyalert::shinyalert(...,
                               type = "error")
      } else {
        shinyalert::shinyalert("Please, check Log window for more information ",
                               type = "error")
      }
      pre <- paste0(icon("xmark", class = "log_error"), " ")
    } else if (type == "warning") {
      if (nchar(...) < 350){
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

#' @title asyncLog
#' @description For internal use. Similar to writeLog but for use inside async
#' functions
#' @param async Whether the function is being used asynchronously
#' @param ... Messages to write to the logger
#' @param type One of `default`, `info`, `error`, `warning`
#' @returns No return value, called for side effects
#' @keywords internal
#' @export
asyncLog <- function(async, ..., type = "default"){
  if (!async) {
    if (type == "error") {
      stop(paste0(..., collapse = ""), call. = FALSE)
    } else if (type == "warning") {
      warning(paste0(..., collapse = ""), call. = FALSE)
    } else {
      message(paste0(..., collapse = ""))
    }
  } else {
    return(as.character(...))
  }
}

####################### #
# LOADING MODAL #
####################### #

#' @title show_loading_modal
#' @description For internal use. Show a modal when something is loading
#' @param message character. The message to display to the user.
#' @returns No return value, called for side effects
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
#' @returns No return value, called for side effects
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
#' @returns No return value, called for side effects
#' @keywords internal
#' @export
country_out <- function(session, common){
  init("country_out")
  renderUI({
    watch("country_out")
    if (is.null(common$selected_country)){
      selectInput(session$ns("country"), "Select country", c("", common$countries$boundaryName), multiple = TRUE)
    } else {
      selectInput(session$ns("country"), "Select country", common$countries$boundaryName, selected = common$selected_country, multiple = TRUE)
    }
  })
}

####################### #
# CHANGING TABS #
####################### #

#' @title show_map
#' @description For internal use. Switches the view to the Map tab
#' @param parent_session Session object of the main server function
#' @returns No return value, called for side effects
#' @keywords internal
#' @export
show_map <- function(parent_session){
  updateTabsetPanel(parent_session, "main", selected = "Map")
}

#' @title show_results
#' @description For internal use. Switches the view to the Results tab
#' @param parent_session Session object of the main server function
#' @returns No return value, called for side effects
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
#' @returns Either a PackedSpatRaster or list of PackedSpatRasters
#' @keywords internal
#' @export
wrap_terra <- function(object){
  if (!is.null(object) && (length(object) > 0)){
    if (inherits(object, "list")) {
      if (inherits(object[[1]], "SpatRaster")){
        object <- lapply(object, terra::wrap)
    }}
    if (inherits(object, "SpatRaster")){
      object <- terra::wrap(object)
    }
  }
  return(object)
}

#' @title unwrap_terra
#' @description For internal use. Flexible function for unwrapping SpatRasters or
#' lists of SpatRasters but only when they exist.
#' @param object The PackedSpatRaster or the list of PackedSpatRasters to unpack
#' @returns Either a SpatRaster or list of SpatRasters
#' @keywords internal
#' @export
unwrap_terra <- function(object){
  if (!is.null(object) && (length(object) > 0)){
    if (inherits(object, "list")) {
      if (inherits(object[[1]], "PackedSpatRaster")){
        object <- lapply(object, terra::unwrap)
    }}
    if (inherits(object, "PackedSpatRaster")){
      object <- terra::unwrap(object)
    }
  }
  return(object)
}

#' @title response_area
#' @description Calculate the area of the response data in square kilometers
#' @param shape sf. The sf object containing the response data
#' @return numeric. Area in square kilometers
#' @keywords internal
#' @export
response_area <- function(shape){
  as.numeric(sum(sf::st_area(shape)) / 1e6)
}



