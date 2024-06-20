resp_edit_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    radioButtons(ns("type"), "Polygons to keep", choices = c("Outside", "Inside")),
    actionButton(ns("run"), "Edit shapefile")
  )
}

resp_edit_module_server <- function(id, common, parent_session, map) {
  moduleServer(id, function(input, output, session) {

  observeEvent(input$run, {

    # TEST MODE - required due to the polygon not being able to be tested correctly.
    if (isTRUE(getOption("shiny.testmode"))) {
      poly_matrix <- matrix(c(40, 40, 55, 55, 40, -10, -20, -20, -10, -10), ncol = 2)
      colnames(poly_matrix) <- c("longitude", "latitude")
      common$poly <- poly_matrix
    }

    # WARNING ####
    if (is.null(common$shape)) {
      common$logger %>% writeLog(type = "error", "Please upload response data first")
      return()
    }
    if (isFALSE(getOption("shiny.testmode"))) {
      if (is.null(common$poly)) {
        common$logger %>% writeLog(type = "error", "Please draw a shape on the map first")
        return()
      }
    }

    # FUNCTION CALL ####
    shape <- resp_edit(common$shape, common$poly, input$type, common$logger)

    # LOAD INTO COMMON ####
    common$shape <- shape

    # METADATA ####
    common$meta$resp_edit$poly <- common$poly
    common$meta$resp_edit$type <- input$type
    common$meta$resp_edit$used <- TRUE
    # TRIGGER
    gargoyle::trigger("resp_edit")
  })

  return(list(
    save = function() {
list(type = input$type)
    },
    load = function(state) {
updateRadioButtons(session, "type", selected = state$type)
    }
  ))
})
}

#initial addition and final removal of the toolbar is handled inside core_mapping_module_server
resp_edit_module_map <- function(map, common) {
  map %>%
    removeControl("Response") %>%
    removeDrawToolbar(clearFeatures = TRUE)
  shape_map(map, common)
  map %>%
    addDrawToolbar(polylineOptions = FALSE, circleOptions = FALSE, rectangleOptions = TRUE,
                   markerOptions = FALSE, circleMarkerOptions = FALSE, singleFeature = TRUE,
                   editOptions = editToolbarOptions(edit = TRUE, remove = TRUE))
}

resp_edit_module_rmd <- function(common) {
  # Variables used in the module's Rmd code
  list(
    resp_edit_knit = !is.null(common$meta$resp_edit$used),
    resp_edit_type = common$meta$resp_edit$type,
    resp_edit_poly = printVecAsis(common$meta$resp_edit$poly)
  )
}

