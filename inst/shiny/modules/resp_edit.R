resp_edit_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    radioButtons(ns("type"), "Polygons to keep", choices = c("Outside", "Inside")),
    actionButton(ns("run"), "Edit shapefile")
  )
}

resp_edit_module_server <- function(id, common) {
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
    common$meta$shape$poly <- common$poly
    common$meta$shape$edit_type <- input$type

    # TRIGGER
    gargoyle::trigger("resp_edit")
  })

  return(list(
    save = function() {
      # Save any values that should be saved when the current session is saved
    },
    load = function(state) {
      # Load
    }
  ))
})
}

resp_edit_module_map <- function(map, common) {
  map %>%
    addDrawToolbar(polylineOptions = FALSE, circleOptions = FALSE, rectangleOptions = TRUE,
                   markerOptions = FALSE, circleMarkerOptions = FALSE, singleFeature = TRUE,
                   editOptions = editToolbarOptions(edit = TRUE, remove = TRUE))

  gargoyle::on("resp_edit", {
    req(common$shape)
    response <- as.numeric(common$shape[[common$meta$shape$response]])
    ex <- as.vector(terra::ext(common$shape))
    common$add_map_layer("Response")
    pal <- colorBin("viridis", domain = response, bins = 9, na.color ="#00000000")
    map %>%
      clearGroup("Response") %>%
      clearControls() %>%
      addPolygons(data = common$shape, fillColor = ~pal(response), color = 'black', fillOpacity = 0.7, weight = 3, group = "Response", popup = ~as.character(round(response,0))) %>%
      fitBounds(lng1 = ex[[1]], lng2 = ex[[2]], lat1 = ex[[3]], lat2 = ex[[4]]) %>%
      addLegend(position = "bottomright", pal = pal, values = response, group = "Response", title = "Response") %>%
      addLayersControl(overlayGroups = common$map_layers, options = layersControlOptions(collapsed = FALSE)) %>%
      removeDrawToolbar(clearFeatures = TRUE)


  })

}

resp_edit_module_rmd <- function(common) {
  # Variables used in the module's Rmd code
  list(
    resp_edit_knit = !is.null(common$some_object),
    var1 = common$meta$setting1,
    var2 = common$meta$setting2
  )
}

