core_mapping_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    leaflet::leafletOutput(ns("map"), height = 700),
    selectInput(ns("bmap"), "Background map",
                choices = c("ESRI Topo" = "Esri.WorldTopoMap",
                            "Open Topo" = "OpenTopoMap",
                            "ESRI Imagery" = "Esri.WorldImagery",
                            "ESRI Nat Geo" = "Esri.NatGeoWorldMap"),
                selected = "Esri.WorldTopoMap")
    )
}

core_mapping_module_server <- function(id, common, main_input, COMPONENT_MODULES) {
  moduleServer(id, function(input, output, session) {

    gargoyle::init("clear_map")

    # create map
    output$map <- renderLeaflet({
      #reset once data is prepared
      gargoyle::watch("clear_map")
      leaflet() %>%
        setView(0, 0, zoom = 2) %>%
        addProviderTiles(isolate(input$bmap))
    })

    #Give the map priority in rendering order
    outputOptions(output, "map", suspendWhenHidden = FALSE , priority = 10)

    # create map proxy to make further changes to existing map
    map <- leafletProxy("map")

    # change provider tile option
    observe({
      map %>% addProviderTiles(input$bmap)
    })

    # Capture coordinates of polygons
    gargoyle::init("change_poly")
    observe({
      coords <- unlist(input$map_draw_new_feature$geometry$coordinates)
      xy <- matrix(c(coords[c(TRUE, FALSE)], coords[c(FALSE, TRUE)]), ncol = 2)
      colnames(xy) <- c("longitude", "latitude")
      common$poly <- xy
      gargoyle::trigger("change_poly")
    }) %>% bindEvent(input$map_draw_new_feature)

    component <- reactive({
      main_input$tabs
    })

    module <- reactive({
      if (component() == "intro") "intro"
      else main_input[[glue("{component()}Sel")]]
    })

    # observe({
    #   req(module())
    #   current_mod <- module()
    #   gargoyle::on(current_mod, {
    #     map_fx <- COMPONENT_MODULES[[component()]][[module()]]$map_function
    #     if (!is.null(map_fx)) {
    #       do.call(map_fx, list(map, common = common))
    #     }
    #   })
    # })

    # Add the draw toolbar when using the resp_edit module
    observe({
      req(module())
      if (module() == "resp_edit"){
        map %>%
          leaflet.extras::addDrawToolbar(polylineOptions = FALSE, circleOptions = FALSE, rectangleOptions = TRUE,
                         markerOptions = FALSE, circleMarkerOptions = FALSE, singleFeature = TRUE,
                         editOptions = editToolbarOptions(edit = TRUE, remove = TRUE))
      }
      if (module() != "resp_edit"){
        map %>%
          leaflet.extras::removeDrawToolbar(clearFeatures = TRUE)
      }
    })


    return(map)
})
}
