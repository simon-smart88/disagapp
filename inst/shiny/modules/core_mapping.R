core_mapping_module_ui <- function(id) {
  ns <- NS(id)
  tagList(
    leaflet::leafletOutput(ns("map"), height = 700),
    tags$div(
      style = "display: flex; gap: 10px;",
    selectInput(ns("bmap"), "Background map",
                choices = c("ESRI Topographic" = "Esri.WorldTopoMap",
                            "Satellite Imagery" = "Esri.WorldImagery"
                            ),
                selected = "Esri.WorldTopoMap"),
    uiOutput(ns("covariates_out"))
    )
  )



}

core_mapping_module_server <- function(id, common, main_input, COMPONENT_MODULES) {
  moduleServer(id, function(input, output, session) {

    init("clear_map")

    # create map
    output$map <- renderLeaflet({
      #reset if a new dataset is loaded
      watch("clear_map")
      leaflet() |>
        setView(0, 0, zoom = 2) |>
        addProviderTiles(isolate(input$bmap))
    })

    #Give the map priority in rendering order
    outputOptions(output, "map", suspendWhenHidden = FALSE , priority = 10)

    # create map proxy to make further changes to existing map
    map <- leafletProxy("map")

    # change provider tile option
    observe({
      map |> addProviderTiles(input$bmap)
    })

    # Capture coordinates of polygons
    init("change_poly")
    observe({
      coords <- unlist(input$map_draw_new_feature$geometry$coordinates)
      xy <- matrix(c(coords[c(TRUE, FALSE)], coords[c(FALSE, TRUE)]), ncol = 2)
      colnames(xy) <- c("longitude", "latitude")
      common$poly <- xy
      trigger("change_poly")
    }) |> bindEvent(input$map_draw_new_feature)

    component <- reactive({
      main_input$tabs
    })

    module <- reactive({
      if (component() == "intro") "intro"
      else main_input[[glue("{component()}Sel")]]
    })


    # Add the draw toolbar when using the resp_edit module
    observe({
      req(module())
      if (module() == "resp_edit"){
        map |>
          leaflet.extras::addDrawToolbar(polylineOptions = FALSE, circleOptions = FALSE, rectangleOptions = TRUE,
                         markerOptions = FALSE, circleMarkerOptions = FALSE, singleFeature = TRUE,
                         editOptions = editToolbarOptions(edit = TRUE, remove = TRUE))
      }
      if (module() != "resp_edit"){
        map |>
          leaflet.extras::removeDrawToolbar(clearFeatures = TRUE)
      }
    })

    #buttons to switch between covariate states
    output$covariates_out <- renderUI({
      watch("prep_summary")
      req(common$covs_prep)
      shinyjs::runjs("Shiny.setInputValue('core_mapping-covariates_load', 'complete');") #required to force update on load
      shinyWidgets::radioGroupButtons(
        session$ns("covariates"),
        label = "Covariates",
        choices = c("Original", "Prepared"),
        selected = "Prepared",
        size = "normal",
        status = "switch_button"
      )
    })

    observe({
      watch("prep_resolution")
      input$covariates_load #required to force update on load
      if (!is.null(common$covs_prep_lores)){
        choices <- c("Original", "High resolution", "Low resolution")
        shinyWidgets::updateRadioGroupButtons(session, "covariates", choices = choices, selected = "Low resolution", status = "switch_button")
      }
    })

    #redraw covariates according to selection
    observe({
      req(input$covariates)
      current_layer <- isolate(input$map_groups)
      if (input$covariates == "Original"){
        replot_raster_map(map, common, "covs", "agg", current_layer)
      }
      if (input$covariates == "Low resolution"){
        replot_raster_map(map, common, "covs_prep_lores", "agg_prep_lores", current_layer)
      }
      if (input$covariates == "High resolution" || input$covariates == "Prepared"){
        replot_raster_map(map, common, "covs_prep", "agg_prep", current_layer)
      }
    })

    return(map)
})
}
