prep_mesh_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    tags$h4("Boundary of outer mesh"),
    shinyWidgets::materialSwitch(ns("outer"), "Change boundary parameters", FALSE, status = "success"),
    uiOutput(ns("outer_parameters")),
    tags$h4("Internal mesh"),
    sliderInput(ns("mesh_edge"), "Max edge", min = 0.1, max = 10, value = c(0.7, 8), step = 0.1),
    sliderInput(ns("mesh_cut"), "Cut", min = 0.01, max = 1, value = 0.05, step = 0.01),
    sliderInput(ns("mesh_offset"), "Offset", min = 0.1, max = 10, value = c(1, 2), step = 0.1),
    bslib::input_task_button(ns("run"), "Make mesh")
  )
}

prep_mesh_module_server <- function(id, common, parent_session, map) {
  moduleServer(id, function(input, output, session) {

  #update default mesh arguments
  observe({
    gargoyle::watch("resp_shape")
    gargoyle::watch("resp_download")
    gargoyle::watch("resp_combine")
    gargoyle::watch("resp_example")
    gargoyle::watch("resp_edit")
    req(common$shape)

    limits <- sf::st_bbox(common$shape)
    hypotenuse <- sqrt((limits$xmax - limits$xmin)^2 + (limits$ymax - limits$ymin)^2)
    maxedge <- as.numeric(hypotenuse/10)
    updateSliderInput(session, "mesh_edge", value = c(maxedge, maxedge * 2))
    updateSliderInput(session, "mesh_offset", value = c(maxedge, maxedge * 2))
  })

  output$outer_parameters <- renderUI({
    if (input$outer){
      tagList(
        sliderInput(session$ns("convex"), "Convex", min = -0.05, max = 0.05, value = -0.01, step = 0.01),
        sliderInput(session$ns("concave"), "Concave", min = -1, max = 1, value = -0.5, step = 0.1),
        sliderInput(session$ns("resolution"), "Resolution", min = 10, max = 1000, value = 300, step = 10),
      )
    }
  })

    common$tasks$prep_mesh <- ExtendedTask$new(function(...) {
      promises::future_promise({
        disaggregation::build_mesh(...)
      }, seed = TRUE)
    }) |> bslib::bind_task_button("run")

  observeEvent(input$run, {
    # WARNING ####
    if (is.null(common$shape)) {
      common$logger |> writeLog(type = "error", "Please upload response data first")
      return()
    }
    # FUNCTION CALL ####
    common$logger |> writeLog(type = "starting", "Starting to build the mesh")
    results$resume()
    common$tasks$prep_mesh$invoke(common$shape,
                                  mesh_args = list(
                                    convex = input$convex,
                                    concave = input$concave,
                                    resolution = input$resolution,
                                    max.edge = input$mesh_edge,
                                    cut = input$mesh_cut,
                                    offset = input$offset))

    # METADATA ####
    common$meta$prep_mesh$used <- TRUE
    common$meta$prep_mesh$convex  <- input$convex
    common$meta$prep_mesh$concave  <- input$concave
    common$meta$prep_mesh$resolution  <- input$resolution
    common$meta$prep_mesh$mesh_edge <- input$mesh_edge
    common$meta$prep_mesh$mesh_cut <- input$mesh_cut
    common$meta$prep_mesh$mesh_offset <- input$mesh_offset
  })

  results <- observe({
    # LOAD INTO COMMON ####
    result <- common$tasks$prep_mesh$result()
    common$mesh <- result
    results$suspend()
    common$logger |> writeLog(type = "complete", glue::glue("The mesh has been built and has {common$mesh$n} nodes"))
    # TRIGGER
    gargoyle::trigger("prep_mesh")
    do.call("prep_mesh_module_map", list(map, common))
    show_map(parent_session)
    shinyjs::runjs("Shiny.setInputValue('prep_mesh-complete', 'complete');")
  })

  return(list(
    save = function() {list(
      ### Manual save start
      ### Manual save end
      convex = input$convex,
      concave = input$concave,
      resolution = input$resolution,
      mesh_edge = input$mesh_edge,
      mesh_cut = input$mesh_cut,
      mesh_offset = input$mesh_offset)
    },
    load = function(state) {
      ### Manual load start
      ### Manual load end
      updateSliderInput(session, "convex", value = state$convex)
      updateSliderInput(session, "concave", value = state$concave)
      updateSliderInput(session, "resolution", value = state$resolution)
      updateSliderInput(session, "mesh_edge", value = state$mesh_edge)
      updateSliderInput(session, "mesh_cut", value = state$mesh_cut)
      updateSliderInput(session, "mesh_offset", value = state$mesh_offset)
    }
  ))
})
}

prep_mesh_module_map <- function(map, common) {
  mesh_map(map, common)
}

prep_mesh_module_rmd <- function(common) {
  # Variables used in the module's Rmd code
  list(
    prep_mesh_knit = !is.null(common$meta$prep_mesh$used),
    prep_mesh_convex = common$meta$prep_mesh$convex,
    prep_mesh_concave = common$meta$prep_mesh$concave,
    prep_mesh_resolution = common$meta$prep_mesh$resolution,
    prep_mesh_edge = printVecAsis(common$meta$prep_mesh$mesh_edge),
    prep_mesh_cut = common$meta$prep_mesh$mesh_cut,
    prep_mesh_offset = printVecAsis(common$meta$prep_mesh$mesh_offset)
  )
}

