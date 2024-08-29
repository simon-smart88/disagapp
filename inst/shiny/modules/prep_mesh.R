prep_mesh_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
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
    common$tasks$prep_mesh$invoke(common$shape, mesh_args = list(max.edge = input$mesh_edge,
                                                              cut = input$mesh_cut,
                                                              offset = input$offset))

    # METADATA ####
    common$meta$prep_mesh$used <- TRUE
    common$meta$prep_mesh$mesh_edge <- input$mesh_edge
    common$meta$prep_mesh$mesh_cut <- input$mesh_cut
    common$meta$prep_mesh$mesh_offset <- input$mesh_offset
  })

  results <- observe({
    # LOAD INTO COMMON ####
    result <- common$tasks$prep_mesh$result()
    common$mesh <- result
    results$suspend()
    common$logger |> writeLog(type = "complete", "The mesh has been built")
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
      mesh_edge = input$mesh_edge,
      mesh_cut = input$mesh_cut,
      mesh_offset = input$mesh_offset)
    },
    load = function(state) {
      ### Manual load start
      ### Manual load end
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
    prep_mesh_edge = printVecAsis(common$meta$prep_mesh$mesh_edge),
    prep_mesh_cut = common$meta$prep_mesh$mesh_cut,
    prep_mesh_offset = printVecAsis(common$meta$prep_mesh$mesh_offset)
  )
}

