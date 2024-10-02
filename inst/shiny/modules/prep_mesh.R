prep_mesh_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(

    tags$h4("Mesh size"),
    sliderInput(ns("max_edge"), "Max edge (degrees)", min = 0.1, max = 10, value = c(0.7, 8), step = 0.1),
    sliderInput(ns("offset"), "Offset (degrees)", min = 0.1, max = 10, value = c(1, 2), step = 0.1),
    sliderInput(ns("cutoff"), "Cutoff (degrees)", min = 0.01, max = 1, value = 0.05, step = 0.01),
    tags$h4("Mesh boundary"),
    shinyWidgets::materialSwitch(ns("outer"), "Change boundary parameters", FALSE, status = "success"),
    conditionalPanel("input.outer === true", ns = ns,
     sliderInput(ns("convex"), "Convex", min = -0.1, max = -0.01, value = -0.01, step = 0.01),
     sliderInput(ns("concave"), "Concave", min = -1, max = -0.1, value = -0.5, step = 0.1),
     sliderInput(ns("resolution"), "Resolution", min = 10, max = 1000, value = 300, step = 10)
    ),
    bslib::input_task_button(ns("run"), "Make mesh"),
    tags$hr(),
    uiOutput(ns("choice_out"))
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
    max_edge <- as.numeric(hypotenuse/10)
    updateSliderInput(session, "max_edge", value = c(max_edge, max_edge * 2))
    updateSliderInput(session, "offset", value = c(max_edge, max_edge * 2))
  })

  output$choice_out <- renderUI({
    gargoyle::watch("prep_mesh")
    req(length(common$mesh) > 0)
    selectInput(session$ns("choice"), "Selected mesh", choices = names(common$mesh), selected = names(common$mesh)[length(common$mesh)])
  })

  outputOptions(output, "choice_out", suspendWhenHidden = FALSE)

  observe({
    req(input$choice)
    common$meta$prep_mesh$selected <- input$choice
    do.call("prep_mesh_module_map", list(map, common))
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
                                    max.edge = input$max_edge,
                                    cutoff = input$cutoff,
                                    offset = input$offset))

    # METADATA ####
    common$meta$prep_mesh$used <- TRUE

    # create empty lists on first use
    if (is.null(common$meta$prep_mesh$convex)){
      common$meta$prep_mesh$convex <- list()
      common$meta$prep_mesh$concave <- list()
      common$meta$prep_mesh$resolution <- list()
      common$meta$prep_mesh$max_edge <- list()
      common$meta$prep_mesh$cutoff <- list()
      common$meta$prep_mesh$offset <- list()
    }

    common$meta$prep_mesh$convex <- append(common$meta$prep_mesh$convex, input$convex)
    common$meta$prep_mesh$concave <- append(common$meta$prep_mesh$concave, input$concave)
    common$meta$prep_mesh$resolution <- append(common$meta$prep_mesh$resolution, input$resolution)
    common$meta$prep_mesh$max_edge <- append(common$meta$prep_mesh$max_edge, list(input$max_edge))
    common$meta$prep_mesh$cutoff <- append(common$meta$prep_mesh$cutoff, input$cutoff)
    common$meta$prep_mesh$offset <- append(common$meta$prep_mesh$offset, list(input$offset))
  })

  results <- observe({
    # LOAD INTO COMMON ####
    result <- common$tasks$prep_mesh$result()
    mesh_name <- glue::glue("Mesh {length(common$mesh) + 1} - {result$n} nodes")
    common$mesh[[mesh_name]] <- result
    common$meta$prep_mesh$selected <- mesh_name
    results$suspend()
    common$logger |> writeLog(type = "complete", glue::glue("Mesh {length(common$mesh)} has been built and has {result$n} nodes"))
    # TRIGGER
    gargoyle::trigger("prep_mesh")
    do.call("prep_mesh_module_map", list(map, common))
    show_map(parent_session)
    shinyjs::runjs("Shiny.setInputValue('prep_mesh-complete', 'complete');")
  })

  output$plot <- renderPlot({
    gargoyle::watch("prep_mesh")
    plot_list <- lapply(seq_along(common$mesh), function(x) plot_mesh(common$mesh[[x]], names(common$mesh)[x]))
    nrows <- ceiling(length(plot_list) / 3)
    cowplot::plot_grid(plotlist = plot_list, nrow = nrows)
  })

  return(list(
    save = function() {list(
      ### Manual save start
      ### Manual save end
      convex = input$convex,
      concave = input$concave,
      resolution = input$resolution,
      max_edge = input$max_edge,
      cutoff = input$cutoff,
      offset = input$offset,
      selected = input$selected)
    },
    load = function(state) {
      ### Manual load start
      ### Manual load end
      updateSliderInput(session, "convex", value = state$convex)
      updateSliderInput(session, "concave", value = state$concave)
      updateSliderInput(session, "resolution", value = state$resolution)
      updateSliderInput(session, "max_edge", value = state$max_edge)
      updateSliderInput(session, "cutoff", value = state$cutoff)
      updateSliderInput(session, "offset", value = state$offset)
      updateSelectInput(session, "selected", value = state$selected)
    }
  ))
})
}

prep_mesh_module_result <- function(id){
  ns <- shiny::NS(id)
  plotOutput(ns("plot"), height = "800px")
}

prep_mesh_module_map <- function(map, common) {
  mesh_map(map, common)
}

prep_mesh_module_rmd <- function(common) {
  selected <- which(names(common$mesh) == common$meta$prep_mesh$selected)
    list(
    prep_mesh_knit = !is.null(common$meta$prep_mesh$used),
    prep_mesh_convex = common$meta$prep_mesh$convex[[selected]],
    prep_mesh_concave = common$meta$prep_mesh$concave[[selected]],
    prep_mesh_resolution = common$meta$prep_mesh$resolution[[selected]],
    prep_mesh_max_edge = printVecAsis(common$meta$prep_mesh$max_edge[[selected]]),
    prep_mesh_cutoff = common$meta$prep_mesh$cutoff[[selected]],
    prep_mesh_offset = printVecAsis(common$meta$prep_mesh$offset[[selected]])
  )
}

