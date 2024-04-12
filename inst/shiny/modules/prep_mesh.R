prep_mesh_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    sliderInput(ns("mesh_edge"), "Max edge", min = 0, max = 10, value = c(0.7, 8), step = 0.1),
    sliderInput(ns("mesh_cut"), "Cut", min = 0, max = 1, value = 0.05, step = 0.01),
    sliderInput(ns("mesh_offset"), "Offset", min = 0, max = 10, value = c(1, 2), step = 0.1),
    actionButton(ns("run"), "Make mesh")
  )
}

prep_mesh_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {

  #update default mesh arguments
  observe({
    gargoyle::watch("resp_shape")
    gargoyle::watch("resp_download")
    gargoyle::watch("resp_combine")
    gargoyle::watch("resp_edit")
    req(common$shape)

    limits <- sf::st_bbox(common$shape)
    hypotenuse <- sqrt((limits$xmax - limits$xmin)^2 + (limits$ymax - limits$ymin)^2)
    maxedge <- as.numeric(hypotenuse/10)
    updateSliderInput(session, "mesh_edge", value = c(maxedge, maxedge * 2))
    updateSliderInput(session, "mesh_offset", value = c(maxedge, maxedge * 2))
  })

  observeEvent(input$run, {
    # WARNING ####
    if (is.null(common$shape)) {
      common$logger %>% writeLog(type = "error", "Please upload response data first")
      return()
    }
    # FUNCTION CALL ####
    show_loading_modal("Please wait whilst the mesh is built")
    mesh <- disaggregation::build_mesh(common$shape, mesh.args = list(max.edge = input$mesh_edge,
                                                              cut = input$mesh_cut,
                                                              offset = input$mesh_offset))
    # LOAD INTO COMMON ####
    common$mesh <- mesh
    common$logger %>% writeLog("The mesh has been built")
    close_loading_modal()
    # METADATA ####
    common$meta$prep_mesh$used <- TRUE
    common$meta$prep_mesh$mesh_edge <- input$mesh_edge
    common$meta$prep_mesh$mesh_cut <- input$mesh_cut
    common$meta$prep_mesh$mesh_offset <- input$mesh_offset
    # TRIGGER
    gargoyle::trigger("prep_mesh")
  })

  return(list(
    save = function() {
      list(
      mesh_edge = input$mesh_edge,
      mesh_cut = input$mesh_cut,
      mesh_offset = input$mesh_offset)
    },
    load = function(state) {
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

