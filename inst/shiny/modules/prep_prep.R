prep_prep_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    # UI
    uiOutput(ns("id_var_out")),
    uiOutput(ns("resp_var_out")),
    sliderInput(ns("mesh_edge"),"Max edge", min = 0, max = 10, value = c(0.7, 8), step = 0.1),
    sliderInput(ns("mesh_cut"),"Cut", min = 0, max = 1, value = 0.05, step = 0.01),
    sliderInput(ns("mesh_offset"), "Offset", min = 0, max = 10, value = c(1, 2), step = 0.1),
    checkboxInput(ns("na_action"), "Handle NAs?", value = TRUE),
    checkboxInput(ns("mesh_make"), "Make mesh?", value = TRUE),
    actionButton(ns("run"), "Prepare data")
  )
}

prep_prep_module_server <- function(id, common) {
  moduleServer(id, function(input, output, session) {

    output$id_var_out <- renderUI({
      gargoyle::watch("resp_shape")
      gargoyle::watch("resp_download")
      gargoyle::watch("resp_combine")
      req(common$shape)
      selectInput(session$ns("id_var"), "Select ID variable", names(common$shape))
    })

    output$resp_var_out <- renderUI({
      #would this be better as an updateSelectInput?
      gargoyle::watch("resp_shape")
      gargoyle::watch("resp_download")
      gargoyle::watch("resp_combine")
      req(common$shape)
      selected_response <- c(common$meta$resp_shape$response,
                             common$meta$resp_combine$response,
                             common$meta$resp_download$response)[1]
      selectInput(session$ns("resp_var"), "Select response variable", names(common$shape), selected = selected_response)
    })

  observeEvent(input$run, {
    # WARNING ####
    if (is.null(common$covs_prep)) {
      common$logger %>% writeLog(type = "error", "Please prepare the covariates")
      return()
    }
    # FUNCTION CALL ####

    show_loading_modal("Please wait while the data is prepared")
    prep <- disaggregation::prepare_data(polygon_shapefile = common$shape,
                                         covariate_rasters = common$covs_prep,
                                         aggregation_raster = common$agg_prep,
                                         id_var = as.character(input$id_var),
                                         response_var = as.character(input$resp_var),
                                         mesh.args = list(max.edge = input$mesh_edge,
                                                          cut = input$mesh_cut,
                                                          offset = input$mesh_offset),
                                         na.action = input$na_action,
                                         makeMesh = input$mesh_make)
    close_loading_modal()
    common$logger %>% writeLog("Data preparation is completed")
    # LOAD INTO COMMON ####
    common$prep <- prep
    # METADATA ####
    common$meta$prep$id_var <- as.character(input$id_var)
    common$meta$prep$resp_var <- as.character(input$resp_var)
    common$meta$prep$mesh_edge <- input$mesh_edge
    common$meta$prep$mesh_cut <- input$mesh_cut
    common$meta$prep$mesh_offset <- input$mesh_offset
    common$meta$prep$na_action <- input$na_action
    common$meta$prep$make_mesh <- input$mesh_make
    # TRIGGER
    gargoyle::trigger("prep_prep")
  })

  output$result <- renderText({
    # Result
  })

  return(list(
    save = function() {
list(mesh_edge = input$mesh_edge,
mesh_cut = input$mesh_cut,
mesh_offset = input$mesh_offset,
na_action = input$na_action,
mesh_make = input$mesh_make,
id_var = input$id_var,
resp_var = input$resp_var)
    },
    load = function(state) {
updateSliderInput(session, "mesh_edge", value = state$mesh_edge)
updateSliderInput(session, "mesh_cut", value = state$mesh_cut)
updateSliderInput(session, "mesh_offset", value = state$mesh_offset)
updateCheckboxInput(session, "na_action", value = state$na_action)
updateCheckboxInput(session, "mesh_make", value = state$mesh_make)
updateSelectInput(session, "id_var", selected = state$id_var)
updateSelectInput(session, "resp_var", selected = state$resp_var)
    }
  ))
})
}

prep_prep_module_result <- function(id) {
  ns <- NS(id)

  # Result UI
  verbatimTextOutput(ns("result"))
}

prep_prep_module_map <- function(map, common) {
  # Map logic
}

prep_prep_module_rmd <- function(common) {
  # Variables used in the module's Rmd code
  list(
    prep_knit = !is.null(common$prep),
    prep_id_var = common$meta$prep$id_var,
    prep_resp_var = common$meta$prep$resp_var,
    prep_mesh_edge = printVecAsis(common$meta$prep$mesh_edge),
    prep_mesh_cut = common$meta$prep$mesh_cut,
    prep_mesh_offset = printVecAsis(common$meta$prep$mesh_offset),
    prep_na_action = common$meta$prep$na_action,
    prep_make_mesh = common$meta$prep$make_mesh
  )
}

