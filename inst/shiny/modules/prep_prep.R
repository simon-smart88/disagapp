prep_prep_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    # UI
    uiOutput(ns("id_var_out")),
    uiOutput(ns("resp_var_out")),
    uiOutput(ns("resolution_out")),
    checkboxInput(ns("na_action"), "Handle missing data?", value = TRUE),
    actionButton(ns("run"), "Prepare data")
  )
}

prep_prep_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {

    output$id_var_out <- renderUI({
      gargoyle::watch("resp_shape")
      gargoyle::watch("resp_download")
      gargoyle::watch("resp_combine")
      req(common$shape)
      selectInput(session$ns("id_var"), "Select ID variable", c("",names(common$shape)))
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

    output$resolution_out <- renderUI({
      gargoyle::watch("prep_resolution")
      req(common$covs_prep_lores)
      selectInput(session$ns("resolution"), "Select covariate resolution", c("Low resolution", "High resolution"))
    })

  observeEvent(input$run, {
    # WARNING ####
    if (input$id_var == "") {
      common$logger %>% writeLog(type = "error", "Please select the ID variable")
      return()
    }
    if (is.null(common$covs_prep)) {
      common$logger %>% writeLog(type = "error", "Please prepare the covariates first")
      return()
    }
    if (is.null(common$mesh)) {
      common$logger %>% writeLog(type = "error", "Please prepare a mesh first")
      return()
    }
    # FUNCTION CALL ####

    show_loading_modal("Please wait while the data is prepared")

    if (is.null(input$resolution) || input$resolution == "High resolution"){
     common$prep <- disaggregation::prepare_data(polygon_shapefile = common$shape,
                                                 covariate_rasters = common$covs_prep,
                                                 aggregation_raster = common$agg_prep,
                                                 id_var = as.character(input$id_var),
                                                 response_var = as.character(input$resp_var),
                                                 na.action = input$na_action,
                                                 makeMesh = FALSE)
    } else {
     common$prep <- disaggregation::prepare_data(polygon_shapefile = common$shape,
                                                  covariate_rasters = common$covs_prep_lores,
                                                  aggregation_raster = common$agg_prep_lores,
                                                  id_var = as.character(input$id_var),
                                                  response_var = as.character(input$resp_var),
                                                  na.action = input$na_action,
                                                  makeMesh = FALSE)
    }

    common$prep$mesh <- common$mesh
    close_loading_modal()
    common$logger %>% writeLog("Data preparation is complete")
    # LOAD INTO COMMON ####

    # METADATA ####
    common$meta$prep_prep$id_var <- as.character(input$id_var)
    common$meta$prep_prep$resp_var <- as.character(input$resp_var)
    common$meta$prep_prep$na_action <- input$na_action
    common$meta$prep_prep$resolution <- input$resolution
    # TRIGGER
    gargoyle::trigger("prep_prep")
    updateTabsetPanel(parent_session, "main", selected = "Map")
  })

  return(list(
    save = function() {
list(na_action = input$na_action,
id_var = input$id_var,
resp_var = input$resp_var)
    },
    load = function(state) {
updateCheckboxInput(session, "na_action", value = state$na_action)
updateSelectInput(session, "id_var", selected = state$id_var)
updateSelectInput(session, "resp_var", selected = state$resp_var)
    }
  ))
})
}

prep_prep_module_map <- function(map, common){
  for (layer in names(common$prep$covariate_rasters)){
    covariate_map(map, common, common$prep$covariate_rasters[[layer]], layer)
  }
  agg_log <- c(common$meta$agg_worldpop$log, common$meta$agg_upload$log)
  if (is.null(common$meta$prep_prep$resolution) || common$meta$prep_prep$resolution == "High resolution"){
    covariate_map(map, common, common$agg_prep, names(common$agg_prep), agg_log)
  } else {
    covariate_map(map, common, common$agg_prep_lores, names(common$agg_prep_lores), agg_log)
  }
}

prep_prep_module_rmd <- function(common) {
  # Variables used in the module's Rmd code
  list(
    prep_knit = !is.null(common$prep),
    prep_id_var = common$meta$prep_prep$id_var,
    prep_resp_var = common$meta$prep_prep$resp_var,
    prep_na_action = common$meta$prep_prep$na_action
  )
}
