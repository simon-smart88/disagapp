prep_final_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    # UI
    uiOutput(ns("id_var_out")),
    uiOutput(ns("resp_var_out")),
    uiOutput(ns("resolution_out")),
    shinyWidgets::materialSwitch(ns("na_action"), "Handle missing data?", value = TRUE, status = "success"),
    actionButton(ns("run"), "Prepare data")
  )
}

prep_final_module_server <- function(id, common, parent_session, map) {
  moduleServer(id, function(input, output, session) {

    output$id_var_out <- renderUI({
      gargoyle::watch("resp_shape")
      gargoyle::watch("resp_download")
      gargoyle::watch("resp_combine")
      gargoyle::watch("resp_example")
      req(common$shape)
      selectInput(session$ns("id_var"), "Select ID variable", c("",names(common$shape)))
    })

    output$resp_var_out <- renderUI({
      #would this be better as an updateSelectInput?
      gargoyle::watch("resp_shape")
      gargoyle::watch("resp_download")
      gargoyle::watch("resp_combine")
      gargoyle::watch("resp_example")
      req(common$shape)
      selectInput(session$ns("resp_var"), "Select response variable", names(common$shape), selected = common$response_name)
    })

    output$resolution_out <- renderUI({
      gargoyle::watch("prep_resolution")
      req(common$covs_prep_lores)
      selectInput(session$ns("resolution"), "Select covariate resolution", c("Low resolution", "High resolution"))
    })

  observeEvent(input$run, {
    # WARNING ####

    mesh_status <- unlist(lapply(common$tasks[grep("^prep_", names(common$tasks), value = TRUE)], function(x){x$status()}))
    mesh_running <- length(mesh_status[mesh_status == "running"])
    if (mesh_running != 0) {
      common$logger |> writeLog(type = "error", "Please wait for the mesh to be built")
      return()
    }

    if (is.null(common$shape)) {
      common$logger |> writeLog(type = "error", "Please upload response data first")
      return()
    }
    if (is.null(common$covs)) {
      common$logger |> writeLog(type = "error", "Please load covariates first")
      return()
    }
    if (input$id_var == "") {
      common$logger |> writeLog(type = "error", "Please select the ID variable")
      return()
    }
    if (is.null(common$covs_prep)) {
      common$logger |> writeLog(type = "error", "Please prepare the covariates first")
      return()
    }
    if (length(common$mesh) == 0) {
      common$logger |> writeLog(type = "error", "Please prepare a mesh first")
      return()
    }
    # FUNCTION CALL ####

    show_loading_modal("Please wait while the data is prepared")

    if (is.null(input$resolution) || input$resolution == "High resolution"){

     common$prep <- tryCatch({disaggregation::prepare_data(polygon_shapefile = common$shape,
                                                 covariate_rasters = common$covs_prep,
                                                 aggregation_raster = common$agg_prep,
                                                 id_var = as.character(input$id_var),
                                                 response_var = as.character(input$resp_var),
                                                 na_action = input$na_action,
                                                 make_mesh = FALSE)},
                             error = function(x){ common$logger |> writeLog(type = "error",
                               paste0("An error occurred whilst preparing the data: ", x))})
    } else {
     common$prep <- tryCatch({disaggregation::prepare_data(polygon_shapefile = common$shape,
                                                  covariate_rasters = common$covs_prep_lores,
                                                  aggregation_raster = common$agg_prep_lores,
                                                  id_var = as.character(input$id_var),
                                                  response_var = as.character(input$resp_var),
                                                  na_action = input$na_action,
                                                  make_mesh = FALSE)},
                             error = function(x){ common$logger |> writeLog(type = "error",
                                paste0("An error occurred whilst preparing the data: ", x))})
    }


    close_loading_modal()
    if (!is.null(common$prep)){
      common$prep$mesh <- common$mesh[[common$meta$prep_mesh$selected]]
      common$logger |> writeLog(type = "complete", "Data preparation is complete")
    }
    # LOAD INTO COMMON ####

    # METADATA ####
    common$meta$prep_final$id_var <- as.character(input$id_var)
    common$meta$prep_final$resp_var <- as.character(input$resp_var)
    common$meta$prep_final$na_action <- input$na_action
    if (is.null(input$resolution)){
      common$meta$prep_final$resolution <- "High resolution"
    } else {
      common$meta$prep_final$resolution <- input$resolution
    }

    # TRIGGER
    gargoyle::trigger("prep_final")
    # do.call("prep_final_module_map", list(map, common))
    show_map(parent_session)
  })

  return(list(
    save = function() {list(
      ### Manual save start
      ### Manual save end
      id_var = input$id_var,
      resp_var = input$resp_var,
      resolution = input$resolution,
      na_action = input$na_action)
    },
    load = function(state) {
      ### Manual load start
      ### Manual load end
      updateSelectInput(session, "id_var", selected = state$id_var)
      updateSelectInput(session, "resp_var", selected = state$resp_var)
      updateSelectInput(session, "resolution", selected = state$resolution)
      shinyWidgets::updateMaterialSwitch(session, "na_action", value = state$na_action)
    }
  ))
})
}

# prep_final_module_map <- function(map, common){
#   for (layer in names(common$prep$covariate_rasters)){
#     raster_map(map, common, common$prep$covariate_rasters[[layer]], layer)
#   }
#   agg_log <- c(common$meta$agg_worldpop$log, common$meta$agg_upload$log)
#   if (is.null(common$meta$prep_final$resolution) || common$meta$prep_final$resolution == "High resolution"){
#     raster_map(map, common, common$agg_prep, names(common$agg_prep), agg_log)
#     if (is.null(common$meta$prep_final$resolution)){
#       shinyjs::runjs('document.querySelector(\'input[name="core_mapping-covariates"][value="Prepared"]\').checked = true;')
#     } else {
#       shinyjs::runjs('document.querySelector(\'input[name="core_mapping-covariates"][value="High resolution"]\').checked = true;')
#     }
#   } else {
#     raster_map(map, common, common$agg_prep_lores, names(common$agg_prep_lores), agg_log)
#     shinyjs::runjs('document.querySelector(\'input[name="core_mapping-covariates"][value="Low resolution"]\').checked = true;')
#   }
#   map |> showGroup("Response")
# }

prep_final_module_rmd <- function(common) {
  # Variables used in the module's Rmd code
  list(
    prep_knit = !is.null(common$prep),
    prep_id_var = common$meta$prep_final$id_var,
    prep_resp_var = common$meta$prep_final$resp_var,
    prep_na_action = common$meta$prep_final$na_action
  )
}
