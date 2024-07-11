prep_summary_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    # UI
    actionButton(ns("run"), "Prepare covariate summary"),
    shinyWidgets::materialSwitch(ns("remove"), "Remove identical rows?", FALSE, status = "success"),
    shinyWidgets::radioGroupButtons(ns("table"), label = "Choose table", choices = c("Original", "Resampled"), justified = TRUE),
    uiOutput(ns("resample_layer_out")),
    actionButton(ns("resample"), "Resample covariates")
  )
}

prep_summary_module_server <- function(id, common, parent_session, map) {
  moduleServer(id, function(input, output, session) {

    output$resample_layer_out <- renderUI({
      gargoyle::watch("cov_access")
      gargoyle::watch("cov_bioclim")
      gargoyle::watch("cov_landuse")
      gargoyle::watch("cov_nightlight")
      gargoyle::watch("cov_water")
      gargoyle::watch("cov_upload")
      gargoyle::watch("agg_worldpop")
      gargoyle::watch("agg_upload")
      gargoyle::watch("agg_uniform")
      req(length(common$covs) > 0)
      selectInput(session$ns("resample_layer"), "Covariate to use as template", choices = c("", names(common$covs), names(common$agg)), multiple = FALSE)
    })


  observeEvent(input$run, {
    # WARNING ####

    ras_status <- unlist(lapply(common$tasks[grep("^(cov_|agg_)", names(common$tasks), value = TRUE)], function(x){x$status()}))
    ras_running <- length(ras_status[ras_status == "running"])
    if (ras_running != 0) {
      common$logger |> writeLog(type = "error", "Please wait for the running tasks to complete")
      return()
    }

    if (length(common$covs) == 0) {
      common$logger |> writeLog(type = "error", "Please upload covariates")
      return()
    }
    if (is.null(common$agg)) {
      common$logger |> writeLog(type = "error", "Please upload an aggregation raster")
      return()
    }
    # FUNCTION CALL ####
    #temporarily add aggregation raster to covs and remove again after the call
    common$covs$Aggregation <- common$agg
    common$covs_summary$original <- prep_summary(common$covs, remove = input$remove)
    common$covs$Aggregation <- NULL

    # TRIGGER
    gargoyle::trigger("prep_summary")
    show_results(parent_session)
  })

    observeEvent(input$resample, {

      if (input$resample_layer == "") {
        common$logger |> writeLog(type = "error", "Please select a covariate to use as a template for resampling")
        return()
      }

      # FUNCTION CALL ####
      common$covs$Aggregation <- common$agg
      common$covs_prep <- lapply(common$covs, terra::resample, common$covs[[input$resample_layer]])
      common$covs_summary$resampled <- prep_summary(common$covs_prep, remove = input$remove)
      common$covs$Aggregation <- NULL
      common$agg_prep <- common$covs_prep$Aggregation
      common$covs_prep$Aggregation <- NULL
      common$logger |> writeLog(type = "complete", "Covariates have been resampled")
      # stack the rasters
      common$covs_prep <- terra::rast(common$covs_prep)
      # LOAD INTO COMMON ####

      # METADATA ####
      common$meta$prep_summary$used <- TRUE
      common$meta$prep_summary$resample_target <- input$resample_layer
      # TRIGGER
      gargoyle::trigger("prep_summary")
      show_results(parent_session)
      shinyWidgets::updateRadioGroupButtons(session, "table", selected = "Resampled")
      do.call("prep_summary_module_map", list(map, common))
    })

    output$cov_table <- DT::renderDataTable({
    gargoyle::watch("prep_summary")
    if (input$table == "Original"){
      req(common$covs_summary$original)
      out <- DT::datatable(common$covs_summary$original, selection = "none", autoHideNavigation = TRUE,
                           options = list(pageLength = 11,
                                          columnDefs = list(list(className = 'dt-center', targets = 0:(length(common$covs)+1))))) |>
        DT::formatSignif(columns = 1:(length(common$covs)+1), rows = c(1:4, 8:11), digits = 3)
    }

    if (input$table == "Resampled"){
      req(common$covs_summary$resampled)
      out <- DT::datatable(common$covs_summary$resampled, selection = "none", autoHideNavigation = TRUE,
                           options = list(pageLength = 11,
                                          columnDefs = list(list(className = 'dt-center', targets = 0:(length(common$covs)+1))))) |>
        DT::formatSignif(columns = 1:(length(common$covs)+1), rows = c(1:4, 8:11), digits = 3)
    }

    out
  }, server = FALSE)

  return(list(
    save = function() {list(
      ### Manual save start
      ### Manual save end
      resample_layer = input$resample_layer, 
      remove = input$remove)
    },
    load = function(state) {
      ### Manual load start
      ### Manual load end
      updateSelectInput(session, "resample_layer", selected = state$resample_layer) 
      shinyWidgets::updateMaterialSwitch(session, "remove", value = state$remove)
    }
  ))
})
}

prep_summary_module_map <- function(map, common){
  for (layer in names(common$covs_prep)){
    raster_map(map, common, common$covs_prep[[layer]], layer)
  }
  raster_map(map, common, common$agg_prep, names(common$agg_prep))
  shinyjs::runjs('document.querySelector(\'input[name="core_mapping-covariates"][value="Prepared"]\').checked = true;')
}

prep_summary_module_result <- function(id) {
  ns <- NS(id)
  DT::dataTableOutput(ns("cov_table"))
}

prep_summary_module_rmd <- function(common) {
  # Variables used in the module's Rmd code
  list(
    prep_summary_knit = !is.null(common$meta$prep_summary$used),
    prep_summary_resample_target = common$meta$prep_summary$resample_target,
    prep_summary_scale = !is.null(common$meta$prep_summary$scale)
  )
}

