prep_summary_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    # UI
    actionButton(ns("run"), "Prepare covariate summary"),
    shinyWidgets::materialSwitch(ns("remove"), "Remove identical rows?", FALSE, status = "success"),
    shinyWidgets::radioGroupButtons(ns("table"), label = "Choose table", choices = c("Original", "Resampled"), justified = TRUE),
    actionButton(ns("resample"), "Resample covariates")
  )
}

prep_summary_module_server <- function(id, common, parent_session, map) {
  moduleServer(id, function(input, output, session) {

  observeEvent(input$run, {
    print("prep_summary")
    # WARNING ####
    if (length(common$covs) == 0) {
      common$logger %>% writeLog(type = "error", "Please upload covariates")
      return()
    }
    if (is.null(common$agg)) {
      common$logger %>% writeLog(type = "error", "Please upload an aggregation raster")
      return()
    }
    # FUNCTION CALL ####
    #temporarily add aggregation raster to covs and remove again after the call
    common$covs$Aggregation <- common$agg
    common$covs_summary$original <- prep_summary(common$covs, remove = input$remove)
    common$covs$Aggregation <- NULL

    # LOAD INTO COMMON ####

    # METADATA ####

    # TRIGGER
    gargoyle::trigger("prep_summary")
    show_results(parent_session)
  })

    observeEvent(input$resample, {
      # WARNING ####
      if (isFALSE(getOption("shiny.testmode"))) {
        if (is.null(input$cov_table_columns_selected)) {
          common$logger %>% writeLog(type = "error", "Please select a column from the table")
          return()
        }
        column_selected <- input$cov_table_columns_selected
      }

      if (isTRUE(getOption("shiny.testmode"))) {
        column_selected <- 1
      }

      # FUNCTION CALL ####
      common$covs$Aggregation <- common$agg
      common$covs_prep <- lapply(common$covs, terra::resample, common$covs[[column_selected]])
      common$covs_summary$resampled <- prep_summary(common$covs_prep, remove = input$remove)
      common$covs$Aggregation <- NULL
      common$agg_prep <- common$covs_prep$Aggregation
      common$covs_prep$Aggregation <- NULL
      common$logger %>% writeLog("Covariates have been resampled")
      # stack the rasters
      common$covs_prep <- terra::rast(common$covs_prep)
      # LOAD INTO COMMON ####

      # METADATA ####
      common$meta$prep_summary$used <- TRUE
      common$meta$prep_summary$resample_target <- colnames(common$covs_summary$resampled)[[column_selected]]
      # TRIGGER
      gargoyle::trigger("prep_summary")
      show_results(parent_session)
      shinyWidgets::updateRadioGroupButtons(session, "table", selected = "Resampled")
    })

    output$cov_table <- DT::renderDataTable({
    gargoyle::watch("prep_summary")
    if (input$table == "Original"){
      req(common$covs_summary$original)
      out <- DT::datatable(common$covs_summary$original, selection = list(mode = "single", target = "column"), autoHideNavigation = TRUE,
                           options = list(pageLength = 11,
                                          columnDefs = list(list(className = 'dt-center', targets = 0:(length(common$covs)+1))))) %>%
        DT::formatSignif(columns = 1:(length(common$covs)+1), rows = c(1:4, 8:11), digits = 3)
    }

    if (input$table == "Resampled"){
      req(common$covs_summary$resampled)
      out <- DT::datatable(common$covs_summary$resampled, selection = "none", autoHideNavigation = TRUE,
                           options = list(pageLength = 11,
                                          columnDefs = list(list(className = 'dt-center', targets = 0:(length(common$covs)+1))))) %>%
        DT::formatSignif(columns = 1:(length(common$covs)+1), rows = c(1:4, 8:11), digits = 3)
    }

    out
  }, server = FALSE)

  return(list(
    save = function() {
list(remove = input$remove)
    },
    load = function(state) {
shinyWidgets::updateMaterialSwitch(session, "remove", value = state$remove)
    }
  ))
})
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

