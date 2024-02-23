prep_summary_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    # UI
    actionButton(ns("run"), "Prepare covariate summary"),
    checkboxInput(ns("remove"), "Remove identical columns?", FALSE),
    shinyWidgets::radioGroupButtons(ns("table"), label = "Choose table", choices = c("Original", "Resampled"), justified = TRUE),
    actionButton(ns("resample"), "Resample covariates"),
    numericInput(ns("resolution_factor"), "Decrease resolution factor", value = 1, min = 1, max = 10, step = 1),
    actionButton(ns("decrease_resolution"), "Decrease resolution"),
    br(),br(),
    actionButton(ns("scale"), "Scale covariates"),
  )
}

prep_summary_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {


  observeEvent(input$run, {
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
    updateTabsetPanel(parent_session, "main", selected = "Results")
  })

    observeEvent(input$resample, {
      # WARNING ####
      if (is.null(input$cov_table_rows_selected)) {
        common$logger %>% writeLog(type = "error", "Please select a row from the table")
        return()
      }
      # FUNCTION CALL ####
      common$covs$Aggregation <- common$agg
      common$covs_prep <- lapply(common$covs, terra::resample, common$covs[[input$cov_table_rows_selected]])
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
      common$meta$prep_summary$resample_target <- rownames(common$covs_summary$resampled)[[input$cov_table_rows_selected]]
      # TRIGGER
      gargoyle::trigger("prep_summary")
      updateTabsetPanel(parent_session, "main", selected = "Results")
      shinyWidgets::updateRadioGroupButtons(session, "table", selected = "Resampled")
    })

    observeEvent(input$decrease_resolution, {
      # WARNING ####
      if (is.null(common$covs_prep)) {
        common$logger %>% writeLog(type = "error", "Please resample the rasters first")
        return()
      }

      common$covs_prep <- terra::aggregate(common$covs_prep, fact = input$resolution_factor, fun = "mean")
      common$agg_prep <- terra::aggregate(common$agg_prep, fact = input$resolution_factor, fun = "sum")
      common$logger %>% writeLog("Covariates have been scaled")
      common$meta$prep_summary$decrease_resolution <- TRUE
      common$meta$prep_summary$resolution_factor <- input$resolution_factor
      common$covs$Aggregation <- common$agg_prep
      common$covs_summary$resampled <- prep_summary(common$covs_prep, remove = FALSE)
      common$covs$Aggregation <- NULL
      gargoyle::trigger("prep_summary")
    })

    observeEvent(input$scale, {
      # WARNING ####
      if (is.null(common$covs_prep)) {
        common$logger %>% writeLog(type = "error", "Please resample the rasters first")
        return()
      }
      common$covs_prep <- terra::scale(common$covs_prep, scale = TRUE, center = TRUE)
      common$logger %>% writeLog("Covariates have been scaled")
      common$meta$prep_summary$scale <- TRUE
    })

    output$cov_table <- DT::renderDataTable({
    gargoyle::watch("prep_summary")
    if (input$table == "Original"){
      req(common$covs_summary$original)
      out <- DT::datatable(common$covs_summary$original, selection = "single")
    }

    if (input$table == "Resampled"){
      req(common$covs_summary$resampled)
      out <- DT::datatable(common$covs_summary$resampled, selection = "none")
    }

    out
  })

  output$hist <- renderPlot({
    gargoyle::watch("prep_summary")
    req(common$covs_prep)
    pixels_per_poly <- terra::extract(common$covs_prep, common$shape) %>%
      dplyr::group_by(ID) %>%
      dplyr::summarise(n_pixels = dplyr::n())

    graphics::hist(pixels_per_poly$n_pixels, main = '', xlab = "Pixels per polygon")
    legend("topright",  bty='n', legend = glue::glue("Mean = {round(mean(pixels_per_poly$n_pixels),2)}"))
  })

  return(list(
    save = function() {
list(remove = input$remove)
    },
    load = function(state) {
updateCheckboxInput(session, "remove", value = state$remove)
    }
  ))
})
}

prep_summary_module_result <- function(id) {
  ns <- NS(id)
  tagList(
  DT::dataTableOutput(ns("cov_table")),
  plotOutput(ns("hist"))
  )
}

prep_summary_module_rmd <- function(common) {
  # Variables used in the module's Rmd code
  list(
    prep_summary_knit = !is.null(common$meta$prep_summary$used),
    prep_summary_resample_target = common$meta$prep_summary$resample_target,
    prep_summary_scale = !is.null(common$meta$prep_summary$scale)
  )
}

