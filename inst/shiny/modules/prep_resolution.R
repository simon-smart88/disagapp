prep_resolution_module_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # UI
    selectInput(ns("plot_type"), "Plot type", choices = c("Histogram" = "histogram", "Boxplot" = "boxplot")),
    actionButton(ns("summarise"), "Summarise data"),
    uiOutput(ns("resolution_out")),
    actionButton(ns("run"), "Decrease resolution"),
  )
}

prep_resolution_module_server <- function(id, common, parent_session, map) {
  moduleServer(id, function(input, output, session) {

    original_resolution <- reactiveValues()

    init("prep_resolution_current")

    observeEvent(input$summarise, {
      # WARNING ####
      if (is.null(common$covs_prep)) {
        common$logger |> writeLog(type = "error", "Please resample the rasters first")
        return()
      }

      if (requireNamespace("geosphere", quietly = TRUE)){
        req(common$covs_summary$resampled)
        # calculate coordinates of top left pixel using minimum and resolution rows
        lng1 <- as.numeric(common$covs_summary$resampled[8, 1])
        lng2 <- as.numeric(common$covs_summary$resampled[8, 1]) + as.numeric(common$covs_summary$resampled[1, 1])
        lat1 <- as.numeric(common$covs_summary$resampled[10, 1])
        lat2 <- as.numeric(common$covs_summary$resampled[10, 1]) + as.numeric(common$covs_summary$resampled[2, 1])

        top_left <- c(lng1, lat1)
        top_right <- c(lng2, lat1)
        bottom_left <- c(lng1, lat2)

        original_resolution$width <- geosphere::distm(top_left, top_right, fun = geosphere::distHaversine)
        original_resolution$height <- geosphere::distm(top_left, bottom_left, fun = geosphere::distHaversine)
        trigger("prep_resolution_current")
        show_results(parent_session)
      } else {
        logger |> writeLog(type = "error", 'This module requires the geosphere package to be installed. Close the app, run install.packages("geosphere") and try again')
      }
    })

    output$resolution_out <- renderUI({
      watch("prep_resolution_current")
      req(original_resolution$width)

      factors <- 2:20
      choices <- round(as.numeric(original_resolution$width) * factors, 0)
      selectInput(session$ns("resolution"), "New cell width (m)", choices = choices)
    })

    output$original_plot <- plotly::renderPlotly({
      watch("prep_resolution_current")
      req(original_resolution$width)
      plot_resolution(input$plot_type, common$covs_prep, common$shape, "original", reactiveValuesToList(original_resolution))
    })

    observeEvent(input$run, {
      # WARNING ####
      if (is.null(common$covs_prep)) {
        common$logger |> writeLog(type = "error", "Please resample the rasters first")
        return()
      }

      factor <- as.integer(as.numeric(input$resolution) / original_resolution$width)

      # FUNCTION CALL ####
      common$covs_prep_lores <- terra::aggregate(common$covs_prep, fact = factor, fun = "mean", na.rm = TRUE)
      common$agg_prep_lores <- terra::aggregate(common$agg_prep, fact = factor, fun = "sum", na.rm = TRUE)

      common$logger |> writeLog(type = "complete", "Low resolution covariates have been created")
      common$meta$prep_resolution$used <- TRUE
      common$meta$prep_resolution$factor <- factor

      #TRIGGER
      trigger("prep_resolution")
      # exceptionally, the mapping for this module is handled in core_mapping
  })

    output$low_plot <- plotly::renderPlotly({
      watch("prep_resolution")
      req(common$covs_prep_lores)
      plot_resolution(input$plot_type, common$covs_prep_lores, common$shape, "low")
    })

  return(list(
    save = function() {list(
      ### Manual save start
      ### Manual save end
      plot_type = input$plot_type,
      resolution = input$resolution)
    },
    load = function(state) {
      ### Manual load start
      ### Manual load end
      updateSelectInput(session, "plot_type", selected = state$plot_type)
      updateSelectInput(session, "resolution", selected = state$resolution)
    }
  ))
})
}

prep_resolution_module_result <- function(id) {
  ns <- NS(id)
  tagList(
    plotly::plotlyOutput(ns("original_plot")),
    plotly::plotlyOutput(ns("low_plot"))
  )
}

prep_resolution_module_rmd <- function(common) {
  # Variables used in the module's Rmd code
  list(
    prep_resolution_knit = !is.null(common$meta$prep_resolution$used),
    prep_resolution_factor = common$meta$prep_resolution$factor,
    prep_resolution_plot_type = common$meta$prep_resolution$plot_type
  )
}

