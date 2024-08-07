prep_resolution_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    # UI
    selectInput(ns("plot_type"), "Plot type", choices = c("Histogram", "Boxplot")),
    actionButton(ns("summarise"), "Summarise data"),
    uiOutput(ns("resolution_out")),
    actionButton(ns("run"), "Decrease resolution"),
  )
}

prep_resolution_module_server <- function(id, common, parent_session, map) {
  moduleServer(id, function(input, output, session) {

    original_resolution <- reactiveValues()

    gargoyle::init("prep_resolution_current")
    gargoyle::on("prep_summary", {
      req(common$covs_summary$resampled)
      #calculate coordinates of top left pixel using minimum and resolution rows
      lng1 <- as.numeric(common$covs_summary$resampled[8, 1])
      lng2 <- as.numeric(common$covs_summary$resampled[8, 1]) + as.numeric(common$covs_summary$resampled[1, 1])
      lat1 <- as.numeric(common$covs_summary$resampled[10, 1])
      lat2 <- as.numeric(common$covs_summary$resampled[10, 1]) + as.numeric(common$covs_summary$resampled[2, 1])

      top_left <- c(lng1, lat1)
      top_right <- c(lng2, lat1)
      bottom_left <- c(lng1, lat2)

      original_resolution$width <- geosphere::distm(top_left, top_right, fun = geosphere::distHaversine)
      original_resolution$height <- geosphere::distm(top_left, bottom_left, fun = geosphere::distHaversine)
      gargoyle::trigger("prep_resolution_current")
    })


    output$resolution_out <- renderUI({
      gargoyle::watch("prep_resolution_current")
      req(original_resolution$width)

      factors <- 2:20
      choices <- round(as.numeric(original_resolution$width) * factors, 0)
      selectInput(session$ns("resolution"), "New cell width (m)", choices = choices)
    })

    observeEvent(input$summarise, {
      show_results(parent_session)
    })


    output$plot <- plotly::renderPlotly({
      req(common$covs_prep)
      if (is.null(common$covs_prep_lores)){
        pixels_per_poly <- terra::extract(common$covs_prep, common$shape) |>
          dplyr::group_by(ID) |>
          dplyr::summarise(n_pixels = dplyr::n())
      } else {
        pixels_per_poly <- terra::extract(common$covs_prep_lores, common$shape) |>
          dplyr::group_by(ID) |>
          dplyr::summarise(n_pixels = dplyr::n())
      }

      if (input$plot_type == "Histogram"){
        plot <- plotly::plot_ly( x = pixels_per_poly$n_pixels,
                                 type = "histogram",
                                 histnorm = "frequency",
                                 marker = list(color = '#0072B2'),
                                 stroke = list(color = 'black')) |>
          plotly::layout(xaxis = list(title = "Cells per polygon"),
                         yaxis = list(title = "Frequency"))
      }

      if (input$plot_type == "Boxplot"){
        plot <- plotly::plot_ly(x = pixels_per_poly$n_pixels,
                                type = "box",
                                fillcolor = "#0072B2",
                                line = list(color = "black"),
                                marker = list(color = "black"),
                                name = "") |>
          plotly::layout(xaxis = list(title = "Cells per polygon", showline = TRUE, zeroline = FALSE),
                         yaxis = list(title = "", showline = TRUE, zeroline = FALSE))
      }

      plot <- plot |>
        plotly::layout(
          annotations = list(
            list(
              x = 1, y = 0.95, xref = "paper", yref = "paper",
              text = glue("Original avg cells per polygon = {round(mean(pixels_per_poly$n_pixels), 2)}"),
              font = list(size = 16, color = "black"),
              showarrow = FALSE
            ),
            list(
              x = 1, y = 0.88, xref = "paper", yref = "paper",
              text = glue("Original cell width (m) = {round(original_resolution$width, 0)}"),
              font = list(size = 16, color = "black"),
              showarrow = FALSE
            ),
            list(
              x = 1, y = 0.81, xref = "paper", yref = "paper",
              text = glue("Original cell height (m) = {round(original_resolution$height, 0)}"),
              font = list(size = 16, color = "black"),
              showarrow = FALSE
            )
          )
        )
      return(plot)
    }) |> bindEvent(input$summarise)

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
      gargoyle::trigger("prep_resolution")
      # exceptionally, the mapping for this module is handled in core_mapping
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
  ns <- shiny::NS(id)
plotly::plotlyOutput(ns("plot"))
}

prep_resolution_module_rmd <- function(common) {
  # Variables used in the module's Rmd code
  list(
    prep_resolution_knit = !is.null(common$meta$prep_resolution$used),
    prep_resolution_factor = common$meta$prep_resolution$factor,
    prep_resolution_plot_type = common$meta$prep_resolution$plot_type
  )
}

