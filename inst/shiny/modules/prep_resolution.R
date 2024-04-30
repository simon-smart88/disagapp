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

prep_resolution_module_server <- function(id, common, parent_session) {
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
      choices <- round(original_resolution$width * factors, 0)
      selectInput(session$ns("resolution"), "New pixel width (m)", choices = choices)
    })


    output$plot <- renderPlot({
      req(common$covs_prep)
      if (is.null(common$covs_prep_lores)){
        pixels_per_poly <- terra::extract(common$covs_prep, common$shape) %>%
          dplyr::group_by(ID) %>%
          dplyr::summarise(n_pixels = dplyr::n())
      } else {
        pixels_per_poly <- terra::extract(common$covs_prep_lores, common$shape) %>%
          dplyr::group_by(ID) %>%
          dplyr::summarise(n_pixels = dplyr::n())
      }

      if (input$plot_type == "Histogram"){
        plot <- list(graphics::hist(pixels_per_poly$n_pixels, main = '', xlab = "Pixels per polygon"))
      }

      if (input$plot_type == "Boxplot"){
        plot <- list(graphics::boxplot(pixels_per_poly$n_pixels, main = '', ylab = "Pixels per polygon"))
      }

      leg <- legend("topright",  bty="n", legend = c(
                      glue::glue("Original average pixels per polygon = {round(mean(pixels_per_poly$n_pixels),2)}"),
                      glue::glue("Original pixel width (m) = {round(original_resolution$width,0)}"),
                      glue::glue("Original pixel height (m) = {round(original_resolution$height,0)}"))
                      )

      plot <- append(plot, leg)

      return(plot)
    }) %>% bindEvent(input$summarise)

    observeEvent(input$run, {
      # WARNING ####
      if (is.null(common$covs_prep)) {
        common$logger %>% writeLog(type = "error", "Please resample the rasters first")
        return()
      }

      factor <- as.integer(as.numeric(input$resolution) / original_resolution$width)

      # FUNCTION CALL ####
      common$covs_prep_lores <- terra::aggregate(common$covs_prep, fact = factor, fun = "mean", na.rm = TRUE)
      common$agg_prep_lores <- terra::aggregate(common$agg_prep, fact = factor, fun = "sum", na.rm = TRUE)

      common$logger %>% writeLog("Low resolution covariates have been created")
      common$meta$prep_resolution$used <- TRUE
      common$meta$prep_resolution$factor <- factor

      gargoyle::trigger("prep_resolution")
  })

  return(list(
    save = function() {
list(plot_type = input$plot_type,
resolution = input$resolution)
    },
    load = function(state) {
updateSelectInput(session, "plot_type", selected = state$plot_type)
updateSelectInput(session, "resolution", selected = state$resolution)
    }
  ))
})
}

prep_resolution_module_result <- function(id) {
  ns <- shiny::NS(id)
plotOutput(ns("plot"))
}

prep_resolution_module_rmd <- function(common) {
  # Variables used in the module's Rmd code
  list(
    prep_resolution_knit = !is.null(common$meta$prep_resolution$used),
    prep_resolution_factor = common$meta$prep_resolution$factor,
    prep_resolution_plot_type = common$meta$prep_resolution$plot_type
  )
}

