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

    current_resolution <- reactive({
      gargoyle::watch("prep_summary")
      req(common$covs_summary$resampled)
      lng1 <- common$covs_summary$resampled$`X minimum`
      lng2 <- common$covs_summary$resampled$`X minimum` + common$covs_summary$resampled$`X resolution`
      lat1 <- common$covs_summary$resampled$`Y minimum`
      lat2 <- common$covs_summary$resampled$`Y minimum` + common$covs_summary$resampled$`Y resolution`

      top_left <- c(lng1, lat1)
      top_right <- c(lng2, lat1)
      bottom_left <- c(lng1, lat2)

      width <- geosphere::distm(top_left, top_right, fun = geosphere::distHaversine)
      height <- geosphere::distm(top_left, bottom_left, fun = geosphere::distHaversine)
      print(c(width,height))
      return(list(width = width, height = height))
    })

    output$resolution_out <- renderUI({
      req(common$covs_prep)
      req(current_resolution())

      factors <- (1:20)^2

      choices <- current_resolution()$width/factors

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
      plot <- list(graphics::hist(pixels_per_poly$n_pixels, main = '', xlab = "Pixels per polygon"),
                   legend("topright",  bty='n', legend = glue::glue("Mean = {round(mean(pixels_per_poly$n_pixels),2)}")))
      }

      if (input$plot_type == "Boxplot"){
        plot <- list(graphics::boxplot(pixels_per_poly$n_pixels, main = '', ylab = "Pixels per polygon"),
                     legend("topright",  bty='n', legend = glue::glue("Mean = {round(mean(pixels_per_poly$n_pixels),2)}")))
      }

      return(plot)
    }) %>% bindEvent(input$summarise)


    observe({
      req(input$resolution)
      print(input$resolution / current_resolution()$width)
    })

    observeEvent(input$run, {
      # WARNING ####
      if (is.null(common$covs_prep)) {
        common$logger %>% writeLog(type = "error", "Please resample the rasters first")
        return()
      }
      common$covs_prep_lores <- terra::aggregate(common$covs_prep, fact = input$factor, fun = "mean")
      common$agg_prep_lores <- terra::aggregate(common$agg_prep, fact = input$factor, fun = "sum")
      common$logger %>% writeLog("Low resolution covariates have been created")
      common$meta$prep_resolution$used <- TRUE
      common$meta$prep_resolution$factor <- input$factor
      shinyjs::click("summarise")
  })

  return(list(
    save = function() {
      # Save any values that should be saved when the current session is saved
    },
    load = function(state) {
      # Load
    }
  ))
})
}

prep_resolution_module_result <- function(id) {
  ns <- shiny::NS(id)
plotOutput(ns("plot"))
}

# prep_resolution_module_map <- function(map, common) {
#   # Map logic
# }

prep_resolution_module_rmd <- function(common) {
  # Variables used in the module's Rmd code
  list(
    prep_resolution_knit = !is.null(common$meta$prep_resolution$used),
    prep_resolution_factor = common$meta$prep_resolution$factor,
    prep_resolution_plot_type = common$meta$prep_resolution$plot_type
  )
}

