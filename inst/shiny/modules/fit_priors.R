fit_priors_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    numericInput(ns("mean_intercept"), "Mean intercept", 0),
    numericInput(ns("sd_intercept"), "Intercept standard deviation", 0),
    numericInput(ns("mean_slope"), "Mean slope", 0),
    numericInput(ns("sd_slope"), "Slope standard deviation", 0),
    numericInput(ns("rho_min"), "Minimum rho", 0),
    numericInput(ns("rho_prob"), "Rho probability", 0.01),
    numericInput(ns("sigma_max"), "Maximum sigma", 0),
    numericInput(ns("sigma_prob"), "Sigma probability", 0.01),
    numericInput(ns("iideffect_max"), "Maximum IID effect", 1),
    numericInput(ns("iideffect_prob"), "IID effect probability", 0.01),
    actionButton(ns("run"), "Save priors")
  )
}

fit_priors_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {

    gargoyle::on("prep_final", {
      limits <- sf::st_bbox(common$shape)
      hypotenuse <- sqrt((limits$xmax - limits$xmin)^2 + (limits$ymax - limits$ymin)^2)
      rho_min <- hypotenuse/3
      sigma_max <- sd(common$prep$polygon_data$response/mean(common$prep$polygon_data$response))
      mean_intercept <- as.integer(log(sum(common$prep$polygon_data$response) / sum(terra::values(common$agg))))
      sd_intercept <- mean_intercept * 0.25

      # check how we can estimate this one
      sd_slope <- 3

      updateNumericInput(session, "mean_intercept", value = mean_intercept)
      updateNumericInput(session, "sd_intercept", value = sd_intercept)
      updateNumericInput(session, "sd_slope", value = sd_slope)
      updateNumericInput(session, "rho_min", value = rho_min)
      updateNumericInput(session, "sigma_max", value = sigma_max)
    })

  observeEvent(input$run, {
    # WARNING ####

    # FUNCTION CALL ####

    common$priors <- list(priormean_intercept = input$mean_intercept,
                          priorsd_intercept = input$sd_intercept,
                          priormean_slope = input$mean_slope,
                          priorsd_slope = input$sd_slope,
                          prior_rho_min = input$rho_min,
                          prior_rho_prob = input$rho_prob,
                          prior_sigma_max = input$sigma_max,
                          prior_sigma_prob = input$sigma_prob,
                          prior_iideffect_sd_max = input$iideffect_max,
                          prior_iideffect_sd_prob = input$iideffect_prob)

    # METADATA ####

    # TRIGGER
    gargoyle::trigger("fit_priors")
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

fit_priors_module_rmd <- function(common) {
  # Variables used in the module's Rmd code
  list(
    fit_priors_knit = !is.null(common$some_object),
    var1 = common$meta$setting1,
    var2 = common$meta$setting2
  )
}

