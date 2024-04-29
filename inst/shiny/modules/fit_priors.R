fit_priors_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    numericInput(ns("mean_intercept"), "Mean intercept", 0),
    numericInput(ns("sd_intercept"), "Intercept standard deviation", 0),
    numericInput(ns("mean_slope"), "Mean slope", 0),
    numericInput(ns("sd_slope"), "Slope standard deviation", 0),
    numericInput(ns("rho_min"), "Minimum rho", 0),
    numericInput(ns("rho_prob"), "Rho probability", value = 0.01, step = 0.01),
    numericInput(ns("sigma_max"), "Maximum sigma", 0),
    numericInput(ns("sigma_prob"), "Sigma probability", value = 0.01, step = 0.01),
    numericInput(ns("iideffect_max"), "Maximum IID effect", value = 1, step = 0.1),
    numericInput(ns("iideffect_prob"), "IID effect probability", value = 0.01, step = 0.01),
    actionButton(ns("run"), "Save priors")
  )
}

#rho and sigma only if field
#iid only if iid

fit_priors_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {

    gargoyle::on("prep_final", {
      limits <- sf::st_bbox(common$shape)
      hypotenuse <- as.numeric(sqrt((limits$xmax - limits$xmin)^2 + (limits$ymax - limits$ymin)^2))
      rho_min <- hypotenuse / 3
      sigma_max <- sd(common$prep$polygon_data$response/mean(common$prep$polygon_data$response))
      resp_sum <- sum(common$prep$polygon_data$response, na.rm = TRUE)
      agg_sum <- sum(terra::values(common$agg), na.rm = TRUE)
      mean_intercept <- log(resp_sum / agg_sum)

      # check how we can estimate these
      sd_intercept <- mean_intercept * 0.25
      # captures range in min and max for cases/polygon and err on the side of wider
      # 1 in 100 to 1 in 1,000,000

      sd_slope <- 3
      #10 - gives lots of flexibility

      #max_iid ?
      # would be higher for infectious or where the variation is very high (more random effects)

      updateNumericInput(session, "mean_intercept", value = round(mean_intercept, 1), step = 0.1)
      updateNumericInput(session, "sd_intercept", value = round(sd_intercept, 1), step = 0.1)
      updateNumericInput(session, "sd_slope", value = round(sd_slope, 1), step = 0.1)
      updateNumericInput(session, "rho_min", value = round(rho_min, 1), step = 0.1)
      updateNumericInput(session, "sigma_max", value = round(sigma_max, 1), step = 0.1)
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

