fit_fit_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    radioButtons(ns("family"), "Model family", c("Gaussian" = "gaussian", "Poisson" = "poisson", "Binomial" = "binomial"), selected = "poisson"),
    radioButtons(ns("link"), "Model link", c("Logit" = "logit", "Log" = "log", "Identity" = "identity"), selected = "log"),
    shinyWidgets::materialSwitch(ns("field"), "Include spatial field", value = TRUE, status = "success"),
    shinyWidgets::materialSwitch(ns("iid"), "Include IID", value = TRUE, status = "success"),
    shinyWidgets::materialSwitch(ns("priors"), "Set priors", value = FALSE, status = "success"),
    conditionalPanel("input.priors === true", uiOutput(ns("priors_out")), ns = ns),
    conditionalPanel("input.priors === true && input.field === true", uiOutput(ns("field_out")), ns = ns),
    conditionalPanel("input.priors === true && input.iid === true", uiOutput(ns("iid_out")), ns = ns),
    actionButton(ns("run"), "Fit model")
  )
}

fit_fit_module_server <- function(id, common, parent_session, map) {
  moduleServer(id, function(input, output, session) {

    output$priors_out <- renderUI({
      gargoyle::watch("prep_final")
      validate(need(common$prep, "You need to prepare the data before setting priors"))

      agg_name <- names(common$agg_prep)
      pop_per_poly <- terra::extract(common$agg_prep, common$shape, fun = "sum", na.rm = T)
      poly_summary <- cbind(pop_per_poly, common$prep$polygon_data)
      poly_summary$rate <-  poly_summary$response / poly_summary[[agg_name]]
      mean_intercept <- log((sum(poly_summary$response)/sum(poly_summary[[agg_name]])))
      min_intercept <- log(min(poly_summary$rate))
      max_intercept <- log(max(poly_summary$rate))
      sd_intercept <- max(c((abs(min_intercept) - abs(mean_intercept)), (abs(max_intercept) - abs(mean_intercept))))

      tagList(
        numericInput(session$ns("mean_intercept"), "Mean intercept", value = round(mean_intercept, 1), step = 0.1),
        numericInput(session$ns("sd_intercept"), "Intercept standard deviation", value = round(sd_intercept, 1), step = 0.1),
        numericInput(session$ns("mean_slope"), "Mean slope", value = 0, step = 0.1),
        numericInput(session$ns("sd_slope"), "Slope standard deviation", value = 10, step = 0.1)
      )
    })

    output$field_out <- renderUI({
      gargoyle::watch("prep_final")
      req(common$prep)

      limits <- sf::st_bbox(common$shape)
      hypotenuse <- as.numeric(sqrt((limits$xmax - limits$xmin)^2 + (limits$ymax - limits$ymin)^2))
      rho_min <- hypotenuse / 3
      sigma_max <- sd(common$prep$polygon_data$response/mean(common$prep$polygon_data$response))

      tagList(
        numericInput(session$ns("rho_min"), "Minimum rho", value = round(rho_min, 1), step = 0.1),
        numericInput(session$ns("rho_prob"), "Rho probability", value = 0.01, step = 0.01),
        numericInput(session$ns("sigma_max"), "Maximum sigma", value = round(sigma_max, 1), step = 0.1),
        numericInput(session$ns("sigma_prob"), "Sigma probability", value = 0.01, step = 0.01)
      )
    })

    output$iid_out <- renderUI({
      gargoyle::watch("prep_final")
      req(common$prep)
      tagList(
        numericInput(session$ns("iideffect_max"), "Maximum IID effect", value = 1, step = 0.1),
        numericInput(session$ns("iideffect_prob"), "IID effect probability", value = 0.01, step = 0.01)
      )
    })

    priors <- reactive({
      if (input$priors){
      out <- list(
         priormean_intercept = input$mean_intercept,
         priorsd_intercept = input$sd_intercept,
         priormean_slope = input$mean_slope,
         priorsd_slope = input$sd_slope,
         prior_rho_min = input$rho_min,
         prior_rho_prob = input$rho_prob,
         prior_sigma_max = input$sigma_max,
         prior_sigma_prob = input$sigma_prob,
         prior_iideffect_sd_max = input$iideffect_max,
         prior_iideffect_sd_prob = input$iideffect_prob)
      } else {
        out <- NULL
      }
      out
    })



  observeEvent(input$run, {
    # WARNING ####
    if (is.null(common$prep)) {
      common$logger %>% writeLog(type = "error", "Please prepare the data first")
      return()
    }

    # FUNCTION CALL ####
    show_loading_modal("Please wait while the model is fitted")
    common$fit <- tryCatch({disaggregation::disag_model(data = common$prep,
                                          priors = priors(),
                                          family = input$family,
                                          link = input$link,
                                          field = input$field,
                                          iid = input$iid)},
    error = function(x){ common$logger %>% writeLog(type = "error", paste0("An error occurred whilst fitting the model: ", x))})

    if (!is.null(common$fit)){
      common$logger %>% writeLog("Model fitting has completed")
    }

    close_loading_modal()

    # LOAD INTO COMMON ####

    # METADATA ####
    common$meta$fit_fit$family <- input$family
    common$meta$fit_fit$link <- input$link
    common$meta$fit_fit$iid <- input$iid
    common$meta$fit_fit$mean_intercept <- input$mean_intercept
    common$meta$fit_fit$sd_intercept <- input$sd_intercept
    common$meta$fit_fit$mean_slope <- input$mean_slope
    common$meta$fit_fit$sd_slope <- input$sd_slope
    common$meta$fit_fit$rho_min <- input$rho_min
    common$meta$fit_fit$rho_prob <- input$rho_prob
    common$meta$fit_fit$sigma_max <- input$sigma_max
    common$meta$fit_fit$sigma_prob <- input$sigma_prob
    common$meta$fit_fit$iideffect_sd_max <- input$iideffect_max
    common$meta$fit_fit$iideffect_sd_prob <- input$iideffect_prob
    # TRIGGER
    gargoyle::trigger("fit_fit")
  })

    output$model_plot <- renderPlot({
      gargoyle::watch("fit_fit")
      req(common$fit)
      plot(common$fit)
    })

  return(list(
    save = function() {
list(mean_intercept = input$mean_intercept, 
sd_intercept = input$sd_intercept, 
mean_slope = input$mean_slope, 
sd_slope = input$sd_slope, 
rho_min = input$rho_min, 
rho_prob = input$rho_prob, 
sigma_max = input$sigma_max, 
sigma_prob = input$sigma_prob, 
iideffect_max = input$iideffect_max, 
iideffect_prob = input$iideffect_prob, 
family = input$family, 
link = input$link, 
field = input$field, 
iid = input$iid, 
priors = input$priors)
    },
    load = function(state) {
updateNumericInput(session, "mean_intercept", value = state$mean_intercept) 
updateNumericInput(session, "sd_intercept", value = state$sd_intercept) 
updateNumericInput(session, "mean_slope", value = state$mean_slope) 
updateNumericInput(session, "sd_slope", value = state$sd_slope) 
updateNumericInput(session, "rho_min", value = state$rho_min) 
updateNumericInput(session, "rho_prob", value = state$rho_prob) 
updateNumericInput(session, "sigma_max", value = state$sigma_max) 
updateNumericInput(session, "sigma_prob", value = state$sigma_prob) 
updateNumericInput(session, "iideffect_max", value = state$iideffect_max) 
updateNumericInput(session, "iideffect_prob", value = state$iideffect_prob) 
updateRadioButtons(session, "family", selected = state$family) 
updateRadioButtons(session, "link", selected = state$link) 
shinyWidgets::updateMaterialSwitch(session, "field", value = state$field) 
shinyWidgets::updateMaterialSwitch(session, "iid", value = state$iid) 
shinyWidgets::updateMaterialSwitch(session, "priors", value = state$priors)
    }
  ))
})
}

fit_fit_module_result <- function(id) {
  ns <- NS(id)
  plotOutput(ns("model_plot"))
}


fit_fit_module_rmd <- function(common) {
  # Variables used in the module's Rmd code
  list(
    fit_knit = !is.null(common$fit),
    fit_family = common$meta$fit_fit$family,
    fit_link = common$meta$fit_fit$link,
    fit_iid = common$meta$fit_fit$iid,
    fit_mean_intercept = common$meta$fit_fit$mean_intercept,
    fit_sd_intercept = common$meta$fit_fit$sd_intercept,
    fit_mean_slope = common$meta$fit_fit$mean_slope,
    fit_sd_slope = common$meta$fit_fit$sd_slope,
    fit_rho_min = common$meta$fit_fit$rho_min,
    fit_rho_prob = common$meta$fit_fit$rho_prob,
    fit_sigma_max = common$meta$fit_fit$sigma_max,
    fit_sigma_prob = common$meta$fit_fit$sigma_prob,
    fit_iid_sd_max = common$meta$fit_fit$iideffect_sd_max,
    fit_iid_sd_prob = common$meta$fit_fit$iideffect_sd_prob
  )
}

