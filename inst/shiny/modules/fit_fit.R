fit_fit_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    radioButtons(ns("family"), "Model family", c("Gaussian" = "gaussian", "Poisson" = "poisson", "Binomial" = "binomial"), selected = "poisson"),
    radioButtons(ns("link"), "Model link", c("Logit" = "logit", "Log" = "log", "Identity" = "identity"), selected = "log"),
    numericInput(ns("iterations"), "Number of iterations", 100, step = 1),
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
         priormean_intercept = as.numeric(input$mean_intercept),
         priorsd_intercept = as.numeric(input$sd_intercept),
         priormean_slope = as.numeric(input$mean_slope),
         priorsd_slope = as.numeric(input$sd_slope),
         prior_rho_min = as.numeric(input$rho_min),
         prior_rho_prob = as.numeric(input$rho_prob),
         prior_sigma_max = as.numeric(input$sigma_max),
         prior_sigma_prob = as.numeric(input$sigma_prob),
         prior_iideffect_sd_max = as.numeric(input$iideffect_max),
         prior_iideffect_sd_prob = as.numeric(input$iideffect_prob))
      } else {
        out <- NULL
      }
      out
    })



  observeEvent(input$run, {
    # WARNING ####
    if (is.null(common$prep)) {
      common$logger |> writeLog(type = "error", "Please prepare the data first")
      return()
    }

    # FUNCTION CALL ####
    show_loading_modal("Please wait while the model is fitted")
    common$fit <- tryCatch({disaggregation::disag_model(data = common$prep,
                                          priors = priors(),
                                          family = input$family,
                                          link = input$link,
                                          iterations = as.numeric(input$iterations),
                                          field = input$field,
                                          iid = input$iid)},
    error = function(x){ common$logger |> writeLog(type = "error", paste0("An error occurred whilst fitting the model: ", x))},
    warning = function(x){ common$logger |> writeLog(type = "warning", paste0("An error occurred whilst fitting the model: ", x))}
    )

    if (!is.null(common$fit)){
      common$logger |> writeLog(type = "complete", "Model fitting has completed")
    }

    close_loading_modal()

    # LOAD INTO COMMON ####

    # METADATA ####
    common$meta$fit_fit$family <- input$family
    common$meta$fit_fit$link <- input$link
    common$meta$fit_fit$iterations <- as.numeric(input$iterations)
    common$meta$fit_fit$field <- input$field
    common$meta$fit_fit$iid <- input$iid
    common$meta$fit_fit$priors <- input$priors
    common$meta$fit_fit$mean_intercept <- as.numeric(input$mean_intercept)
    common$meta$fit_fit$sd_intercept <- as.numeric(input$sd_intercept)
    common$meta$fit_fit$mean_slope <- as.numeric(input$mean_slope)
    common$meta$fit_fit$sd_slope <- as.numeric(input$sd_slope)
    common$meta$fit_fit$rho_min <- as.numeric(input$rho_min)
    common$meta$fit_fit$rho_prob <- as.numeric(input$rho_prob)
    common$meta$fit_fit$sigma_max <- as.numeric(input$sigma_max)
    common$meta$fit_fit$sigma_prob <- as.numeric(input$sigma_prob)
    common$meta$fit_fit$iideffect_sd_max <- as.numeric(input$iideffect_max)
    common$meta$fit_fit$iideffect_sd_prob <- as.numeric(input$iideffect_prob)
    # TRIGGER
    gargoyle::trigger("fit_fit")
    show_results(parent_session)
  })


    output$model_plot <- plotly::renderPlotly({
      gargoyle::watch("fit_fit")
      req(common$fit$sd_out)

      parameter <- sd <- obs <- pred <- NULL
      posteriors <- as.data.frame(summary(common$fit$sd_out, select = 'fixed'))
      posteriors <- dplyr::mutate(posteriors, name = rownames(posteriors))
      names(posteriors) <- c('mean', 'sd', 'parameter')
      posteriors$fixed <- grepl('slope', posteriors$parameter)
      posteriors$type <- ifelse(posteriors$fixed, 'Slope', 'Other')

      # Check name lengths match before substituting.
      lengths_match <- terra::nlyr(common$fit$data$covariate_rasters) == sum(posteriors$fixed)
      if(lengths_match){
        posteriors$parameter[grepl('slope', posteriors$parameter)] <- names(common$fit$data$covariate_rasters)
      }

      # Unique types for faceting
      unique_types <- unique(posteriors$type)

      # Create subplots for each type
      plots <- lapply(unique_types, function(type) {
        subset_data <- posteriors[posteriors$type == type, ]

        plotly::plot_ly(subset_data,
                y = ~parameter,
                x = ~mean,
                type = 'scatter',
                mode = 'markers',
                marker = list(color = 'black'),
                error_x = list(array = ~sd, color = "blue")) |>
          plotly::layout(title = list(text = type, x = 0.5),
                 xaxis = list(title = 'SD', showline = TRUE, zeroline = FALSE),
                 yaxis = list(title = 'Parameter', showline = TRUE, zeroline = FALSE,
                              range = c(-1, nrow(subset_data))),
                 margin = list(t = 100))
      })

      # Combine subplots into a single plot
      final_plot <- plotly::subplot(plots, nrows = 1, shareX = FALSE, margin = 0.05) |>
        plotly::layout(title = "Model parameters (excluding random effects)",
               showlegend = FALSE)

      final_plot

    })


    output$obs_pred_plot <- plotly::renderPlotly({
      gargoyle::watch("fit_fit")
      req(common$fit)
      report <- common$fit$obj$report()

      # Form of the observed and predicted results depends on the likelihood function used
      if( common$fit$model_setup$family == 'gaussian') {
        observed_data = report$polygon_response_data/report$reportnormalisation
        predicted_data = report$reportprediction_rate
        title <- 'In sample performance: incidence rate'
      } else if( common$fit$model_setup$family == 'binomial') {
        observed_data =  common$fit$data$polygon_data$response / common$fit$data$polygon_data$N
        predicted_data = report$reportprediction_rate
        title <- 'In sample performance: prevalence rate'
      } else if( common$fit$model_setup$family == 'poisson') {
        observed_data = report$polygon_response_data/report$reportnormalisation
        predicted_data = report$reportprediction_rate
        title <- 'In sample performance: incidence rate'
      }

      data <- data.frame(obs = observed_data, pred = predicted_data)

      title <- "Observed vs Predicted"

      # Define range for the identity line
      x_range <- range(data$obs, data$pred)
      identity_line <- data.frame(x = x_range, y = x_range)

      # Create scatter plot and add identity line
      obspred_plot <- plotly::plot_ly(data, x = ~obs, y = ~pred, type = 'scatter', mode = 'markers') |>
        plotly::add_lines(data = identity_line, x = ~x, y = ~y, line = list(color = 'blue')) |>
        plotly::layout(title = list(text = title, x = 0.5),
               xaxis = list(title = 'Observed', showline = TRUE, zeroline = FALSE),
               yaxis = list(title = 'Predicted', showline = TRUE, zeroline = FALSE),
               margin = list(t = 100),
               showlegend = FALSE)

      # Display the plot
      obspred_plot
    })

  return(list(
    save = function() {list(
      ### Manual save start
      ### Manual save end
      iterations = input$iterations, 
      mean_intercept = input$mean_intercept, 
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
      ### Manual load start
      ### Manual load end
      updateNumericInput(session, "iterations", value = state$iterations) 
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
  tagList(
  plotly::plotlyOutput(ns("model_plot")),
  plotly::plotlyOutput(ns("obs_pred_plot"))
  )
}

fit_fit_module_rmd <- function(common) {
  list(
    fit_knit = !is.null(common$fit),
    fit_family = common$meta$fit_fit$family,
    fit_link = common$meta$fit_fit$link,
    fit_iterations = common$meta$fit_fit$iterations,
    fit_field = common$meta$fit_fit$field,
    fit_iid = common$meta$fit_fit$iid,
    fit_priors_knit = !is.null(common$meta$fit_fit$priors),
    fit_priors = common$meta$fit_fit$priors,
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

