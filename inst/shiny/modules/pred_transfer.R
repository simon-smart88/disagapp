pred_transfer_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    uiOutput(ns("country_out")),
    uiOutput(ns("cov_out")),
    uiOutput(ns("agg_out")),
    bslib::input_task_button(ns("run"), "Transfer predictions", type = "default")
  )
}

pred_transfer_module_server <- function(id, common, parent_session, map) {
  moduleServer(id, function(input, output, session) {

    output$country_out <- renderUI({
      selectInput(session$ns("country"), "Select country", choices = c("", common$countries$boundaryName), multiple = FALSE)
    })

    output$cov_out <- renderUI({
      if (!is.null(common$meta$cov_upload$used)){
      fileInput(inputId = session$ns("cov"),
                label = "Upload covariate data",
                multiple = TRUE,
                accept = c(".tif"))
      }
    })

    output$agg_out <- renderUI({
      if (!is.null(common$meta$agg_upload$used)){
        fileInput(inputId = session$ns("agg"),
                  label = "Upload aggregation data",
                  accept = c(".tif"))
      }
    })

    common$tasks$pred_transfer <- ExtendedTask$new(function(...) {
      promises::future_promise({
        pred_transfer(...)
      }, seed = TRUE)
    }) |> bslib::bind_task_button("run")

  observeEvent(input$run, {

    if (is.null(common$fit)) {
      common$logger |> writeLog(type = "error", "Please fit a model first")
      return()
    }

    if (is.null(input$country)) {
      common$logger |> writeLog(type = "error", "Please choose a country to transfer the model to")
      return()
    }

    country_code <- common$countries$boundaryISO[common$countries$boundaryName %in% input$country]

    # FUNCTION CALL ####

    common$logger |> writeLog(type = "starting", "Starting to transfer the model to the new area - this will take a long time")

    common$fit$data$covariate_rasters <- wrap_terra(common$fit$data$covariate_rasters)

    if (is.null(common$meta$cov_upload$used) & is.null(common$meta$agg_upload$used)){
      common$tasks$pred_transfer$invoke(country_code, common$fit, common$meta, async = TRUE)
    }

    if (!is.null(common$meta$cov_upload$used) & is.null(common$meta$agg_upload$used)){
      if (is.null(input$cov)) {
        common$logger |> writeLog(type = "error", "Please upload covariates")
        return()
      }
      common$tasks$pred_transfer$invoke(country_code, common$fit, common$meta, covdf = input$cov, async = TRUE)
    }

    if (is.null(common$meta$cov_upload$used) & !is.null(common$meta$agg_upload$used)){
      if (is.null(input$agg)) {
        common$logger |> writeLog(type = "error", "Please upload an aggregation raster")
        return()
      }
      common$tasks$pred_transfer$invoke(country_code, common$fit, common$meta, aggdf = input$agg, async = TRUE)
    }

    if (!is.null(common$meta$cov_upload$used) & !is.null(common$meta$agg_upload$used)){
      if (is.null(input$cov)) {
        common$logger |> writeLog(type = "error", "Please upload covariates")
        return()
      }
      if (is.null(input$agg)) {
        common$logger |> writeLog(type = "error", "Please upload an aggregation raster")
        return()
      }
      common$tasks$pred_transfer$invoke(country_code, common$fit, common$meta, covdf = input$cov, aggdf = input$agg, async = TRUE)
    }

    common$fit$data$covariate_rasters <- unwrap_terra(common$fit$data$covariate_rasters)
    results$resume()

    # METADATA ####
    common$meta$pred_transfer$used <- TRUE
    common$meta$pred_transfer$country <- country_code
    common$meta$pred_transfer$cov <- as.vector(input$cov$name)
    common$meta$pred_transfer$agg <- as.vector(input$agg$name)
    if (common$meta$prep_final$resolution == "High resolution"){
      common$meta$pred_transfer$hires <- TRUE
      common$meta$pred_transfer$lores <- FALSE
    }
    if (common$meta$prep_final$resolution == "Low resolution"){
      common$meta$pred_transfer$hires <- FALSE
      common$meta$pred_transfer$lores <- TRUE
    }

  })

  results <- observe({
    # LOAD INTO COMMON ####

    common$transfer <- common$tasks$pred_transfer$result()
    results$suspend()
    common$transfer$agg <- unwrap_terra(common$transfer$agg)
    common$transfer$cases <- unwrap_terra(common$transfer$cases)
    common$transfer$prediction <- unwrap_terra(common$transfer$prediction)
    common$transfer$covariates <- unwrap_terra(common$transfer$covariates)

    common$logger |> writeLog(type = "complete", "The model has been transferred to the new area")
    # TRIGGER
    trigger("pred_transfer")
    do.call("pred_transfer_module_map", list(map, common))
    show_map(parent_session)
    shinyjs::runjs("Shiny.setInputValue('pred_transfer-complete', 'complete');")

  })

  return(list(
    save = function() {list(
      ### Manual save start
      ### Manual save end
      country = input$country)
    },
    load = function(state) {
      ### Manual load start
      ### Manual load end
      updateSelectInput(session, "country", selected = state$country)
    }
  ))
})
}

pred_transfer_module_map <- function(map, common) {
  ex <- as.vector(terra::ext(common$transfer$prediction))
  map |> fitBounds(lng1 = ex[[1]], lng2 = ex[[2]], lat1 = ex[[3]], lat2 = ex[[4]])
  raster_map(map, common, common$transfer$prediction, "Transferred prediction (rate)")
  raster_map(map, common, common$transfer$cases, "Transferred prediction (cases)")
  for (layer in names(common$transfer$covariates)){
    raster_map(map, common, common$transfer$covariates[[layer]], paste0(layer, " (transferred)"))
  }
}

pred_transfer_module_rmd <- function(common) {
  list(
    pred_transfer_knit = !is.null(common$meta$pred_transfer$used),
    pred_transfer_country = common$meta$pred_transfer$country,
    pred_transfer_cov = common$meta$pred_transfer$cov,
    pred_transfer_agg = common$meta$pred_transfer$agg,
    pred_transfer_hires = common$meta$pred_transfer$hires,
    pred_transfer_lores = common$meta$pred_transfer$lores,
    meta = common$meta
  )
}

