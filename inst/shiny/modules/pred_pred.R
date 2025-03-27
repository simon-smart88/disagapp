pred_pred_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    uiOutput(ns("iid_out")),
    shinyWidgets::materialSwitch(ns("cases"), "Include cases?", FALSE, status = "success"),
    shinyWidgets::materialSwitch(ns("uncertain"), "Include uncertainty?", FALSE, status = "success"),
    conditionalPanel("input.uncertain === true", ns = ns,
       tags$label("Uncertainty parameters"),
       numericInput(ns("uncertain_n"), "Number of realisations", value = 100, step = 1),
       numericInput(ns("uncertain_ci"), "Credible interval", value = 0.95, step = 0.01, max = 1, min = 0)
    ),
    bslib::input_task_button(ns("run"), "Produce model predictions"),
    tags$br(),
    uiOutput(ns("dl_out"))
  )
}

pred_pred_module_server <- function(id, common, parent_session, map) {
  moduleServer(id, function(input, output, session) {

  output$iid_out <- renderUI({
    watch("fit_fit")
    req(common$fit)
    if (common$meta$fit_fit$iid){
      out <- shinyWidgets::materialSwitch(session$ns("iid"), "Include IID effect?", FALSE, status = "success")
    } else {
      out <- NULL
    }
    out
  })

  output$dl_out <- renderUI({
    watch("pred_pred")
    req(common$pred)
    downloadButton(session$ns("download"), "Download model predictions")
  })

  common$tasks$pred_pred <- ExtendedTask$new(function(...) {
    promises::future_promise({
      pred_pred(...)
    }, seed = TRUE)
  }) |> bslib::bind_task_button("run")

  observeEvent(input$run, {
    # WARNING ####
    if (is.null(common$fit)){
      common$logger |> writeLog(type = "error", "Please fit a model first")
      return()
    }

    if (is.null(input$iid)){
      predict_iid <- FALSE
    } else {
      predict_iid <- input$iid
    }

    if (is.null(common$meta$prep_final$resolution) || common$meta$prep_final$resolution == "High resolution"){
      aggregation <- "agg_prep"
    } else {
      aggregation <- "agg_prep_lores"
    }

    common$fit$data$covariate_rasters <- wrap_terra(common$fit$data$covariate_rasters)
    common[[aggregation]] <- wrap_terra(common[[aggregation]])

    common$logger |> writeLog(type = "starting", "Starting to generate predictions")

    if (!input$uncertain){
      common$tasks$pred_pred$invoke(fit = common$fit,
                                    aggregation = common[[aggregation]],
                                    cases = input$cases,
                                    predict_iid = predict_iid,
                                    async = TRUE)
    } else {
      common$tasks$pred_pred$invoke(fit = common$fit,
                                    aggregation = common[[aggregation]],
                                    cases = input$cases,
                                    predict_iid = predict_iid,
                                    uncertain = input$uncertain,
                                    N = input$uncertain_n,
                                    CI = input$uncertain_ci,
                                    async = TRUE)
    }

    common$fit$data$covariate_rasters <- unwrap_terra(common$fit$data$covariate_rasters)
    common[[aggregation]]<- unwrap_terra(common[[aggregation]])
    results$resume()

    # METADATA ####
    common$meta$pred_pred$used <- TRUE
    common$meta$pred_pred$cases <- input$cases

    if (input$uncertain){
      common$meta$pred_pred$uncertain <- input$uncertain
      common$meta$pred_pred$uncertain_n <- input$uncertain_n
      common$meta$pred_pred$uncertain_ci <- input$uncertain_ci
    }
    if (is.null(input$iid)){
      common$meta$pred_pred$iid <- FALSE
    } else {
      common$meta$pred_pred$iid <- input$iid
    }

  })

  results <- observe({
    # LOAD INTO COMMON ####

    common$pred <- common$tasks$pred_pred$result()
    results$suspend()
    common$pred$field <- unwrap_terra(common$pred$field)
    common$pred$`prediction (rate)` <- unwrap_terra(common$pred$`prediction (rate)`)
    common$pred$`prediction (cases)` <- unwrap_terra(common$pred$`prediction (cases)`)
    common$pred$covariates <- unwrap_terra(common$pred$covariates)
    common$pred$iid <- unwrap_terra(common$pred$iid)
    common$pred$uncertainty_lower <- unwrap_terra(common$pred$uncertainty_lower)
    common$pred$uncertainty_upper <- unwrap_terra(common$pred$uncertainty_upper)

    common$logger |> writeLog(type = "complete", "Model predictions are available")
    # TRIGGER
    trigger("pred_pred")
    do.call("pred_pred_module_map", list(map, common))
    show_map(parent_session)
    shinyjs::runjs("Shiny.setInputValue('pred_pred-complete', 'complete');")

  })

  output$download <- downloadHandler(
    filename = function() {
      paste0("disagapp-predictions-", Sys.Date(), ".zip")
    },
    content = function(file) {
      directory <- tempdir()
      dir.create(file.path(directory, "pred_pred"))

      lapply(names(common$pred), function(x){
        if (inherits(common$pred[[x]], "SpatRaster")){
          terra::writeRaster(common$pred[[x]], overwrite = TRUE,
                             filename = file.path(directory, "pred_pred", paste0(x, ".tif")))
        }
      })

      owd <- setwd(file.path(directory, "pred_pred"))
      on.exit(setwd(owd))

      files <- list.files(".")

      zip::zipr(zipfile = file,
                files = files,
                mode = "mirror",
                include_directories = FALSE)

    }
  )

  return(list(
    save = function() {list(
      ### Manual save start
      ### Manual save end
      cases = input$cases,
      uncertain_n = input$uncertain_n,
      uncertain_ci = input$uncertain_ci,
      uncertain = input$uncertain,
      iid = input$iid)
    },
    load = function(state) {
      ### Manual load start
      ### Manual load end
      updateNumericInput(session, "uncertain_n", value = state$uncertain_n)
      updateNumericInput(session, "uncertain_ci", value = state$uncertain_ci)
      shinyWidgets::updateMaterialSwitch(session, "uncertain", value = state$uncertain)
      shinyWidgets::updateMaterialSwitch(session, "iid", value = state$iid)
      shinyWidgets::updateMaterialSwitch(session, "cases", value = state$cases)
    }
  ))
})
}

pred_pred_module_map <- function(map, common) {
  for (variable in c("Field", "Prediction (rate)", "Prediction (cases)", "IID")){
    if (!is.null(common$pred[[tolower(variable)]])){
      raster_map(map, common, common$pred[[tolower(variable)]], variable)
    }
  }
  if (!is.null(common$meta$pred_pred$uncertain)){
    raster_map(map, common, common$pred$uncertainty_lower, "Lower credible interval")
    raster_map(map, common, common$pred$uncertainty_upper, "Upper credible interval")
  }
}

pred_pred_module_rmd <- function(common) {
  # Variables used in the module's Rmd code
  list(
    pred_knit = !is.null(common$meta$pred_pred$used),
    pred_iid = common$meta$pred_pred$iid,
    pred_cases = common$meta$pred_pred$cases,
    pred_uncertain = common$meta$pred_pred$uncertain,
    pred_uncertain_n = common$meta$pred_pred$uncertain_n,
    pred_uncertain_ci = common$meta$pred_pred$uncertain_ci
  )
}

