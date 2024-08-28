pred_pred_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    uiOutput(ns("iid_out")),
    shinyWidgets::materialSwitch(ns("uncertain"), "Include uncertainty?", FALSE, status = "success"),
    uiOutput(ns("uncertain_parameters")),
    actionButton(ns("run"), "Produce model predictions"),
    tags$br(),
    uiOutput(ns("dl_out"))
  )
}

pred_pred_module_server <- function(id, common, parent_session, map) {
  moduleServer(id, function(input, output, session) {

  output$iid_out <- renderUI({
    gargoyle::watch("fit_fit")
    req(common$fit)
    if (common$meta$fit_fit$iid){
      out <- shinyWidgets::materialSwitch(session$ns("iid"), "Include IID effect?", FALSE, status = "success")
    } else {
      out <- NULL
    }
    out
  })

  output$uncertain_parameters <- renderUI({
    if (input$uncertain){
      out <- tagList(
        tags$label("Uncertainty parameters"),
        numericInput(session$ns("uncertain_n"), "Number of realisations", value = 100, step = 1),
        numericInput(session$ns("uncertain_ci"), "Credible interval", value = 0.95, step = 0.01, max = 1, min = 0),
      )
    } else {
      out <- NULL
    }
    out
  })

  output$dl_out <- renderUI({
    gargoyle::watch("pred_pred")
    req(common$pred)
    downloadButton(session$ns("download"), "Download model predictions")
  })


  observeEvent(input$run, {
    # WARNING ####
    if (is.null(common$fit)){
      common$logger |> writeLog(type = "error", "Please fit a model first")
      return()
    }
    # FUNCTION CALL ####
    show_loading_modal("Please wait while the model predictions are generated")

    if (is.null(input$iid)){
      predict_iid <- FALSE
    } else {
      predict_iid <- input$iid
    }

    prediction <- disaggregation::predict_model(common$fit, predict_iid = predict_iid)
    if (!is.null(prediction$field)){
      terra::crs(prediction$field) <- terra::crs(common$prep$covariate_rasters[[1]])
      prediction$field <- terra::mask(prediction$field, common$prep$covariate_rasters[[1]])
    }
    if (input$uncertain){
      uncertainty <- disaggregation::predict_uncertainty(common$fit,
                                                         predict_iid = predict_iid,
                                                         N = input$uncertain_n,
                                                         CI = input$uncertain_ci)
    }
    close_loading_modal()
    common$logger |> writeLog(type = "complete", "Model predictions are available")
    # LOAD INTO COMMON ####
    common$pred <- prediction

    if (is.null(common$meta$prep_final$resolution) || common$meta$prep_final$resolution == "High resolution"){
      common$pred$cases <- common$pred$prediction * common$agg_prep
    } else {
      common$pred$cases <- common$pred$prediction * common$agg_prep_lores
    }

    if (input$uncertain){
      common$pred$uncertainty <- uncertainty
    }
    # METADATA ####
    common$meta$pred_pred$used <- TRUE
    if (input$uncertain){
      common$meta$pred_pred$uncertain <- input$uncertain
    }
    if (is.null(input$iid)){
      common$meta$pred_pred$iid <- FALSE
    }
    common$meta$pred_pred$iid <- input$iid
    if (input$uncertain){
      common$meta$pred_pred$uncertain_n <- input$uncertain_n
      common$meta$pred_pred$uncertain_ci <- input$uncertain_ci
    }

    names(common$pred)[which(names(common$pred) == "prediction")] <- "prediction (rate)"
    names(common$pred)[which(names(common$pred) == "cases")] <- "prediction (cases)"

    # TRIGGER
    gargoyle::trigger("pred_pred")
    do.call("pred_pred_module_map", list(map, common))
    show_map(parent_session)
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


  # output$result <- renderText({
  #   # Result
  # })

  return(list(
    save = function() {list(
      ### Manual save start
      ### Manual save end
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
    }
  ))
})
}

# pred_pred_module_result <- function(id) {
#   ns <- NS(id)
#
#   # Result UI
#   verbatimTextOutput(ns("result"))
# }

pred_pred_module_map <- function(map, common) {
  for (variable in c("Field", "Prediction (rate)", "Prediction (cases)", "IID")){
    if (!is.null(common$pred[[tolower(variable)]])){
      raster_map(map, common, common$pred[[tolower(variable)]], variable)
    }
  }
  if (!is.null(common$meta$pred_pred$uncertain)){
    raster_map(map, common, common$pred$uncertainty$predictions_ci$`lower CI`, "Lower credible interval")
    raster_map(map, common, common$pred$uncertainty$predictions_ci$`upper CI`, "Upper credible interval")
  }
}

pred_pred_module_rmd <- function(common) {
  # Variables used in the module's Rmd code
  list(
    pred_knit = !is.null(common$meta$pred_pred$used),
    pred_iid = common$meta$pred_pred$iid,
    pred_uncertain_knit = !is.null(common$meta$pred_pred$uncertain),
    pred_uncertain = common$meta$pred_pred$uncertain,
    pred_uncertain_n = common$meta$pred_pred$uncertain_n,
    pred_uncertain_ci = common$meta$pred_pred$uncertain_ci
  )
}

