pred_pred_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    # UI
    actionButton(ns("run"), "Produce model predictions")
  )
}

pred_pred_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {

  observeEvent(input$run, {
    # WARNING ####
    if (is.null(common$fit)){
      common$logger %>% writeLog(type = "error", "Please fit a model first")
      return()
    }
    # FUNCTION CALL ####
    show_loading_modal("Please wait while the model predictions are generated")
    prediction <- disaggregation::predict_model(common$fit)
    terra::crs(prediction$field) <- terra::crs(common$prep$covariate_rasters[[1]])
    prediction$field <- terra::mask(prediction$field, common$prep$covariate_rasters[[1]])
    close_loading_modal()
    common$logger %>% writeLog('Model predictions are available')
    # LOAD INTO COMMON ####
    common$pred <- prediction
    # METADATA ####
    common$meta$pred_pred$used <- TRUE
    # TRIGGER
    gargoyle::trigger("pred_pred")
  })

  # output$result <- renderText({
  #   # Result
  # })

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

# pred_pred_module_result <- function(id) {
#   ns <- NS(id)
#
#   # Result UI
#   verbatimTextOutput(ns("result"))
# }

pred_pred_module_map <- function(map, common) {
  for (variable in c("Field", "Prediction")){
    covariate_map(map, common, common$pred[[tolower(variable)]], variable)
  }
}

pred_pred_module_rmd <- function(common) {
  # Variables used in the module's Rmd code
  list(
    pred_knit = !is.null(common$meta$pred_pred$used)
  )
}

