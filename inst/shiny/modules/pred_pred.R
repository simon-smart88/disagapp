pred_pred_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    # UI
    actionButton(ns("run"), "Produce model predictions")
  )
}

pred_pred_module_server <- function(id, common) {
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
    common$meta$pred$used <- TRUE
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
  gargoyle::on("pred_pred",{
  req(common$pred)
  common$add_map_layer(c("Field", "Prediction"))

  ex <- as.vector(terra::ext(common$shape))
  pal1 <- colorBin("YlOrRd", domain = terra::values(common$pred$field), bins = 9, na.color = "#00000000")
  pal2 <- colorBin("YlOrRd", domain = terra::values(common$pred$prediction), bins = 9, na.color = "#00000000")
  map %>%
    addRasterImage(raster::raster(common$pred$field), group = "Field", colors = pal1) %>%
    addLegend(position = "bottomleft", pal = pal1, values = terra::values(common$pred$field), group = "Field", title = "Field") %>%
    addRasterImage(raster::raster(common$pred$prediction), group = "Prediction", colors = pal2) %>%
    addLegend(position = "bottomright", pal = pal2, values = terra::values(common$pred$prediction), group = "Prediction", title = "Prediction") %>%
    fitBounds(lng1 = ex[[1]], lng2 = ex[[2]], lat1 = ex[[3]], lat2 = ex[[4]]) %>%
    addLayersControl(overlayGroups = common$map_layers, options = layersControlOptions(collapsed = FALSE)) %>%
    hideGroup(common$map_layers[2:(length(common$map_layers)-2)]) #hide all but first and last two layers
  })
}

pred_pred_module_rmd <- function(common) {
  # Variables used in the module's Rmd code
  list(
    pred_knit = !is.null(common$meta$pred$used)
  )
}

