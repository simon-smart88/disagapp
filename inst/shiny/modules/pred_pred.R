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

    # FUNCTION CALL ####
    common$logger %>% writeLog(type='info', 'Model predictions are being generated - please be patient')
    prediction <- disaggregation::predict(common$fit)
    prediction$mean_prediction$field <- mask(prediction$mean_prediction$field, common$agg)
    crs(prediction$mean_prediction$field) <- crs(common$covs[[1]])
    common$logger %>% writeLog('Model predictions are available')
    # LOAD INTO COMMON ####
    common$pred <- prediction
    # METADATA ####
    common$meta$pred$used <- TRUE
    # TRIGGER
    gargoyle::trigger("pred_pred")
  })

  output$result <- renderText({
    # Result
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

pred_pred_module_result <- function(id) {
  ns <- NS(id)

  # Result UI
  verbatimTextOutput(ns("result"))
}

pred_pred_module_map <- function(map, common) {
  observeEvent(input$predict,{
  common$add_map_layer(c('Field','Prediction'))

  ex <- extent(common$shape)

  pal1 <- colorBin("YlOrRd", domain = values(common$pred$mean_prediction$field), bins = 9, na.color ="#00000000")
  pal2 <- colorBin("YlOrRd", domain = values(common$pred$mean_prediction$prediction), bins = 9, na.color ="#00000000")
  map %>%
    addRasterImage(common$pred$mean_prediction$field,group='Field',colors = pal1) %>%
    addLegend(position ="bottomleft",pal = pal1, values = values(common$pred$mean_prediction$field), group='Field', title='Field') %>%
    addRasterImage(common$pred$mean_prediction$prediction,group='Prediction',colors = pal2) %>%
    addLegend(position ="bottomright",pal = pal2, values = values(common$pred$mean_prediction$prediction), group='Prediction', title='Prediction') %>%
    fitBounds(lng1=ex@xmin,lng2=ex@xmax,lat1=ex@ymin,lat2=ex@ymax) %>%
    addLayersControl(overlayGroups = common$map_layers, options = layersControlOptions(collapsed = FALSE)) %>%
    hideGroup(common$map_layers[2:(length(common$map_layers)-2)]) #hide all but first and last two layers
  })
}

pred_pred_module_rmd <- function(common) {
  # Variables used in the module's Rmd code
  list(
    pred_pred_knit = !is.null(common$some_object),
    var1 = common$meta$setting1,
    var2 = common$meta$setting2
  )
}

