agg_upload_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    fileInput(inputId = ns("agg"),
              label = "Upload aggregation data",
              multiple = FALSE,
              accept = c('.tif')),
    textInput(ns("name"),
              label = "Aggregation name",
              value = "Population density"),
    checkboxInput(ns("log"),
                  label = 'Plot as log values',
                  value = TRUE),
    actionButton(ns("run"), "Upload file")
  )
}


agg_upload_module_server <- function(id, common) {
  moduleServer(id, function(input, output, session) {

  observeEvent(input$run, {
    # WARNING ####
    # check a file is selected
    if (is.null(input$agg)) {
      common$logger %>% writeLog(type = "error", "Please select a raster file")
      return()
    }

    # check all files are .tif


    # FUNCTION CALL ####
    aggdf <- input$agg
    agg_raster <- agg_upload(aggdf)

    # LOAD INTO COMMON ####
    common$agg <- agg_raster

    # METADATA ####
    common$meta$agg$path <- aggdf$name
    common$meta$agg$name <- input$name
    common$meta$agg$log <- input$log
    common$meta$agg$upload <- TRUE
    # TRIGGER
    gargoyle::trigger("agg_upload")
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

agg_upload_module_result <- function(id) {
  ns <- NS(id)

  # Result UI
  verbatimTextOutput(ns("result"))
}

agg_upload_module_map <- function(map, common) {
  observeEvent(gargoyle::watch("agg_upload"), {
    req(common$agg)
    common$add_map_layer(common$meta$agg$name)
    if (common$meta$agg$log == TRUE){
      agg_map_raster = log10(common$agg)
      agg_map_title = paste0(common$meta$agg$name, " (log 10)")
      } else {
      agg_map_raster = common$agg
      agg_map_title = common$meta$agg$name
      }
    map %>%
      clearGroup(common$meta$agg$name) %>%
      addRasterImage(agg_map_raster, group = common$meta$agg$name, colors = pal) %>%
      addLegend(position = "bottomleft", pal = pal, values = values(agg_map_raster), group = common$meta$agg$name, title = agg_map_title) %>%
      addLayersControl(overlayGroups = common$map_layers, options = layersControlOptions(collapsed = FALSE))
  })
}

agg_upload_module_rmd <- function(common) {
  # Variables used in the module's Rmd code
  list(
    agg_upload_knit = common$meta$agg$upload,
    upload_agg_path = common$meta$agg$path,
    upload_agg_name = common$meta$agg$name
  )
}

