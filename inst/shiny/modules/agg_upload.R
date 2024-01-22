agg_upload_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    fileInput(inputId = ns("agg"),
              label = "Upload aggregation data",
              multiple = FALSE,
              accept = c('.tif')),
    checkboxInput(ns("example"), "Use example data", TRUE),
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
    if (input$example == FALSE){
      if (is.null(input$agg)) {
        common$logger %>% writeLog(type = "error", "Please select a raster file")
        return()
      }
      aggdf <- input$agg
    }

    # check all files are .tif(?)


    if (input$example == TRUE){
    aggdf <- data.frame(datapath = list.files(system.file("extdata/aggregation", package="disagapp"), full.names = TRUE),
                        name = list.files(system.file("extdata/aggregation", package="disagapp")))
    }
    # FUNCTION CALL ####

    agg_raster <- agg_upload(aggdf$datapath)

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
list(agg_upload_example = input$example, 
agg_upload_name = input$name, 
agg_upload_log = input$log)
    },
    load = function(state) {
updateCheckboxInput(session, "example", value = common$state$agg_upload_example) 
updateTextInput(session, "name", value = common$state$agg_upload_name) 
updateCheckboxInput(session, "log", value = common$state$agg_upload_log)
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
  gargoyle::on("agg_upload", {
    req(common$agg)
    common$add_map_layer(common$meta$agg$name)
    if (common$meta$agg$log == TRUE){
      agg_map_raster = log10(common$agg)
      agg_map_title = paste0(common$meta$agg$name, " (log 10)")
      pal <- colorBin("YlOrRd", domain = log10(terra::values(common$agg)), bins = 9, na.color = "#00000000")
      } else {
      agg_map_raster = common$agg
      agg_map_title = common$meta$agg$name
      }
    map %>%
      clearGroup(common$meta$agg$name) %>%
      addRasterImage(raster::raster(agg_map_raster), group = common$meta$agg$name, colors = pal) %>%
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

