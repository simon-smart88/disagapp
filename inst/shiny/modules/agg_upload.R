agg_upload_module_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(inputId = ns("agg"),
              label = "Upload aggregation data",
              multiple = FALSE,
              accept = c(".tif")),
    uiOutput(ns("example_out")),
    textInput(ns("name"),
              label = "Aggregation name",
              value = "Population"),
    shinyWidgets::materialSwitch(ns("log"),
                  label = "Plot as log values",
                  value = TRUE, status = "success"),
    actionButton(ns("run"), "Upload file", icon = icon("arrow-turn-down"))
  )
}


agg_upload_module_server <- function(id, common, parent_session, map) {
  moduleServer(id, function(input, output, session) {

  output$example_out <- renderUI({
    watch("resp_example")
    if (!is.null(common$meta$resp_example$dataset) && (common$meta$resp_example$dataset == "mad")){
      shinyWidgets::materialSwitch(session$ns("example"), "Use example data", value = TRUE, status = "success")
    }
  })

  observeEvent(input$run, {
    # WARNING ####
    # check a file is selected
    if (is.null(input$example) || (input$example == FALSE)){
      if (is.null(input$agg)) {
        common$logger |> writeLog(type = "error", "Please select a raster file")
        return()
      }
      aggdf <- input$agg
    }

    # check all files are .tif(?)

    if (is.null(input$example) || (input$example == TRUE)){
    aggdf <- data.frame(datapath = list.files(system.file("extdata", "aggregation", package="disagapp"), full.names = TRUE),
                        name = list.files(system.file("extdata", "aggregation", package="disagapp")))
    }
    # FUNCTION CALL ####
    agg_raster <- agg_upload(common$shape, aggdf$datapath, common$logger)
    if (is.null(agg_raster)){
      return()
    }

    # LOAD INTO COMMON ####
    common$agg <- agg_raster
    names(common$agg) <- input$name

    # METADATA ####
    common$meta$agg_upload$path <- aggdf$name
    common$meta$agg_upload$name <- input$name
    common$meta$agg_upload$log <- input$log
    common$meta$agg_upload$used <- TRUE
    # TRIGGER
    trigger("agg_upload")
    do.call("agg_upload_module_map", list(map, common))
    common$logger |> writeLog(type = "complete", "Aggregation data has been uploaded")
  })

  output$plot <- renderPlot({
    watch("agg_upload")
    req(common$meta$agg_upload)
    plot_raster(list(common$agg), 1, log = input$log)
  })

  return(list(
    save = function() {list(
      ### Manual save start
      ### Manual save end
      name = input$name,
      log = input$log,
      example = input$example)
    },
    load = function(state) {
      ### Manual load start
      ### Manual load end
      updateTextInput(session, "name", value = state$name)
      shinyWidgets::updateMaterialSwitch(session, "log", value = state$log)
      shinyWidgets::updateMaterialSwitch(session, "example", value = state$example)
    }
  ))
})
}

agg_upload_module_result <- function(id) {
  ns <- NS(id)
  plotOutput(ns("plot"))
}

agg_upload_module_map <- function(map, common) {
  raster_map(map, common, common$agg, common$meta$agg_upload$name, common$meta$agg_upload$log)
}

agg_upload_module_rmd <- function(common) {
  # Variables used in the module's Rmd code
  list(
    agg_upload_knit = !is.null(common$meta$agg_upload$used),
    agg_upload_path = common$meta$agg_upload$path,
    agg_upload_name = common$meta$agg_upload$name
  )
}

