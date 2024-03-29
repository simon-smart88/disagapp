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
              value = "Population"),
    checkboxInput(ns("log"),
                  label = 'Plot as log values',
                  value = TRUE),
    actionButton(ns("run"), "Upload file")
  )
}


agg_upload_module_server <- function(id, common, parent_session) {
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
    common$meta$agg_upload$path <- aggdf$name
    common$meta$agg_upload$name <- input$name
    common$meta$agg_upload$log <- input$log
    common$meta$agg_upload$used <- TRUE
    # TRIGGER
    gargoyle::trigger("agg_upload")
  })


  return(list(
    save = function() {
list(example = input$example,
name = input$name,
log = input$log)
    },
    load = function(state) {
updateCheckboxInput(session, "example", value = state$example)
updateTextInput(session, "name", value = state$name)
updateCheckboxInput(session, "log", value = state$log)
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
  covariate_map(map, common, common$agg, common$meta$agg_upload$name, common$meta$agg_upload$log)
}

agg_upload_module_rmd <- function(common) {
  # Variables used in the module's Rmd code
  list(
    agg_upload_knit = !is.null(common$meta$agg_upload$used),
    agg_upload_path = common$meta$agg_upload$path,
    agg_upload_name = common$meta$agg_upload$name
  )
}

