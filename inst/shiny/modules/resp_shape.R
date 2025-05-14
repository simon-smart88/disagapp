resp_shape_module_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(inputId = ns("shape"),
              label = "Upload all shapefile data",
              multiple = TRUE,
              accept = c('.shp','.dbf','.sbn','.sbx','.shx','.prj')),
    uiOutput(ns("resp_var_out")),
    uiOutput(ns("reset_out")),
    actionButton(ns("run"), "Load data")
  )
}

resp_shape_module_server <- function(id, common, parent_session, map) {
  moduleServer(id, function(input, output, session) {

    # not using the usual paradigm in this module due to needing to specify
    # the response variable prior to running
    shape <- reactive({

      req(input$shape)
      shpdf <- input$shape

      # WARNING ####
      if (nrow(shpdf) != 4) {
        common$logger |> writeLog(type = "error", "Please upload four files")
        return()
      }

      # FUNCTION CALL ####
      shape <- resp_shape(shpdf)

      crs <- sf::st_crs(shape)
      if (crs$input != "EPSG:4326"){
        shape <- sf::st_transform(shape, crs = 4326)
      }

      # METADATA ####
      common$meta$resp_shape$used <- TRUE
      common$meta$resp_shape$path <- shpdf$name

      return(shape)
    }) |> bindEvent(input$shape)


   output$resp_var_out <- renderUI({
     req(input$shape)
     req(shape())
     selectInput(session$ns("resp_var"), "Select response variable", c("", names(shape())))
   })

   output$reset_out <- renderUI({
     reset_data_ui(session, common)
   })

  observeEvent(input$run, {

    # WARNING ####
    if (!is.null(input$reset) && (input$reset == FALSE)){
      common$logger |> writeLog(type = "error",
                                 "Uploading new response data will delete all the existing data - toggle the switch and press the button again to continue")
      return()
    }

    if (is.null(input$shape)) {
      common$logger |> writeLog(type = "error", "Please upload a shapefile")
      return()
    }

    if (input$resp_var == "") {
      common$logger |> writeLog(type = "error", "Please select a response variable")
      return()
    }

    # LOAD INTO COMMON ####
    reset_data(common)
    common$shape <- shape()
    common$response_name <- input$resp_var
    # METADATA
    common$meta$resp_shape$response <- input$resp_var

    # TRIGGER
    trigger("resp_shape")
    do.call("resp_shape_module_map", list(map, common))
    common$logger |> writeLog(type = "complete", "Response data has been uploaded and is summarised in the results tab")

    response <- common$shape[[common$response_name]]
    if (!isTRUE(all.equal(response, as.integer(response)))){
      common$logger |> writeLog(type = "info", "Some response data is not integers")
    }
    if (any(response < 0)){
      common$logger |> writeLog(type = "info", "The response data contains negative values")
    }
    if (response_area(common$shape) > 1e6){
      common$logger |> writeLog(type = "warning", glue::glue("The total area of the response data is {response_area(common$shape)} km2.
                                                             Some covariates may be slow to download."))
    }
  })

  output$plot <- plotly::renderPlotly({
    req(common$shape)
    watch("resp_shape")
    watch("resp_edit")
    response <- common$shape[[common$response_name]]
    plot_response(response)
  })

  output$table <- DT::renderDataTable({
    req(common$shape)
    watch("resp_shape")
    watch("resp_edit")
    common$shape |> sf::st_drop_geometry()
  })

  return(list(
    save = function() {list(
      ### Manual save start
      ### Manual save end
      resp_var = input$resp_var)
    },
    load = function(state) {
      ### Manual load start
      ### Manual load end
      updateSelectInput(session, "resp_var", selected = state$resp_var)
    }
  ))
})
}

resp_shape_module_result <- function(id) {
  ns <- NS(id)
  tagList(
    plotly::plotlyOutput(ns("plot")),
    DT::dataTableOutput(ns("table"))
  )
}

resp_shape_module_map <- function(map, common) {
  shape_map(map, common)
}


resp_shape_module_rmd <- function(common) {
  # Variables used in the module's Rmd code
  list(
    resp_shape_knit = !is.null(common$meta$resp_shape$used),
    resp_shape_path = common$meta$resp_shape$path,
    resp_shape_resp = common$meta$resp_shape$response,
    resp_shape_response_name = common$response_name
  )
}

