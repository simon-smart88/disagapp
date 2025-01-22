resp_example_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    selectInput(ns("dataset"), "Dataset", choices = c("Malaria in Madagascar" = "mad",
                                                      "Leukemia in New York State" = "nys",
                                                      "Lip cancer in Scotland" = "scot")),
    uiOutput(ns("reset_out")),
    actionButton(ns("run"), "Load data")
  )
}

resp_example_module_server <- function(id, common, parent_session, map) {
  moduleServer(id, function(input, output, session) {

  output$reset_out <- renderUI({
    reset_data_ui(session, common)
  })

  observeEvent(input$run, {

    # WARNING
    if (!is.null(input$reset) && (input$reset == FALSE)){
      common$logger |> writeLog(type = "error",
                                 "Uploading new response data will delete all the existing data - toggle the switch and press the button again to continue")
      return()
    }

    if (input$dataset != "mad" && !requireNamespace("SpatialEpi", quietly = TRUE)){
      common$logger |> writeLog(type = "error",
                              'This dataset requires the SpatialEpi package to be installed. Close the app, run install.packages("SpatialEpi") and try again')
      return()
    }

    # LOAD INTO COMMON ####
    reset_data(common)

    switch(input$dataset,
           "mad" = {
             shpdf <- data.frame(datapath = list.files(system.file("extdata", "shapes", package="disagapp"), full.names = TRUE),
                                 name = list.files(system.file("extdata", "shapes", package="disagapp")))
             shape <- resp_shape(shpdf)
           },
           "nys" = {
             shape <- SpatialEpi::NYleukemia_sf
           },
           "scot" = {
             shape <- SpatialEpi::scotland_sf
             shape$geometry <- shape$geometry * 1000
             shape <- sf::st_set_crs(shape, 27700)
             shape <- sf::st_transform(shape, crs = 4326)
           }
    )

    common$shape <- shape
    switch(input$dataset,
           "mad" = {common$response_name <- "inc"},
           "nys" = {common$response_name <- "cases"},
           "scot" = {common$response_name <- "cases"},
    )
    # METADATA ####
    common$meta$resp_example$used <- TRUE
    common$meta$resp_example$dataset <- input$dataset
    common$meta$resp_example$response <- common$response_name

    # TRIGGER
    gargoyle::trigger("resp_example")
    do.call("resp_example_module_map", list(map, common))
    common$logger |> writeLog(type = "complete", "Response data has been loaded and is summarised in the results tab")

    response <- common$shape[[common$response_name]]
    if (!isTRUE(all.equal(response, as.integer(response)))){
      common$logger |> writeLog(type = "info", "Some response data is not integers")
    }
    if (any(response < 0)){
      common$logger |> writeLog(type = "info", "The response data contains negative values")
    }
  })

  gargoyle::on("resp_example", {
    if (input$dataset == "mad"){
      shinyalert::shinyalert(title = "Load more example data?",
                             type = "info",
                            "Would you like to load the example covariates and aggregation raster as well?",
                            confirmButtonText = "Yes",
                            showCancelButton = TRUE,
                            cancelButtonText = "No",
                            immediate = TRUE,
                            inputId = "load_more")
    }
  })

  observeEvent(input$load_more, {
    if(input$load_more){
      shinyjs::runjs("Shiny.setInputValue('cov_upload-example', 'TRUE');
                      document.getElementById('cov_upload-run').click();
                      Shiny.setInputValue('agg_upload-example', 'TRUE');
                      document.getElementById('agg_upload-run').click();")
    }
  })

  output$plot <- plotly::renderPlotly({
    req(common$shape)
    gargoyle::watch("resp_example")
    gargoyle::watch("resp_edit")
    response <- common$shape[[common$response_name]]
    plot_response(response)
  })

  output$table <- DT::renderDataTable({
    req(common$shape)
    gargoyle::watch("resp_example")
    gargoyle::watch("resp_edit")
    common$shape |> sf::st_drop_geometry()
  })

  return(list(
    save = function() {list(
      ### Manual save start
      ### Manual save end
      dataset = input$dataset)
    },
    load = function(state) {
      ### Manual load start
      ### Manual load end
      updateSelectInput(session, "dataset", selected = state$dataset)
    }
  ))
})
}

resp_example_module_result <- function(id) {
  ns <- NS(id)
  tagList(
    plotly::plotlyOutput(ns("plot")),
    DT::dataTableOutput(ns("table"))
  )
}

resp_example_module_map <- function(map, common) {
  shape_map(map, common)
}

resp_example_module_rmd <- function(common) {
  list(
    resp_example_knit = !is.null(common$meta$resp_example$used),
    resp_example_dataset = common$meta$resp_example$dataset,
    resp_example_response = common$meta$resp_example$response
  )
}

