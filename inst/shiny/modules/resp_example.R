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

  shape <- reactive({

    switch(input$dataset,
           "mad" = {
             shpdf <- data.frame(datapath = list.files(system.file("extdata/shapes", package="disagapp"), full.names = TRUE),
                                 name = list.files(system.file("extdata/shapes", package="disagapp")))
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

  shape
  })

  output$reset_out <- renderUI({
    reset_data_ui(session, common)
  })

  observeEvent(input$run, {

    # WARNING
    if (!is.null(input$reset) && (input$reset == FALSE)){
      common$logger %>% writeLog(type = "error",
                                 "Uploading new response data will delete all the existing data - toggle the switch and press the button again to continue")
      return()
    }

    # LOAD INTO COMMON ####
    common$reset()
    gargoyle::trigger("clear_map")
    common$shape <- shape()
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
  })

  return(list(
    save = function() {
list(dataset = input$dataset)
    },
    load = function(state) {
updateSelectInput(session, "dataset", selected = state$dataset)
    }
  ))
})
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

