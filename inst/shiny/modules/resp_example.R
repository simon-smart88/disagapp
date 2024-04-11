resp_example_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    selectInput(ns("dataset"), "Dataset", choices = c("Malaria in Madagascar" = "mad",
                                                      "Leukemia in New York State" = "nys",
                                                      "Lip cancer in Scotland" = "scot")),
    actionButton(ns("run"), "Load data")
  )
}

resp_example_module_server <- function(id, common, parent_session) {
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

  observeEvent(input$run, {
    # LOAD INTO COMMON ####
    common$reset()
    common$shape <- shape()
    # METADATA ####
    common$meta$resp_example$used <- TRUE
    common$meta$resp_example$dataset <- input$dataset
    switch(input$dataset,
           "mad" = {common$meta$resp_example$response <- "inc"},
           "nys" = {common$meta$resp_example$response <- "cases"},
           "scot" = {common$meta$resp_example$response <- "cases"},
           )

    # TRIGGER
    gargoyle::trigger("resp_example")
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

resp_example_module_map <- function(map, common) {
  response <- as.numeric(common$shape[[common$meta$resp_example$response]])
  shape_map(map, common, response)
}

resp_example_module_rmd <- function(common) {
  list(
    resp_example_knit = !is.null(common$meta$resp_example$used),
    resp_example_dataset = common$meta$resp_example$dataset,
    resp_example_response = common$meta$resp_example$response
  )
}

