cov_nightlight_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    # UI
    selectInput(ns("year"), "Year", choices = c(2022:2012)),
    uiOutput(ns("bearer_out")),
    bslib::input_task_button(ns("run"), "Download data")
  )
}

cov_nightlight_module_server <- function(id, common, parent_session, map) {
  moduleServer(id, function(input, output, session) {

  #use the environmental variable if set, if not display box to enter it
  output$bearer_out <- renderUI({
    if (Sys.getenv("NASA_bearer") == ""){
    textInput(session$ns("bearer"), "NASA bearer token")}
  })

  bearer <- reactive({
  if (Sys.getenv("NASA_bearer") != ""){
    bearer = Sys.getenv("NASA_bearer")
  } else {
    bearer = input$bearer
  }
  bearer
  })


  common$tasks$cov_nightlight <- ExtendedTask$new(function(...) {
    promises::future_promise({
      cov_nightlight(...)
    }, seed = TRUE)
  }) |> bslib::bind_task_button("run")

  observeEvent(input$run, {
    # WARNING ####

    if (curl::has_internet() == FALSE){
      common$logger %>% writeLog(type = "error", "This module requires an internet connection")
      return()
    }

    if (is.null(common$shape)) {
      common$logger %>% writeLog(type = "error", "Please upload response data first")
      return()
    }

    if (bearer() == ""){
      logger %>% writeLog(type = "error", "A NASA bearer token is required to download nighttime light data.
      See the module guidance for details on how to obtain one")
      return()
    }

    # FUNCTION CALL ####
    common$tasks$cov_nightlight$invoke(common$shape, input$year, bearer(), TRUE)
    common$logger %>% writeLog(type = "starting", "Starting to download nightlight data")
    results$resume()
    # METADATA ####
    common$meta$cov_nightlight$used <- TRUE
    common$meta$cov_nightlight$year <- input$year
    common$meta$cov_nightlight$bearer <- input$bearer
  })

  results <- observe({
    # LOAD INTO COMMON ####
    result <- common$tasks$cov_nightlight$result()
    results$suspend()
    if (class(result) == "PackedSpatRaster"){
      common$covs[["Nighttime light"]] <- unwrap_terra(result)
      common$logger %>% writeLog(type = "complete", "Nighttime light data has been downloaded")
      # TRIGGER
      gargoyle::trigger("cov_nightlight")
      do.call("cov_nightlight_module_map", list(map, common))
      shinyjs::runjs("Shiny.setInputValue('cov_nightlight-complete', 'complete');")
    } else {
      common$logger %>% writeLog(type = "error", result)
    }
  })

  return(list(
    save = function() {
list(year = input$year,
bearer = input$bearer)
    },
    load = function(state) {
updateSelectInput(session, "year", selected = state$year)
updateTextInput(session, "bearer", value = state$bearer)
    }
  ))
})
}

cov_nightlight_module_map <- function(map, common) {
  raster_map(map, common, common$covs[["Nighttime light"]], "Nighttime light")
}

cov_nightlight_module_rmd <- function(common) {
  # Variables used in the module's Rmd code
  list(
    cov_nightlight_knit = !is.null(common$meta$cov_nightlight$used),
    cov_nightlight_year = common$meta$cov_nightlight$year,
    cov_nightlight_bearer = common$meta$cov_nightlight$bearer
  )
}

