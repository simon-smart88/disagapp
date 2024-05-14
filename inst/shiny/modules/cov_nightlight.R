cov_nightlight_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    # UI
    selectInput(ns("year"), "Year", choices = c(2022:2012)),
    uiOutput(ns("bearer_out")),
    actionButton(ns("run"), "Download data")
  )
}

cov_nightlight_module_server <- function(id, common, parent_session) {
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
    show_loading_modal("Please wait while the data is loaded")
    light <- cov_nightlight(common$shape, input$year, bearer(), common$logger)
    # LOAD INTO COMMON ####
    common$covs[["Nighttime light"]] <- light
    common$logger %>% writeLog("Nighttime light data has been downloaded")
    close_loading_modal()
    # METADATA ####
    common$meta$cov_nightlight$used <- TRUE
    common$meta$cov_nightlight$year <- input$year
    common$meta$cov_nightlight$bearer <- input$bearer
    # TRIGGER
    gargoyle::trigger("cov_nightlight")
  })

  return(list(
    save = function() {
list(year = input$year)
    },
    load = function(state) {
updateSelectInput(session, "year", selected = state$year)
    }
  ))
})
}

cov_nightlight_module_map <- function(map, common) {
  covariate_map(map, common, common$covs[["Nighttime light"]], "Nighttime light")
}

cov_nightlight_module_rmd <- function(common) {
  # Variables used in the module's Rmd code
  list(
    cov_nightlight_knit = !is.null(common$meta$cov_nightlight$used),
    cov_nightlight_year = common$meta$cov_nightlight$year,
    cov_nightlight_bearer = common$meta$cov_nightlight$bearer
  )
}

