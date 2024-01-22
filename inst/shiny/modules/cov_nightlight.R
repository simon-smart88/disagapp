cov_nightlight_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    # UI
    selectInput(ns("year"), "Year", choices = c(2012:2022)),
    actionButton(ns("run"), "Download data")
  )
}

cov_nightlight_module_server <- function(id, common) {
  moduleServer(id, function(input, output, session) {

  observeEvent(input$run, {
    # WARNING ####
    if (is.null(common$shape)) {
      common$logger %>% writeLog(type = "error", "Please upload response data first")
      return()
    }
    # FUNCTION CALL ####
    show_loading_modal("Please wait while the data is loaded")
    light <- cov_nightlight(common$shape, input$year)
    # LOAD INTO COMMON ####
    if (is.null(common$covs)){
      common$covs <- list(light)
    } else {
      common$covs <- append(common$covs, light)
    }
    common$logger %>% writeLog("Nighttime light data has been downloaded")
    close_loading_modal()
    # METADATA ####
    common$meta$night$used <- TRUE
    common$meta$night$year <- input$year
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
  # Map logic
}

cov_nightlight_module_rmd <- function(common) {
  # Variables used in the module's Rmd code
  list(
    cov_nightlight_knit = common$meta$night$used,
    cov_nightlight_year = common$meta$night$year
  )
}

