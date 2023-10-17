cov_bioclim_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    uiOutput(ns("country_out")),
    selectInput(ns("variables"), "Bioclim variables", multiple = TRUE,
                choices = c("Mean temperature",
                            "Mean diurnal range",
                            "Isothermality",
                            "Temperature seasonality",
                            "Maximum temperature warmest month",
                            "Minimum temperature coldest month",
                            "Temperature range",
                            "Mean temperature wettest quarter",
                            "Mean temperature driest quarter",
                            "Mean temperature warmest quarter",
                            "Mean temperature coldest quarter",
                            "Total precipitation",
                            "Precipitation wettest month",
                            "Precipitation driest month",
                            "Precipitation seasonality",
                            "Precipitation wettest quarter",
                            "Precipitation driest quarter",
                            "Precipitation warmest quarter",
                            "Precipitation coldest quarter")),
    actionButton(ns("run"), "Download data")
  )
}

cov_bioclim_module_server <- function(id, common) {
  moduleServer(id, function(input, output, session) {

    countries <- readRDS(system.file("ex/countries.rds", package="geodata"))

    output$country_out <- renderUI({
      selectInput(session$ns("country"), "Select country", countries$NAME)
    })

  observeEvent(input$run, {
    # WARNING ####
    if (is.null(input$country)) {
      common$logger %>% writeLog(type = "error", "Please select a country")
      return()
    }
    if (is.null(input$variables)) {
      common$logger %>% writeLog(type = "error", "Please select the variables to download")
      return()
    }
    # FUNCTION CALL ####
    showModal(modalDialog(title = "Info", "Please wait while the data is loaded.
                          This window will close once it is complete.", easyClose = FALSE))
    country_code <- countries$ISO3[countries$NAME == input$country]
    bioclim <- cov_bioclim(country_code, input$variables)
    # LOAD INTO COMMON ####
    common$covs <- append(common$covs, bioclim)
    removeModal()
    common$logger %>% writeLog("Bioclim data has been downloaded")
    # METADATA ####
    common$meta$bioclim$used <- T
    common$meta$bioclim$country <- input$country
    common$meta$bioclim$variables <- input$variables

    # TRIGGER
    gargoyle::trigger("cov_bioclim")
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

cov_bioclim_module_map <- function(map, common) {
  # Map logic
}

cov_bioclim_module_rmd <- function(common) {
  # Variables used in the module's Rmd code
  list(
    cov_bioclim_knit = common$meta$bioclim$used,
    cov_bioclim_country = common$meta$bioclim$country,
    cov_bioclim_variables = printVecAsis(common$meta$bioclim$variables)
  )
}

