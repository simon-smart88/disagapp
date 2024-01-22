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

    output$country_out <- country_out(session, common)

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
    show_loading_modal("Please wait while the data is loaded")
    country_code <- common$countries$ISO3[common$countries$NAME == input$country]
    bioclim <- cov_bioclim(country_code, input$variables)

    # LOAD INTO COMMON ####
    common$covs <- append(common$covs, bioclim)
    common$selected_country <- input$country
    close_loading_modal()
    common$logger %>% writeLog("Bioclim data has been downloaded")

    # METADATA ####
    common$meta$bioclim$used <- TRUE
    common$meta$bioclim$country <- country_code
    common$meta$bioclim$variables <- input$variables

    # TRIGGER
    gargoyle::trigger("cov_bioclim")
    gargoyle::trigger("country_out")
  })

  return(list(
    save = function() {
list(cov_bioclim_variables = input$variables)
    },
    load = function(state) {
updateSelectInput(session, "variables", selected = common$state$cov_bioclim_variables)
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

