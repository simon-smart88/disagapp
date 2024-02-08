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
    bioclim <- cov_bioclim(country_code, input$variables, common$shape)

    # LOAD INTO COMMON ####
    common$covs <- append(common$covs, bioclim)
    common$selected_country <- input$country
    close_loading_modal()
    common$logger %>% writeLog("Bioclim data has been downloaded")

    # METADATA ####
    common$meta$cov_bioclim$used <- TRUE
    common$meta$cov_bioclim$country <- country_code
    common$meta$cov_bioclim$variables <- input$variables

    # TRIGGER
    gargoyle::trigger("cov_bioclim")
    gargoyle::trigger("country_out")
  })

  return(list(
    save = function() {
list(variables = input$variables)
    },
    load = function(state) {
updateSelectInput(session, "variables", selected = state$variables)
    }
  ))
})
}

cov_bioclim_module_map <- function(map, common) {
  gargoyle::on("cov_bioclim", {
  for (variable in common$meta$cov_bioclim$variables){
    covariate_map(map, common, common$covs[[variable]], variable)
  }
  })
}

cov_bioclim_module_rmd <- function(common) {
  # Variables used in the module's Rmd code
  list(
    cov_bioclim_knit = !is.null(common$meta$cov_bioclim$used),
    cov_bioclim_country = common$meta$cov_bioclim$country,
    cov_bioclim_variables = printVecAsis(common$meta$cov_bioclim$variables)
  )
}

