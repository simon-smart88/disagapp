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
    bslib::input_task_button(ns("run"), "Download data")
  )
}

cov_bioclim_module_server <- function(id, common, parent_session, map) {
  moduleServer(id, function(input, output, session) {

  output$country_out <- country_out(session, common)

  common$tasks$cov_bioclim <- ExtendedTask$new(function(...) {
    promises::future_promise({
      cov_bioclim(...)
    }, seed = TRUE)
  }) |> bslib::bind_task_button("run")

  observeEvent(input$run, {
    # WARNING ####

    if (is.null(common$shape)) {
      common$logger |> writeLog(type = "error", "Please upload response data first")
      return()
    }

    if (curl::has_internet() == FALSE){
      common$logger |> writeLog(type = "error", "This module requires an internet connection")
      return()
    }

    if (input$country[1] == "") {
      common$logger |> writeLog(type = "error", "Please select a country")
      return()
    }

    if (is.null(input$variables)) {
      common$logger |> writeLog(type = "error", "Please select the variables to download")
      return()
    }
    # FUNCTION CALL ####
    country_code <- common$countries$ISO3[common$countries$NAME %in% input$country]
    common$tasks$cov_bioclim$invoke(country_code, input$variables, common$shape, TRUE)
    common$logger |> writeLog(type = "starting", "Starting to download bioclim data")
    results$resume()

    # METADATA ####
    common$meta$cov_bioclim$used <- TRUE
    common$meta$cov_bioclim$country <- country_code
    common$meta$cov_bioclim$variables <- input$variables
    common$selected_country <- input$country
    gargoyle::trigger("country_out")
  })

  results <- observe({
    # LOAD INTO COMMON ####
    result <- common$tasks$cov_bioclim$result()
    results$suspend()
    if (class(result) == "list"){
      common$covs <- append(common$covs, unwrap_terra(result))
      common$logger |> writeLog(type = "complete", "Bioclim data has been downloaded")
      # TRIGGER
      gargoyle::trigger("cov_bioclim")
      do.call("cov_bioclim_module_map", list(map, common))
      shinyjs::runjs("Shiny.setInputValue('cov_bioclim-complete', 'complete');")
    } else {
      common$logger |> writeLog(type = "error", result)
    }
  })

  return(list(
    save = function() {list(
      ### Manual save start
      ### Manual save end
      variables = input$variables)
    },
    load = function(state) {
      ### Manual load start
      ### Manual load end
      updateSelectInput(session, "variables", selected = state$variables)
    }
  ))
})
}

cov_bioclim_module_map <- function(map, common) {
  for (variable in common$meta$cov_bioclim$variables){
    raster_map(map, common, common$covs[[variable]], variable)
  }
}

cov_bioclim_module_rmd <- function(common) {
  # Variables used in the module's Rmd code
  list(
    cov_bioclim_knit = !is.null(common$meta$cov_bioclim$used),
    cov_bioclim_country = printVecAsis(common$meta$cov_bioclim$country),
    cov_bioclim_variables = printVecAsis(common$meta$cov_bioclim$variables)
  )
}

