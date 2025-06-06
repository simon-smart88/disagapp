cov_bioclim_module_ui <- function(id) {
  ns <- NS(id)
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
    input_task_button(ns("run"), "Load data", type = "default", icon = icon("arrow-turn-down"))
  )
}

cov_bioclim_module_server <- function(id, common, parent_session, map) {
  moduleServer(id, function(input, output, session) {

  output$country_out <- country_out(session, common)

  common$tasks$cov_bioclim <- ExtendedTask$new(function(...) {
    promises::future_promise({
      cov_bioclim(...)
    }, seed = TRUE)
  }) |> bind_task_button("run")

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

    if (is.null(input$country)) {
      common$logger |> writeLog(type = "error", "Please select a country")
      return()
    }

    if (is.null(input$variables)) {
      common$logger |> writeLog(type = "error", "Please select the variables to download")
      return()
    }

    if (length(common$covs_prep) > 0) {
      common$logger |> writeLog(type = "warning", "You will need to prepare the data
                                again to include climate data in the model")
    }

    # FUNCTION CALL ####
    country_code <- common$countries$boundaryISO[common$countries$boundaryName %in% input$country]
    common$tasks$cov_bioclim$invoke(common$shape, country_code, input$variables, TRUE)
    common$logger |> writeLog(type = "starting", "Starting to download bioclim data")
    results$resume()

    # METADATA ####
    common$meta$cov_bioclim$used <- TRUE
    common$meta$cov_bioclim$country <- country_code
    common$meta$cov_bioclim$variables <- input$variables
    common$meta$cov_bioclim$plot_height <- length(input$variables) * 400
    common$selected_country <- input$country
    trigger("country_out")
  })

  results <- observe({
    # LOAD INTO COMMON ####
    result <- common$tasks$cov_bioclim$result()
    results$suspend()
    if (inherits(result, "list")){
      common$covs <- append(common$covs, unwrap_terra(result))
      common$logger |> writeLog(type = "complete", "Bioclim data has been downloaded")
      # TRIGGER
      trigger("cov_bioclim")
      do.call("cov_bioclim_module_map", list(map, common))
      shinyjs::runjs("Shiny.setInputValue('cov_bioclim-complete', 'complete');")
    } else {
      common$logger |> writeLog(type = "error", result)
    }
  })

  output$plot <- renderPlot({
    watch("cov_bioclim")
    req(common$meta$cov_bioclim)
    plot_raster(common$covs, common$meta$cov_bioclim$variables)
  }, height = function(){ifelse(is.null(common$meta$cov_bioclim$plot_height), 400, common$meta$cov_bioclim$plot_height)})

  return(list(
    save = function() {list(
      ### Manual save start
      country = input$country,
      ### Manual save end
      variables = input$variables)
    },
    load = function(state) {
      ### Manual load start
      updateSelectInput(session, "country", selected = state$country)
      ### Manual load end
      updateSelectInput(session, "variables", selected = state$variables)
    }
  ))
})
}

cov_bioclim_module_result <- function(id) {
  ns <- NS(id)
  plotOutput(ns("plot"))
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
    cov_bioclim_country = common$meta$cov_bioclim$country,
    cov_bioclim_variables = common$meta$cov_bioclim$variables,
    cov_bioclim_plot_height = common$meta$cov_bioclim$plot_height)
}

