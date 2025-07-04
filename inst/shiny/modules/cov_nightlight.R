cov_nightlight_module_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # UI
    uiOutput(ns("country_out")),
    selectInput(ns("year"), "Year", choices = c(2023:2015)),
    shinyWidgets::materialSwitch(ns("log"), label = 'Plot as log values', value = TRUE, status = "success"),
    input_task_button(ns("run"), "Load data", type = "default", icon = icon("arrow-turn-down"))
  )
}

cov_nightlight_module_server <- function(id, common, parent_session, map) {
  moduleServer(id, function(input, output, session) {

  output$country_out <- country_out(session, common)

  common$tasks$cov_nightlight <- ExtendedTask$new(function(...) {
    promises::future_promise({
      cov_nightlight(...)
    }, seed = TRUE)
  }) |> bind_task_button("run")

  observeEvent(input$run, {
    # WARNING ####

    if (curl::has_internet() == FALSE){
      common$logger |> writeLog(type = "error", "This module requires an internet connection")
      return()
    }

    if (is.null(common$shape)) {
      common$logger |> writeLog(type = "error", "Please upload response data first")
      return()
    }

    if (is.null(input$country)) {
      common$logger |> writeLog(type = "error", "Please select a country")
      return()
    }

    if (length(common$covs_prep) > 0) {
      common$logger |> writeLog(type = "warning", "You will need to prepare the data
                                again to include nighttime light data in the model")
    }

    # FUNCTION CALL ####
    country_code <- common$countries$boundaryISO[common$countries$boundaryName %in% input$country]
    common$tasks$cov_nightlight$invoke(common$shape, country_code, as.numeric(input$year), TRUE)
    common$logger |> writeLog(type = "starting", "Starting to download nightlight data")
    results$resume()
    # METADATA ####
    common$meta$cov_nightlight$used <- TRUE
    common$meta$cov_nightlight$country <- country_code
    common$meta$cov_nightlight$year <- as.numeric(input$year)
    common$meta$cov_nightlight$log <- input$log
  })

  results <- observe({
    # LOAD INTO COMMON ####
    result <- common$tasks$cov_nightlight$result()
    results$suspend()
    if (inherits(result, "PackedSpatRaster")){
      common$covs[["Nighttime light"]] <- unwrap_terra(result)
      common$logger |> writeLog(type = "complete", "Nighttime light data has been downloaded")
      # TRIGGER
      trigger("cov_nightlight")
      do.call("cov_nightlight_module_map", list(map, common))
      shinyjs::runjs("Shiny.setInputValue('cov_nightlight-complete', 'complete');")
    } else {
      common$logger |> writeLog(type = "error", result)
    }
  })

  output$plot <- renderPlot({
    watch("cov_nightlight")
    req(common$meta$cov_nightlight)
    plot_raster(common$covs, "Nighttime light", log = common$meta$cov_nightlight$log)
  })

  return(list(
    save = function() {list(
      ### Manual save start
      country = input$country,
      ### Manual save end
      year = input$year,
      log = input$log)
    },
    load = function(state) {
      ### Manual load start
      updateSelectInput(session, "country", selected = state$country)
      ### Manual load end
      updateSelectInput(session, "year", selected = state$year)
      shinyWidgets::updateMaterialSwitch(session, "log", value = state$log)
    }
  ))
})
}

cov_nightlight_module_result <- function(id) {
  ns <- NS(id)
  plotOutput(ns("plot"))
}

cov_nightlight_module_map <- function(map, common) {
  raster_map(map, common, common$covs[["Nighttime light"]], "Nighttime light", common$meta$cov_nightlight$log)
}

cov_nightlight_module_rmd <- function(common) {
  # Variables used in the module's Rmd code
  list(
    cov_nightlight_knit = !is.null(common$meta$cov_nightlight$used),
    cov_nightlight_year = common$meta$cov_nightlight$year,
    cov_nightlight_country = common$meta$cov_nightlight$country,
    cov_nightlight_log = common$meta$cov_nightlight$log
  )
}

