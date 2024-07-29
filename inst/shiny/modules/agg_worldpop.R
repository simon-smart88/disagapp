agg_worldpop_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    # UI
    uiOutput(ns("country_out")),
    selectInput(ns("method"), "Method", choices = c("Constrained", "Unconstrained")),
    selectInput(ns("resolution"), "Resolution", choices = c("1km", "100m")),
    uiOutput(ns("year_out")),
    shinyWidgets::materialSwitch(ns("log"), label = 'Plot as log values', value = TRUE, status = "success"),
    bslib::input_task_button(ns("run"), "Download data")
  )
}

agg_worldpop_module_server <- function(id, common, parent_session, map) {
  moduleServer(id, function(input, output, session) {

  output$country_out <- country_out(session, common)

  output$year_out <- renderUI({
    if (input$method == "Unconstrained"){
      selectInput(session$ns("year"), "Year", choices = c(2000:2020))
    }
    if (input$method == "Constrained"){
      selectInput(session$ns("year"), "Year", choices = c(2020))
    }
  })

  common$tasks$agg_worldpop <- ExtendedTask$new(function(...) {
    promises::future_promise({
      agg_worldpop(...)
    }, seed = TRUE)
  }) |> bslib::bind_task_button("run")

  observeEvent(input$run, {
    # WARNING ####
    if (curl::has_internet() == FALSE){
      common$logger |> writeLog(type = "error", "This module requires an internet connection")
      return()
    }

    if (input$country[1] == "") {
      common$logger |> writeLog(type = "error", "Please select a country")
      return()
    }
    # FUNCTION CALL ####
    country_code <- common$countries$ISO3[common$countries$NAME %in% input$country]
    common$logger |> writeLog(type = "starting", "Starting to download Worldpop data")
    common$tasks$agg_worldpop$invoke(common$shape, country_code, input$method, input$resolution, as.numeric(input$year), TRUE)
    results$resume()
    # METADATA ####
    common$meta$agg_worldpop$name <- "Population"
    common$meta$agg_worldpop$log <- input$log
    common$meta$agg_worldpop$used <- TRUE
    common$meta$agg_worldpop$country <- country_code
    common$meta$agg_worldpop$method <- input$method
    common$meta$agg_worldpop$resolution <- input$resolution
    common$meta$agg_worldpop$year <- as.numeric(input$year)

    common$selected_country <- input$country
    gargoyle::trigger("country_out")
  })

  results <- observe({
    # LOAD INTO COMMON ####
    result <- common$tasks$agg_worldpop$result()
    results$suspend()
    if (class(result) == "PackedSpatRaster"){
      result <- unwrap_terra(result)
      common$agg <- result
      common$logger |> writeLog(type = "complete", "Worldpop data has been downloaded")
      # TRIGGER
      gargoyle::trigger("agg_worldpop")
      do.call("agg_worldpop_module_map", list(map, common))
      shinyjs::runjs("Shiny.setInputValue('agg_worldpop-complete', 'complete');")
    } else {
      common$logger |> writeLog(type = "error", result)
    }
  })


  return(list(
    save = function() {list(
      ### Manual save start
      country = input$country,
      ### Manual save end
      method = input$method,
      resolution = input$resolution,
      year = as.numeric(input$year),
      log = input$log)
    },
    load = function(state) {
      ### Manual load start
      updateSelectInput(session, "country", selected = state$country)
      ### Manual load end
      updateSelectInput(session, "method", selected = state$method)
      updateSelectInput(session, "resolution", selected = state$resolution)
      updateSelectInput(session, "year", selected = state$year)
      shinyWidgets::updateMaterialSwitch(session, "log", value = state$log)
    }
  ))
})
}

agg_worldpop_module_map <- function(map, common) {
  raster_map(map, common, common$agg, "Population", common$meta$agg_worldpop$log)
}

agg_worldpop_module_rmd <- function(common) {
  # Variables used in the module's Rmd code
  list(
    agg_worldpop_knit = !is.null(common$meta$agg_worldpop$used),
    agg_worldpop_country = printVecAsis(common$meta$agg_worldpop$country),
    agg_worldpop_method = common$meta$agg_worldpop$method,
    agg_worldpop_resolution = common$meta$agg_worldpop$resolution,
    agg_worldpop_year = common$meta$agg_worldpop$year
  )
}

