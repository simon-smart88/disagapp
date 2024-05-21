agg_worldpop_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    # UI
    uiOutput(ns("country_out")),
    selectInput(ns("method"), "Method", choices = c("Constrained", "Unconstrained")),
    selectInput(ns("resolution"), "Resolution", choices = c("1km", "100m")),
    uiOutput(ns("year_out")),
    shinyWidgets::materialSwitch(ns("log"), label = 'Plot as log values', value = TRUE, status = "success"),
    actionButton(ns("run"), "Fetch data")
  )
}

agg_worldpop_module_server <- function(id, common, parent_session) {
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

  observeEvent(input$run, {
    # WARNING ####
    if (curl::has_internet() == FALSE){
      common$logger %>% writeLog(type = "error", "This module requires an internet connection")
      return()
    }

    if (is.null(input$country)) {
      common$logger %>% writeLog(type = "error", "Please select a country")
      return()
    }
    # FUNCTION CALL ####
    show_loading_modal("Please wait while the data is loaded")
    country_code <- common$countries$ISO3[common$countries$NAME == input$country]
    agg_ras <- agg_worldpop(common$shape, country_code, input$method, input$resolution, input$year, common$logger)

    # LOAD INTO COMMON ####
    common$agg <- agg_ras
    common$selected_country <- input$country
    close_loading_modal()
    common$logger %>% writeLog("Worldpop data has been downloaded")

    # METADATA ####
    common$meta$agg_worldpop$name <- "Population"
    common$meta$agg_worldpop$log <- input$log
    common$meta$agg_worldpop$used <- TRUE
    common$meta$agg_worldpop$country <- country_code
    common$meta$agg_worldpop$method <- input$method
    common$meta$agg_worldpop$resolution <- input$resolution
    common$meta$agg_worldpop$year <- input$year

    # TRIGGER
    gargoyle::trigger("agg_worldpop")
    gargoyle::trigger("country_out")
  })

  return(list(
    save = function() {
list(method = input$method, 
resolution = input$resolution, 
year = input$year, 
log = input$log)
    },
    load = function(state) {
updateSelectInput(session, "method", selected = state$method) 
updateSelectInput(session, "resolution", selected = state$resolution) 
updateSelectInput(session, "year", selected = state$year) 
shinyWidgets::updateMaterialSwitch(session, "log", value = state$log)
    }
  ))
})
}

agg_worldpop_module_map <- function(map, common) {
  covariate_map(map, common, common$agg, "Population", common$meta$agg_worldpop$log)
}

agg_worldpop_module_rmd <- function(common) {
  # Variables used in the module's Rmd code
  list(
    agg_worldpop_knit = !is.null(common$meta$agg_worldpop$used),
    agg_worldpop_country = common$meta$agg_worldpop$country,
    agg_worldpop_method = common$meta$agg_worldpop$method,
    agg_worldpop_resolution = common$meta$agg_worldpop$resolution,
    agg_worldpop_year = common$meta$agg_worldpop$year
  )
}

