agg_worldpop_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    # UI
    uiOutput(ns("country_out")),
    selectInput(ns("method"), "Method", choices = c("Constrained", "Unconstrained")),
    selectInput(ns("resolution"), "Resolution", choices = c("1km", "100m")),
    uiOutput(ns("year_out")),
    checkboxInput(ns("log"),
                  label = 'Plot as log values',
                  value = TRUE),
    actionButton(ns("run"), "Fetch data")
  )
}

agg_worldpop_module_server <- function(id, common) {
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
    if (is.null(input$country)) {
      common$logger %>% writeLog(type = "error", "Please select a country")
      return()
    }
    # FUNCTION CALL ####
    show_loading_modal("Please wait while the data is loaded")
    country_code <- common$countries$ISO3[common$countries$NAME == input$country]
    agg_ras <- agg_worldpop(country_code, input$method, input$resolution, input$year, common$logger)

    # LOAD INTO COMMON ####
    common$agg <- agg_ras
    common$selected_country <- input$country
    close_loading_modal()
    common$logger %>% writeLog("Worldpop data has been downloaded")

    # METADATA ####
    common$meta$worldpop$name <- "Population"
    common$meta$worldpop$log <- input$log
    common$meta$worldpop$used <- TRUE
    common$meta$worldpop$country <- country_code
    common$meta$worldpop$method <- input$method
    common$meta$worldpop$resolution <- input$resolution
    common$meta$worldpop$year <- input$year

    # TRIGGER
    gargoyle::trigger("agg_worldpop")
    gargoyle::trigger("country_out")
  })

  return(list(
    save = function() {
list(method = input$method, 
resolution = input$resolution, 
log = input$log, 
year = input$year)
    },
    load = function(state) {
updateSelectInput(session, "method", selected = state$method) 
updateSelectInput(session, "resolution", selected = state$resolution) 
updateCheckboxInput(session, "log", value = state$log) 
updateSelectInput(session, "year", selected = state$year)
    }
  ))
})
}

agg_worldpop_module_map <- function(map, common) {
    gargoyle::on("agg_worldpop", {
      req(common$agg)
      common$add_map_layer(common$meta$worldpop$name)
      if (common$meta$worldpop$log == TRUE){
        agg_map_raster = log10(common$agg)
        agg_map_title = paste0(common$meta$worldpop$name, " (log 10)")
        pal <- colorBin("YlOrRd", domain = log10(terra::values(common$agg)), bins = 9, na.color = "#00000000")
      } else {
        agg_map_raster = common$agg
        agg_map_title = common$meta$worldpop$name
      }
      map %>%
        clearGroup(common$meta$agg$name) %>%
        addRasterImage(raster::raster(agg_map_raster), group = common$meta$agg$name, colors = pal) %>%
        addLegend(position = "bottomleft", pal = pal, values = values(agg_map_raster), group = common$meta$agg$name, title = agg_map_title) %>%
        addLayersControl(overlayGroups = common$map_layers, options = layersControlOptions(collapsed = FALSE))
    })
}

agg_worldpop_module_rmd <- function(common) {
  # Variables used in the module's Rmd code
  list(
    agg_worldpop_knit = common$meta$worldpop$used,
    agg_worldpop_country = common$meta$worldpop$country,
    agg_worldpop_method = common$meta$worldpop$method,
    agg_worldpop_resolution = common$meta$worldpop$resolution,
    agg_worldpop_year = common$meta$worldpop$year
  )
}

