agg_worldpop_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    # UI
    uiOutput(ns("country_out")),
    checkboxInput(ns("log"),
                  label = 'Plot as log values',
                  value = TRUE),
    actionButton(ns("run"), "Fetch data")
  )
}

agg_worldpop_module_server <- function(id, common) {
  moduleServer(id, function(input, output, session) {

  output$country_out <- country_out(session, common)

  observeEvent(input$run, {
    # WARNING ####
    if (is.null(input$country)) {
      common$logger %>% writeLog(type = "error", "Please select a country")
      return()
    }
    # FUNCTION CALL ####
    show_loading_modal("Please wait while the data is loaded")
    country_code <- common$countries$ISO3[common$countries$NAME == input$country]
    agg_ras <- agg_worldpop(country_code)

    # LOAD INTO COMMON ####
    common$agg <- agg_ras
    common$selected_country <- input$country
    close_loading_modal()
    common$logger %>% writeLog("Worldpop data has been downloaded")

    # METADATA ####
    common$meta$agg$name <- "Population"
    common$meta$agg$log <- input$log

    # TRIGGER
    gargoyle::trigger("agg_worldpop")
    gargoyle::trigger("country_out")
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

agg_worldpop_module_map <- function(map, common) {
    observeEvent(gargoyle::watch("agg_worldpop"), {
      req(common$agg)
      common$add_map_layer(common$meta$agg$name)
      if (common$meta$agg$log == TRUE){
        agg_map_raster = log10(common$agg)
        agg_map_title = paste0(common$meta$agg$name, " (log 10)")
        pal <- colorBin("YlOrRd", domain = log10(terra::values(common$agg)), bins = 9, na.color = "#00000000")
      } else {
        agg_map_raster = common$agg
        agg_map_title = common$meta$agg$name
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
    agg_worldpop_knit = !is.null(common$some_object),
    var1 = common$meta$setting1,
    var2 = common$meta$setting2
  )
}

