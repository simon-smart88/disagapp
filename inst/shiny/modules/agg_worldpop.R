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

  countries <- readRDS(system.file("ex/countries.rds", package="geodata"))

  output$country_out <- renderUI({
    selectInput(session$ns("country"), "Select country", countries$NAME)
  })

  observeEvent(input$run, {
    # WARNING ####

    # FUNCTION CALL ####
    country_code <- countries$ISO3[countries$NAME == input$country]
    agg_ras <- agg_worldpop(country_code)
    # LOAD INTO COMMON ####
    common$agg <- agg_ras
    # METADATA ####
    common$meta$agg$name <- "Population"
    common$meta$agg$log <- input$log
    # TRIGGER
    gargoyle::trigger("agg_worldpop")
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

