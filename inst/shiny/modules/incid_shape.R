incid_shape_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    fileInput(inputId = ns("shape"),
              label = "Upload all shapefile data",
              multiple = TRUE,
              accept = c('.shp','.dbf','.sbn','.sbx','.shx','.prj')),
    checkboxInput(ns("example"), "Use example data", TRUE),
    actionButton(ns("run"), "Load data")
  )
}

incid_shape_module_server <- function(id, common) {
  moduleServer(id, function(input, output, session) {

  observeEvent(input$run, {

    if (input$example == FALSE){
      shpdf <- input$shape
    }

    if (input$example == TRUE){
      shpdf <- data.frame(datapath = list.files(system.file("extdata/shapes", package="shinydisag"), full.names = TRUE),
                          name = list.files(system.file("extdata/shapes", package="shinydisag")))
    }

    # WARNING ####
    if (nrow(shpdf) != 4) {
      common$logger %>% writeLog(type = "error", "Please upload four files")
      return()
    }

    # FUNCTION CALL ####
    shape_file_path <- shpdf$name[grep(pattern = "*.shp$", shpdf$name)]
    shape <- incid_shape(shpdf)
    # LOAD INTO COMMON ####
    common$shape <- shape
    # METADATA ####
    common$meta$shape$path <- shape_file_path
    # TRIGGER
    gargoyle::trigger("incid_shape")
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

incid_shape_module_map <- function(map, common) {
  observeEvent(gargoyle::watch("incid_shape"), {
  req(common$shape)
  ex <- as.vector(terra::ext(common$shape))
  common$add_map_layer("Incidence")
  pal <- colorBin("YlOrRd", domain = as.numeric(common$shape$inc), bins = 9,na.color ="#00000000")
  map %>%
    clearGroup("Incidence") %>%
    addPolygons(data = common$shape, fillColor = ~pal(as.numeric(common$shape$inc)), color = 'black', fillOpacity = 0.7, weight = 3, group = "Incidence") %>%
    fitBounds(lng1 = ex[[1]], lng2 = ex[[2]], lat1 = ex[[3]], lat2 = ex[[4]]) %>%
    addLegend(position = "bottomright", pal = pal, values = as.numeric(common$shape$inc), group = "Incidence", title = "Incidence") %>%
    addLayersControl(overlayGroups = common$map_layers, options = layersControlOptions(collapsed = FALSE))
  })
  }


incid_shape_module_rmd <- function(common) {
  # Variables used in the module's Rmd code
  list(
    incid_shape_knit = !is.null(common$shape),
    incid_shape_path = common$meta$shape$path
  )
}

