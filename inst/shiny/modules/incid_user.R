incid_user_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    fileInput(inputId = ns("shape"),
              label = "Upload all shapefile data",
              multiple = TRUE,
              accept = c('.shp','.dbf','.sbn','.sbx','.shx','.prj')),
    actionButton(ns("run"), "Load data")
  )
}

incid_user_module_server <- function(id, common) {
  moduleServer(id, function(input, output, session) {

  observeEvent(input$run, {

    shpdf <- input$shape

    # WARNING ####
    if (nrow(shpdf) != 4) {
      common$logger %>% writeLog(type = "error", "Please upload four files")
      return()
    }

    # FUNCTION CALL ####
    shape_file_path <- shpdf$name[grep(pattern = "*.shp$", shpdf$name)]
    shape <- incid_user(shpdf)
    # LOAD INTO COMMON ####
    common$shape <- shape
    # METADATA ####
    common$meta$shape$path <- shape_file_path
    # TRIGGER
    gargoyle::trigger("incid_user")
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

incid_user_module_map <- function(map, common) {
  req(common$shape)
  ex <- extent(common$shape)
  common$add_map_layer("Incidence")
  pal <- colorBin("YlOrRd", domain = as.numeric(common$shape$inc), bins = 9,na.color ="#00000000")
  map %>%
    clearGroup("Incidence") %>%
    addPolygons(data=common$shape,fillColor = ~ pal(as.numeric(common$shape$inc)),color='black',fillOpacity = 0.7,weight=3, group="Incidence") %>%
    fitBounds(lng1=ex@xmin,lng2=ex@xmax,lat1=ex@ymin,lat2=ex@ymax) %>%
    addLegend(position ="bottomright",pal = pal, values = as.numeric(common$shape$inc), group="Incidence", title="Incidence") %>%
    addLayersControl(overlayGroups = common$map_layers, options = layersControlOptions(collapsed = FALSE))
}

incid_user_module_rmd <- function(common) {
  # Variables used in the module's Rmd code
  list(
    incid_user_knit = !is.null(common$shape),
    incid_user_path = common$meta$shape$path
  )
}

