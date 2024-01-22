resp_shape_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    fileInput(inputId = ns("shape"),
              label = "Upload all shapefile data",
              multiple = TRUE,
              accept = c('.shp','.dbf','.sbn','.sbx','.shx','.prj')),
    checkboxInput(ns("example"), "Use example data", TRUE),
    uiOutput(ns("resp_var_out")),
    actionButton(ns("run"), "Load data")
  )
}

resp_shape_module_server <- function(id, common) {
  moduleServer(id, function(input, output, session) {

    #not using the usual paradigm in this module due to needing to specify
    #the response variable prior to running
    shape <- reactive({

      if (input$example == FALSE){
        req(input$shape)
        shpdf <- input$shape
      }

      if (input$example == TRUE){
        shpdf <- data.frame(datapath = list.files(system.file("extdata/shapes", package="disagapp"), full.names = TRUE),
                            name = list.files(system.file("extdata/shapes", package="disagapp")))
        updateSelectInput(session, "resp_var", selected = 'inc')
      }

      # WARNING ####
      if (nrow(shpdf) != 4) {
        common$logger %>% writeLog(type = "error", "Please upload four files")
        return()
      }

      # FUNCTION CALL ####
      shape_file_path <- shpdf$name[grep(pattern = "*.shp$", shpdf$name)]
      shape <- resp_shape(shpdf)

      # METADATA ####
      common$meta$resp_shape$used <- TRUE
      common$meta$resp_shape$path <- shape_file_path

      return(shape)
    }) %>% bindEvent(input$shape)


   output$resp_var_out <- renderUI({
     req(input$shape)
     req(shape())
     selectInput(session$ns("resp_var"), "Select response variable", c("", names(shape())))
   })


  observeEvent(input$run, {

    # WARNING ####
    if (input$resp_var == "") {
      common$logger %>% writeLog(type = "error", "Please select a response variable")
      return()
    }

    # commented out for now while example data is in use
    # if (is.null(input$shape)) {
    #   common$logger %>% writeLog(type = "error", "Please upload a shapefile")
    #   return()
    # }

    # LOAD INTO COMMON ####
    common$shape <- shape()

    # METADATA
    common$meta$resp_shape$response <- input$resp_var

    # TRIGGER
    gargoyle::trigger("resp_shape")
  })

  return(list(
    save = function() {
list(example = input$example, 
resp_var = input$resp_var)
    },
    load = function(state) {
updateCheckboxInput(session, "example", value = state$example) 
updateSelectInput(session, "resp_var", selected = state$resp_var)
    }
  ))
})
}

resp_shape_module_map <- function(map, common) {
  gargoyle::on("resp_shape", {
  req(common$shape)
  response <- as.numeric(common$shape[[common$meta$resp_shape$response]])
  ex <- as.vector(terra::ext(common$shape))
  common$add_map_layer("Response")
  pal <- colorBin("viridis", domain = response, bins = 9, na.color ="#00000000")
  map %>%
    clearGroup("Response") %>%
    addPolygons(data = common$shape, fillColor = ~pal(response), color = 'black', fillOpacity = 0.7, weight = 3, group = "Response", popup = ~as.character(round(response,0))) %>%
    fitBounds(lng1 = ex[[1]], lng2 = ex[[2]], lat1 = ex[[3]], lat2 = ex[[4]]) %>%
    addLegend(position = "bottomright", pal = pal, values = response, group = "Response", title = "Response") %>%
    addLayersControl(overlayGroups = common$map_layers, options = layersControlOptions(collapsed = FALSE))
  })
  }


resp_shape_module_rmd <- function(common) {
  # Variables used in the module's Rmd code
  list(
    resp_shape_knit = common$meta$resp_shape$used,
    resp_shape_path = common$meta$resp_shape$path,
    resp_shape_resp = common$meta$resp_shape$response
  )
}

