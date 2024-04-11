resp_shape_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    fileInput(inputId = ns("shape"),
              label = "Upload all shapefile data",
              multiple = TRUE,
              accept = c('.shp','.dbf','.sbn','.sbx','.shx','.prj')),
    uiOutput(ns("resp_var_out")),
    actionButton(ns("run"), "Load data")
  )
}

resp_shape_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {

    #not using the usual paradigm in this module due to needing to specify
    #the response variable prior to running
    shape <- reactive({

      req(input$shape)
      shpdf <- input$shape

      # WARNING ####
      if (nrow(shpdf) != 4) {
        common$logger %>% writeLog(type = "error", "Please upload four files")
        return()
      }

      # FUNCTION CALL ####
      #keeping this for now, but only needed if the Rmd uses sf
      #shape_file_path <- shpdf$name[grep(pattern = "*.shp$", shpdf$name)]
      shape <- resp_shape(shpdf)

      # METADATA ####
      common$meta$resp_shape$used <- TRUE
      common$meta$resp_shape$path <- shpdf$name

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

    if (is.null(input$shape)) {
      common$logger %>% writeLog(type = "error", "Please upload a shapefile")
      return()
    }

    # LOAD INTO COMMON ####
    common$reset()
    common$shape <- shape()

    # METADATA
    common$meta$resp_shape$response <- input$resp_var

    # TRIGGER
    gargoyle::trigger("resp_shape")
  })

  return(list(
    save = function() {
list(
resp_var = input$resp_var)
    },
    load = function(state) {
updateSelectInput(session, "resp_var", selected = state$resp_var)
    }
  ))
})
}

resp_shape_module_map <- function(map, common) {
  response <- as.numeric(common$shape[[common$meta$resp_shape$response]])
  shape_map(map, common, response)
}


resp_shape_module_rmd <- function(common) {
  # Variables used in the module's Rmd code
  list(
    resp_shape_knit = !is.null(common$meta$resp_shape$used),
    resp_shape_path = printVecAsis(common$meta$resp_shape$path),
    resp_shape_resp = common$meta$resp_shape$response
  )
}

