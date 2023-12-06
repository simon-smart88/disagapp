resp_download_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    fileInput(inputId = ns("spread"),
              label = "Upload response spreadsheet",
              multiple = FALSE,
              accept = c(".csv", ".xlsx")),
    uiOutput(ns("area_column_out")),
    uiOutput(ns("resp_column_out")),
    uiOutput(ns("country_out")),
    selectInput(ns("admin"), "Administrative level", c("ADM1", "ADM2")),
    actionButton(ns("run"), "Load data")
  )
}

resp_download_module_server <- function(id, common) {
  moduleServer(id, function(input, output, session) {

    output$country_out <- country_out(session, common)

    df <- reactive({
      req(input$spread)
    file_format <- tools::file_ext(input$spread$datapath[1])
    if (file_format == "csv"){
      df <- read.csv(input$spread$datapath[1])
    } else if (file_format == "xlsx"){
      df <- openxlsx::read.xlsx(input$spread$datapath[1])
    } else {
      common$logger %>% writeLog("The uploaded file was not a .csv or .xlsx")
      return()
    }
    df
    })

    output$area_column_out <- renderUI({
      req(df())
      selectInput(session$ns("area_column"), "Select area column", c("", colnames(df())))
    })

    output$resp_column_out <- renderUI({
      req(df())
      selectInput(session$ns("response_column"), "Select response column", c("",colnames(df())))
    })

  observeEvent(input$run, {
    # WARNING ####
    if (input$response_column == "") {
      common$logger %>% writeLog(type = "error", "Please select the response column")
      return()
    }
    if (input$area_column == "") {
      common$logger %>% writeLog(type = "error", "Please select the area column")
      return()
    }

    # FUNCTION CALL ####
    show_loading_modal("Please wait while the data is loaded")
    country_code <- common$countries$ISO3[common$countries$NAME == input$country]
    shape <- resp_download(df = df(),
                         area_column = input$area_column,
                         resp_column = input$response_column,
                         country_code = country_code,
                         admin_level = input$admin,
                         logger = common$logger)

    close_loading_modal()
    # LOAD INTO COMMON ####
    common$shape <- shape
    common$selected_country <- input$country
    # METADATA ####
    common$meta$shape$download <- TRUE
    common$meta$shape$datapath <- input$spread$name[1]
    common$meta$shape$response <- input$response_column
    common$meta$shape$area_column <- input$area_column
    common$meta$shape$admin_level <- input$admin
    common$meta$shape$country <- country_code
    # TRIGGER
    gargoyle::trigger("resp_download")
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

resp_download_module_map <- function(map, common) {
  gargoyle::on("resp_download", {
    req(common$shape)
    ex <- as.vector(terra::ext(common$shape))
    common$add_map_layer("Response")
    response <- as.numeric(common$shape[[common$meta$shape$response]])
    pal <- colorBin("viridis", domain = response, bins = 9, na.color = "#00000000")
    map %>%
      clearGroup("Response") %>%
      addPolygons(data = common$shape, fillColor = ~pal(response), color = 'black', fillOpacity = 0.7, weight = 3, group = "Response") %>%
      fitBounds(lng1 = ex[[1]], lng2 = ex[[2]], lat1 = ex[[3]], lat2 = ex[[4]]) %>%
      addLegend(position = "bottomright", pal = pal, values = response, group = "Response", title = "Response") %>%
      addLayersControl(overlayGroups = common$map_layers, options = layersControlOptions(collapsed = FALSE))
  })
}

resp_download_module_rmd <- function(common) {
  # Variables used in the module's Rmd code
  list(
    resp_download_knit = common$meta$shape$download,
    data_path <- common$meta$shape$datapath,
    resp_column = common$meta$shape$resp_column,
    area_column = common$meta$shape$area_column,
    admin_level = common$meta$shape$admin_level,
    country_code = common$meta$shape$country
  )
}

