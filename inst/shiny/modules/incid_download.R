incid_download_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    fileInput(inputId = ns("spread"),
              label = "Upload incidence spreadsheet",
              multiple = FALSE,
              accept = c(".csv", ".xlsx")),
    uiOutput(ns("area_column_out")),
    uiOutput(ns("incid_column_out")),
    uiOutput(ns("country_out")),
    selectInput(ns("admin"), "Administrative level", c("ADM1", "ADM2")),
    actionButton(ns("run"), "Run module incid_download")
  )
}

incid_download_module_server <- function(id, common) {
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
      selectInput(session$ns("area_column"), "Select area column", colnames(df()))
    })

    output$incid_column_out <- renderUI({
      req(df())
      selectInput(session$ns("response_column"), "Select response column", colnames(df()))
    })

  observeEvent(input$run, {
    # WARNING ####

    # FUNCTION CALL ####
    show_loading_modal("Please wait while the data is loaded")
    country_code <- common$countries$ISO3[common$countries$NAME == input$country]
    shape <- incid_download(df = df(),
                         area_column = input$area_column,
                         incid_column = input$response_column,
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
    gargoyle::trigger("incid_download")
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

incid_download_module_map <- function(map, common) {
  observeEvent(gargoyle::watch("incid_download"), {
    req(common$shape)
    ex <- as.vector(terra::ext(common$shape))
    common$add_map_layer("Incidence")
    response <- as.numeric(common$shape[[common$meta$shape$incid_column]])
    pal <- colorBin("viridis", domain = response, bins = 9, na.color = "#00000000")
    map %>%
      clearGroup("Incidence") %>%
      addPolygons(data = common$shape, fillColor = ~pal(response), color = 'black', fillOpacity = 0.7, weight = 3, group = "Incidence") %>%
      fitBounds(lng1 = ex[[1]], lng2 = ex[[2]], lat1 = ex[[3]], lat2 = ex[[4]]) %>%
      addLegend(position = "bottomright", pal = pal, values = response, group = "Incidence", title = "Incidence") %>%
      addLayersControl(overlayGroups = common$map_layers, options = layersControlOptions(collapsed = FALSE))
  })
}

incid_download_module_rmd <- function(common) {
  # Variables used in the module's Rmd code
  list(
    incid_download_knit = common$meta$shape$download,
    data_path <- common$meta$shape$datapath,
    incid_column = common$meta$shape$incid_column,
    area_column = common$meta$shape$area_column,
    admin_level = common$meta$shape$admin_level,
    country_code = common$meta$shape$country
  )
}

