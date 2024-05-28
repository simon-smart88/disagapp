resp_combine_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    fileInput(inputId = ns("spread"),
              label = "Upload response spreadsheet",
              multiple = FALSE,
              accept = c(".csv", ".xlsx")),
    uiOutput(ns("spread_area_column_out")),
    uiOutput(ns("spread_resp_column_out")),
    fileInput(inputId = ns("shape"),
              label = "Upload all shapefile data",
              multiple = TRUE,
              accept = c('.shp','.dbf','.sbn','.sbx','.shx','.prj')),
    uiOutput(ns("shape_area_column_out")),
    uiOutput(ns("reset_out")),
    actionButton(ns("run"), "Combine data")
  )
}

resp_combine_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {

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
      common$meta$resp_combine$spread_path <- input$spread$name[1]

      df
    }) %>% bindEvent(input$spread)

    output$spread_area_column_out <- renderUI({
      req(df())
      selectInput(session$ns("spread_area_column"), "Select spreadsheet area column", c("", colnames(df())))
    })

    output$spread_resp_column_out <- renderUI({
      req(df())
      selectInput(session$ns("spread_response_column"), "Select spreadsheet response column", c("", colnames(df())))
    })

    output$reset_out <- renderUI({
      reset_data_ui(session, common)
    })

    shape <- reactive({

        req(input$shape)
        shpdf <- input$shape

      # WARNING ####
      if (nrow(shpdf) != 4) {
        common$logger %>% writeLog(type = "error", "Please upload four files")
        return()
      }

      # FUNCTION CALL ####
      shape_file_path <- shpdf$name[grep(pattern = "*.shp$", shpdf$name)]
      shape <- resp_shape(shpdf)

      # METADATA ####
      common$meta$resp_combine$shape_path <- shape_file_path

      return(shape)
    }) %>% bindEvent(input$shape)

    output$shape_area_column_out <- renderUI({
      req(shape())
      selectInput(session$ns("shape_area_column"), "Select shapefile area column", c("", names(shape())))
    })


  observeEvent(input$run, {
    # WARNING ####
    if (!is.null(input$reset) && (input$reset == FALSE)){
      common$logger %>% writeLog(type = "error",
                                 "Uploading new response data will delete all the existing data - toggle the switch and press the button again to continue")
      return()
    }

    if (is.null(input$shape)) {
      common$logger %>% writeLog(type = "error", "Please upload a shapefile")
      return()
    }

    if (is.null(input$spread)) {
      common$logger %>% writeLog(type = "error", "Please upload a spreadsheet")
      return()
    }

    if (input$spread_response_column == "") {
      common$logger %>% writeLog(type = "error", "Please select the spreadsheet response column")
      return()
    }
    if (input$spread_area_column == "") {
      common$logger %>% writeLog(type = "error", "Please select the spreadsheet area column")
      return()
    }
    if (input$shape_area_column == "") {
      common$logger %>% writeLog(type = "error", "Please select the shapefile area column")
      return()
    }

    # FUNCTION CALL ####
    shape <- resp_combine(df(), input$spread_area_column, input$spread_response_column, shape(), input$shape_area_column, common$logger)
    # LOAD INTO COMMON ####
    common$reset()
    gargoyle::trigger("clear_map")
    common$shape <- shape
    common$response_name <- input$spread_response_column
    # METADATA ####
    common$meta$resp_combine$used <- TRUE
    common$meta$resp_combine$response <- input$spread_response_column
    common$meta$resp_combine$spread_area <- input$spread_area_column
    common$meta$resp_combine$shape_area <- input$shape_area_column

    # TRIGGER
    gargoyle::trigger("resp_combine")
  })

  return(list(
    save = function() {
list(spread_area_column = input$spread_area_column,
spread_response_column = input$spread_response_column,
shape_area_column = input$shape_area_column)
    },
    load = function(state) {
updateSelectInput(session, "spread_area_column", selected = state$spread_area_column)
updateSelectInput(session, "spread_response_column", selected = state$spread_response_column)
updateSelectInput(session, "shape_area_column", selected = state$shape_area_column)
    }
  ))
})
}

resp_combine_module_map <- function(map, common) {
  shape_map(map, common)
}

resp_combine_module_rmd <- function(common) {
  # Variables used in the module's Rmd code
  list(
    resp_combine_knit = !is.null(common$meta$resp_combine$used),
    resp_combine_shape_path = common$meta$resp_combine$shape_path,
    resp_combine_spread_path = common$meta$resp_combine$spread_path,
    resp_combine_response = common$meta$resp_combine$response,
    resp_combine_spread_area = common$meta$resp_combine$spread_area,
    resp_combine_shape_area = common$meta$resp_combine$shape_area
  )

}

