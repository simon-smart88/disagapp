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
    uiOutput(ns("reset_out")),
    actionButton(ns("run"), "Load data")
  )
}

resp_download_module_server <- function(id, common, parent_session, map) {
  moduleServer(id, function(input, output, session) {

    output$country_out <- country_out(session, common)

    observe({
      req(input$country != "")
      ADM_levels <- c("ADM1", "ADM2", "ADM3", "ADM4", "ADM5")
      country_code <- common$countries$boundaryISO[common$countries$boundaryName %in% input$country]
      boundary_metadata <- read.csv(system.file("extdata", "boundary_data.csv", package = "disagapp"))
      # -1 to exclude ADM0
      available_ADM <- boundary_metadata[boundary_metadata$boundaryISO %in% country_code,] |>
         dplyr::group_by(boundaryISO) |>
         dplyr::summarise(n = dplyr::n() - 1)
      # only select the highest level available for all countries
      n_ADM <- min(available_ADM$n)
      updateSelectInput(session, "admin", choices = ADM_levels[1:n_ADM])
    })

    df <- reactive({
      req(input$spread)
    file_format <- tools::file_ext(input$spread$datapath[1])
    if (file_format == "csv"){
      df <- read.csv(input$spread$datapath[1])
    } else if (file_format == "xlsx"){
      df <- openxlsx::read.xlsx(input$spread$datapath[1])
    } else {
      common$logger |> writeLog("The uploaded file was not a .csv or .xlsx")
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

    output$reset_out <- renderUI({
      reset_data_ui(session, common)
    })

  observeEvent(input$run, {
    # WARNING ####
    if (curl::has_internet() == FALSE){
      common$logger |> writeLog(type = "error", "This module requires an internet connection")
      return()
    }

    if (!is.null(input$reset) && (input$reset == FALSE)){
      common$logger |> writeLog(type = "error",
                                 "Uploading new response data will delete all the existing data - toggle the switch and press the button again to continue")
      return()
    }

    if (is.null(input$spread)) {
      common$logger |> writeLog(type = "error", "Please upload a spreadsheet")
      return()
    }

    if (input$country[1] == "") {
      common$logger |> writeLog(type = "error", "Please select a country")
      return()
    }

    if (input$response_column == "") {
      common$logger |> writeLog(type = "error", "Please select the response column")
      return()
    }
    if (input$area_column == "") {
      common$logger |> writeLog(type = "error", "Please select the area column")
      return()
    }

    # FUNCTION CALL ####
    show_loading_modal("Please wait while the data is loaded")
    country_code <- common$countries$boundaryISO[common$countries$boundaryName %in% input$country]
    shape <- resp_download(df = df(),
                         area_column = input$area_column,
                         resp_column = input$response_column,
                         country_code = country_code,
                         admin_level = input$admin,
                         logger = common$logger)

    close_loading_modal()
    if (!is.null(shape)){
      # LOAD INTO COMMON ####
      reset_data(common)
      common$shape <- shape
      common$selected_country <- input$country
      common$response_name <- input$response_column
      # METADATA ####
      common$meta$resp_download$used <- TRUE
      common$meta$resp_download$datapath <- input$spread$name[1]
      common$meta$resp_download$response <- input$response_column
      common$meta$resp_download$area_column <- input$area_column
      common$meta$resp_download$admin_level <- input$admin
      common$meta$resp_download$country <- country_code
      # TRIGGER
      trigger("resp_download")
      trigger("country_out")
      do.call("resp_download_module_map", list(map, common))
      common$logger |> writeLog(type = "complete", "Response data has been uploaded and is summarised in the results tab")

      response <- common$shape[[common$response_name]]
      if (!isTRUE(all.equal(response, as.integer(response)))){
        common$logger |> writeLog(type = "info", "Some response data is not integers")
      }
      if (any(response < 0)){
        common$logger |> writeLog(type = "info", "The response data contains negative values")
      }

    }
  })

  output$plot <- plotly::renderPlotly({
    req(common$shape)
    watch("resp_download")
    watch("resp_edit")
    response <- common$shape[[common$response_name]]
    plot_response(response)
  })

  output$table <- DT::renderDataTable({
    req(common$shape)
    watch("resp_download")
    watch("resp_edit")
    common$shape |> sf::st_drop_geometry()
  })

  return(list(
    save = function() {list(
      ### Manual save start
      country = input$country,
      ### Manual save end
      admin = input$admin,
      area_column = input$area_column,
      response_column = input$response_column)
    },
    load = function(state) {
      ### Manual load start
      updateSelectInput(session, "country", selected = state$country)
      ### Manual load end
      updateSelectInput(session, "admin", selected = state$admin)
      updateSelectInput(session, "area_column", selected = state$area_column)
      updateSelectInput(session, "response_column", selected = state$response_column)
    }
  ))
})
}

resp_download_module_result <- function(id) {
  ns <- NS(id)
  tagList(
    plotly::plotlyOutput(ns("plot")),
    DT::dataTableOutput(ns("table"))
  )
}

resp_download_module_map <- function(map, common) {
  shape_map(map, common)
}

resp_download_module_rmd <- function(common) {
  # Variables used in the module's Rmd code
  list(
    resp_download_knit = !is.null(common$meta$resp_download$used),
    resp_download_data_path = common$meta$resp_download$datapath,
    resp_download_resp_column = common$meta$resp_download$response,
    resp_download_area_column = common$meta$resp_download$area_column,
    resp_download_admin_level = common$meta$resp_download$admin_level,
    resp_download_country_code = common$meta$resp_download$country,
    resp_download_response_name = common$response_name
  )
}

