cov_water_module_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("token_out")),
    input_task_button(ns("run"), "Load data", type = "default", icon = icon("arrow-turn-down"))
  )
}

cov_water_module_server <- function(id, common, parent_session, map) {
  moduleServer(id, function(input, output, session) {

    #use the environmental variable if set, if not display box to enter it
    output$token_out <- renderUI({
      if (Sys.getenv("ARCGIS_CLIENT") == ""){
        textInput(session$ns("token"), "ArcGIS token")}
    })

    token <- reactive({
      if (Sys.getenv("ARCGIS_CLIENT") != ""){
        token <- arcgisutils::auth_client()
      } else {
        token <- httr2::oauth_token(input$token, arcgis_host = arcgisutils::arc_host())
      }
      token
    })

  common$tasks$cov_water <- ExtendedTask$new(function(...) {
    promises::future_promise({
      cov_water(...)
    }, seed = TRUE)
  }) |> bind_task_button("run")

  observeEvent(input$run, {
    # WARNING ####
    if (curl::has_internet() == FALSE){
      common$logger |> writeLog(type = "error", "This module requires an internet connection")
      return()
    }

    if (is.null(common$shape)) {
      common$logger |> writeLog(type = "error", "Please upload response data first")
      return()
    }

    if ((Sys.getenv("ARCGIS_CLIENT") == "") && (input$token == "")){
      common$logger |> writeLog(type = "error", "An ArcGIS token is required to use this module.
                          See the module guidance for details of how to obtain one.")
      return()
    }

    if (length(common$covs_prep) > 0) {
      common$logger |> writeLog(type = "warning", "You will need to prepare the data
                                again to include distance to water data in the model")
    }

    # FUNCTION CALL ####
    common$tasks$cov_water$invoke(common$shape, token(), TRUE)
    common$logger |> writeLog(type = "starting", "Starting to download distance to water data")
    # METADATA ####
    common$meta$cov_water$used <- TRUE
    common$meta$cov_water$token <- input$token
  })

  results <- observe({
    # LOAD INTO COMMON ####
    result <- common$tasks$cov_water$result()
    results$suspend()
    if (inherits(result, "PackedSpatRaster")){
      common$covs[["Distance to water"]] <- unwrap_terra(result)
      common$logger |> writeLog(type = "complete", "Distance to water data has been downloaded")
      # TRIGGER
      do.call("cov_water_module_map", list(map, common))
      trigger("cov_water")
      shinyjs::runjs("Shiny.setInputValue('cov_water-complete', 'complete');")
    } else {
      common$logger |> writeLog(type = "error", result)
    }
  })

  output$plot <- renderPlot({
    watch("cov_water")
    req(common$meta$cov_water)
    plot_raster(common$covs, "Distance to water")
  })

})
}

cov_water_module_result <- function(id) {
  ns <- NS(id)
  plotOutput(ns("plot"))
}

cov_water_module_map <- function(map, common) {
  raster_map(map, common, common$covs[["Distance to water"]], "Distance to water")
}

cov_water_module_rmd <- function(common) {
  # Variables used in the module's Rmd code
  list(
    cov_water_knit = !is.null(common$meta$cov_water$used),
    cov_water_token = common$meta$cov_water$token
  )
}

