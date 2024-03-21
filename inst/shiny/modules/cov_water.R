cov_water_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    uiOutput(ns("token_out")),
    actionButton(ns("run"), "Download data")
  )
}

cov_water_module_server <- function(id, common, parent_session) {
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
        token = httr2::oauth_token(input$token, arcgis_host = arcgisutils::arc_host())
      }
      token
    })

  observeEvent(input$run, {
    # WARNING ####
    if (is.null(common$shape)) {
      common$logger %>% writeLog(type = "error", "Please upload response data first")
      return()
    }
    if ((Sys.getenv("ARCGIS_CLIENT") == "") & (input$token == "")){
      common$logger %>% writeLog(type = "error", "An ArcGIS token is required to use this module.
                          See the module guidance for details of how to obtain one.")
      return()
    }
    # FUNCTION CALL ####
    show_loading_modal("Please wait while the data is loaded")
    water <- cov_water(common$shape, token(), common$logger)
    # LOAD INTO COMMON ####
    common$covs[["Distance to water"]] <- water
    common$logger %>% writeLog("Distance to water data has been downloaded")
    close_loading_modal()
    # METADATA ####
    common$meta$cov_water$used <- TRUE
    # TRIGGER
    gargoyle::trigger("cov_water")
  })

})
}

cov_water_module_map <- function(map, common) {
  covariate_map(map, common, common$covs[["Distance to water"]], "Distance to water")
}

cov_water_module_rmd <- function(common) {
  # Variables used in the module's Rmd code
  list(
    cov_water_knit = !is.null(common$meta$cov_water$used)
  )
}

