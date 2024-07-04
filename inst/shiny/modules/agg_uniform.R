agg_uniform_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    actionButton(ns("run"), "Create uniform raster")
  )
}

agg_uniform_module_server <- function(id, common, parent_session, map) {
  moduleServer(id, function(input, output, session) {

  observeEvent(input$run, {
    # WARNING ####
    if (length(common$covs) == 0) {
      common$logger |> writeLog(type = "error", "Please load covariate data first")
      return()
    }

    # FUNCTION CALL ####
    agg <- agg_uniform(common$covs[[1]])

    # LOAD INTO COMMON ####
    common$agg <- agg

    # METADATA ####
    common$meta$agg_uniform$used <- TRUE

    # TRIGGER
    gargoyle::trigger("agg_uniform")
    common$logger |> writeLog(type = "complete", "A uniform aggregation raster has been created")
  })

})
}

agg_uniform_module_rmd <- function(common) {
  # Variables used in the module's Rmd code
  list(
    agg_uniform_knit = !is.null(common$meta$agg_uniform$used)
  )
}

