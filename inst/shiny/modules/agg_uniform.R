agg_uniform_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    actionButton(ns("run"), "Create uniform raster")
  )
}

agg_uniform_module_server <- function(id, common) {
  moduleServer(id, function(input, output, session) {

  observeEvent(input$run, {
    # WARNING ####
    if (is.null(common$covs)) {
      common$logger %>% writeLog(type = "error", "Please load covariate data first")
      return()
    }

    # FUNCTION CALL ####
    agg <- agg_uniform(common$covs[[1]])

    # LOAD INTO COMMON ####
    common$agg <- agg

    # METADATA ####
    common$meta$uniform$used <- TRUE

    # TRIGGER
    gargoyle::trigger("agg_uniform")
  })

})
}

agg_uniform_module_rmd <- function(common) {
  # Variables used in the module's Rmd code
  list(
    agg_uniform_knit = common$meta$uniform$used
  )
}

