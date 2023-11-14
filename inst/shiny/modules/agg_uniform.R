agg_uniform_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    # UI
    actionButton(ns("run"), "Create uniform raster")
  )
}

agg_uniform_module_server <- function(id, common) {
  moduleServer(id, function(input, output, session) {

  observeEvent(input$run, {
    # WARNING ####

    # FUNCTION CALL ####
    agg <- agg_uniform(common$covs[[1]])

    # LOAD INTO COMMON ####
    common$agg <- agg

    # METADATA ####

    # TRIGGER
    gargoyle::trigger("agg_uniform")
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

agg_uniform_module_rmd <- function(common) {
  # Variables used in the module's Rmd code
  list(
    agg_uniform_knit = !is.null(common$some_object),
    var1 = common$meta$setting1,
    var2 = common$meta$setting2
  )
}

