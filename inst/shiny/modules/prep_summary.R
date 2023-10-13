prep_summary_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    # UI
    actionButton(ns("run"), "Prepare covariate summary")
  )
}

prep_summary_module_server <- function(id, common) {
  moduleServer(id, function(input, output, session) {

  observeEvent(input$run, {
    # WARNING ####

    # FUNCTION CALL ####
    cov_summary <- prep_summary(common$covs)
    # LOAD INTO COMMON ####
    common$cov_sum <- cov_summary
    # METADATA ####

    # TRIGGER
    gargoyle::trigger("prep_summary")
  })

  output$result <- DT::renderDataTable({
    common$cov_sum
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

prep_summary_module_result <- function(id) {
  ns <- NS(id)

  # Result UI

  DT::dataTableOutput(ns("result"))
}

prep_summary_module_rmd <- function(common) {
  # Variables used in the module's Rmd code
  list(
    prep_summary_knit = !is.null(common$some_object),
    var1 = common$meta$setting1,
    var2 = common$meta$setting2
  )
}

