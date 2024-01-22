prep_summary_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    # UI
    actionButton(ns("run"), "Prepare covariate summary"),
    checkboxInput(ns("remove"), "Remove identical columns?", FALSE),
    actionButton(ns("resample"), "Resample covariates")
  )
}

prep_summary_module_server <- function(id, common) {
  moduleServer(id, function(input, output, session) {

  observeEvent(input$run, {
    # WARNING ####

    # FUNCTION CALL ####
    cov_summary <- prep_summary(common$covs, remove = input$remove)
    # LOAD INTO COMMON ####
    common$cov_sum <- cov_summary
    # METADATA ####

    # TRIGGER
    gargoyle::trigger("prep_summary")
  })

    observeEvent(input$resample, {
      # WARNING ####
      req(input$result_rows_selected)
      # FUNCTION CALL ####
      common$covs <- lapply(common$covs, terra::resample, common$covs[[input$result_rows_selected]])
      common$cov_sum <- prep_summary(common$covs, remove = input$remove)
      # LOAD INTO COMMON ####

      # METADATA ####

      # TRIGGER
      gargoyle::trigger("prep_summary")
    })

  output$result <- DT::renderDataTable({
    gargoyle::watch("prep_summary")
    DT::datatable(common$cov_sum,selection = 'single')
  })

  return(list(
    save = function() {
list(remove = input$remove)
    },
    load = function(state) {
updateCheckboxInput(session, "remove", value = common$state$remove)
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

