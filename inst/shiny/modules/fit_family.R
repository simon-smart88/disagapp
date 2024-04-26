fit_family_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    radioButtons(ns("family"), "Model family", c("Gaussian" = "gaussian", "Poisson" = "poisson", "Binomial" = "binomial"), selected = "poisson"),
    actionButton(ns("run"), "Save choice")
  )
}

fit_family_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {

  observeEvent(input$run, {
    # LOAD INTO COMMON ####
    common$family <- input$family

    # METADATA ####
    common$meta$fit_family$used <- TRUE
    common$meta$fit_family$family <- input$family
    # TRIGGER
    gargoyle::trigger("fit_family")
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

fit_family_module_rmd <- function(common) {
  # Variables used in the module's Rmd code
  list(
    fit_family_knit = !is.null(common$meta$fit_family$used),
    fit_family = common$meta$fit_family$family
  )
}

