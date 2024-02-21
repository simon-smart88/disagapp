fit_fit_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    radioButtons(ns("family"), "Model family", c("gaussian", "poisson", "binomial"), selected = "poisson"),
    radioButtons(ns("link"), "Model link", c("logit", "log", "identity"), selected = "log"),
    checkboxInput(ns("field"), "Use field", value = TRUE),
    checkboxInput(ns("iid"), "iid", value = TRUE),
    actionButton(ns("run"), "Fit model")
  )
}

fit_fit_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {

  observeEvent(input$run, {
    # WARNING ####
    if (is.null(common$prep)) {
      common$logger %>% writeLog(type = "error", "Please prepare the data first")
      return()
    }
    # FUNCTION CALL ####

    show_loading_modal("Please wait while the model is fitted")
    common$fit <- suppressMessages(disaggregation::disag_model(data = common$prep,
                                          family = input$family,
                                          link = input$link,
                                          field = input$field,
                                          iid = input$iid))
    close_loading_modal()
    common$logger %>% writeLog('Model fitting has completed')
    # LOAD INTO COMMON ####

    # METADATA ####
    common$meta$fit_fit$family <- input$family
    common$meta$fit_fit$link <- input$link
    common$meta$fit_fit$iid <- input$iid
    # TRIGGER
    gargoyle::trigger("fit_fit")
  })

    output$model_plot <- renderPlot({
      gargoyle::watch("fit_fit")
      req(common$fit)
      plot(common$fit)
    })

  return(list(
    save = function() {
list(field = input$field,
iid = input$iid,
family = input$family,
link = input$link)
    },
    load = function(state) {
updateCheckboxInput(session, "field", value = state$field)
updateCheckboxInput(session, "iid", value = state$iid)
updateRadioButtons(session, "family", selected = state$family)
updateRadioButtons(session, "link", selected = state$link)
    }
  ))
})
}

fit_fit_module_result <- function(id) {
  ns <- NS(id)
  plotOutput(ns("model_plot"))
}


fit_fit_module_rmd <- function(common) {
  # Variables used in the module's Rmd code
  list(
    fit_knit = !is.null(common$fit),
    fit_family = common$meta$fit_fit$family,
    fit_link = common$meta$fit_fit$link,
    fit_iid = common$meta$fit_fit$iid
  )
}

