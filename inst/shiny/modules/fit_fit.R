fit_fit_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    radioButtons(ns("family"), "Model family", c("gaussian", "poisson", "binomial"), selected = "poisson"),
    radioButtons(ns("link"), "Model link", c("logit", "log", "identity"), selected = "log"),
    checkboxInput(ns("field"), "Use field", value=TRUE),
    checkboxInput(ns("iid"), "iid", value=TRUE),
    actionButton(ns("run"), "Fit model")
  )
}

fit_fit_module_server <- function(id, common) {
  moduleServer(id, function(input, output, session) {

  observeEvent(input$run, {
    # WARNING ####

    # FUNCTION CALL ####

    show_loading_modal("Please wait while the model is fitted")
    fitted <- disaggregation::disag_model(data = common$prep,
                                          family = input$family,
                                          link = input$link,
                                          iid = input$iid)
    close_loading_modal()
    common$logger %>% writeLog('Model fitting has completed')
    # LOAD INTO COMMON ####
    common$fit <- fitted
    # METADATA ####
    common$meta$fit$family <- input$family
    common$meta$fit$link <- input$link
    common$meta$fit$iid <- input$iid
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
list(fit_fit_field = input$field, 
fit_fit_iid = input$iid, 
fit_fit_family = input$family, 
fit_fit_link = input$link)
    },
    load = function(state) {
updateCheckboxInput(session, "field", value = common$state$fit_fit_field) 
updateCheckboxInput(session, "iid", value = common$state$fit_fit_iid) 
updateRadioButtons(session, "family", selected = common$state$fit_fit_family) 
updateRadioButtons(session, "link", selected = common$state$fit_fit_link)
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
    fit_family = common$meta$fit$family,
    fit_link = common$meta$fit$link,
    fit_iid = common$meta$fit$iid
  )
}

