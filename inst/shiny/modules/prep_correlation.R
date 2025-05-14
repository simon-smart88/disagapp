prep_correlation_module_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # UI
    selectInput(ns("method"), "Correlation matrix method",
                choices = c("Circle" = "circle",
                            "Square" = "square",
                            "Ellipse" = "ellipse",
                            "Number" = "number",
                            "Pie" = "pie",
                            "Shade" = "shade",
                            "Color" = "color")),
    selectInput(ns("type"), "Correlation matrix type",
                choices = c("Lower" = "lower",
                            "Upper" = "upper",
                            "Full" = "full")),
    shinyWidgets::materialSwitch(ns("self"), "Include self-correlations?", value = FALSE, status = "success"),
    actionButton(ns("run"), "Plot correlation matrix", icon = icon("arrow-turn-down")),
    uiOutput(ns("cov_layers_out")),
    actionButton(ns("remove"), "Remove selected covariate")
  )
}

prep_correlation_module_server <- function(id, common, parent_session, map) {
  moduleServer(id, function(input, output, session) {

  output$cov_layers_out <- renderUI({
    watch("prep_correlation")
    selectInput(session$ns("cov_layers"), "Covariate layers to remove", choices = c("", names(common$covs_prep)), multiple = TRUE)})

  observeEvent(input$run, {
    # WARNING ####
    if (is.null(common$covs_prep)) {
      common$logger |> writeLog(type = "error", "Please resample the rasters first")
      return()
    }
    # FUNCTION CALL ####
    corr_matrix <- prep_correlation(common$covs_prep, common$logger)
    # LOAD INTO COMMON ####
    common$covs_matrix <- corr_matrix
    # METADATA ####
    common$meta$prep_correlation$used <- TRUE
    common$meta$prep_correlation$method <- input$method
    common$meta$prep_correlation$type <- input$type
    common$meta$prep_correlation$self <- input$self
    # TRIGGER
    trigger("prep_correlation")
    updateTabsetPanel(parent_session, "main", selected = "Results")
  })

  output$corr_plot <- renderPlot({
    watch("prep_correlation")
    req(common$covs_matrix)
    corrplot::corrplot(common$covs_matrix,
                       method = common$meta$prep_correlation$method,
                       type = common$meta$prep_correlation$type,
                       diag = common$meta$prep_correlation$self)
  })


  observeEvent(input$remove,{
    # WARNING ####
    if (input$cov_layers == "") {
      common$logger |> writeLog(type = "error", "Please select a covariate to remove")
      return()
    }
    # UPDATE COMMON ####
    for (covariate in input$cov_layers){
    common$covs_prep[[covariate]] <- NULL
    if (!is.null(common$covs_prep_lores)){
      common$covs_prep_lores[[covariate]] <- NULL
    }
    }
    # METADATA ####
    common$meta$prep_correlation$removed <- TRUE
    common$meta$prep_correlation$removed_layers <- input$cov_layers
  })

  return(list(
    save = function() {list(
      ### Manual save start
      ### Manual save end
      method = input$method,
      type = input$type,
      cov_layers = input$cov_layers,
      self = input$self)
    },
    load = function(state) {
      ### Manual load start
      ### Manual load end
      updateSelectInput(session, "method", selected = state$method)
      updateSelectInput(session, "type", selected = state$type)
      updateSelectInput(session, "cov_layers", selected = state$cov_layers)
      shinyWidgets::updateMaterialSwitch(session, "self", value = state$self)
    }
  ))
})
}

prep_correlation_module_result <- function(id) {
  ns <- NS(id)
  plotOutput(ns("corr_plot"))
}

prep_correlation_module_rmd <- function(common) {
  # Variables used in the module's Rmd code
  list(
    prep_correlation_knit = !is.null(common$meta$prep_correlation$used),
    prep_correlation_method = common$meta$prep_correlation$method,
    prep_correlation_type = common$meta$prep_correlation$type,
    prep_correlation_self = common$meta$prep_correlation$self,
    prep_correlation_removed = !is.null(common$meta$prep_correlation$removed),
    prep_correlation_removed_layers = common$meta$prep_correlation$removed_layers
  )
}

