pred_transfer_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    selectInput(ns("country"), "Select country", choices = common$countries, multiple = FALSE),
    uiOutput(ns("cov_out")),
    uiOutput(ns("agg_out")),
    actionButton(ns("run"), "Transfer predictions")
  )
}

pred_transfer_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {

    output$cov_out <- renderUI({
      if (!is.null(common$meta$cov_upload$used)){
      fileInput(inputId = session$ns("cov"),
                label = "Upload covariate data",
                multiple = TRUE,
                accept = c(".tif"))
      }
    })

    output$agg_out <- renderUI({
      if (!is.null(common$meta$agg_upload$used)){
        fileInput(inputId = session$ns("agg"),
                  label = "Upload aggregation data",
                  accept = c(".tif"))
      }
    })

  observeEvent(input$run, {


    country_code <- common$countries$ISO3[common$countries$NAME == input$country]

    # FUNCTION CALL ####
    if (is.null(common$meta$cov_upload$used) & is.null(common$meta$agg_upload$used)){
      show_loading_modal("Please wait while the model is transferred to the new area - this will take a long time")
      common$transfer <- pred_transfer(country_code, common)
      close_loading_modal()
    }

    if (!is.null(common$meta$cov_upload$used) & is.null(common$meta$agg_upload$used)){
      if (is.null(input$cov)) {
        common$logger %>% writeLog(type = "error", "Please upload covariates")
        return()
      }
      show_loading_modal("Please wait while the model is transferred to the new area - this will take a long time")
      common$transfer <- pred_transfer(country_code, common, covdf = input$cov)
      close_loading_modal()
    }

    if (is.null(common$meta$cov_upload$used) & !is.null(common$meta$agg_upload$used)){
      if (is.null(input$agg)) {
        common$logger %>% writeLog(type = "error", "Please upload an aggregation raster")
        return()
      }
      show_loading_modal("Please wait while the model is transferred to the new area - this will take a long time")
      common$transfer <- pred_transfer(country_code, common, covdf = input$cov)
      close_loading_modal()
    }

    if (!is.null(common$meta$cov_upload$used) & !is.null(common$meta$agg_upload$used)){
      if (is.null(input$cov)) {
        common$logger %>% writeLog(type = "error", "Please upload covariates")
        return()
      }
      if (is.null(input$agg)) {
        common$logger %>% writeLog(type = "error", "Please upload an aggregation raster")
        return()
      }
      show_loading_modal("Please wait while the model is transferred to the new area - this will take a long time")
      common$transfer <- pred_transfer(country_code, common, covdf = input$cov, aggdf = input$agg$datapath)
      close_loading_modal()
    }


    # METADATA ####

    # TRIGGER
    gargoyle::trigger("pred_transfer")
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

pred_transfer_module_map <- function(map, common) {
  covariate_map(map, common, common$transfer$prediction, "Transferred prediction (rate)")
  covariate_map(map, common, common$transfer$cases, "Transferred prediction (cases)")
}

pred_transfer_module_rmd <- function(common) {
  # Variables used in the module's Rmd code
  list(
    pred_transfer_knit = !is.null(common$some_object),
    var1 = common$meta$setting1,
    var2 = common$meta$setting2
  )
}

