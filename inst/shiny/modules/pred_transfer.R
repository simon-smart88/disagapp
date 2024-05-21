pred_transfer_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    uiOutput(ns("country_out")),
    uiOutput(ns("cov_out")),
    uiOutput(ns("agg_out")),
    actionButton(ns("run"), "Transfer predictions")
  )
}

pred_transfer_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {

    output$country_out <- renderUI({
      selectInput(session$ns("country"), "Select country", choices = c("", common$countries$NAME), multiple = FALSE)
    })

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
    common$meta$pred_transfer$used <- TRUE
    common$meta$pred_transfer$country <- country_code

    # TRIGGER
    gargoyle::trigger("pred_transfer")
    show_map(parent_session)
  })

  return(list(
    save = function() {
list(country = input$country)
    },
    load = function(state) {
updateSelectInput(session, "country", selected = state$country)
    }
  ))
})
}

pred_transfer_module_map <- function(map, common) {
  covariate_map(map, common, common$transfer$prediction, "Transferred prediction (rate)")
  covariate_map(map, common, common$transfer$cases, "Transferred prediction (cases)")
  for (layer in names(common$transfer$covariates)){
    covariate_map(map, common, common$prep$covariate_rasters[[layer]], paste0(layer, " (transferred)"))
  }
}

pred_transfer_module_rmd <- function(common) {
  list(
    pred_transfer_knit = !is.null(common$meta$pred_transfer$used),
    pred_transfer_country = common$meta$pred_transfer$country
  )
}

